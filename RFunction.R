library(move2)
library(sf)
library(elevatr)
library(terra)
library(ggplot2)

######################## helpers: height columns ########################

# Standard Movebank height attributes (column names as in move2) and their height type
known_height_types <- c(
  height_above_ellipsoid      = "ellipsoid",
  height_above_msl            = "msl",
  height_above_mean_sea_level = "msl",
  height_above_ground_level   = "ground_level",
  barometric_height           = "barometric",
  height_raw                  = "raw"
)
adaptable_types <- c("msl", "ellipsoid", "ground_level")

# columns that look like heights but are never adapted
ignored_height_cols <- c(
  argos_altitude = "Altitude assumed by Argos for the location calculation, not a measured height."
)

# lower case, all separators replaced by "_"
norm_name <- function(x) gsub("_+", "_", gsub("[^a-z0-9]+", "_", tolower(x)))

# convert numbers, units and number-like text to plain numeric
to_num <- function(x) suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))

has_numeric_values <- function(x) any(is.finite(to_num(x)))

# mean / sd that return NA instead of NaN when there are no values
safe_mean <- function(x) if (any(is.finite(x))) mean(x, na.rm = TRUE) else NA_real_
safe_sd   <- function(x) if (sum(is.finite(x)) > 1) sd(x, na.rm = TRUE) else NA_real_

# all columns that might contain heights (standard names + names that look like heights)
find_possible_height_cols <- function(data) {
  nm <- setdiff(names(sf::st_drop_geometry(data)), c("ground.elevation", "egm08.geoid"))
  n  <- norm_name(nm)
  looks_like_height <- grepl("height|altitude", n) |
    grepl("(^|_)(alt|elev|elevation|hae|msl|amsl|asl|agl)(_|$)", n)
  is_noise <- grepl("error|accuracy|dop|precision|speed", n) | grepl("\\.adapted$", nm)
  nm[nm %in% names(known_height_types) | nm %in% names(ignored_height_cols) | (looks_like_height & !is_noise)]
}

# type guess from the column name only (NA if the name gives no hint)
type_from_name <- function(cl) {
  if (cl %in% names(known_height_types)) return(unname(known_height_types[cl]))
  n <- norm_name(cl)
  if (grepl("ellipsoid|(^|_)hae(_|$)|wgs84", n))                 return("ellipsoid")
  if (grepl("msl|sea_level|(^|_)a?msl(_|$)|(^|_)asl(_|$)", n))   return("msl")
  if (grepl("(^|_)agl(_|$)|above_ground|ground_level", n))        return("ground_level")
  if (grepl("baro", n))                                           return("barometric")
  NA_character_
}

# type guess from the values: at resting locations the true height above ground is close to 0,
# so the type whose height above ground is closest to 0 there is the most likely one
type_from_values <- function(h, ground, geoid, speed, min_n = 30, speed_max = 1.5) {
  rest <- is.finite(h) & is.finite(ground) & is.finite(geoid) & is.finite(speed) & speed < speed_max
  if (sum(rest) < min_n) {
    return(list(type = NA_character_, confidence = "none",
                note = paste0("too few resting locations (", sum(rest), ") to suggest a type")))
  }
  med <- c(msl          = median(h[rest] - ground[rest]),
           ellipsoid    = median(h[rest] - geoid[rest] - ground[rest]),
           ground_level = median(h[rest]))
  score <- sort(abs(med))
  gap   <- score[2] - score[1]
  conf  <- if (gap > 20) "high" else if (gap > 8) "medium" else "low"
  best  <- names(score)[1]
  if (best %in% c("msl", "ellipsoid") && median(abs(geoid[rest])) < 15) {
    conf <- "low"   # geoid close to 0: msl and ellipsoid look almost the same
  }
  list(type = best, confidence = conf,
       note = paste0("median height above ground at ", sum(rest), " resting locations: ",
                     paste0(names(med), " ", round(med), " m", collapse = ", ")))
}

# which column(s) to adapt and with which type; option 3 adapts standard Movebank names only
resolve_height_columns <- function(data, height_selection, height_attr_list,
                                   height_attr_freetext, height_type_freetext) {
  empty <- data.frame(col = character(0), type = character(0))

  if (identical(height_selection, "calculate_all_height_columns")) {
    cols <- intersect(names(known_height_types), names(data))
    if (length(cols) == 0) {
      logger.warn("None of the standard Movebank height attributes is present in the data.")
      return(empty)
    }
    logger.info(paste0("Standard Movebank height attributes found: ", paste(cols, collapse = ", "), "."))
    return(data.frame(col = cols, type = unname(known_height_types[cols])))
  }

  if (identical(height_selection, "type_height_attr_name")) {
    txt <- if (is.null(height_attr_freetext)) character(0) else trimws(as.character(unlist(height_attr_freetext)))
    if (length(txt) == 0 || !nzchar(txt[1])) {
      logger.warn("You selected to type the column name, but no column name was provided.")
      return(empty)
    }
    cl   <- txt[1]
    type <- if (is.null(height_type_freetext) || !nzchar(height_type_freetext)) "unknown" else height_type_freetext
    if (type == "unknown" && cl %in% names(known_height_types)) {
      type <- unname(known_height_types[cl])
      logger.info(paste0("'", cl, "' is a standard Movebank attribute, so its height type '", type, "' was used."))
    }
    logger.info(paste0("Custom height/altitude column: ", cl, " (height type: ", type, ")."))
    return(data.frame(col = cl, type = type))
  }

  if (is.null(height_attr_list) || identical(height_attr_list, "no_selection")) {
    logger.info("No height/altitude attribute was selected.")
    return(empty)
  }
  cl <- if (identical(height_attr_list, "height_above_msl_or_mean_sea_level")) {
    found <- intersect(c("height_above_msl", "height_above_mean_sea_level"), names(data))
    if (length(found)) found[1] else "height_above_msl"
  } else height_attr_list
  logger.info(paste0("Selected standard Movebank height/altitude attribute: ", cl, "."))
  data.frame(col = cl, type = unname(known_height_types[cl]))
}

# NA if the column can be adapted, otherwise the reason why not
why_not_adaptable <- function(data, cl, type) {
  if (cl %in% names(ignored_height_cols)) return(ignored_height_cols[[cl]])
  if (!cl %in% names(data))               return("Column does not exist in the data.")
  if (type == "barometric") return("Barometric heights are not adapted: they depend on the weather and can be off by tens of metres.")
  if (type == "raw")        return("Raw heights are not adapted: they are text and their reference is not known.")
  if (!type %in% adaptable_types) return("Height type is unknown. Select it in 'Height reference type'.")
  if (!has_numeric_values(data[[cl]])) return("Column contains no numeric values.")
  NA_character_
}

# height above ground for one height type (geoid = geoid undulation N of EGM2008)
adapt_height <- function(h, type, ground, geoid) {
  switch(type,
         msl          = h - ground,
         ellipsoid    = h - geoid - ground,
         ground_level = h)
}

######################## helpers: artefacts ########################

parse_thresholds <- function(height_props) {
  txt <- if (is.null(height_props)) character(0) else trimws(as.character(unlist(height_props)))
  txt <- txt[nzchar(txt)]
  if (length(txt) == 0) return(NULL)
  vals <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt, ",")))))
  if (any(is.na(vals))) logger.warn("Some height thresholds could not be read as numbers and were ignored.")
  vals <- sort(unique(vals[!is.na(vals)]))
  if (length(vals) == 0) NULL else vals
}

# duration-weighted mean and sd
weighted_stats <- function(h, dur) {
  ok <- is.finite(h) & is.finite(dur)
  if (!any(ok) || sum(dur[ok]) <= 0) return(c(NA_real_, NA_real_))
  mu <- sum(h[ok] * dur[ok]) / sum(dur[ok])
  c(mu, sqrt(sum((h[ok] - mu)^2 * dur[ok]) / sum(dur[ok])))
}

write_statistics <- function(h, track, dur, label) {
  one_row <- function(name, hh, dd) {
    w <- weighted_stats(hh, dd)
    data.frame(track = name, n.pts = sum(is.finite(hh)),
               mean.pts.height.above.ground = safe_mean(hh), sd.pts.height.above.ground = safe_sd(hh),
               mean.dur.height.above.ground = w[1], sd.dur.height.above.ground = w[2])
  }
  stats <- do.call(rbind, c(lapply(unique(track), function(id) one_row(id, h[track == id], dur[track == id])),
                            list(one_row("all", h, dur))))
  stats[-(1:2)] <- lapply(stats[-(1:2)], round, 2)
  file <- paste0(label, "_statistics.csv")
  write.csv(stats, appArtifactPath(file), row.names = FALSE)
  logger.info(paste0("The file ", file, " was created."))
}

plot_height_hist <- function(x, title, xlab, thresholds) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    logger.info(paste0("No height values for ", title, ", so no histogram was drawn."))
    return(invisible(NULL))
  }
  brks <- if (!is.null(thresholds)) sort(unique(c(min(x), 0, thresholds, max(x)))) else NULL
  hist_aes <- aes(y = after_stat(density) * ifelse(after_stat(x) < 0, -1, 1))  # negative heights drawn downwards
  hist_geom <- if (length(brks) >= 2) {
    geom_histogram(hist_aes, breaks = brks, color = "darkblue", fill = "skyblue")
  } else {
    geom_histogram(hist_aes, bins = 50, color = "darkblue", fill = "skyblue")
  }
  p <- ggplot(data.frame(height = x), aes(x = height)) +
    hist_geom +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    scale_y_continuous(labels = abs) +
    labs(title = title, x = xlab, y = "Density") +
    theme_minimal()
  print(p)
}

write_histograms <- function(h, track, label, thresholds) {
  file <- paste0(label, "_histograms.pdf")
  pdf(appArtifactPath(file), width = 12, height = 8)
  on.exit(dev.off())
  plot_height_hist(h, "Histogram of all tracks", label, thresholds)
  for (id in unique(track)) plot_height_hist(h[track == id], paste("Histogram of", id), label, thresholds)
  logger.info(paste0("The file ", file, " was created."))
}

# one row per track (plus mean and sd across tracks), three columns per threshold
write_threshold_proportions <- function(h, track, dur, label, thresholds) {
  ids <- unique(track)
  one_track <- function(hh, dd) {
    valid <- is.finite(hh)
    unlist(lapply(thresholds, function(thr) {
      below <- valid & hh < thr
      c(if (any(valid)) sum(below) else NA_real_,   # NA, not 0, for tracks without height values
        if (any(valid)) sum(below) / sum(valid) else NA_real_,
        if (sum(dd[valid], na.rm = TRUE) > 0) sum(dd[below], na.rm = TRUE) / sum(dd[valid], na.rm = TRUE) else NA_real_)
    }))
  }
  m <- do.call(rbind, lapply(ids, function(id) one_track(h[track == id], dur[track == id])))
  colnames(m) <- paste0(rep(c("n.loc", "prop.locs", "prop.dur"), times = length(thresholds)), "_",
                        rep(paste0(format(thresholds, scientific = FALSE, trim = TRUE, drop0trailing = TRUE), "m"),
                            each = 3))
  across_tracks <- function(fun) apply(m, 2, function(x) if (all(is.na(x))) NA_real_ else fun(x, na.rm = TRUE))
  prop_table <- data.frame(track = c(ids, "mean", "sd"),
                           rbind(m, across_tracks(mean), across_tracks(sd)),
                           check.names = FALSE, row.names = NULL)
  is_prop <- startsWith(names(prop_table), "prop.")
  prop_table[is_prop]  <- lapply(prop_table[is_prop], round, 3)
  prop_table[!is_prop & names(prop_table) != "track"] <- lapply(prop_table[!is_prop & names(prop_table) != "track"], round, 0)
  file <- paste0(label, "_threshold_proportions.csv")
  write.csv(prop_table, appArtifactPath(file), row.names = FALSE)
  logger.info(paste0("The file ", file, " was created."))
}

# one row per possible height column: what happened to it and, for other names, a suggested type
write_height_overview <- function(data, cand, adapted, reasons, track, ground, geoid, speed) {
  rows <- lapply(cand, function(cl) {
    h        <- to_num(data[[cl]])
    standard <- cl %in% names(known_height_types)
    vals     <- h[is.finite(h)]
    status   <- if (cl %in% names(adapted)) "height above ground calculated"
                else if (cl %in% names(ignored_height_cols)) "ignored (not a measured height)"
                else "height above ground not calculated"
    note     <- if (cl %in% names(adapted)) paste0("New column: ", cl, ".adapted")
                else if (cl %in% names(ignored_height_cols)) paste("Ignored.", ignored_height_cols[[cl]])
                else if (cl %in% names(reasons)) reasons[[cl]]
                else if (!standard && !cl %in% names(ignored_height_cols)) "Not a standard Movebank name, so it is only adapted when typed by name (option 2)."
                else "Not selected."

    sugg <- list(type = NA_character_, confidence = "", note = "")
    advice <- ""
    if (!standard && !cl %in% names(ignored_height_cols) && length(vals) > 0) {
      by_name  <- type_from_name(cl)
      by_value <- type_from_values(h, ground, geoid, speed)
      sugg <- if (!is.na(by_value$type) && by_value$confidence %in% c("high", "medium")) by_value
              else if (!is.na(by_name)) list(type = by_name, confidence = "low (from the column name only)", note = by_value$note)
              else by_value
      if (!is.na(by_name) && !is.na(by_value$type) && by_name != by_value$type) {
        sugg$note <- paste0(sugg$note, "; the column name suggests '", by_name, "'")
      }
      advice <- if (cl %in% names(adapted)) {
        if (!is.na(sugg$type) && sugg$type != adapted[[cl]] && sugg$confidence == "high") {
          paste0("The values look more like type '", sugg$type, "' than '", adapted[[cl]], "'. Please check the type you selected.")
        } else ""
      } else if (!is.na(sugg$type) && sugg$type %in% adaptable_types) {
        paste0(if (sugg$confidence != "high") "This suggestion is uncertain. " else "",
               "Please make sure this is correct (e.g. from the tag documentation). Then run the App again with ",
               "'Type the exact height/altitude column name' = ", cl, " and 'Height reference type' = ", sugg$type, ".")
      } else {
        "No reliable type suggestion. Please check the tag documentation."
      }
    }

    data.frame(column = cl,
               standard_movebank_name = standard,
               status = status,
               type_used = if (cl %in% names(adapted)) adapted[[cl]] else "",
               note = note,
               n_tracks_with_values = length(unique(track[is.finite(h)])),
               min = if (length(vals)) min(vals) else NA_real_,
               median = if (length(vals)) median(vals) else NA_real_,
               max = if (length(vals)) max(vals) else NA_real_,
               suggested_type = if (is.na(sugg$type)) "" else sugg$type,
               suggestion_basis = sugg$note,
               advice = advice)
  })
  overview <- do.call(rbind, rows)
  write.csv(overview, appArtifactPath("height_columns_overview.csv"), row.names = FALSE)
  logger.info("The file height_columns_overview.csv was created. It lists all possible height columns, what was done with them and, for non-standard names, a suggested height type.")

  is_adapted <- overview$column %in% names(adapted)
  todo <- overview[!is_adapted & overview$advice != "" & overview$suggested_type %in% adaptable_types, ]
  for (i in seq_len(nrow(todo))) {
    logger.info(paste0("Possible height column '", todo$column[i], "': suggested type '", todo$suggested_type[i],
                       "'. ", todo$advice[i]))
  }
  mismatch <- overview[is_adapted & overview$advice != "", ]
  for (i in seq_len(nrow(mismatch))) {
    logger.warn(paste0("'", mismatch$column[i], "': ", mismatch$advice[i]))
  }
}

######################## main function ########################

rFunction <- function(data, height_selection = "select_height_attr_list", height_attr_list = "no_selection",
                      height_attr_freetext = NULL, height_type_freetext = "unknown",
                      height_props = NULL, ...) {

  if (is.null(data) || nrow(data) == 0) {
    logger.info("Input is NULL or has 0 rows, so NULL is returned.")
    return(NULL)
  }

  # lon/lat copy only for the elevation, geoid and speed look-ups; the returned data keep their projection
  data_ll <- if (sf::st_is_longlat(data)) data else sf::st_transform(data, 4326)
  has_pt  <- !sf::st_is_empty(data_ll)
  pts     <- sf::st_sf(geometry = sf::st_geometry(data_ll)[has_pt])
  track   <- as.character(mt_track_id(data))

  ##### ground elevation
  ground <- rep(NA_real_, nrow(data))
  # one elevation grid for the area of all locations; zoom 5 = cells of about 3-5 km (finer zooms
  # are not feasible for large, e.g. continental, study areas with this download method)
  ground[has_pt] <- elevatr::get_elev_point(locations = pts, src = "aws", z = 5)$elevation
  data$ground.elevation <- ground
  logger.info(paste("The variable ground.elevation in metre was added to your data",
                    "(AWS Terrain Tiles, zoom level 5, grid cells of about 3-5 km)."))

  ##### which height column(s) to adapt
  cand <- find_possible_height_cols(data)
  cols <- resolve_height_columns(data, height_selection, height_attr_list,
                                 height_attr_freetext, height_type_freetext)
  reasons <- vapply(seq_len(nrow(cols)), function(i) why_not_adaptable(data, cols$col[i], cols$type[i]), character(1))
  names(reasons) <- cols$col
  for (cl in names(reasons)[!is.na(reasons)]) {
    logger.warn(paste0("'", cl, "' was not adapted. ", reasons[[cl]]))
  }
  reasons <- reasons[!is.na(reasons)]
  cols    <- cols[!cols$col %in% names(reasons), , drop = FALSE]

  ##### geoid undulation N: for ellipsoid heights and for the type suggestions of other columns
  others_with_values <- cand[!cand %in% c(names(known_height_types), names(ignored_height_cols))]
  others_with_values <- others_with_values[vapply(others_with_values, function(cl) has_numeric_values(data[[cl]]), logical(1))]
  geoid <- rep(NA_real_, nrow(data))
  if (any(cols$type == "ellipsoid") || length(others_with_values) > 0) {
    geoid_rast <- terra::rast(getAuxiliaryFilePath("egm_file"))
    v <- terra::vect(pts)
    terra::crs(v) <- terra::crs(geoid_rast)   # both are WGS84 lon/lat; avoids a needless transformation warning
    geoid[has_pt] <- terra::extract(geoid_rast, v)[, 2]
  }
  if (any(cols$type == "ellipsoid")) {
    data$egm08.geoid <- geoid
    logger.info("The variable egm08.geoid (EGM2008 geoid height in metre) was added to your data for the ellipsoid correction.")
  }

  ##### time lag (seconds) for the duration-weighted statistics; not added to the data
  timelag    <- as.numeric(move2::mt_time_lags(data, units = "secs"))
  thresholds <- parse_thresholds(height_props)
  if (nrow(cols) > 0 && is.null(thresholds)) {
    logger.info("No height thresholds were provided, so threshold proportions are not calculated.")
  }

  ##### adapt each column and write its artefacts
  adapted <- list()
  for (i in seq_len(nrow(cols))) {
    cl      <- cols$col[i]
    type    <- cols$type[i]
    new_col <- paste0(cl, ".adapted")

    h_adapted <- adapt_height(to_num(data[[cl]]), type, ground, geoid)
    if (new_col %in% names(data)) logger.warn(paste0("The existing column '", new_col, "' was overwritten."))
    data[[new_col]] <- h_adapted
    adapted[[cl]] <- type
    logger.info(paste0("'", cl, "' (", type, ") was adapted to height above ground. New column: ", new_col, "."))

    n_per_track <- tapply(is.finite(h_adapted), track, sum)
    no_vals <- names(n_per_track)[n_per_track == 0]
    if (length(no_vals)) {
      logger.warn(paste0(new_col, " has no values for track(s): ", paste(no_vals, collapse = ", "), "."))
    }

    write_statistics(h_adapted, track, timelag, new_col)
    write_histograms(h_adapted, track, new_col, thresholds)
    if (!is.null(thresholds)) write_threshold_proportions(h_adapted, track, timelag, new_col, thresholds)
  }
  if (length(adapted) == 0) logger.info("No height/altitude column was adapted. Only ground.elevation was added.")

  ##### overview of all possible height columns (incl. type suggestions for non-standard names)
  if (length(cand) > 0) {
    speed <- rep(NA_real_, nrow(data))
    if (length(others_with_values) > 0) {
      speed <- tryCatch(as.numeric(move2::mt_speed(data_ll, units = "m/s")), error = function(e) {
        logger.warn("Speeds could not be calculated, so no height type can be suggested from the values.")
        rep(NA_real_, nrow(data))
      })
    }
    write_height_overview(data, cand, adapted, reasons, track, ground, geoid, speed)
  } else {
    logger.info("No possible height/altitude columns were found in the data.")
  }

  return(data)
}
