library(move2)
library(sf)
library(ggplot2)

# The calculations, formulas and artefacts are the same as in the Shiny version of this App
# (branch shiny_version, ShinyModule.R). Only the way the settings are chosen differs.

######################## helpers: height columns (as in the R version) ########################

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
to_num <- function(x) {
  if (is.list(x)) return(rep(NA_real_, length(x)))   # list columns (e.g. Argos locations) have no single values
  suppressWarnings(as.numeric(if (is.factor(x)) as.character(x) else x))
}

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

# combined suggestion from the values and the name (as in the overview table of the R version)
suggest_type <- function(cl, h, ground, geoid, speed) {
  by_name  <- type_from_name(cl)
  by_value <- type_from_values(h, ground, geoid, speed)
  sugg <- if (!is.na(by_value$type) && by_value$confidence %in% c("high", "medium")) by_value
          else if (!is.na(by_name)) list(type = by_name, confidence = "low (from the column name only)", note = by_value$note)
          else by_value
  if (!is.na(by_name) && !is.na(by_value$type) && by_name != by_value$type) {
    sugg$note <- paste0(sugg$note, "; the column name suggests '", by_name, "'")
  }
  sugg
}

# NA if the column can be adapted, otherwise the reason why not
why_not_adaptable <- function(data, cl, type) {
  if (cl %in% names(ignored_height_cols)) return(ignored_height_cols[[cl]])
  if (!cl %in% names(data))               return("Column does not exist in the data.")
  if (type == "barometric") return("Barometric heights are not adapted: they depend on the weather and can be off by tens of metres.")
  if (type == "raw")        return("Raw heights are not adapted: they are text and their reference is not known.")
  if (!type %in% adaptable_types) return("Height type is unknown. Select it in 'Option 2: Height reference type'.")
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

######################## helpers: height column detection ########################


# All height columns of the data, detected ONCE from the data alone (independent of the user's
# selection). The result is used everywhere: option 3, the log and the overview table.
# - standard:   standard Movebank height columns with values that can be adapted
# - possible:   columns whose name looks like a height, with values
# - other:      all other numeric event attributes with values (not detected as heights)
# - not_usable: detected height columns that cannot be adapted (named: reason; short: short reason)
detect_height_columns <- function(data) {
  df <- sf::st_drop_geometry(data)
  nm <- names(df)[!names(df) %in% c("ground.elevation", "egm08.geoid") & !grepl("\\.adapted$", names(df))]
  is_num   <- vapply(nm, function(cl) is.numeric(df[[cl]]) && !inherits(df[[cl]], c("POSIXt", "Date")), logical(1))
  has_vals <- vapply(nm, function(cl) has_numeric_values(df[[cl]]), logical(1))
  detected <- nm[nm %in% find_possible_height_cols(data)]

  why <- vapply(detected, function(cl) {
    type <- if (cl %in% names(known_height_types)) known_height_types[[cl]] else ""
    if (cl %in% names(ignored_height_cols)) return(paste("Ignored.", ignored_height_cols[[cl]]))
    if (!has_vals[[cl]])      return("Column contains no values.")
    if (type == "barometric") return("Barometric heights are not adapted: they depend on the weather and can be off by tens of metres.")
    if (type == "raw")        return("Raw heights are not adapted: they are text and their reference is not known.")
    if (!is_num[[cl]])        return("Column is not numeric.")
    NA_character_
  }, character(1))
  short <- c("Ignored." = "assumed by Argos, not measured", "Barometric" = "barometric", "Raw heights" = "raw",
             "Column contains no values." = "no values", "Column is not numeric." = "not numeric")
  not_usable <- why[!is.na(why)]
  not_usable_short <- vapply(not_usable, function(r) unname(short[startsWith(r, names(short))][1]), character(1))

  usable   <- nm[is_num & has_vals & !nm %in% names(not_usable)]
  standard <- intersect(names(known_height_types), usable)
  possible <- setdiff(intersect(usable, detected), standard)
  list(standard = standard, possible = possible, other = setdiff(usable, c(standard, possible)),
       not_usable = not_usable, not_usable_short = not_usable_short)
}

######################## helpers: artefacts (as in the R version) ########################

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

statistics_table <- function(h, track, dur) {
  one_row <- function(name, hh, dd) {
    w <- weighted_stats(hh, dd)
    data.frame(track = name, n.pts = sum(is.finite(hh)),
               mean.pts.height.above.ground = safe_mean(hh), sd.pts.height.above.ground = safe_sd(hh),
               mean.dur.height.above.ground = w[1], sd.dur.height.above.ground = w[2])
  }
  stats <- do.call(rbind, c(lapply(unique(track), function(id) one_row(id, h[track == id], dur[track == id])),
                            list(one_row("all", h, dur))))
  stats[-(1:2)] <- lapply(stats[-(1:2)], round, 2)
  stats
}

# histogram as ggplot object (NULL if there are no values)
# histogram of the height above ground (the same plot as in the Shiny version):
# - all negative values (below ground, i.e. errors) in ONE orange bar left of 0, with no
#   negative numbers on the axis, so that no height below ground can be read from it
# - the values from 0 upwards in about 20 equal bins
# - the share of the locations with a ground elevation below 0 (sea = TRUE, probably over the sea)
#   in red inside the bars
# - the thresholds as thin dashed lines
# axis limit for outliers. q is the value below which the lowest 1 % of the negative values lie.
# The axis is cut at q only if these 1 % stretch the axis more than twice as far as the other 99 %
# (distance from 0 to q), i.e. only for real outliers: normal and exponential tails stay below this
# (tested), so nothing is cut there. Used for the upper end of the axis, with -x.
lower_axis_limit <- function(x) {
  neg <- x[x < 0]
  if (length(neg) < 2) return(NA_real_)
  q <- unname(quantile(neg, 0.01))
  if (q >= 0 || (q - min(x)) <= 2 * abs(q)) return(NA_real_)
  q
}

# notes = TRUE: the orange and red notes are written under the plot (the Shiny UI shows them as
# text under the plot instead, notes = FALSE)
height_histogram <- function(x, title, xlab, thresholds, sea = NULL, notes = TRUE) {
  if (!is.null(sea)) sea <- sea[is.finite(x)]
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NULL)
  fmt <- function(v) format(round(v), big.mark = ",")
  n_all <- length(x)
  is_neg <- x < 0
  caption <- NULL
  if (notes && any(is_neg)) {
    caption <- sprintf("Orange bar: all %s locations below ground (%.1f %%, lowest: %s m). These values are errors.",
                       fmt(sum(is_neg)), 100 * mean(is_neg), fmt(min(x)))
  }
  if (notes && !is.null(sea) && any(sea, na.rm = TRUE)) {
    caption <- c(caption, sprintf(paste0("Red: %s locations (%.1f %%) have a ground elevation below 0 m, probably over the sea. ",
                                         "Their heights above ground are probably much too high."),
                                  fmt(sum(sea, na.rm = TRUE)), 100 * mean(sea, na.rm = TRUE)))
  }
  # very high outliers are shown in the rightmost bar, so that they do not stretch the axis
  hi <- -lower_axis_limit(-x)
  if (!is.na(hi)) {
    n_hi <- sum(x > hi)
    caption <- c(caption, sprintf("%s location%s above %s m (highest: %s m) %s included in the rightmost bar.",
                                  fmt(n_hi), if (n_hi == 1) "" else "s", fmt(hi), fmt(max(x)),
                                  if (n_hi == 1) "is" else "are"))
    x <- pmin(x, hi)
  }
  # each note on its own line(s), wrapped so that it fits the width of the plot
  caption <- if (length(caption)) paste(unlist(lapply(caption, strwrap, width = 95)), collapse = "\n") else NULL

  # about 20 equal bins from 0 to the highest value; one more bin of the same width left of 0 holds
  # all negative values
  top  <- max(x, 0)
  w    <- if (top > 0) top / 20 else 1
  brks <- seq(-w, (floor(top / w) + 1) * w, by = w)
  x[is_neg] <- -w / 2
  p <- ggplot(data.frame(height = x), aes(x = height)) +
    geom_histogram(aes(y = after_stat(density), fill = after_stat(x) < 0, color = after_stat(x) < 0),
                   breaks = brks, closed = "left") +
    scale_fill_manual(values = c("FALSE" = "skyblue", "TRUE" = "orange"), guide = "none") +
    scale_color_manual(values = c("FALSE" = "darkblue", "TRUE" = "darkorange3"), guide = "none")
  if (!is.null(sea) && any(sea, na.rm = TRUE)) {
    # same density scale as the whole histogram, so the red part is the share of each bar
    p <- p + geom_histogram(data = data.frame(height = x[sea %in% TRUE]),
                            aes(y = after_stat(count) / (n_all * after_stat(width))),
                            breaks = brks, closed = "left", fill = "red", color = "darkred")
  }
  thr <- thresholds[thresholds > 0 & thresholds < top]
  if (length(thr)) {
    # vertical labels; a label is left out if it is too close to the previous one
    lab <- thr[1]
    for (t in thr[-1]) if (t - lab[length(lab)] >= 0.04 * (top + w)) lab <- c(lab, t)
    p <- p +
      geom_vline(xintercept = thr, linetype = "dashed", color = "gray40", linewidth = 0.4) +
      annotate("text", x = lab, y = Inf, label = paste(lab, "m"), angle = 90, hjust = 1.1, vjust = -0.4,
               size = 4, color = "gray30")
  }
  # x axis: only 0 and positive numbers
  pos_breaks <- pretty(c(0, top))
  pos_breaks <- pos_breaks[pos_breaks >= 0 & pos_breaks <= top + w]
  p +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    scale_x_continuous(breaks = pos_breaks, labels = format(pos_breaks, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = title, x = xlab, y = "Density", caption = caption) +
    theme_minimal() +
    theme(text = element_text(size = 16),
          plot.caption = element_text(color = "gray20", hjust = 0, size = 13))
}

write_histograms <- function(h, track, label, thresholds, sea) {
  file <- paste0(label, "_histograms.pdf")
  pdf(appArtifactPath(file), width = 10, height = 6.5)
  on.exit(dev.off())
  for (id in c(NA, unique(track))) {
    title <- if (is.na(id)) "Histogram of all tracks" else paste("Histogram of", id)
    p <- if (is.na(id)) height_histogram(h, title, label, thresholds, sea)
         else height_histogram(h[track == id], title, label, thresholds, sea[track == id])
    if (is.null(p)) logger.info(paste0("No height values for ", title, ", so no histogram was drawn."))
    else print(p)
  }
  logger.info(paste0("The file ", file, " was created."))
  file
}

# one row per track (plus mean and sd across tracks), three columns per threshold
threshold_table <- function(h, track, dur, thresholds) {
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
  prop_table
}

write_csv_artefact <- function(table, file) {
  write.csv(table, appArtifactPath(file), row.names = FALSE)
  logger.info(paste0("The file ", file, " was created."))
  file
}

# one row per detected height column (see detect_height_columns) plus one row per selected column
# that was not detected (extra): what happened to it and, for other names, a suggested type
height_overview <- function(data, cand, extra, not_usable, adapted, reasons, track, ground, geoid, speed) {
  rows <- lapply(cand, function(cl) {
    h        <- to_num(data[[cl]])
    standard <- cl %in% names(known_height_types)
    vals     <- h[is.finite(h)]
    status   <- if (cl %in% names(adapted)) "height above ground calculated"
                else if (cl %in% names(ignored_height_cols)) "ignored (not a measured height)"
                else "height above ground not calculated"
    note     <- if (cl %in% names(adapted)) paste0("New column: ", cl, ".adapted",
                                                   if (cl %in% extra) ". Not detected as a height column; adapted because it was typed (option 2)." else "")
                else if (cl %in% names(not_usable)) not_usable[[cl]]
                else if (cl %in% names(reasons)) paste0(reasons[[cl]], if (cl %in% extra) " Not detected as a height column; listed because it was typed (option 2)." else "")
                else "Not selected."

    sugg <- list(type = NA_character_, confidence = "", note = "")
    advice <- ""
    if (cl %in% extra) {
      # not detected as a height column: no type suggestion (it would be meaningless for e.g. temperature)
      advice <- "Not detected as a height column. Please check the tag documentation."
    } else if (!standard && !cl %in% names(not_usable) && length(vals) > 0) {
      sugg <- suggest_type(cl, h, ground, geoid, speed)
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
  overview
}

######################## calculation (same steps as rFunction of the R version) ########################

# base: ground elevation, tracks, time lags; geoid() and speed() are looked up only when needed
adapt_all <- function(data, detected, base, geoid_fun, speed_fun, cols, thresholds) {
  ground <- base$ground
  track  <- base$track

  # overview rows: the detected height columns (in data order) and the selected, not detected ones
  found <- c(detected$standard, detected$possible, names(detected$not_usable))
  cand  <- names(data)[names(data) %in% found]
  extra <- setdiff(cols$col, cand)
  data$ground.elevation <- ground

  reasons <- vapply(seq_len(nrow(cols)), function(i) why_not_adaptable(data, cols$col[i], cols$type[i]), character(1))
  names(reasons) <- cols$col
  for (cl in names(reasons)[!is.na(reasons)]) {
    logger.warn(paste0("'", cl, "' was not adapted. ", reasons[[cl]]))
  }
  reasons <- reasons[!is.na(reasons)]
  cols    <- cols[!cols$col %in% names(reasons), , drop = FALSE]

  # non-standard columns with values in the overview: they get a type suggestion (needs geoid and speed)
  others_with_values <- detected$possible
  geoid <- rep(NA_real_, nrow(data))
  if (any(cols$type == "ellipsoid") || length(others_with_values) > 0) geoid <- geoid_fun()
  if (any(cols$type == "ellipsoid")) {
    data$egm08.geoid <- geoid
    logger.info("The variable egm08.geoid (EGM2008 geoid height in metre) was added to your data for the ellipsoid correction.")
  }

  timelag <- base$timelag
  if (nrow(cols) > 0 && is.null(thresholds)) {
    logger.info("No height thresholds were provided, so threshold proportions are not calculated.")
  }

  adapted <- list()
  tables  <- list()
  files   <- character(0)
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

    files <- c(files, write_csv_artefact(statistics_table(h_adapted, track, timelag), paste0(new_col, "_statistics.csv")))
    files <- c(files, write_histograms(h_adapted, track, new_col, thresholds, ground < 0))
    thr_table <- NULL
    if (!is.null(thresholds)) {
      thr_table <- threshold_table(h_adapted, track, timelag, thresholds)
      files <- c(files, write_csv_artefact(thr_table, paste0(new_col, "_threshold_proportions.csv")))
    }
    tables[[new_col]] <- thr_table
  }
  if (length(adapted) == 0) logger.info("No height/altitude column was adapted. Only ground.elevation was added.")

  cand <- c(cand, intersect(extra, names(data)))
  if (length(cand) > 0) {
    speed <- if (length(others_with_values) > 0) speed_fun() else rep(NA_real_, nrow(data))
    overview <- height_overview(data, cand, extra, detected$not_usable, adapted, reasons, track, ground, geoid, speed)
    files <- c(files, write_csv_artefact(overview, "height_columns_overview.csv"))
    logger.info("height_columns_overview.csv lists all possible height columns, what was done with them and, for non-standard names, a suggested height type.")
  } else {
    logger.info("No possible height/altitude columns were found in the data.")
  }

  list(data = data, reasons = reasons, thresholds = thresholds, tables = tables, files = files)
}


######################## settings: which column(s) to adapt ########################

# which column(s) to adapt and with which type (option 1: one standard column from the list,
# option 2: a typed column name and its type, option 3: all usable standard Movebank columns)
resolve_height_columns <- function(data, detected, height_selection, height_attr_list,
                                   height_attr_freetext, height_type_freetext) {
  empty <- data.frame(col = character(0), type = character(0))

  if (identical(height_selection, "calculate_all_height_columns")) {
    cols <- detected$standard
    if (length(cols) == 0) {
      logger.warn("None of the standard Movebank height attributes with values is present in the data.")
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

######################## main function ########################

rFunction <- function(data, height_selection = "select_height_attr_list", height_attr_list = "no_selection",
                      height_attr_freetext = NULL, height_type_freetext = "unknown",
                      height_props = "100,200,300,400,500", ...) {

  if (is.null(data) || nrow(data) == 0) {
    logger.info("Input is NULL or has 0 rows, so NULL is returned.")
    return(NULL)
  }

  # lon/lat copy only for the elevation, geoid and speed look-ups; the returned data keep their projection
  data_ll <- if (sf::st_is_longlat(data)) data else sf::st_transform(data, 4326)
  has_pt  <- !sf::st_is_empty(data_ll)
  pts     <- sf::st_sf(geometry = sf::st_geometry(data_ll)[has_pt])

  ##### ground elevation
  ground <- rep(NA_real_, nrow(data))
  # one elevation grid for the area of all locations; zoom 5 = cells of about 3-5 km (finer zooms
  # are not feasible for large, e.g. continental, study areas with this download method)
  ground[has_pt] <- elevatr::get_elev_point(locations = pts, src = "aws", z = 5)$elevation
  logger.info(paste("The variable ground.elevation in metre was added to your data",
                    "(AWS Terrain Tiles, zoom level 5, grid cells of about 3-5 km)."))

  base <- list(ground = ground, track = as.character(mt_track_id(data)),
               timelag = as.numeric(move2::mt_time_lags(data, units = "secs")))

  # geoid undulation N of EGM2008 and speed (m/s): only looked up when they are needed
  geoid_fun <- function() {
    geoid_rast <- terra::rast(getAuxiliaryFilePath("egm_file"))
    v <- terra::vect(pts)
    terra::crs(v) <- terra::crs(geoid_rast)   # both are WGS84 lon/lat; avoids a needless transformation warning
    g <- rep(NA_real_, nrow(data))
    g[has_pt] <- terra::extract(geoid_rast, v)[, 2]
    g
  }
  speed_fun <- function() {
    tryCatch(as.numeric(move2::mt_speed(data_ll, units = "m/s")), error = function(e) {
      logger.warn("Speeds could not be calculated, so no height type can be suggested from the values.")
      rep(NA_real_, nrow(data))
    })
  }

  ##### which height column(s) to adapt; the height columns are detected once from the data
  detected <- detect_height_columns(data)
  for (cl in names(detected$not_usable)) {
    logger.info(paste0("Height column '", cl, "' cannot be adapted: ", detected$not_usable[[cl]]))
  }
  cols <- resolve_height_columns(data, detected, height_selection, height_attr_list,
                                 height_attr_freetext, height_type_freetext)

  ##### adapt the column(s) and write the artefacts (the same steps as in the Shiny version)
  res <- adapt_all(data, detected, base, geoid_fun, speed_fun, cols, parse_thresholds(height_props))
  res$data
}
