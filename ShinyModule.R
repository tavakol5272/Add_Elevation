library("shiny")
library("move2")
library("sf")
library("ggplot2")

# The calculations, formulas and artefacts are the same as in the R version of this App
# (branch new_R_version, RFunction.R). Only the way the settings are chosen differs.

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
  if (!type %in% adaptable_types) return("Height type is unknown. Select its height type in the sidebar.")
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

######################## helpers: column choices of the UI ########################

# All height columns of the data, detected ONCE from the data alone (independent of the user's
# selection). The result is used everywhere: the dropdown groups, the "not usable" line under the
# dropdown, the overview table and the message for data without height values.
# - standard:   standard Movebank height columns with values that can be adapted
# - possible:   columns whose name looks like a height, with values
# - other:      all other numeric event attributes with values (selectable, not detected as heights)
# - not_usable: detected height columns that cannot be adapted (named: reason; short: for the UI)
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

# input ID of the height type dropdown of a non-standard column
type_input_id <- function(cl) paste0("type_", gsub("[^A-Za-z0-9_]", "_", cl))

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
# histogram of the height above ground (the same plot in the UI and in the PDF):
# - all negative values (below ground, i.e. errors) in ONE orange bar left of 0, labelled "below 0"
#   instead of negative numbers, so that no height below ground can be read from the axis
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

# notes = TRUE (PDF): the orange and red notes are written under the plot; the UI shows them as
# text under the plot instead (notes = FALSE)
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

# short version of the threshold table for the UI only (one row per threshold, for one track or
# the mean +- sd across tracks); the artefact keeps the full table
threshold_summary <- function(tab, track_id) {
  thr  <- sub("^n\\.loc_", "", names(tab)[startsWith(names(tab), "n.loc_")])
  pct  <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f %%", 100 * x))
  pm   <- function(m, s) ifelse(is.na(m), "NA",
                                ifelse(is.na(s), sprintf("%.1f %%", 100 * m),
                                       sprintf("%.1f ± %.1f %%", 100 * m, 100 * s)))
  out  <- data.frame(Threshold = sub("m$", " m", thr), check.names = FALSE)
  if (track_id == "all") {
    m <- tab[tab$track == "mean", ]
    s <- tab[tab$track == "sd", ]
    out[["% of locations (mean ± sd)"]] <- pm(unlist(m[paste0("prop.locs_", thr)]), unlist(s[paste0("prop.locs_", thr)]))
    out[["% of time (mean ± sd)"]]      <- pm(unlist(m[paste0("prop.dur_", thr)]),  unlist(s[paste0("prop.dur_", thr)]))
  } else {
    r <- tab[tab$track == track_id, ]
    out[["Locations below"]] <- as.integer(unlist(r[paste0("n.loc_", thr)]))
    out[["% of locations"]]  <- pct(unlist(r[paste0("prop.locs_", thr)]))
    out[["% of time"]]       <- pct(unlist(r[paste0("prop.dur_", thr)]))
  }
  out
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
                                                   if (cl %in% extra) ". Not detected as a height column; adapted because it was selected." else "")
                else if (cl %in% names(not_usable)) not_usable[[cl]]
                else if (cl %in% names(reasons)) paste0(reasons[[cl]], if (cl %in% extra) " Not detected as a height column; listed because it was selected." else "")
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
               "Please make sure this is correct (e.g. from the tag documentation). Then select ", cl,
               " under 'Height columns' and set its height type to '", sugg$type, "'.")
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

######################## UI ########################

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel("Add Elevation and Height Above Ground"),
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Height columns"),
        uiOutput(ns("column_ui")),
        uiOutput(ns("not_usable_ui")),
        hr(),
        h4("Height type per column"),
        uiOutput(ns("standard_types")),
        uiOutput(ns("type_ui")),
        hr(),
        h4("Thresholds for proportions"),
        textInput(ns("height_props"), "Heights above ground (m), separated by commas",
                  value = "100,200,300,400,500"),
        helpText("Used only for the table and the dashed lines in the histogram.",
                 "They do not change the calculated heights above ground."),
        actionButton(ns("apply"), "Calculate", class = "btn-primary btn-block"),
        uiOutput(ns("messages")),
        hr(),
        h4("Download"),
        downloadButton(ns("download_zip"), "Download results (zip)", class = "btn-sm")
      ),
      mainPanel(width = 9,
        uiOutput(ns("main_warning")),
        fluidRow(column(6, uiOutput(ns("hist_col_ui"))),
                 column(6, uiOutput(ns("hist_track_ui"))), style = "margin-bottom: 20px;"),
        shinycssloaders::withSpinner(plotOutput(ns("histogram"), height = "42vh")),
        uiOutput(ns("negative_note")),
        div(style = "margin-top: 30px;", uiOutput(ns("table_title"))),
        tableOutput(ns("threshold_table")),
        helpText("% of time = share of the tracking time spent below the threshold.",
                 "The full tables per track are in the download.")
      )
    )
  )
}

######################## server ########################

shinyModule <- function(input, output, session, data) {
  ns <- session$ns

  if (is.null(data) || nrow(data) == 0) {
    logger.info("Input is NULL or has 0 rows, so NULL is returned.")
    output$column_ui <- renderUI(p("The input data are empty."))
    return(reactive(NULL))
  }

  choices  <- detect_height_columns(data)   # detected once, used everywhere
  standard <- choices$standard
  others   <- c(choices$possible, choices$other)

  # default selection when the App is opened without stored settings: the most probable height
  # column. First a standard column (ellipsoid = the GPS height itself, then sea level, then ground
  # level); if there is none, the possible height column (by name) with the most values
  default_col <- {
    pri <- intersect(c("height_above_ellipsoid", "height_above_msl", "height_above_mean_sea_level",
                       "height_above_ground_level"), standard)
    if (length(pri)) pri[1]
    else if (length(choices$possible)) {
      n_vals <- vapply(choices$possible, function(cl) sum(is.finite(to_num(data[[cl]]))), numeric(1))
      choices$possible[which.max(n_vals)]
    } else character(0)
  }

  #### ground elevation, tracks and time lags: looked up once
  base <- reactive({
    withProgress(message = "Looking up the ground elevation ...", value = 0.5, {
      data_ll <- if (sf::st_is_longlat(data)) data else sf::st_transform(data, 4326)
      has_pt  <- !sf::st_is_empty(data_ll)
      pts     <- sf::st_sf(geometry = sf::st_geometry(data_ll)[has_pt])
      ground  <- rep(NA_real_, nrow(data))
      # one elevation grid for the area of all locations; zoom 5 = cells of about 3-5 km
      ground[has_pt] <- elevatr::get_elev_point(locations = pts, src = "aws", z = 5)$elevation
      logger.info(paste("The variable ground.elevation in metre was added to your data",
                        "(AWS Terrain Tiles, zoom level 5, grid cells of about 3-5 km)."))
      list(data_ll = data_ll, has_pt = has_pt, pts = pts, ground = ground,
           track = as.character(mt_track_id(data)),
           timelag = as.numeric(move2::mt_time_lags(data, units = "secs")))
    })
  })

  # geoid undulation N of EGM2008
  geoid <- reactive({
    b <- base()
    geoid_rast <- terra::rast(getAuxiliaryFilePath("egm_file"))
    v <- terra::vect(b$pts)
    terra::crs(v) <- terra::crs(geoid_rast)   # both are WGS84 lon/lat; avoids a needless transformation warning
    g <- rep(NA_real_, nrow(data))
    g[b$has_pt] <- terra::extract(geoid_rast, v)[, 2]
    g
  })

  # speed (m/s) for the type suggestion
  speed <- reactive({
    tryCatch(as.numeric(move2::mt_speed(base()$data_ll, units = "m/s")), error = function(e) {
      logger.warn("Speeds could not be calculated, so no height type can be suggested from the values.")
      rep(NA_real_, nrow(data))
    })
  })

  #### sidebar: column selection and height types
  output$column_ui <- renderUI({
    # every group is shown; an empty group gets a "(none in this dataset)" line, which is ignored
    group <- function(cols, id) if (length(cols)) as.list(cols) else setNames(list(id), "(none in this dataset)")
    grouped <- list(
      "Standard Movebank height attributes" = group(standard, "none_standard"),
      "Possible height columns (by name)"   = group(choices$possible, "none_possible"),
      "Other event attributes"              = group(choices$other, "none_other")
    )
    tagList(
      selectInput(ns("height_cols"), "Columns for which height above ground is calculated",
                  choices = grouped, selected = default_col, multiple = TRUE),
      # set once the column selection exists, so the calculation waits for it
      div(style = "display: none;", textInput(ns("ui_ready"), NULL, value = "yes"))
    )
  })

  # detected height columns that cannot be adapted (also listed in the overview table)
  output$not_usable_ui <- renderUI({
    nu <- choices$not_usable_short
    if (length(nu) == 0) return(NULL)
    p(style = "color: gray; font-size: 90%;",
      "Not usable: ", paste0(names(nu), " (", nu, ")", collapse = ", "))
  })

  output$standard_types <- renderUI({
    sel <- intersect(standard, input$height_cols)
    if (length(sel) == 0) return(NULL)
    tagList(lapply(sel, function(cl) p(strong(cl), ": ", known_height_types[[cl]], " (standard Movebank attribute)")))
  })

  # one type dropdown per non-standard column, shown only when the column is selected
  output$type_ui <- renderUI({
    tagList(lapply(others, function(cl) {
      cl_js <- gsub("(['\\\\])", "\\\\\\1", cl)
      conditionalPanel(
        condition = sprintf("(input.height_cols || []).indexOf('%s') > -1", cl_js), ns = ns,
        selectInput(ns(type_input_id(cl)), cl,
                    choices = c("unknown / do not adapt" = "unknown", "height above mean sea level" = "msl",
                                "height above ellipsoid" = "ellipsoid", "height above ground level" = "ground_level"),
                    selected = "unknown"),
        uiOutput(ns(paste0("sugg_", type_input_id(cl))))
      )
    }),
    # set once the type dropdowns exist, so the first calculation waits for them
    div(style = "display: none;", textInput(ns("types_ready"), NULL, value = "yes")))
  })

  # suggested type under each dropdown (only calculated when the dropdown is shown)
  lapply(others, function(cl) {
    output[[paste0("sugg_", type_input_id(cl))]] <- renderUI({
      # no suggestion for columns not detected as heights: for other values (e.g. temperature) it
      # would be meaningless
      if (cl %in% choices$other) {
        return(p(style = "color: #8a6d3b; font-size: 90%;",
                 "Not detected as a height column. Please check the tag documentation."))
      }
      s <- suggest_type(cl, to_num(data[[cl]]), base()$ground, geoid(), speed())
      txt <- if (is.na(s$type) || !s$type %in% adaptable_types) "No reliable type suggestion. Please check the tag documentation."
             else paste0("Suggested: ", s$type, if (s$confidence != "high") " (uncertain)" else "",
                         ". Please make sure this is correct (e.g. from the tag documentation).")
      p(style = "color: #8a6d3b; font-size: 90%;", txt)
    })
  })

  #### settings used for the calculation
  current_settings <- reactive({
    req(input$ui_ready, input$types_ready)
    sel  <- intersect(input$height_cols, names(data))   # drops the "(none in this dataset)" lines
    sel  <- c(intersect(names(known_height_types), sel), setdiff(intersect(names(data), sel), names(known_height_types)))
    type <- vapply(sel, function(cl) {
      if (cl %in% names(known_height_types)) return(unname(known_height_types[cl]))
      v <- input[[type_input_id(cl)]]
      if (is.null(v)) "unknown" else v
    }, character(1))
    list(cols = data.frame(col = sel, type = unname(type)), thresholds = parse_thresholds(input$height_props))
  })

  # one calculation at the start with the stored (or default) settings, so that the next App always
  # gets the data; after that, changes in the sidebar are only calculated when "Calculate" is clicked
  start_settings <- reactiveVal(NULL)
  observe({
    s <- current_settings()
    if (is.null(isolate(start_settings()))) start_settings(s)
  })
  # The start calculation uses what the sidebar shows (defaults or stored settings). Only clicks made
  # after the App was opened count: "Store settings" also stores the click counter of the button,
  # so it can start with a value > 0
  apply_start <- reactiveVal(NULL)
  observeEvent(input$apply, apply_start(as.numeric(input$apply)), once = TRUE)
  active_settings <- reactive({
    req(!is.null(apply_start()))
    if (as.numeric(input$apply) == apply_start()) {
      req(start_settings())
      return(start_settings())
    }
    isolate(current_settings())
  })

  written <- reactiveVal(character(0))

  result <- reactive({
    s <- active_settings()
    res <- adapt_all(data, choices, base(), geoid, speed, s$cols, s$thresholds)
    old <- isolate(written())
    unlink(vapply(setdiff(old, res$files), appArtifactPath, character(1)))   # files of an earlier calculation
    written(res$files)
    res
  })

  # warning at the top of the main panel for selected columns that were not converted
  output$main_warning <- renderUI({
    r <- result()
    if (length(r$reasons) == 0) return(NULL)
    div(style = "color: #a94442; font-weight: bold; margin-bottom: 10px;",
        lapply(names(r$reasons), function(cl) {
          unknown <- startsWith(r$reasons[[cl]], "Height type is unknown")
          p("⚠ ", cl, " was not converted: ",
            if (unknown && cl %in% choices$other) "its height type is unknown. Set the type (e.g. from the tag documentation) and click 'Calculate'."
            else if (unknown) "its height type is unknown. Check the suggestion under its type in the sidebar, set the type and click 'Calculate'."
            else r$reasons[[cl]])
        }))
  })

  output$messages <- renderUI({
    # columns that were not converted are shown in the main panel (main_warning)
    if (identical(current_settings(), active_settings())) return(NULL)
    p(style = "color: #8a6d3b; margin-top: 8px;", "Settings changed. Click 'Calculate' to update the results.")
  })

  #### main panel: histogram and threshold table
  adapted_cols <- reactive(names(result()$data)[endsWith(names(result()$data), ".adapted")])

  output$hist_col_ui <- renderUI({
    cols <- adapted_cols()
    if (length(cols) == 0) {
      if (length(standard) + length(choices$possible) == 0) {
        # no usable height column in the data (the same detection as in the dropdown and the overview)
        nu <- choices$not_usable_short
        all_empty <- length(nu) > 0 && all(nu == "no values")
        listed <- if (length(nu) > 1) paste(paste(head(names(nu), -1), collapse = ", "), "and", tail(names(nu), 1)) else names(nu)
        return(p(style = "font-weight: bold; font-size: 120%;",
                 if (all_empty) paste0("This dataset has no height values (", listed,
                                       if (length(nu) == 1) " is" else " are", " empty).")
                 else if (length(nu)) paste0("This dataset has no usable height column (", listed,
                                             if (length(nu) == 1) " is" else " are", " not usable, see the sidebar).")
                 else "This dataset has no height columns.",
                 br(),
                 "Only the ground elevation was added to the data."))
      }
      if (length(result()$reasons)) return(NULL)   # the warning above already explains it
      return(p("No column was adapted. Select height columns and their type in the sidebar."))
    }
    selectInput(ns("hist_col"), "Column", choices = cols, selected = cols[1])
  })

  output$hist_track_ui <- renderUI({
    req(length(adapted_cols()) > 0)
    selectInput(ns("hist_track"), "Track", choices = c("All tracks" = "all", unique(base()$track)), selected = "all")
  })

  output$histogram <- renderPlot({
    r <- result()
    req(input$hist_col %in% adapted_cols(), input$hist_track)
    h <- r$data[[input$hist_col]]
    sea <- base()$ground < 0
    all_tracks <- input$hist_track == "all"
    if (!all_tracks) {
      h   <- h[base()$track == input$hist_track]
      sea <- sea[base()$track == input$hist_track]
    }
    title <- if (all_tracks) "Histogram of all tracks" else paste("Histogram of", input$hist_track)
    p <- height_histogram(h, title, input$hist_col, r$thresholds, sea = sea, notes = FALSE)   # the UI shows these notes under the plot
    validate(need(!is.null(p), "No height values for this selection."))
    p
  })

  # share of locations below ground for the shown column and track
  output$negative_note <- renderUI({
    r <- result()
    req(input$hist_col %in% adapted_cols(), input$hist_track)
    h <- r$data[[input$hist_col]]
    ground <- base()$ground
    if (input$hist_track != "all") {
      keep <- base()$track == input$hist_track
      h <- h[keep]
      ground <- ground[keep]
    }
    ok <- is.finite(h)
    h <- h[ok]
    ground <- ground[ok]
    if (length(h) == 0) return(NULL)
    below_ground <- if (any(h < 0)) {
      p(style = "color: #b35900;",
        "⚠ ", strong(sprintf("%.1f %% of the locations", 100 * mean(h < 0))), " are below ground (orange). ",
        "These values are errors, caused by the GPS height error or the coarse ground elevation (3-5 km cells).")
    }
    over_sea <- if (any(ground < 0, na.rm = TRUE)) {
      p(style = "color: #c00000;",
        "⚠ ", strong(sprintf("%.1f %% of the locations", 100 * mean(ground < 0, na.rm = TRUE))),
        " (red) have a ground elevation below 0 m, probably over the sea. ",
        "Their heights above ground are probably much too high.")
    }
    tagList(below_ground, over_sea)
  })

  output$table_title <- renderUI({
    req(input$hist_col %in% adapted_cols(), input$hist_track)
    h4("Below each height threshold: ",
       if (input$hist_track == "all") "all tracks" else input$hist_track)
  })

  output$threshold_table <- renderTable({
    r <- result()
    req(input$hist_col %in% adapted_cols(), input$hist_track)
    validate(need(!is.null(r$thresholds), "No thresholds were given."))
    threshold_summary(r$tables[[input$hist_col]], input$hist_track)
  }, align = "l", na = "NA")

  #### download of all artefacts as one zip file
  output$download_zip <- downloadHandler(
    filename = function() paste0("Add_Elevation_results_", Sys.Date(), ".zip"),
    content = function(file) {
      zip::zip(file, files = vapply(written(), appArtifactPath, character(1)), mode = "cherry-pick")
    }
  )

  return(reactive(result()$data))
}
