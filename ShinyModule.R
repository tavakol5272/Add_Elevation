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

# numeric columns with values that can be selected, in three groups
column_choices <- function(data) {
  df <- sf::st_drop_geometry(data)
  not_selectable <- c(names(ignored_height_cols),
                      names(known_height_types)[!known_height_types %in% adaptable_types],
                      "ground.elevation", "egm08.geoid")
  usable <- names(df)[vapply(names(df), function(cl) {
    x <- df[[cl]]
    is.numeric(x) && !inherits(x, c("POSIXt", "Date")) && has_numeric_values(x)
  }, logical(1))]
  usable   <- usable[!usable %in% not_selectable & !grepl("\\.adapted$", usable)]
  standard <- intersect(names(known_height_types), usable)
  possible <- setdiff(intersect(usable, find_possible_height_cols(data)), standard)
  list(standard = standard, possible = possible, other = setdiff(usable, c(standard, possible)))
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
height_histogram <- function(x, title, xlab, thresholds) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NULL)
  brks <- if (!is.null(thresholds)) sort(unique(c(min(x), 0, thresholds, max(x)))) else NULL
  hist_aes <- aes(y = after_stat(density) * ifelse(after_stat(x) < 0, -1, 1))  # negative heights drawn downwards
  hist_geom <- if (length(brks) >= 2) {
    geom_histogram(hist_aes, breaks = brks, color = "darkblue", fill = "skyblue")
  } else {
    geom_histogram(hist_aes, bins = 50, color = "darkblue", fill = "skyblue")
  }
  ggplot(data.frame(height = x), aes(x = height)) +
    hist_geom +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    scale_y_continuous(labels = abs) +
    labs(title = title, x = xlab, y = "Density") +
    theme_minimal()
}

write_histograms <- function(h, track, label, thresholds) {
  file <- paste0(label, "_histograms.pdf")
  pdf(appArtifactPath(file), width = 12, height = 8)
  on.exit(dev.off())
  for (id in c(NA, unique(track))) {
    title <- if (is.na(id)) "Histogram of all tracks" else paste("Histogram of", id)
    p <- height_histogram(if (is.na(id)) h else h[track == id], title, label, thresholds)
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

# one row per possible height column: what happened to it and, for other names, a suggested type
height_overview <- function(data, cand, adapted, reasons, track, ground, geoid, speed) {
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
                else "Not selected."

    sugg <- list(type = NA_character_, confidence = "", note = "")
    advice <- ""
    if (!standard && !cl %in% names(ignored_height_cols) && length(vals) > 0) {
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
adapt_all <- function(data, base, geoid_fun, speed_fun, cols, thresholds) {
  ground <- base$ground
  track  <- base$track
  data$ground.elevation <- ground

  cand <- find_possible_height_cols(data)
  reasons <- vapply(seq_len(nrow(cols)), function(i) why_not_adaptable(data, cols$col[i], cols$type[i]), character(1))
  names(reasons) <- cols$col
  for (cl in names(reasons)[!is.na(reasons)]) {
    logger.warn(paste0("'", cl, "' was not adapted. ", reasons[[cl]]))
  }
  reasons <- reasons[!is.na(reasons)]
  cols    <- cols[!cols$col %in% names(reasons), , drop = FALSE]

  others_with_values <- cand[!cand %in% c(names(known_height_types), names(ignored_height_cols))]
  others_with_values <- others_with_values[vapply(others_with_values, function(cl) has_numeric_values(data[[cl]]), logical(1))]
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
    files <- c(files, write_histograms(h_adapted, track, new_col, thresholds))
    thr_table <- NULL
    if (!is.null(thresholds)) {
      thr_table <- threshold_table(h_adapted, track, timelag, thresholds)
      files <- c(files, write_csv_artefact(thr_table, paste0(new_col, "_threshold_proportions.csv")))
    }
    tables[[new_col]] <- thr_table
  }
  if (length(adapted) == 0) logger.info("No height/altitude column was adapted. Only ground.elevation was added.")

  if (length(cand) > 0) {
    speed <- if (length(others_with_values) > 0) speed_fun() else rep(NA_real_, nrow(data))
    overview <- height_overview(data, cand, adapted, reasons, track, ground, geoid, speed)
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
        hr(),
        h4("Height type per column"),
        uiOutput(ns("standard_types")),
        uiOutput(ns("type_ui")),
        hr(),
        h4("Thresholds"),
        textInput(ns("height_props"), "Height thresholds above ground (m), separated by commas",
                  value = "50,100,200,300,500"),
        actionButton(ns("apply"), "Calculate", class = "btn-primary btn-block"),
        uiOutput(ns("messages")),
        hr(),
        h4("Download"),
        downloadButton(ns("download_zip"), "Download results (zip)", class = "btn-sm")
      ),
      mainPanel(width = 9,
        fluidRow(column(6, uiOutput(ns("hist_col_ui"))),
                 column(6, uiOutput(ns("hist_track_ui")))),
        shinycssloaders::withSpinner(plotOutput(ns("histogram"), height = "55vh")),
        h4("Proportion of locations and of time below each threshold"),
        tableOutput(ns("threshold_table"))
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

  choices  <- column_choices(data)
  standard <- choices$standard
  others   <- c(choices$possible, choices$other)

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
    grouped <- list()
    if (length(standard))         grouped[["Standard Movebank height attributes"]] <- as.list(standard)
    if (length(choices$possible)) grouped[["Possible height columns (by name)"]]  <- as.list(choices$possible)
    if (length(choices$other))    grouped[["Other numeric columns"]]             <- as.list(choices$other)
    tagList(
      selectInput(ns("height_cols"), "Columns for which height above ground is calculated",
                  choices = grouped, selected = standard, multiple = TRUE),
      # set once the column selection exists, so the calculation waits for it
      div(style = "display: none;", textInput(ns("ui_ready"), NULL, value = "yes"))
    )
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
    }))
  })

  # suggested type under each dropdown (only calculated when the dropdown is shown)
  lapply(others, function(cl) {
    output[[paste0("sugg_", type_input_id(cl))]] <- renderUI({
      s <- suggest_type(cl, to_num(data[[cl]]), base()$ground, geoid(), speed())
      txt <- if (is.na(s$type) || !s$type %in% adaptable_types) "No reliable type suggestion. Please check the tag documentation."
             else paste0("Suggested: ", s$type, if (s$confidence != "high") " (uncertain)" else "",
                         ". Please make sure this is correct (e.g. from the tag documentation).")
      p(style = "color: #8a6d3b; font-size: 90%;", txt)
    })
  })

  #### settings used for the calculation
  current_settings <- reactive({
    req(input$ui_ready)
    sel  <- input$height_cols
    sel  <- c(intersect(names(known_height_types), sel), setdiff(intersect(names(data), sel), names(known_height_types)))
    type <- vapply(sel, function(cl) {
      if (cl %in% names(known_height_types)) return(unname(known_height_types[cl]))
      v <- input[[type_input_id(cl)]]
      if (is.null(v)) "unknown" else v
    }, character(1))
    list(cols = data.frame(col = sel, type = unname(type)), thresholds = parse_thresholds(input$height_props))
  })

  # results follow the settings until "Calculate" is clicked the first time, then only on click
  active_settings <- reactive({
    if (input$apply == 0) return(current_settings())
    input$apply
    isolate(current_settings())
  })

  written <- reactiveVal(character(0))

  result <- reactive({
    s <- active_settings()
    res <- adapt_all(data, base(), geoid, speed, s$cols, s$thresholds)
    old <- isolate(written())
    unlink(vapply(setdiff(old, res$files), appArtifactPath, character(1)))   # files of an earlier calculation
    written(res$files)
    res
  })

  output$messages <- renderUI({
    r <- result()
    if (length(r$reasons) == 0) return(NULL)
    div(style = "color: #a94442; margin-top: 8px;",
        lapply(names(r$reasons), function(cl) p(strong(cl), ": not adapted. ", r$reasons[[cl]])))
  })

  #### main panel: histogram and threshold table
  adapted_cols <- reactive(names(result()$data)[endsWith(names(result()$data), ".adapted")])

  output$hist_col_ui <- renderUI({
    cols <- adapted_cols()
    if (length(cols) == 0) return(p("No column was adapted. Select height columns and their type in the sidebar."))
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
    all_tracks <- input$hist_track == "all"
    if (!all_tracks) h <- h[base()$track == input$hist_track]
    title <- if (all_tracks) "Histogram of all tracks" else paste("Histogram of", input$hist_track)
    p <- height_histogram(h, title, input$hist_col, r$thresholds)
    validate(need(!is.null(p), "No height values for this selection."))
    p
  })

  output$threshold_table <- renderTable({
    r <- result()
    req(input$hist_col %in% adapted_cols())
    validate(need(!is.null(r$thresholds), "No thresholds were given."))
    tab <- r$tables[[input$hist_col]]
    is_n <- startsWith(names(tab), "n.loc")
    tab[is_n] <- lapply(tab[is_n], as.integer)
    tab
  }, digits = 3, na = "NA")

  #### download of all artefacts as one zip file
  output$download_zip <- downloadHandler(
    filename = function() paste0("Add_Elevation_results_", Sys.Date(), ".zip"),
    content = function(file) {
      zip::zip(file, files = vapply(written(), appArtifactPath, character(1)), mode = "cherry-pick")
    }
  )

  return(reactive(result()$data))
}
