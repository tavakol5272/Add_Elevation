library('move2')
library('sf')
library('elevatr')
library('terra')


rFunction <- function(data, height_attr_list, height_attr_freetext, height_type_freetext, height_props = NULL, ...) {
  
  #input_data <- data
  
  if (is.null(data) || nrow(data) == 0) {
    logger.info("Input is NULL or has 0 rows — returning NULL.")
    return(NULL)
  }
  
  if (!sf::st_is_longlat(data)) data <- sf::st_transform(data, 4326)
  
  # Extract coordinates
  locs <- as.data.frame(sf::st_coordinates(data))
  locs <- locs[, c("X", "Y")]
  names(locs) <- c("x", "y")
  locs_sf <- sf::st_as_sf(locs,coords = c("x", "y"),crs = sf::st_crs(data))
  
  # ground elevation
  data$ground.elevation <- elevatr::get_elev_point(locations = locs_sf, src = "aws")$elevation
  logger.info("The variable ground.elevation in metre was added to your data.")
  
  ############
  #################### Helper: suggest possible height/altitude columns ####################
  
  log_possible_height_columns <- function(data) {
    
    height_ix <- grep("height", names(data), ignore.case = TRUE)
    altitude_ix <- grep("altitude", names(data), ignore.case = TRUE)
    possible_ix <- unique(c(height_ix, altitude_ix))
    
    possible_cols <- names(data)[possible_ix]
    
    if (length(possible_cols) == 0) {
      logger.info("No possible height/altitude columns were found in the data.")
      return(NULL)
    }
    
    possible_cols_with_values <- possible_cols[sapply(possible_cols, function(col) {
      sum(!is.na(data[[col]])) > 0
    })]
    
    if (length(possible_cols_with_values) > 0) {
      logger.info(paste0(
        "Possible height/altitude columns found: ",
        paste(possible_cols_with_values, collapse = ", "),
        ". Please choose 'other', enter the exact column name, and specify the height type."
      ))
    } else {
      logger.info(paste0(
        "Possible height/altitude columns were found, but they contain only NA values: ",
        paste(possible_cols, collapse = ", ")
      ))
    }
  }
  #################################### Select height/altitude column ###################
  
  if (height_attr_list == "no_selection") {
    height_col <- NULL
    logger.info("No height/altitude attribute selected.")
    log_possible_height_columns(data)
    return(data)
    
  } else if (height_attr_list == "height_above_msl_or_mean_sea_level") {
    
    possible_cols <- c("height_above_mean_sea_level", "height_above_msl")
    height_col <- intersect(possible_cols, names(data))[1]
    
    if (is.na(height_col)) {
      height_col <- NULL
      logger.info("Neither height_above_mean_sea_level nor height_above_msl exists in the data.")
      log_possible_height_columns(data)
      return(data)
      
    } else {
      logger.info(paste0("Selected height above mean sea level attribute: ", height_col))
    }
    
  } else if (height_attr_list == "other") {
    
    if (
      is.null(height_attr_freetext) ||
      length(height_attr_freetext) == 0 ||
      identical(height_attr_freetext, list()) ||
      trimws(as.character(height_attr_freetext)) == ""
    ) {
      height_col <- NULL
      logger.info("You selected 'other', but did not provide a custom height/altitude column name. Only ground elevation will be added.")
      log_possible_height_columns(data)
      return(data)
      
    } else {
      height_col <- trimws(height_attr_freetext)
      logger.info(paste0("Custom height/altitude attribute provided: ", height_col))
    }
    
  } else {
    
    height_col <- height_attr_list
    logger.info(paste0("Selected standard Movebank height/altitude attribute: ", height_col))
  }
  

  if (is.null(height_col)) {return(data)}
  
  # Check that provided column exists
  if (!height_col %in% names(data)) {
    logger.info(paste0( "The selected/provided height attribute '", height_col, "' does not exist in the data set."))
    log_possible_height_columns(data)
    return(data)
  }
  
  # Extract selected height values
  hei_0 <- as.numeric(data[[height_col]])
  
  if (all(is.na(hei_0))) {
    logger.info(paste0("The selected height/altitude attribute '",height_col, "' contains only NA values."))
    log_possible_height_columns(data)
    return(data)
  }
  
  
  ##################### Calculate height above ground  #######################################
  if (
    height_col %in% c("height_above_mean_sea_level", "height_above_msl") ||
    (height_attr_list == "other" && height_type_freetext == "msl")
  ) {
    
    height_above_ground <- hei_0 - as.numeric(data$ground.elevation)
    data$height_above_ground <- height_above_ground
    logger.info("You have selected that your data are height above mean sea level.")
    
  } else if (
    height_col == "height_above_ellipsoid" ||
    (height_attr_list == "other" && height_type_freetext == "ellipsoid")
  ) {
    
    # Add EGM2008 geoid correction values
    egm.file.path <- getAuxiliaryFilePath("egm_file")
    geoid <- terra::rast(egm.file.path)
    
    ann <- terra::extract(geoid, locs_sf)
    data$egm08.geoid <- ann[, 2]
    logger.info("The variable egm08.geoid in metre was added to your data for ellipsoid height correction.")
    
    height_above_ground <- hei_0 - as.numeric(data$ground.elevation) + as.numeric(data$egm08.geoid)
    data$height_above_ground <- height_above_ground
    logger.info("Height above ground was calculated using ground.elevation and EGM2008 geoid correction.")
    
  } else if (
    height_col == "height_above_ground_level" ||
    (height_attr_list == "other" && height_type_freetext == "ground_level")
  ) {
    
    data$height_above_ground <- hei_0
    logger.info("Height is already above ground level. No correction was applied.")
    
  } else if (height_col == "barometric_height") {
    logger.info("Barometric height cannot be safely converted automatically; height above ground was not calculated.")
    return(data)
    
  } else if (height_col == "height_raw") {
    
    logger.info("Raw height values may require preprocessing, so height above ground was not calculated.")
    return(data)
    
  } else {
    logger.info("The selected height type is unknown or unsupported, so height above ground was not calculated.")
    return(data)
  }
############################################################################
  #########  artefact/statistics part
  data.split <- split(data, mt_track_id(data))
  ids <- names(data.split)
  n <- length(ids)
  
  ############### Create statistics CSV ###########################
  
  height_stats <- data.frame(
    track = c(ids, "all"),
    n.pts = numeric(n + 1),
    mean.pts.height.above.ground = rep(NA_real_, n + 1),
    sd.pts.height.above.ground = rep(NA_real_, n + 1),
    mean.dur.height.above.ground = rep(NA_real_, n + 1),
    sd.dur.height.above.ground = rep(NA_real_, n + 1)
  )
  
  for (i in seq_along(data.split)) {
    
    datai <- data.split[[i]]
    height_above_ground_i <- as.numeric(datai$height_above_ground)
    
    height_stats$n.pts[i] <- sum(!is.na(height_above_ground_i))
    height_stats$mean.pts.height.above.ground[i] <- mean(height_above_ground_i, na.rm = TRUE)
    height_stats$sd.pts.height.above.ground[i] <- sd(height_above_ground_i, na.rm = TRUE)
    
    if ("timelag" %in% names(datai)) {
      
      dur_i <- as.numeric(datai$timelag)
      ok <- !is.na(height_above_ground_i) & !is.na(dur_i)
      
      if (sum(dur_i[ok], na.rm = TRUE) > 0) {
        mu_i <- sum(height_above_ground_i[ok] * dur_i[ok], na.rm = TRUE) / sum(dur_i[ok], na.rm = TRUE)
        
        height_stats$mean.dur.height.above.ground[i] <- mu_i
        height_stats$sd.dur.height.above.ground[i] <-
          sqrt(sum((height_above_ground_i[ok] - mu_i)^2 * dur_i[ok], na.rm = TRUE) / sum(dur_i[ok], na.rm = TRUE))
      }
      
    } else {
      #ask about this:   data$timelag <- mt_time_lags(data, units)
      height_stats$mean.dur.height.above.ground[i] <- NA
      height_stats$sd.dur.height.above.ground[i] <- NA
    }
  }
  
  height_all <- as.numeric(data$height_above_ground)
  last_row <- n + 1
  
  height_stats$n.pts[last_row] <- sum(!is.na(height_all))
  height_stats$mean.pts.height.above.ground[last_row] <- mean(height_all, na.rm = TRUE)
  height_stats$sd.pts.height.above.ground[last_row] <- sd(height_all, na.rm = TRUE)
  
  if ("timelag" %in% names(data)) {
    dur_all <- as.numeric(data$timelag)
    ok <- !is.na(height_all) & !is.na(dur_all)
    
    if (sum(dur_all[ok], na.rm = TRUE) > 0) {
      mu_all <- sum(height_all[ok] * dur_all[ok], na.rm = TRUE) / sum(dur_all[ok], na.rm = TRUE)
      
      height_stats$mean.dur.height.above.ground[last_row] <- mu_all
      height_stats$sd.dur.height.above.ground[last_row] <-
        sqrt(sum((height_all[ok] - mu_all)^2 * dur_all[ok], na.rm = TRUE) / sum(dur_all[ok], na.rm = TRUE))
    }
  } else {
    #ask about this:   data$timelag <- mt_time_lags(data, units)
    height_stats$mean.dur.height.above.ground[last_row] <- NA
    height_stats$sd.dur.height.above.ground[last_row] <- NA
  }
  
  
  write.csv(height_stats, appArtifactPath("height_above_ground_statistics.csv"), row.names = FALSE)
  logger.info("The file height_above_ground_statistics.csv was created.")
  
  #################### Histogram PDF ####################
  
  if ("height_above_ground" %in% names(data)) {
    
    
    # helper function for one histogram
    plot_height_hist <- function(x, title) {
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      
      if (length(x) == 0) {
        logger.info(paste0("No height_above_ground values for ", title))
        return(NULL)
      }
      
      xlim_vals <- as.numeric(quantile(x, probs = c(0.01, 0.99), na.rm = TRUE))
      
      if (!all(is.finite(xlim_vals)) || xlim_vals[1] == xlim_vals[2]) {
        xlim_vals <- range(x, na.rm = TRUE)
      }
      
      if (
        !is.null(height_props) &&
        !identical(height_props, list()) &&
        length(height_props) > 0 &&
        trimws(as.character(height_props)) != ""
      ) {
        
        hei_props <- sort(as.numeric(trimws(strsplit(as.character(height_props), ",")[[1]])))
        brks <- sort(unique(c(min(x), 0, hei_props, max(x))))
        
        if (length(brks) >= 2) {
          hist(x,breaks = brks, main = title, xlab = "height_above_ground", freq = FALSE, xlim = xlim_vals)
        } else {
          hist( x,main = title,xlab = "height_above_ground",freq = FALSE, xlim = xlim_vals)
        }
        
      } else {
        
        hist( x,main = title, xlab = "height_above_ground", freq = FALSE, xlim = xlim_vals)
      }
      
      abline(v = 0, lty = 2)
    }
    
    pdf(appArtifactPath("height_above_ground_histograms.pdf"), width = 12, height = 8)
    
    # per track
    lapply(names(data.split), function(id) {
      plot_height_hist(data.split[[id]]$height_above_ground,
                       paste("Histogram of", id))
    })
    
    # all tracks
    plot_height_hist(data$height_above_ground, "Histogram of all tracks")
    
    dev.off()
    
    logger.info("The file height_above_ground_histograms.pdf was created.")
    
  } else {
    logger.info("height_above_ground was not created, so no histogram PDF was produced.")
  } 
  
  
  #################### Threshold proportions ####################
  
  if (
    !is.null(height_props) &&
    !identical(height_props, list()) &&
    length(height_props) > 0 &&
    trimws(as.character(height_props)) != ""
  ) {
    
    height_thresholds <- sort(as.numeric(trimws(strsplit(as.character(height_props), ",")[[1]])))
    height_thresholds <- height_thresholds[!is.na(height_thresholds)]
    
    if (length(height_thresholds) == 0) {
      logger.info("No valid height thresholds were provided. Threshold proportions were not calculated.")
    } else {
      
      logger.info(paste0(
        "Height threshold proportions will be calculated for: ",
        paste(height_thresholds, collapse = ", ")
      ))
      
      prop_table <- data.frame()
      
      for (id in ids) {
        
        datai <- data.split[[id]]
        height_i <- as.numeric(datai$height_above_ground)
        
        if ("timelag" %in% names(datai)) {
          dur_i <- as.numeric(datai$timelag)
        } else {
          dur_i <- rep(NA, length(height_i))
        }
        
        for (thr in height_thresholds) {
          
          valid_height <- !is.na(height_i)
          below_thr <- valid_height & height_i < thr
          
          n_loc <- sum(below_thr)
          
          if (sum(valid_height) > 0) {
            prop_locs <- n_loc / sum(valid_height)
          } else {
            prop_locs <- NA
          }
          
          
          if ("timelag" %in% names(datai) && sum(dur_i[valid_height], na.rm = TRUE) > 0) {
            prop_dur <- sum(dur_i[below_thr], na.rm = TRUE) / sum(dur_i[valid_height], na.rm = TRUE)
          } else {
            prop_dur <- NA
          }
          
          prop_table <- rbind(prop_table, data.frame(track = id,height_threshold = thr,n.loc = n_loc, prop.locs = prop_locs, prop.dur = prop_dur) )
        }
      }
      
      # Add mean and sd rows across tracks
      mean_rows <- aggregate(
        cbind(n.loc, prop.locs, prop.dur) ~ height_threshold,
        data = prop_table,
        FUN = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
        na.action = na.pass
      )
      mean_rows$track <- "mean"
      
      sd_rows <- aggregate(
        cbind(n.loc, prop.locs, prop.dur) ~ height_threshold,
        data = prop_table,
        FUN = function(x) if (all(is.na(x))) NA_real_ else sd(x, na.rm = TRUE),
        na.action = na.pass
      )
      sd_rows$track <- "sd"
      
      prop_table <- rbind(prop_table, mean_rows[, names(prop_table)], sd_rows[, names(prop_table)])
      write.csv(prop_table,appArtifactPath("height_above_ground_threshold_proportions.csv"),row.names = FALSE )
      logger.info("The file height_above_ground_threshold_proportions.csv was created.")
    }
    
  } else {
    logger.info("No height thresholds were provided. Threshold proportions were not calculated.")
  }
#################### 
  
  
  return(data)
  #return(input_data)
} 








