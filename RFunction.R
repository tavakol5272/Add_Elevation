library('move2')
library('sf')
#library('progress')
library('elevatr')
library('terra')
library('units')

#need to update all to move2
rFunction <- function(data, height_attr_list, height_attr_freetext, height_type_freetext, height_props = NULL, ...) {
  
  input_data <- data
  
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
  
  
  #################################### Select height/altitude column ###################
  
  if (height_attr_list == "no_selection") {
    height_col <- NULL
    logger.info("No height/altitude attribute selected.")
    
  } else if (height_attr_list == "height_above_msl_or_mean_sea_level") {
    
    possible_cols <- c("height_above_mean_sea_level", "height_above_msl")
    height_col <- intersect(possible_cols, names(data))[1]
    
    if (is.na(height_col)) {
      height_col <- NULL
      logger.info("Neither height_above_mean_sea_level nor height_above_msl exists in the data.")
    } else {
      logger.info(paste0("Selected height above mean sea level attribute: ", height_col))
    }
    
  } else if (height_attr_list == "other") {
    
    if (is.null(height_attr_freetext) || trimws(height_attr_freetext) == "") {
      height_col <- NULL
      logger.info("You selected 'other', but did not provide a custom height/altitude column name. Only ground elevation will be added.")
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
    return(data)
  }
  
  # Extract selected height values
  hei_0 <- as.numeric(data[[height_col]])
  
  if (all(is.na(hei_0))) {
    logger.info(paste0("The selected height/altitude attribute '",height_col, "' contains only NA values."))
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
 #important ###You are right. In the new version, it is better to put the final result in one standard column:
  
  ####  artefact/statistics part
  
  
  
  
####################  
  
  
  
  return(input_data)
} 








