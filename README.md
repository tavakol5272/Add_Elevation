# Add Elevation and Height Above Ground

MoveApps

Github repository: *github.com/movestore/Add_Elevation*

## Description
This App adds ground elevation to each location in a move2 object and calculates height above ground from a selected height/altitude attribute. 
The App returns the updated data with added columns such as `ground.elevation`, `height_above_ground`, and for ellipsoid heights, `egm08.geoid`. It also creates summary statistics, histogram, and optional threshold-proportion artefacts for height above ground.

## Documentation
First the App appends a ground elevation estimate to each location by using the R package `elevatr`. The app gets ground elevation from a global DEM source called Amazon Web Services Terrain Tiles.
then it calculates height above ground from a selected height/altitude attribute. Before running the App, the user can select a standard Movebank height attribute from a dropdown. 
If the required attribute is not listed, the user can choose “other”, enter the exact column name, and specify the height type.
The following height types are handled:

- height above mean sea level / height above msl: `height_above_ground = selected height - ground.elevation`.
- height above ellipsoid: `height_above_ground = height_above_ellipsoid - ground.elevation + egm08.geoid` (The geoid correction(egm08.geoid) is added to the data as well)
- height above ground level: the selected values are already height above ground and are copied to `height_above_ground`.
- for barometric height and height raw types you get just the log info because these are not converted automatically.

If the selected column is missing or contains only NA values, the App searches for possible height/altitude columns by looking for column names containing “height” or “altitude”. 
These possible columns are reported in the log, and user can run the app again with these given suggestions.
Negative height_above_ground values could occur.
### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The App also creates artefacts:  
- `height_above_ground_statistics.csv`: summary statistics per track and overall, including number of points, mean, standard deviation, and duration-weighted mean and standard deviation by calculating the Timelag in seconds.  
- `height_above_ground_histograms.pdf`: histograms of `height_above_ground` for all tracks together and for each track separately.  
- `height_above_ground_threshold_proportions.csv`: if height thresholds are provided, this table gives the proportion of locations and duration below each threshold.  


### Settings 

**`height_above_ground`**: The user selects which height/altitude attribute should be used to calculate it.  
Options:  
- `no_selection`: No height column is selected. The App only adds `ground.elevation`. If possible height/altitude columns exist, they are shown in the log.  
- `height_above_ellipsoid`:so the App applies the EGM2008 geoid correction: height_above_ground = height_above_ellipsoid - ground.elevation + egm08.geoid  
- `height_above_msl_or_mean_sea_level` :  The App calculates height_above_ground = height - ground.elevation  
- `height_above_ground_level` : The selected values are already height above ground.  
- `barometric_height` / `height_raw`:  These are not converted automatically just show the message in log.  
- `other` : Use this for a custom height/altitude column. The exact column name must be entered in **`height_attr_freetext`**, and the height type must be selected in **`height_type_freetext`**.  
options for **`height_type_freetext`** are: `msl`, `ellipsoid`, `ground_level`, `unknown`.    
If the type is `unknown`, the App does not calculate `height_above_ground`.  

**`height_props`**: The optional height thresholds allows users to enter height-above-ground thresholds in meters, for example: `100,200,300,500` separated by comma.  
If thresholds are provided, the App creates a threshold-proportion table and adapts histogram breaks to include the minimum height, zero, the threshold(s), and the maximum height.   
If no thresholds are provided, histograms use 50 bins.  

### Changes in output data
The App returns the updated move2 object with the added columns. Depending on the selected option, these may include ground.elevation, egm08.geoid (if height above ellipsoid selected ), and height_above_ground.


### Most common errors

- **No height/altitude column selected**: The App only adds `ground.elevation` and does not calculate `height_above_ground`.

- **Selected column not found**: The App reports possible columns containing “height” or “altitude” in the log.

- **Selected column contains only NA values**: `height_above_ground` cannot be calculated.

- **Unknown custom height type**: `height_above_ground` cannot be calculated. The user must select `msl`, `ellipsoid`, or `ground_level`.


### Null or error handling

- If the input data is `NULL` or has 0 rows, the App returns `NULL`.
- If no height/altitude column is selected, the App returns the data with `ground.elevation` only.
- If the selected column does not exist, the App reports possible height/altitude columns in the log.
- If the selected column contains only `NA` values, `height_above_ground` is not calculated.
- If the custom height type is `unknown`, or the selected attribute is `barometric_height` or `height_raw`, the App stops the height calculation safely.  
- If height thresholds are missing or invalid, no threshold-proportion file is created.