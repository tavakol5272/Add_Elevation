# Add Elevation and Height Above Ground

MoveApps

Github repository: https://github.com/movestore/Add_Elevation

## Description
Adds ground elevation to each location and converts recorded heights into height above ground, accounting for different height reference systems. Users can interactively select height columns, specify their height types, visualize height distributions, and calculate the proportions of locations and time below user-defined height thresholds.

## Documentation
The App looks up the elevation of the ground at every location and then converts the height recorded by the tag into height above ground. Tags record height in different ways: relative to sea level, relative to the reference shape of the Earth used by satellite positioning, or already as height above the ground. The App takes the height type into account.

The App finds the columns of the data that contain heights and offers them for selection. The user can also select other columns, e.g. a height column with an unusual name. For columns that are not standard Movebank height columns, the user sets the height type; for columns whose name looks like a height, the App suggests a height type, which the user should check before using it. Each converted column results in its own new column, so that heights from different sources stay separate and identifiable.

When the App is opened, it calculates once with the stored settings (or the default settings). After that, changes are only calculated when the user clicks `Calculate`. The screen shows a histogram of the height above ground and a table with the proportion of locations and of time below chosen heights, for all tracks together or for one track. Values below ground and values that are probably over the sea are marked, because they are errors. All results can be downloaded.

**Be aware that height values collected by tags can be prone to large errors, which result in unrealistic height above ground values.**

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. It was tested with data of birds.

#### Required data properties
The App should work for any kind of location data. Height above ground can only be calculated if the data contain a height column with numeric values.

Height above ground is not meaningful for locations over the sea, because there the elevation data contain the depth of the sea floor instead of the water surface. Over lakes, the elevation data give the water surface.

Suggesting the height type requires locations with slow movement (see Technical details).

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The following files can be downloaded optionally (to disk):

`Add_Elevation_results_<date>.zip`: by clicking the `Download results (zip)` button. Contains all files listed below, from the last calculation.

The following files are saved with every Workflow run and updated with every calculation. The first three are created for every converted height column; `<column>` stands for the name of the original height column (e.g. `height_above_ellipsoid`). All heights are in metres.

`<column>.adapted_statistics.csv`: one row per track and one row for all tracks together (`all`), with the number of locations with a height above ground (`n.pts`), the mean and standard deviation of the height above ground over all locations (`mean.pts.height.above.ground`, `sd.pts.height.above.ground`) and weighted by time (`mean.dur.height.above.ground`, `sd.dur.height.above.ground`).

`<column>.adapted_histograms.pdf`: the histogram shown in the App, first for all tracks together and then for each track. A note under each histogram gives the number of locations below ground, probably over the sea and in the rightmost bar (see Settings, `Column` and `Track`).

`<column>.adapted_threshold_proportions.csv`: only created if height thresholds are provided. One row per track, followed by the `mean` and the standard deviation (`sd`) across tracks. For every threshold (e.g. `100m`) there are three columns: the number of locations below the threshold (`n.loc_100m`), the proportion of locations below it (`prop.locs_100m`) and the proportion of time below it (`prop.dur_100m`).

`height_columns_overview.csv`: one row per column the App found as a possible height column, plus one row per selected column that was not found as a height column. Columns: the column name (`column`), whether it is a standard Movebank height column (`standard_movebank_name`), what was done with it (`status`), the height type used (`type_used`), an explanation (`note`), the number of tracks with values (`n_tracks_with_values`), the minimum, median and maximum value, and, for columns whose name looks like a height, the suggested height type (`suggested_type`), the evidence for it (`suggestion_basis`) and what to do next (`advice`).

### Settings
**Sidebar**

`Columns for which height above ground is calculated` (height_cols): one or more columns, in three groups: `Standard Movebank height attributes`, `Possible height columns (by name)` and `Other event attributes` (all other numeric columns). Only columns with numeric values are offered; an empty group shows `(none in this dataset)`. Columns found as height columns that cannot be converted are listed in grey under the selection with the reason (e.g. barometric, no values). Default: the most probable height column: `height_above_ellipsoid`, else `height_above_msl` / `height_above_mean_sea_level`, else `height_above_ground_level`, else the possible height column with the most values.

`Height type per column` (type_<column>): for each selected column that is not a standard Movebank height column, the height type. Options: `unknown / do not adapt`, `height above mean sea level`, `height above ellipsoid`, `height above ground level`. For columns from `Possible height columns (by name)` a suggested height type is shown under the selection. Standard Movebank height columns have a fixed height type, which is shown as text. Default: `unknown / do not adapt` (the column is not converted).

`Heights above ground (m), separated by commas` (height_props), under `Thresholds for proportions`: heights above ground for the threshold proportions and the dashed lines in the histogram. They do not change the calculated heights above ground. Unit: `metres`. Default: `100,200,300,400,500`.

`Calculate` (apply): calculates with the current settings of the sidebar.

**Main panel**

`Column` (hist_col) and `Track` (hist_track): the converted column and the track (or `All tracks`) shown in the histogram and in the table below it. They change only what is shown, not the calculation. Default: the first converted column, `All tracks`.

### Changes in output data
The App adds to the input data:

- `ground.elevation`: elevation of the ground at the location, in metres above mean sea level. Always added.
- `egm08.geoid`: geoid height, i.e. the height of mean sea level above the reference shape of the Earth used by satellite positioning, in metres. Only added if a height above ellipsoid is converted.
- `<column>.adapted`: height above ground calculated from the height column `<column>`, in metres, e.g. `height_above_ellipsoid.adapted`. One column per converted height column.

All other columns, the locations and the projection of the input data remain unchanged.

### Errors and null handling
**Input data:** If the input data are NULL or contain no locations, the App returns NULL.

**No usable height column:** If the data contain no height column with values, the App says so on the screen and returns the data with only `ground.elevation` added.

**Column not converted:** If a selected column has the height type `unknown / do not adapt` or contains no numeric values, a warning at the top of the main panel names it and the reason; the other selected columns are converted.

**Heights not converted on purpose:** `barometric_height` is not converted, because heights estimated from air pressure change with the weather and can be wrong by tens of metres. `height_raw` is not converted, because it is stored as text and its reference is not known. `argos_altitude` is ignored, because it is the altitude assumed by the Argos system to calculate the location, not a measured height.

**Columns not found as height columns:** Columns from `Other event attributes` get no suggested height type, because the suggestion would be meaningless for values that are not heights (e.g. temperature). Check the tag documentation before converting such a column.

**Possibly wrong height type:** If the values of a converted column clearly fit another height type better than the selected one, a warning is written to the log and to `height_columns_overview.csv`. Confirm the height type (e.g. in the tag documentation) and calculate again if needed.

**Tracks without heights:** Tracks without values in the converted column are named in the log. Their values in the artefacts are NA, and the `mean` and `sd` rows are calculated from the other tracks.

**Setting `height_props`:** Entries that are not numbers are ignored with a warning. Without valid thresholds, no threshold proportions are calculated.

**Unrealistic heights:** Heights below ground are errors of the height recorded by the tag or of the coarse ground elevation (see Technical details); they are shown in one orange bar. Locations with a ground elevation below 0 m are probably over the sea, where the heights above ground are much too high; they are shown in red. A note under the histogram gives their share.

### Technical details
- **Ground elevation:** `elevatr::get_elev_point()` with `src = "aws"` and zoom level `z = 5` (Amazon Web Services Terrain Tiles). One elevation grid is downloaded for the area of all locations, once when the App is opened, and each location gets the value of the grid cell it falls in. At zoom level 5 the cells are about 3–5 km wide (about 4.9 km at the equator), so the value can differ considerably from the actual ground elevation at the location, especially in hilly or mountainous terrain. Finer zoom levels are not used, because the grid would become too large for study areas spanning continents. This step needs internet access.
- **Geoid height:** the EGM2008 geoid undulation N from the 1 arc-minute grid `us_nga_egm2008_1.tif` (provided with the App), extracted with `terra::extract()`.
- **Height above ground:** height above mean sea level: `h − ground.elevation`; height above ellipsoid: `h − egm08.geoid − ground.elevation`; height above ground level: `h` (copied). Earlier versions of this App added the geoid height instead of subtracting it for heights above ellipsoid, which shifted the results by twice the geoid height: too high where the geoid height is positive (e.g. by about 90–100 m in Central Europe) and too low where it is negative (e.g. by about 200 m in parts of the Indian Ocean).
- **Finding height columns:** standard Movebank height columns, plus columns whose name contains e.g. `height`, `altitude`, `alt`, `elev`, `hae`, `msl`, `asl` or `agl` (not names with `error`, `accuracy`, `dop`, `precision` or `speed`). The same result is used for the selection, the grey list of columns that cannot be converted and `height_columns_overview.csv`.
- **Suggested height type (heuristic):** locations with a speed below 1.5 m/s (`move2::mt_speed()`, at least 30 locations) are assumed to be close to the ground, and the height type whose median height above ground at these locations is closest to 0 m is suggested. Slow movement does not necessarily mean that the animal is on the ground (e.g. perching in trees or on buildings, soaring or hovering), so the suggestion is only a rough indication. If the values give no clear answer, a hint in the column name (e.g. `msl`, `hae`, `agl`) is used. The suggestion is marked as uncertain if the medians of the height types differ by less than 20 m, or if the geoid height is close to 0 m, where heights above mean sea level and above ellipsoid look alike.
- **Histogram:** all heights below 0 m in one orange bar left of 0 (no negative values on the axis); heights from 0 m upwards in about 20 equal bars. The bar heights are densities, so the area of a bar is the share of locations in it. The share of locations with a ground elevation below 0 m (probably over the sea) is drawn in red inside the bars; this is only an indication, because land below sea level (e.g. the Netherlands, the Dead Sea) also has negative elevations. If the highest 1 % of the values stretch the axis more than twice as far as the other 99 %, they are included in the rightmost bar, so that single outliers (e.g. GPS errors) do not squeeze the plot. This adjustment affects only the visualization. No locations are excluded, and the original height values remain unchanged for all statistical calculations and threshold proportions.
- **Time-weighted statistics:** each location is weighted by the time until the next location of the same track (`move2::mt_time_lags()`, seconds). Long gaps, e.g. at night, are attributed completely to the last location before the gap.
- **Processing level:** statistics and proportions are calculated per track and for all tracks together. Elevation, geoid height and speed are looked up on a copy of the locations in geographic coordinates (EPSG:4326).
- **Packages:** `shiny`, `move2`, `sf`, `ggplot2`, `elevatr`, `terra`, `shinycssloaders`, `zip`.
