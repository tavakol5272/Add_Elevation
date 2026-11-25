# Add Elevation and Height Above Ground

MoveApps

Github repository: https://github.com/movestore/Add_Elevation

## Description
Adds the ground elevation to every location and calculates how high above the ground the animal was, based on the height recorded by the tag. Summary statistics, histograms and, optionally, the proportion of locations and of time spent below chosen heights are provided as files.

## Documentation
The App looks up the elevation of the ground at every location and then converts the height recorded by the tag into height above ground. Tags record height in different ways: relative to sea level, relative to the reference shape of the Earth used by satellite positioning, or already as height above the ground. The App takes the kind of height into account. Some kinds of height are not converted, because their reference is uncertain (see Errors and null handling).

The user chooses which height column(s) are converted (see Settings). Each converted column results in its own new column, so that heights from different sources stay separate and identifiable.

For each converted column, the App provides summary statistics, histograms and, optionally, the proportion of locations and of time the animals spent below chosen heights. An overview file lists all columns that may contain heights and what was done with each of them; for columns with non-standard names it gives a rough suggestion of the kind of height, which the user should check before using it (see Artefacts).

**Be aware that height values collected by tags can be prone to large errors, which result in unrealistic height above ground values.**

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. It was tested with data of birds.

#### Required data properties
The App should work for any kind of location data. Height above ground can only be calculated if the data contain a height column with numeric values.

Height above ground is not meaningful for locations over the sea, because there the elevation data contain the depth of the sea floor instead of the water surface. Over lakes, the elevation data give the water surface.

Suggesting the kind of height for columns with non-standard names requires locations with slow movement (see Technical details).

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
The first three files are created for every converted height column; `<column>` stands for the name of the original height column (e.g. `height_above_ellipsoid`). All heights are in metres.

`<column>.adapted_statistics.csv`: one row per track and one row for all tracks together (`all`), with the number of locations with a height above ground (`n.pts`), the mean and standard deviation of the height above ground over all locations (`mean.pts.height.above.ground`, `sd.pts.height.above.ground`) and weighted by time (`mean.dur.height.above.ground`, `sd.dur.height.above.ground`).

`<column>.adapted_histograms.pdf`: histograms of the height above ground, first for all tracks together and then for each track. Bars of negative heights (below the ground) are drawn downwards; the dashed vertical line marks the ground (0 m).

`<column>.adapted_threshold_proportions.csv`: only created if height thresholds are provided. One row per track, followed by the `mean` and the standard deviation (`sd`) across tracks. For every threshold (e.g. `100m`) there are three columns: the number of locations below the threshold (`n.loc_100m`), the proportion of locations below it (`prop.locs_100m`) and the proportion of time below it (`prop.dur_100m`).

`height_columns_overview.csv`: created whenever the data contain columns that may contain heights. One row per such column, with: whether it is a standard Movebank height column (`standard_movebank_name`), what was done with it (`status`), the kind of height used (`type_used`), an explanation (`note`), the number of tracks with values (`n_tracks_with_values`), the minimum, median and maximum value, and, for columns with non-standard names, the suggested kind of height (`suggested_type`), the evidence for it (`suggestion_basis`) and what to do next (`advice`).

### Settings
`Height/altitude column selection` (height_selection): how the height column(s) are chosen. Options: `1: Choose one standard Movebank height attribute from the list` (uses the setting `Option 1`), `2: Type the exact height/altitude column name` (uses the two `Option 2` settings), `3: Calculate for all standard Movebank height attributes in the data` (converts every column `height_above_ellipsoid`, `height_above_msl` / `height_above_mean_sea_level` and `height_above_ground_level` present in the data; the `Option 1` and `Option 2` settings are ignored). Default: `1`.

`Option 1: Standard Movebank height attribute` (height_attr_list): the standard Movebank height column to convert. Options: `no selection`, `height_above_ellipsoid`, `height_above_mean_sea_level / height_above_msl`, `height_above_ground_level`. Default: `no selection` (only the ground elevation is added).

`Option 2: Exact height/altitude column name` (height_attr_freetext): name of the height column, exactly as in the data. Default: empty.

`Option 2: Height reference type` (height_type_freetext): the kind of height in the typed column. Options: `unknown / do not adapt`, `height above mean sea level`, `height above ellipsoid`, `height above ground level`. With `unknown`, the column is not converted, unless it is a standard Movebank height column, whose kind is known. Default: `unknown / do not adapt`.

`Height thresholds for proportional use` (height_props): heights above ground for the threshold proportions, separated by commas, e.g. `100,200,300,500`. Unit: `metres`. If thresholds are provided, the histogram bars are bounded by the minimum height, 0 m, the thresholds and the maximum height; otherwise the histograms use 50 equal bars. Default: empty (no threshold proportions).

### Changes in output data
The App adds to the input data:

- `ground.elevation`: elevation of the ground at the location, in metres above mean sea level. Always added.
- `egm08.geoid`: geoid height, i.e. the height of mean sea level above the reference shape of the Earth used by satellite positioning, in metres. Only added if a height above ellipsoid is converted.
- `<column>.adapted`: height above ground calculated from the height column `<column>`, in metres, e.g. `height_above_ellipsoid.adapted`. One column per converted height column.

All other columns, the locations and the projection of the input data remain unchanged.

### Errors and null handling
**Input data:** If the input data are NULL or contain no locations, the App returns NULL.

**No height column converted:** If no column is selected, or the selected or typed column does not exist, contains no numeric values or has the kind of height `unknown`, the App returns the data with only `ground.elevation` added and gives the reason in the log. `height_columns_overview.csv` lists the possible height columns of the data.

**Heights not converted on purpose:** `barometric_height` is not converted, because heights estimated from air pressure change with the weather and can be wrong by tens of metres. `height_raw` is not converted, because it is stored as text and its reference is not known. `argos_altitude` is ignored, because it is the altitude assumed by the Argos system to calculate the location, not a measured height.

**Possibly wrong kind of height:** If the values of a converted column clearly fit another kind of height better than the selected one, a warning is written to the log and to `height_columns_overview.csv`. This check is a rough heuristic; confirm the kind of height (e.g. in the tag documentation) and run the App again if needed.

**Tracks without heights:** Tracks without values in the converted column are named in the log. Their values in the artefacts are NA, and the `mean` and `sd` rows are calculated from the other tracks.

**Setting `height_props`:** Entries that are not numbers are ignored with a warning.

**Unrealistic heights:** Negative or very large heights above ground usually result from errors in the heights recorded by the tag, from the coarse ground elevation (see Technical details) or from locations over the sea (see Required data properties).

### Technical details
- **Ground elevation:** `elevatr::get_elev_point()` with `src = "aws"` and zoom level `z = 5` (Amazon Web Services Terrain Tiles). One elevation grid is downloaded for the area of all locations, and each location gets the value of the grid cell it falls in. At zoom level 5 the cells are about 3–5 km wide (about 4.9 km at the equator), so the value can differ considerably from the actual ground elevation at the location, especially in hilly or mountainous terrain. Finer zoom levels are not used, because the grid would become too large for study areas spanning continents. This step needs internet access.
- **Geoid height:** the EGM2008 geoid undulation N from the 1 arc-minute grid `us_nga_egm2008_1.tif` (provided with the App), extracted with `terra::extract()`.
- **Height above ground:** height above mean sea level: `h − ground.elevation`; height above ellipsoid: `h − egm08.geoid − ground.elevation`; height above ground level: `h` (copied). Earlier versions of this App added the geoid height instead of subtracting it for heights above ellipsoid, which shifted the results by twice the geoid height: too high where the geoid height is positive (e.g. by about 90–100 m in Central Europe) and too low where it is negative (e.g. by about 200 m in parts of the Indian Ocean).
- **Suggested kind of height (heuristic):** locations with a speed below 1.5 m/s (`move2::mt_speed()`, at least 30 locations) are assumed to be close to the ground, and the kind of height whose median height above ground at these locations is closest to 0 m is suggested. Slow movement does not necessarily mean that the animal is on the ground (e.g. perching in trees or on buildings, soaring or hovering), so the suggestion is only a rough indication, not a reliable identification. If the values give no clear answer, a hint in the column name (e.g. `msl`, `hae`, `agl`) is used. The suggestion is marked as uncertain if the medians of the kinds differ by less than 20 m, or if the geoid height is close to 0 m, where heights above mean sea level and above ellipsoid look alike.
- **Time-weighted statistics:** each location is weighted by the time until the next location of the same track (`move2::mt_time_lags()`, seconds). Long gaps, e.g. at night, are attributed completely to the last location before the gap.
- **Processing level:** statistics and proportions are calculated per track and for all tracks together. Elevation, geoid height and speed are looked up on a copy of the locations in geographic coordinates (EPSG:4326).
- **Packages:** `move2`, `sf`, `elevatr`, `terra`, `ggplot2`.
