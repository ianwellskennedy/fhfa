# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "writexl", "openxlsx", "sf", "arcgisbinding", "lubridate", "lwgeom", "fredr", "xts")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# Setting file paths and environment variables ----

# Set the FRED API Key
fredr_set_key(key = 'c1f7f3d38687246c6d6e5b83898af5a1')

# Edit the input_file_path each quarter once you have added data to a new quarter's folder
## Access the data here if needing to download updated historical data: https://www.fhfa.gov/data/national-mortgage-database-aggregate-statistics 
## Download the data for 'Outstanding Residential Mortgage Statistics' covering 'All Geographic Areas'
input_file_path <- "National Mortgage Database/inputs/1Q25/nmdb-outstanding-mortgage-statistics-all-quarterly.csv"
state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/States/cb_2023_us_state_20m.shp"

output_filepath_for_cleaned_data <- "National Mortgage Database/outputs/outstanding_mortgages_by_mortgage_rate.xlsx"

output_file_path_for_current_quarter_shapefile <- "National Mortgage Database/outputs/shapefiles/outstanding_mortgages_by_mortgage_rate.shp"
output_file_path_for_historical_shapefile <- "National Mortgage Database/outputs/shapefiles/outstanding_mortgages_by_mortgage_rate_historical.shp"
output_file_path_for_quarter_label_shapefile <- "National Mortgage Database/outputs/shapefiles/quarterly_label.shp"
output_file_path_for_mortgage_label_shapefile <- "National Mortgage Database/outputs/shapefiles/mortgage_label.shp"

current_quarter <- '2025Q1'
previous_quarter <- '2024Q4'
year_ago_quarter <- '2024Q1'

mortgage_market <- 'All Mortgages'

# Reading in data ----

data <- read.csv(input_file_path)

# Reading in shape files ----

state_shapefile <- st_read(state_shapefile_file_path)
state_shapefile <- state_shapefile %>%
  select(NAME)

# Reading in mortgage rate data ----

mortgage_rates <- fredr(series_id = 'MORTGAGE30US', observation_start = as.Date('2013-01-01'), observation_end = Sys.Date(), frequency = 'q', aggregation_method = 'avg')

mortgage_rates <- mortgage_rates %>%
  select(date, value) %>%
  rename(mortgage_rate = value, Quarter = date) %>%
  mutate(Quarter = as.character(as.yearqtr(Quarter)))

# Clean current/previous quarter state data ----

state_data <- data %>%
  filter(GEOLEVEL == 'State' & MARKET == mortgage_market & startsWith(SERIESID, 'PCT_INTRATE_')) %>%
  select(-c(GEOLEVEL, FREQUENCY, YEAR:SUPPRESSED, VALUE2))

# Create state_data_[Quarter]' by filtering the data for the following conditions
state_data_Q1 <- state_data %>%
  filter(PERIOD == current_quarter)

state_data_Q1 <- state_data_Q1 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

state_data_Q4 <- state_data %>%
  filter(PERIOD == previous_quarter)

state_data_Q4 <- state_data_Q4 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

state_data_Q1_24 <- state_data %>%
  filter(PERIOD == year_ago_quarter)

state_data_Q1_24 <- state_data_Q1_24 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

# Create a state-level qoq / yoy difference file ----

state_data_qoq_diff <- state_data_Q1 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_pct_blw_5 = pct_blw_5)

state_data_Q4_qoq_diff <- state_data_Q4 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q4_pct_blw_5 = pct_blw_5)

state_data_Q1_yoy_diff <- state_data_Q1_24 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_24_pct_blw_5 = pct_blw_5)

state_data_diff <- state_data_qoq_diff %>%
  left_join(state_data_Q4_qoq_diff, by = 'GEONAME') %>%
  left_join(state_data_Q1_yoy_diff, by = 'GEONAME')

state_data_diff <- state_data_diff %>%
  mutate(qoq_diff = q1_pct_blw_5 - q4_pct_blw_5,
         yoy_diff = q1_pct_blw_5 - q1_24_pct_blw_5)

rm(state_data_qoq_diff, state_data_Q3_qoq_diff, state_data_Q4_yoy_diff)


# Clean historical state data ----

state_data_historical <- state_data %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

convert_to_date_char <- function(quarter) {
  year <- substr(quarter, 1, 4)
  qtr <- as.numeric(substr(quarter, 6, 6))
  # Calculate the first month of the quarter
  month <- (qtr - 1) * 3 + 1
  # Create the date string
  sprintf("%s-%02d-01", year, month)
}

state_data_historical <- state_data_historical %>%
  mutate(Quarter = gsub("(\\d{4})(Q\\d)", "\\1 \\2", PERIOD))

state_data_historical$PERIOD <- sapply(state_data_historical$PERIOD, convert_to_date_char)

state_data_historical <- state_data_historical %>%
  select(SOURCE:PERIOD, Quarter, everything())

state_data_historical <- state_data_historical %>%
  left_join(mortgage_rates, by = 'Quarter') 

# Clean current/previous quarter national data ----

national_data <- data %>%
  filter(GEOLEVEL == 'National' & MARKET == mortgage_market & startsWith(SERIESID, 'PCT_INTRATE_')) %>%
  select(-c(GEOLEVEL, FREQUENCY, YEAR:SUPPRESSED, VALUE2))

national_data_Q1 <- national_data %>%
  filter(PERIOD == current_quarter)

national_data_Q1 <- national_data_Q1 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

national_data_Q4 <- national_data %>%
  filter(PERIOD == previous_quarter)

national_data_Q4 <- national_data_Q4 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

national_data_Q1_24 <- national_data %>%
  filter(PERIOD == year_ago_quarter)

national_data_Q1_24 <- national_data_Q1_24 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

# Create a national qoq / yoy difference file ----

national_data_qoq_diff <- national_data_Q1 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_pct_blw_5 = pct_blw_5)

national_data_Q4_qoq_diff <- national_data_Q4 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q4_pct_blw_5 = pct_blw_5)

national_data_Q1_yoy_diff <- national_data_Q1_24 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_24_pct_blw_5 = pct_blw_5)

national_data_diff <- national_data_qoq_diff %>%
  left_join(national_data_Q4_qoq_diff, by = 'GEONAME') %>%
  left_join(national_data_Q1_yoy_diff, by = 'GEONAME')

national_data_diff <- national_data_diff %>%
  mutate(qoq_diff = q1_pct_blw_5 - q4_pct_blw_5,
         yoy_diff = q1_pct_blw_5 - q1_24_pct_blw_5)

rm(national_data_qoq_diff, national_data_Q3_qoq_diff, national_data_Q4_yoy_diff)

# Clean historical national data ----

national_data_historical <- national_data %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

national_data_historical <- national_data_historical %>%
  mutate(PERIOD = gsub("(\\d{4})(Q\\d)", "\\1 \\2", PERIOD))

# Outputting tabluar data ----

dataset_list <- list('State (current)' = state_data_Q1, 'State (hist.)' = state_data_historical, 
                     'National (current)' = national_data_Q1, 'National (hist.)' = national_data_historical)

write.xlsx(dataset_list, output_filepath_for_cleaned_data)

# Outputting shape files (ignore if not interested in spatial files) ----

state_data_Q1 <- state_data_Q1 %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()

state_data_historical <- state_data_historical %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()

object.size(state_data_historical)

state_data_historical <- st_simplify(state_data_historical, dTolerance = 500, preserveTopology = TRUE)

arc.check_product()

arc.write(path = output_file_path_for_current_quarter_shapefile, data = state_data_Q1, overwrite = TRUE, validate = TRUE)
arc.write(path = output_file_path_for_historical_shapefile, data = state_data_historical, overwrite = TRUE, validate = TRUE)

# Make a quarter label for the map (ignore if not updating the historical web map!) ----

quarter_label <- state_data_historical %>%
  filter(GEONAME == 'Louisiana') %>%
  select(PERIOD, Quarter, geometry)

# Alternatively, buffer the coastline to approximate "coastal" areas
# Create a buffer of 5 km around the coastline
coastal_buffer <- quarter_label %>%
  st_union() %>%                       # Merge polygons into one geometry
  st_boundary() %>%                    # Extract the boundary
  st_buffer(dist = 5000)

coastal_islands <- st_intersection(quarter_label, coastal_buffer)

bbox <- st_bbox(coastal_islands)

# Calculate the midpoint latitude
mid_latitude <- (bbox["ymin"] + bbox["ymax"]) / 2

# Create a horizontal line at the midpoint latitude
split_line <- st_sfc(
  st_linestring(rbind(c(bbox["xmin"], mid_latitude), c(bbox["xmax"], mid_latitude))),
  crs = st_crs(coastal_islands)
)

# Step 3: Split the shapefile using the horizontal line
split_parts <- st_split(coastal_islands, split_line) %>% st_collection_extract("POLYGON")

# Step 4: Retain only the southern portion
quarter_label <- split_parts %>%
  filter(st_centroid(.) %>% st_coordinates() %>% .[, 2] < mid_latitude) %>%
  distinct(Quarter, .keep_all = T) %>%
  st_as_sf()

rm(coastal_islands, coastal_buffer, bbox, split_parts, split_line)

arc.write(path = output_file_path_for_quarter_label_shapefile, data = quarter_label, overwrite = TRUE, validate = TRUE)

# Make a mortgage rate label for the map (ignore if not updating the historical web map!) ----

mortgage_label <- state_data_historical %>%
  filter(GEONAME == 'New Jersey') %>%
  select(PERIOD, Quarter, mortgage_rate, geometry)

# Alternatively, buffer the coastline to approximate "coastal" areas
# Create a buffer of 5 km around the coastline
coastal_buffer <- mortgage_label %>%
  st_union() %>%                       # Merge polygons into one geometry
  st_boundary() %>%                    # Extract the boundary
  st_buffer(dist = 5000)

coastal_islands <- st_intersection(mortgage_label, coastal_buffer)

bbox <- st_bbox(coastal_islands)

# Calculate the midpoint latitude
mid_latitude <- (bbox["ymin"] + bbox["ymax"]) / 2

# Create a horizontal line at the midpoint latitude
split_line <- st_sfc(
  st_linestring(rbind(c(bbox["xmin"], mid_latitude), c(bbox["xmax"], mid_latitude))),
  crs = st_crs(coastal_islands)
)

# Step 3: Split the shapefile using the horizontal line
split_parts <- st_split(coastal_islands, split_line) %>% st_collection_extract("POLYGON")

# Step 4: Retain only the southern portion
mortgage_label <- split_parts %>%
  filter(st_centroid(.) %>% st_coordinates() %>% .[, 2] < mid_latitude) %>%
  distinct(Quarter, .keep_all = T) %>%
  st_as_sf()


rm(coastal_islands, coastal_buffer, bbox, split_parts, split_line)

arc.write(path = output_file_path_for_mortgage_label_shapefile, data = mortgage_label, overwrite = TRUE, validate = TRUE)
