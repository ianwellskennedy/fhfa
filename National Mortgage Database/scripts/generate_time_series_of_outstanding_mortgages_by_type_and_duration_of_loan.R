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
fredr_set_key(key = 'cbebc48b543b6420b4aa3ff9bd7a9878')

input_file_path <- "National Mortgage Database/inputs/1Q25/nmdb-outstanding-mortgage-statistics-all-quarterly.csv"
state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/States/cb_2023_us_state_20m.shp"

output_file_path_for_cleaned_data <- "National Mortgage Database/outputs/outstanding_mortgages_by_type_and_duration_of_loan.xlsx"

output_file_path_for_current_quarter_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/outstanding_mortgages/outstanding_mortgages_by_type_and_duration_of_loan.shp"
output_file_path_for_historical_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/outstanding_mortgages/outstanding_mortgages_by_type_and_duration_of_loan_historical.shp"

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

# Clean current/previous quarter state data ----

state_data <- data %>%
  filter(GEOLEVEL == 'State' & MARKET == mortgage_market & startsWith(SERIESID, 'PCT_TERM_')) %>%
  select(-c(GEOLEVEL, FREQUENCY, YEAR:SUPPRESSED, VALUE2))

# Create state_data_[Quarter]' by filtering the data for the following conditions
state_data_Q4 <- state_data %>%
  filter(PERIOD == current_quarter)

state_data_Q4 <- state_data_Q4 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pctarm14 = PCT_TERM_ARM_1_4, pctarm5p = PCT_TERM_ARM_5PL, pctfr15 = PCT_TERM_FRM_15, pctfr30 = PCT_TERM_FRM_30) %>%
  mutate(pctarm = pctarm14 + pctarm5p,
         pctfr = pctfr15 + pctfr30)

# Clean historical state data ----

state_data_historical <- state_data %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pctarm14 = PCT_TERM_ARM_1_4, pctarm5p = PCT_TERM_ARM_5PL, pctfr15 = PCT_TERM_FRM_15, pctfr30 = PCT_TERM_FRM_30) %>%
  mutate(pctarm = pctarm14 + pctarm5p,
         pctfr = pctfr15 + pctfr30)

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

# Clean current/previous quarter national data ----

national_data <- data %>%
  filter(GEOLEVEL == 'National' & MARKET == mortgage_market & startsWith(SERIESID, 'PCT_TERM_')) %>%
  select(-c(GEOLEVEL, FREQUENCY, YEAR:SUPPRESSED, VALUE2))

national_data_Q4 <- national_data %>%
  filter(PERIOD == current_quarter)

national_data_Q4 <- national_data_Q4 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pctarm14 = PCT_TERM_ARM_1_4, pctarm5p = PCT_TERM_ARM_5PL, pctfr15 = PCT_TERM_FRM_15, pctfr30 = PCT_TERM_FRM_30) %>%
  mutate(pctarm = pctarm14 + pctarm5p,
         pctfr = pctfr15 + pctfr30)

# Clean historical national data ----

national_data_historical <- national_data %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pctarm14 = PCT_TERM_ARM_1_4, pctarm5p = PCT_TERM_ARM_5PL, pctfr15 = PCT_TERM_FRM_15, pctfr30 = PCT_TERM_FRM_30) %>%
  mutate(pctarm = pctarm14 + pctarm5p,
         pctfr = pctfr15 + pctfr30)


national_data_historical <- national_data_historical %>%
  mutate(PERIOD = gsub("(\\d{4})(Q\\d)", "\\1 \\2", PERIOD))

# Outputting data ----

dataset_list <- list('State (current)' = state_data_Q4, 'State (hist.)' = state_data_historical, 
                     'National (current)' = national_data_Q4, 'National (hist.)' = national_data_historical)

write.xlsx(dataset_list, output_file_path_for_cleaned_data)

state_data_Q4 <- state_data_Q4 %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()

state_data_historical <- state_data_historical %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()

class(state_data_historical)
object.size(state_data_historical)

plot(state_data_historical)

arc.check_product()

arc.write(path = output_file_path_for_current_quarter_shapefile, data = state_data_Q4, overwrite = TRUE, validate = TRUE)
arc.write(path = output_file_path_for_historical_shapefile, data = state_data_historical, overwrite = TRUE, validate = TRUE)
