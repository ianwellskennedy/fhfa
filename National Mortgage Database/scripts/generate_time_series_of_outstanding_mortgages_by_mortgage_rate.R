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
input_file_path <- "National Mortgage Database/inputs/2Q25/nmdb-outstanding-mortgage-statistics-all-quarterly.csv"
state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2024/States/cb_2024_us_state_20m.shp"

output_filepath_for_cleaned_data <- "National Mortgage Database/outputs/outstanding_mortgages_by_mortgage_rate.xlsx"

output_file_path_for_current_quarter_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/outstanding_mortgages/outstanding_mortgages_by_mortgage_rate.shp"
output_file_path_for_historical_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/outstanding_mortgages/outstanding_mortgages_by_mortgage_rate_historical.shp"
output_file_path_for_quarter_label_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/quarterly_label.shp"
output_file_path_for_mortgage_label_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/mortgage_label.shp"

current_quarter <- '2025Q2'
previous_quarter <- '2025Q1'
year_ago_quarter <- '2024Q2'

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
state_data_Q2 <- state_data %>%
  filter(PERIOD == current_quarter)

state_data_Q2 <- state_data_Q2 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

state_data_Q1 <- state_data %>%
  filter(PERIOD == previous_quarter)

state_data_Q1 <- state_data_Q1 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

state_data_Q2_24 <- state_data %>%
  filter(PERIOD == year_ago_quarter)

state_data_Q2_24 <- state_data_Q2_24 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_6 = pct_blw_3 + pct_3_4 + pct_4_5 + pct_5_6,
         pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5,
         pct_blw_4 = pct_blw_3 + pct_3_4)

# Create a state-level qoq / yoy difference file ----

state_data_diff <- state_data_Q2 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q2_pct_blw_5 = pct_blw_5)

state_data_diff_prev_qtr <- state_data_Q1 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_pct_blw_5 = pct_blw_5)

state_data_diff_prev_yr <- state_data_Q2_24 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q2_24_pct_blw_5 = pct_blw_5)

state_data_diff <- state_data_diff %>%
  left_join(state_data_diff_prev_qtr, by = 'GEONAME') %>%
  left_join(state_data_diff_prev_yr, by = 'GEONAME')

state_data_diff <- state_data_diff %>%
  mutate(qoq_diff = q2_pct_blw_5 - q1_pct_blw_5,
         yoy_diff = q2_pct_blw_5 - q2_24_pct_blw_5)

rm(state_data_diff_prev_qtr, state_data_diff_prev_yr)

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

# converter (vectorized via a loop, robust for a variety of input formats)
convert_to_midnight_char <- function(qtr_vec) {
  s <- as.character(qtr_vec)
  s <- trimws(s)
  out <- character(length(s))
  
  for (i in seq_along(s)) {
    si <- s[i]
    if (is.na(si) || si == "") { out[i] <- NA_character_; next }
    
    # 1) If it already looks like a YYYY-MM-DD date, use that date part
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", si)) {
      datepart <- regmatches(si, regexpr("\\d{4}-\\d{2}-\\d{2}", si))
      out[i] <- sprintf("%s 00:00:00", datepart)
      next
    }
    
    # 2) Extract 4-digit year
    yr_match <- regexpr("\\d{4}", si)
    if (yr_match == -1) { out[i] <- NA_character_; next }
    year <- substr(si, yr_match, yr_match + 3)
    
    # 3) Try to find "Q" followed by 1-4 (e.g. Q2, Q 2)
    q_match <- regexec("Q\\s*([1-4])", si, perl = TRUE)
    q_parts <- regmatches(si, q_match)[[1]]
    if (length(q_parts) >= 2 && nchar(q_parts[2]) > 0) {
      qtr <- q_parts[2]
    } else {
      # 4) Fallback: find first digit 1-4 after the year (handles "2025 2" or "2025-2")
      post <- substring(si, yr_match + 4)
      qpos <- regexpr("[1-4]", post)
      if (qpos == -1) { out[i] <- NA_character_; next }
      qtr <- substr(post, qpos, qpos)
    }
    
    # 5) Compute month from quarter and return formatted string
    month <- (as.integer(qtr) - 1) * 3 + 1
    out[i] <- sprintf("%s-%02d-01 00:00:00", year, month)
  }
  
  out
}

state_data_historical <- state_data_historical %>%
  mutate(
    Quarter = gsub("(\\d{4})(Q\\d)", "\\1 \\2", PERIOD),
    PERIOD = convert_to_midnight_char(PERIOD))

state_data_historical <- state_data_historical %>%
  select(SOURCE:PERIOD, Quarter, everything())

state_data_historical <- state_data_historical %>%
  left_join(mortgage_rates, by = 'Quarter') 

# Clean current/previous quarter national data ----

national_data <- data %>%
  filter(GEOLEVEL == 'National' & MARKET == mortgage_market & startsWith(SERIESID, 'PCT_INTRATE_')) %>%
  select(-c(GEOLEVEL, FREQUENCY, YEAR:SUPPRESSED, VALUE2))

national_data_Q2 <- national_data %>%
  filter(PERIOD == current_quarter)

national_data_Q2 <- national_data_Q2 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

national_data_Q1 <- national_data %>%
  filter(PERIOD == previous_quarter)

national_data_Q1 <- national_data_Q1 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

national_data_Q2_24 <- national_data %>%
  filter(PERIOD == year_ago_quarter)

national_data_Q2_24 <- national_data_Q2_24 %>%
  pivot_wider(id_cols = c(SOURCE, GEOID, GEONAME, PERIOD), names_from = SERIESID, values_from = VALUE1) %>%
  rename(pct_blw_3 = PCT_INTRATE_LT_3, pct_3_4 = PCT_INTRATE_3_4, pct_4_5 = PCT_INTRATE_4_5, pct_5_6 = PCT_INTRATE_5_6, pct_6_plus = PCT_INTRATE_GE_6) %>%
  mutate(pct_blw_5 = pct_blw_3 + pct_3_4 + pct_4_5)

# Create a national qoq / yoy difference file ----

national_data_diff <- national_data_Q2 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q2_pct_blw_5 = pct_blw_5)

national_data_diff_prev_qtr <- national_data_Q1 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q1_pct_blw_5 = pct_blw_5)

national_data_diff_prev_yr <- national_data_Q2_24 %>%
  select(GEONAME, pct_blw_5) %>%
  rename(q2_24_pct_blw_5 = pct_blw_5)

national_data_diff <- national_data_diff %>%
  left_join(national_data_diff_prev_qtr, by = 'GEONAME') %>%
  left_join(national_data_diff_prev_yr, by = 'GEONAME')

national_data_diff <- national_data_diff %>%
  mutate(qoq_diff = q2_pct_blw_5 - q1_pct_blw_5,
         yoy_diff = q2_pct_blw_5 - q2_24_pct_blw_5)

rm(national_data_diff_prev_qtr, national_data_diff_prev_yr)

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

dataset_list <- list('State (current)' = state_data_Q2, 'State (hist.)' = state_data_historical, 
                     'National (current)' = national_data_Q2, 'National (hist.)' = national_data_historical)

write.xlsx(dataset_list, output_filepath_for_cleaned_data)

# Outputting shape files (ignore if not interested in spatial files) ----

state_data_Q2 <- state_data_Q2 %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()

state_data_historical <- state_data_historical %>%
  left_join(state_shapefile, by = c('GEONAME' = 'NAME')) %>%
  st_as_sf()


object.size(state_data_historical)

state_data_historical <- st_simplify(state_data_historical, dTolerance = 500, preserveTopology = TRUE)

arc.check_product()

arc.write(path = output_file_path_for_current_quarter_shapefile, data = state_data_Q2, overwrite = TRUE, validate = TRUE)
arc.write(path = output_file_path_for_historical_shapefile, data = state_data_historical, overwrite = TRUE, validate = TRUE)
