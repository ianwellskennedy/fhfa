# NOTES ----

# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "sf", "arcgisbinding", "openxlsx")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# Setting file paths ----

# Note: The files below are not available within this R project. 
# If anyone other than Ian is running this script download the latest data from this link: https://www.fhfa.gov/data/uniform-appraisal-dataset-aggregate-statistics
# Make sure to download data for 'Enterprise Single Family Appraisals' for 'States' and the '100 Largest Metro Areas'

input_state_fha_data_file_path <- "Uniform Appraisal Dataset/inputs/UADAggs_fha_sf_state_v3_3.csv"
input_metro_fha_data_file_path <- "Uniform Appraisal Dataset/inputs/UADAggs_fha_sf_cbsa_v3_3.csv"

input_state_ent_data_file_path <- "Uniform Appraisal Dataset/inputs/UADAggs_ent_sf_state_v3_3.csv" 

# Change this file path in the future, this file is too large for github to accept!
input_metro_ent_data_file_path <- "Uniform Appraisal Dataset/inputs/UADAggs_ent_sf_cbsa_v3_3.csv"

# Set the output file path
output_file_path_for_historical_data <- "Uniform Appraisal Dataset/outputs/effective_age_of_enterprise_appraised_sf_homes_2013_to_2023.xlsx"

# Note: The Uniform Appraisal Dataset utilizes metro definitions from 2020 (i.e. they are the same as in 2018)
state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/States/cb_2023_us_state_20m.shp"
metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/CBSAs/cb_2023_us_cbsa_500k.shp"
metro_division_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/Metro Divisions/cb_2023_us_metdiv_500k.shp"

output_file_path_for_state_ent_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_state_ent.shp"
output_file_path_for_state_fha_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_state_fha.shp"
output_file_path_for_state_combined_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_state_both.shp"

output_file_path_for_metro_ent_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_metro_ent.shp"
output_file_path_for_metro_fha_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_metro_fha.shp"
output_file_path_for_metro_combined_shapefile <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/effective_age/effective_age_of_all_appraised_homes_by_metro_both.shp"

latest_quarter_of_data <- 4
latest_year_of_data <- 2023

# Reading in data ----

state_data_fha <- read.csv(input_state_fha_data_file_path)
metro_data_fha <- read.csv(input_metro_fha_data_file_path)

state_data_ent <- read.csv(input_state_ent_data_file_path)
metro_data_ent <- read.csv(input_metro_ent_data_file_path)

# Creating state and metro level historical data sets ----

# Enterprise
state_data_historical_ent <- state_data_ent %>%
  filter(PURPOSE == 'Both' & SERIES == 'Count of Appraisals' & CHARACTERISTIC1 == 'Effective Age' & CATEGORY1 != 'Missing' & GEONAME != 'Puerto Rico' & QUARTER != '5') %>%
  select(GEONAME:STATEFIPS, YEAR, QUARTER, CATEGORY1, VALUE) %>%
  rename(count_of_appraisals = VALUE, state = GEONAME, state_abbr = STATEPOSTAL, state_fips_code = STATEFIPS, year = YEAR, quarter = QUARTER)

state_data_historical_ent <- state_data_historical_ent %>%
  pivot_wider(names_from = 'CATEGORY1', values_from = 'count_of_appraisals', id_cols = c('state':'state_fips_code', 'year', 'quarter'))

metro_data_historical_ent <- metro_data_ent %>%
  filter(PURPOSE == 'Both' & SERIES == 'Count of Appraisals' & CHARACTERISTIC1 == 'Effective Age' & CATEGORY1 != 'Missing' & QUARTER != '5') %>%
  select(GEONAME, METRO, YEAR, QUARTER, CATEGORY1, VALUE) %>%
  rename(count_of_appraisals = VALUE, metro_name = GEONAME, metro_code = METRO, year = YEAR, quarter = QUARTER)

metro_data_historical_ent <- metro_data_historical_ent %>%
  pivot_wider(names_from = 'CATEGORY1', values_from = 'count_of_appraisals', id_cols = c('metro_name':'quarter'))

# FHA
state_data_historical_fha <- state_data_fha %>%
  filter(PURPOSE == 'Both' & SERIES == 'Count of Appraisals' & CHARACTERISTIC1 == 'Effective Age' & CATEGORY1 != 'Missing' & GEONAME != 'Puerto Rico' & QUARTER != '5') %>%
  select(GEONAME:STATEFIPS, YEAR, QUARTER, CATEGORY1, VALUE) %>%
  rename(count_of_appraisals = VALUE, state = GEONAME, state_abbr = STATEPOSTAL, state_fips_code = STATEFIPS, year = YEAR, quarter = QUARTER)

state_data_historical_fha <- state_data_historical_fha %>%
  pivot_wider(names_from = 'CATEGORY1', values_from = 'count_of_appraisals', id_cols = c('state':'state_fips_code', 'year', 'quarter'))

metro_data_historical_fha <- metro_data_fha %>%
  filter(PURPOSE == 'Both' & SERIES == 'Count of Appraisals' & CHARACTERISTIC1 == 'Effective Age' & CATEGORY1 != 'Missing' & QUARTER != '5') %>%
  select(GEONAME, METRO, YEAR, QUARTER, CATEGORY1, VALUE) %>%
  rename(count_of_appraisals = VALUE, metro_name = GEONAME, metro_code = METRO, year = YEAR, quarter = QUARTER)

metro_data_historical_fha <- metro_data_historical_fha %>%
  pivot_wider(names_from = 'CATEGORY1', values_from = 'count_of_appraisals', id_cols = c('metro_name':'quarter'))

# Joining enterprise and fha data sets ----

# State
state_data_historical_both <- state_data_historical_ent %>%
  left_join(state_data_historical_fha, by = c('state', 'state_abbr', 'state_fips_code', 'year', 'quarter'))
  
state_data_historical_both <- state_data_historical_both %>%
  mutate(`0 to 5 Years` = `0 to 5 Years.x` + `0 to 5 Years.y`,
         `6 to 10 years` = `6 to 10 years.x` + `6 to 10 years.y`,
         `11 to 15 Years` = `11 to 15 Years.x` + `11 to 15 Years.y`,
         `16 to 20 Years` = `16 to 20 Years.x` + `16 to 20 Years.y`,
         `More than 20 Years` = `More than 20 Years.x` + `More than 20 Years.y`) %>%
  select(-matches("\\.")) %>%
  filter(year >= 2017)

# Metro
metro_data_historical_both <- metro_data_historical_ent %>%
  left_join(metro_data_historical_fha, by = c('metro_name', 'metro_code', 'year', 'quarter'))

metro_data_historical_both <- metro_data_historical_both %>%
  mutate(`0 to 5 Years` = `0 to 5 Years.x` + `0 to 5 Years.y`,
         `6 to 10 years` = `6 to 10 years.x` + `6 to 10 years.y`,
         `11 to 15 Years` = `11 to 15 Years.x` + `11 to 15 Years.y`,
         `16 to 20 Years` = `16 to 20 Years.x` + `16 to 20 Years.y`,
         `More than 20 Years` = `More than 20 Years.x` + `More than 20 Years.y`) %>%
  select(-matches("\\.")) %>%
  filter(year >= 2017)

# Outputting the historical data sets ----

dataset_list <- list('State - Enterprise' = state_data_historical_ent,
                     'State - FHA' = state_data_historical_fha,
                     'State - Combined' = state_data_historical_both,
                     
                     'Metro - Enterprise' = metro_data_historical_ent,
                     'Metro - FHA' = metro_data_historical_fha,
                     'Metro - Combined' = metro_data_historical_both)

write.xlsx(dataset_list, output_file_path_for_historical_data)

# Reading in shape files ----

state_shapefile <- st_read(state_shapefile_file_path) %>%
  select(STATEFP) %>%
  rename(STATEFIPS = STATEFP) %>%
  mutate(STATEFIPS = as.integer(STATEFIPS))

metro_shapefile <- st_read(metro_shapefile_file_path) %>%
  select(NAME, GEOID) %>%
  rename(METRO = GEOID) %>%
  mutate(METRO = as.integer(METRO))

crs <- sf::st_crs(metro_shapefile)

metro_division_shapefile <- st_read(metro_division_shapefile_file_path) %>%
  select(NAME, METDIVFP) %>%
  rename(METRO = METDIVFP) %>%
  mutate(METRO = as.integer(METRO))

metro_division_shapefile <- st_transform(metro_division_shapefile, crs)

metro_shapefile <- metro_shapefile %>%
  rbind(metro_division_shapefile)

# Creating spatial files ----

clean_state_data_for_spatial <- function(data) {
  data <- data %>%
    mutate(month = (quarter - 1) * 3 + 1,
           date = ymd(paste(year, month, "01", sep = "-")),
           date = as.character(as.Date(date))) %>%
    select(-c(month)) %>%
    mutate(quarter = paste0(year, " Q", quarter)) %>%
    select(state, state_abbr, state_fips_code, year, quarter, date, everything()) %>%
    rename(under_5 = `0 to 5 Years`, 
           six_ten = `6 to 10 years`,
           ele_fif = `11 to 15 Years`,
           six_twe = `16 to 20 Years`,
           over_20 = `More than 20 Years`) %>%
    mutate(across(under_5:over_20,
                  ~ . / rowSums(across(c(under_5, six_ten, ele_fif, six_twe, over_20))) * 100,
                  .names = "{.col}_p" # new column names
    )
    ) %>%
    left_join(state_shapefile, by = c('state_fips_code' = 'STATEFIPS')) %>%
    st_as_sf()
  
}

state_data_historical_ent <- state_data_historical_ent %>%
  clean_state_data_for_spatial() 
state_data_historical_fha <- state_data_historical_fha %>%
  clean_state_data_for_spatial()
state_data_historical_both <- state_data_historical_both %>%
  clean_state_data_for_spatial()

clean_metro_data_for_spatial <- function(data) {
  data <- data %>%
    mutate(month = (quarter - 1) * 3 + 1,
           date = ymd(paste(year, month, "01", sep = "-")),
           date = as.character(as.Date(date))) %>%
    select(-c(month)) %>%
    mutate(quarter = paste0(year, " Q", quarter)) %>%
    select(metro_name, metro_code, year, quarter, date, everything()) %>%
    rename(under_5 = `0 to 5 Years`, 
           six_ten = `6 to 10 years`,
           ele_fif = `11 to 15 Years`,
           six_twe = `16 to 20 Years`,
           over_20 = `More than 20 Years`) %>%
    mutate(across(under_5:over_20,
                  ~ . / rowSums(across(c(under_5, six_ten, ele_fif, six_twe, over_20))) * 100,
                  .names = "{.col}_p" # new column names
    )
    ) %>%
    left_join(metro_shapefile, by = c('metro_code' = 'METRO')) %>%
    st_as_sf()
}

metro_data_historical_ent <- metro_data_historical_ent %>%
  clean_metro_data_for_spatial() 
metro_data_historical_fha <- metro_data_historical_fha %>%
  clean_metro_data_for_spatial()
metro_data_historical_both <- metro_data_historical_both %>%
  clean_metro_data_for_spatial()

# Finalizing spatial data and outputting ----

arc.check_product()

arc.write(state_data_historical_ent, path = output_file_path_for_state_ent_shapefile, overwrite = T, validate = T)
arc.write(state_data_historical_fha, path = output_file_path_for_state_fha_shapefile, overwrite = T, validate = T)
arc.write(state_data_historical_both, path = output_file_path_for_state_combined_shapefile, overwrite = T, validate = T)

arc.write(metro_data_historical_ent, path = output_file_path_for_metro_ent_shapefile, overwrite = T, validate = T)
arc.write(metro_data_historical_fha, path = output_file_path_for_metro_fha_shapefile, overwrite = T, validate = T)
arc.write(metro_data_historical_both, path = output_file_path_for_metro_combined_shapefile, overwrite = T, validate = T)