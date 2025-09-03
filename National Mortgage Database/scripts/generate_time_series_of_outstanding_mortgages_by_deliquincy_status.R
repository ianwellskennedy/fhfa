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
input_file_path <- "National Mortgage Database/inputs/1Q25/nmdb-mortgage-performance-statistics-all-quarterly.csv"

# Note: The Uniform Appraisal Dataset utilizes metro definitions from 2020 (i.e. they are the same as in 2018)
state_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/States/cb_2023_us_state_20m.shp"
metro_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/CBSAs/cb_2023_us_cbsa_500k.shp"
metro_division_shapefile_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/Metro Divisions/cb_2023_us_metdiv_500k.shp"

output_filepath_for_cleaned_data <- "National Mortgage Database/outputs/outstanding_mortgages_by_delinquincy_status.xlsx"

output_file_path_for_current_quarter_shapefile <- "National Mortgage Database/outputs/shapefiles/outstanding_mortgages_by_delinquincy_status.shp"
output_file_path_for_historical_shapefile <- "National Mortgage Database/outputs/shapefiles/outstanding_mortgages_by_delinquincy_status_historical.shp"

current_quarter <- '2025Q1'
previous_quarter <- '2024Q4'
year_ago_quarter <- '2024Q1'

mortgage_market <- 'All Mortgages'

# Reading in data ----

data <- read.csv(input_file_path)

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

# Creating state and metro level historical data sets ----

data <- data %>%
  filter(GEOLEVEL %in% c("Metro Area", "State") & MARKET == "All Mortgages") %>%
  select(GEOLEVEL:GEONAME, YEAR, QUARTER, SERIESID, VALUE1) %>%
  rename(count_of_appraisals = VALUE1, year = YEAR, quarter = QUARTER, variable = SERIESID)

data_state <- data %>%
  filter(GEOLEVEL == 'State') %>%
  select(-GEOLEVEL) %>%
  rename(state_name = GEONAME, state_abbr = GEOID) %>%
  select(state_name, state_abbr, everything())

data_metro <- data %>%
  filter(GEOLEVEL == "Metro Area") %>%
  select(-GEOLEVEL) %>%
  rename(metro_name = GEONAME, metro_code = GEOID) %>%
  select(metro_name, metro_code, everything()) 


data_state <- data_state %>%
  pivot_wider(names_from = 'variable', values_from = 'count_of_appraisals', id_cols = c('state':'state_abbr', 'year', 'quarter'))

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
