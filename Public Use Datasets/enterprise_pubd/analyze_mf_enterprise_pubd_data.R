# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "sf", "arcgisbinding", "openxlsx", "spatstat")

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

input_freddie_mac_data_file_path <- "Public Use Datasets/enterprise_pubd/inputs/2024_pudb_mf_ctf/2024_pudb_mf_ctf_fhlmc.csv"
input_fannie_mae_data_file_path <- "Public Use Datasets/enterprise_pubd/inputs/2024_pudb_mf_ctf/2024_pudb_mf_ctf_fnma.csv"

# Set the output file path
output_file_path_for_historical_data <- "enterprise_pubd/outputs/output.xlsx"

# Note: The Uniform Appraisal Dataset utilizes metro definitions from 2020 (i.e. they are the same as in 2018)
state_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/States/cb_2023_us_state_20m.shp"
metro_shp_file_path <- "C:/Users/ianwe/Downloads/shapefiles/2023/CBSAs/cb_2023_us_cbsa_500k.shp"

output_file_path_for_state_shp <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/.shp"

output_file_path_for_metro_shp <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/.shp"

output_file_path_for_county_shp <- "C:/Users/ianwe/Downloads/ArcGIS projects for github/fhfa/shapefiles/.shp"

latest_year_of_data <- 2024

# Reading in data ----

freddie_mac_data <- read.csv(input_freddie_mac_data_file_path)
fannie_mae_data <- read.csv(input_fannie_mae_data_file_path)

# Reading in shape files ----

state_shp <- st_read(state_shp_file_path) %>%
  select(STATEFP) %>%
  rename(STATEFIPS = STATEFP) %>%
  mutate(STATEFIPS = as.integer(STATEFIPS))

metro_shp <- st_read(metro_shp_file_path) %>%
  select(NAME, GEOID) %>%
  rename(METRO = GEOID) 

# Joining files and cleaning joined data ----

joined_data <- freddie_mac_data %>%
  rbind(fannie_mae_data)

joined_data <- joined_data %>%
  select(record_num_mf_ctf, enterprise, tract_2020, county_fips, cbsa_metro_code, state_fips, 
         purpose_ctf, seller_type_mf_ctf, fed_guarantee_ctf, lien_status, ltv, term_orig, units_num_cat, 
         rate_orig, upb_orig, property_value, term_prepay_penalty, construct_method) 

joined_data <- joined_data %>%
  mutate(
    enterprise = case_when(
      enterprise == 1 ~ 'Freddie Mac',
      enterprise == 2 ~ 'Fannie Mae',
      T ~ NA_character_
    ),
    tract_2020 = as.character(tract_2020),
    county_fips = as.character(county_fips),
    cbsa_metro_code = as.character(cbsa_metro_code),
    state_fips = as.character(state_fips),
    purpose_ctf = case_when(
      purpose_ctf == 1 ~ 'purchase',
      purpose_ctf == 2 ~ 'refi_non_cash_out',
      purpose_ctf == 4 ~ 'hi_rehab',
      purpose_ctf == 7 ~ 'refi_cash_out',
      purpose_ctf == 9 ~ 'NA_other',
      T ~ NA_character_
    ),
    seller_type_mf_ctf = case_when(
      seller_type_mf_ctf == 1 ~ 'mortgage_company',
      seller_type_mf_ctf == 2 ~ 'saif_inst',
      seller_type_mf_ctf == 3 ~ 'bif_inst',
      seller_type_mf_ctf == 4 ~ 'credit_union',
      seller_type_mf_ctf == 5 ~ 'other_or_unknown',
      T ~ NA_character_
    ),
    fed_guarantee_ctf = case_when(
      fed_guarantee_ctf == 1 ~ 'conventional',
      fed_guarantee_ctf == 2 ~ 'fha',
      fed_guarantee_ctf == 3 ~ 'va',
      fed_guarantee_ctf == 4 ~ 'fsa_rhs',
      T ~ NA_character_
    ),
    lien_status = case_when(
      lien_status == 1 ~ 'sec_by_first_lien',
      lien_status == 2 ~ 'sec_by_sub_lien',
      lien_status == 3 ~ 'not_sec_by_lien',
      lien_status == 4 ~ NA,
      T ~ NA_character_
    ),
    units_num_cat = case_when(
      units_num_cat == 1 ~ '5_to_24',
      units_num_cat == 2 ~ '25_to_50',
      units_num_cat == 3 ~ '51_to_99',
      units_num_cat == 4 ~ '100_to_149',
      units_num_cat == 5 ~ 'over_149',
      units_num_cat == 9 ~ 'unknown',
      T ~ NA_character_
    ),
    construct_method = case_when(
      construct_method == 1 ~ 'site_built',
      construct_method == 2 ~ 'manufactured_mobile',
      construct_method == 9 ~ NA,
      T ~ NA_character_
    ),
    property_value = case_when(
      property_value == 999999999 ~ NA_integer_,
      T ~ property_value
    ),
    ,
    upb_orig = case_when(
      upb_orig == 999999999 ~ NA_integer_,
      T ~ upb_orig
    )
    )


# Creating state and metro level historical data sets ----

state_data <- joined_data %>%
  group_by(state_fips) %>%
  summarize(properties = n(),
            avg_ltv = mean(ltv, na.rm = T),
            avg_ltv_w = weighted.mean(ltv, w = upb_orig, na.rm = T),
            med_ltv = median(ltv, na.rm = T),
            med_ltv_w = weighted.median(ltv, w = upb_orig, na.rm = T),
            avg_prop_val = mean(property_value, na.rm = T),
            med_prop_val = median(property_value, na.rm = T)) %>%
  ungroup() 

cbsa_data <- joined_data %>%
  group_by(cbsa_metro_code) %>%
  summarize(properties = n(),
            avg_ltv = mean(ltv, na.rm = T),
            avg_ltv_w = weighted.mean(ltv, w = upb_orig, na.rm = T),
            med_ltv = median(ltv, na.rm = T),
            med_ltv_w = weighted.median(ltv, w = upb_orig, na.rm = T),
            avg_prop_val = mean(property_value, na.rm = T),
            med_prop_val = median(property_value, na.rm = T)) %>%
  ungroup()


# Outputting the historical data sets ----

dataset_list <- list('State' = state_data,
                     'Metro' = metro_data)

write.xlsx(dataset_list, output_file_path_for_historical_data)

# Creating spatial files ----

# Finalizing spatial data and outputting ----

arc.check_product()

arc.write(metro_data, path = output_file_path_for_state_shp, overwrite = T, validate = T)
arc.write(state_data, path = output_file_path_for_state_shp, overwrite = T, validate = T)
