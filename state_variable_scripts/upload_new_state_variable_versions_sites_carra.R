#### --------------------------------------------------------------------------------------------------------- ####
### CLIMATE STATE VARIABLES FOR COAT SITES BASED ON CARRA
### CREATE NEW STATE VARIABLE VERSIONS AND UPDATE WITH NEW STATE VARIABLE DATA
### Date: November 2026
### Author: Guro Bang Synnes Aase, based on scripts by Hanna Bohner
#### --------------------------------------------------------------------------------------------------------- ####

## ------------------------------------------------------------------ ##
## WORKFLOW
## ------------------------------------------------------------------ ##

# This script formats new annual files for all selected climate state variables for COAT sites based on CARRA datasets from Thredds,
# creates new versions of corresponding state variable datasets on the COAT data portal and uploads the annual files to these new versions

# STEPS
# 1.  import libraries, set up connection to COAT data portal and Thredds, import function to get COAT coordinates from GitHub, set directories
# 2.  select state variable names and years for Thredds data to upload to new versions on the data portal, set month names and numbers,
#     import a configuration table with information about Thredds folder structures and COAT format settings for all state variables
# 3.  get coordinates for all COAT sites from the data portal, format these coordinates to suit raster layers from Thredds
# 4.  define functions for creating Thredds data file names and download urls
# 5.  define function that, for a row in the configuration table, downloads corresponding files from Thredds and extracts data from COAT coordinates
# 6.  run main loop, which downloads Thredds data and extracts data values for COAT sites for all selected state variables
# 7.  for each selected state variable, write annual data file for the selected year with formatting settings specific to each state variable
# 8.  define function that creates a new state variable version on the COAT data portal and uploads files from previous version and annual file
# 9.  check upload summary and make sure annual files in temporary directory look correct
# 10. for all selected state variables, create new state variable versions on the data portal and upload corresponding files

# CONFIGURATION TABLE
# One configuration row controls the following:
# folder_name_thredds   folder name on Thredds server
# base_name_thredds     base name of data file on Thredds server (without version specification)
# base_name_coat        base of COAT climate state variable name
# frequency             whether the data represent annual or monthly intervals
# year_type             whether the data represent whole years, summer seasons or winter seasons
# value_name            desired name for the data value column in annual file formatted after COAT standards
# decimals_number       desired number of decimals in annual file formatted after COAT standards


## ------------------------------------------------------------------ ##
## 1. SETUP
## ------------------------------------------------------------------ ##

## clear workspace
rm(list = ls())

## load libraries
if (!require("remotes")) install.packages("remotes")
if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")};library("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) {install.packages("lubridate")};library("lubridate")
if (!requireNamespace("jsonlite", quietly = TRUE)) {install.packages("jsonlite")};library("jsonlite")
if (!requireNamespace("sf", quietly = TRUE)) {install.packages("sf")};library("sf")
if (!requireNamespace("ckanr", quietly = TRUE)) {install.packages("ckanr")};library("ckanr")
if (!requireNamespace("ncdf4", quietly = TRUE)) {install.packages("ncdf4")};library("ncdf4")
library(purrr)
library(leaflet)
library(htmlwidgets)
library(terra)
library(readxl)
library(dplyr)
library(raster)
library(stringr)
library(tibble)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

## set up the connection to the COAT data portal
COAT_url <- "https://data.coat.no/"  # write here the url to the COAT data portal
COAT_key <- Sys.getenv("COAT_key")  # paste your API token here (Manage -> API tokens on data.coat.no) or save it as an Environment variable
ckanr_setup(url = COAT_url, key = if (COAT_key == "") NULL else COAT_key)

## set up the connection to thredds.met.no
thredds_path <- "https://thredds.met.no/thredds/fileServer/coat/gridded_indicators_beta/carra"

## load functions from GitHub
source("https://github.com/COATnor/climate_module/blob/main/functions/function_get_coat_coordinates.R?raw=TRUE")

## set directories
temp_dir <- file.path(tempdir(), "COAT_met_downloads")
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)


## ---------------------------------------------------------------------------- ##
## 2. SELECT STATE VARIABLES, SET YEARS AND MONTHS, IMPORT CONFIGURATION TABLE
## ---------------------------------------------------------------------------- ##

## select desired state variables
state_variables_upload <- c(#"c101_annual_mean_air_temperature_sites_carra",
  "c102_monthly_mean_air_temperature_sites_carra",
  #"c103_winter_melt_days_sites_carra",
  "c104_cold_days_sites_carra",
  #"c105_annual_degree_day_sum_sites_carra",
  #"c106_growing_season_length_sites_carra",
  #"c107_freeze_melt_days_sites_carra",
  "c108_tree_growing_season_sites_carra"
  #"c301_annual_sum_precipitation_sites_carra",
  #"c302_annual_sum_rain_snow_sites_carra",
  #"c303_monthly_sum_precipitation_sites_carra",
  #"c501_snow_cover_duration_sites_carra",
  #"c701_rain_on_snow_days_sites_carra"
)

## select desired upload year
years <- 2025

## set months and import configuration table
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month_numbers <- setNames(1:12, months)

configuration_file <- paste0("C:/Users/ADD_USER_HERE/Box/COAT/Data Management/material_COAT_state_variables/climate_state_variable_names_thredds.xlsx")
configuration_table <- read_excel(path = configuration_file) %>%
  select(folder_name_thredds, base_name_thredds, base_name_coat, state_variable_data_portal_version, frequency, year_type, value_name, decimals_number) %>%
  filter(base_name_coat %in% state_variables_upload)


## ------------------------------------------------------------------ ##
## 3. GET AND FORMAT COORDINATES FROM THE COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## download all coordinates from the COAT dataportal (IMPORTANT: the coordinates and the functions have to be checked carefully once before the state variables are updated)
#coords <- get_coat_coordinates()

## this is just a temporary example file as the get_coat_coordinates() function doesn't work at the moment
coordinates_file <- paste0("C:/Users/ADD_USER_HERE/Downloads/S_ptarmigan_counts_coordinates.txt")
coords_test <- read.csv(file = coordinates_file, sep = ";", stringsAsFactors = FALSE)

## generate test columns to make df similar to output from get_coat_coordinates()
coords_test <- coords_test %>%
  mutate(
    sn_region = "region",
    sn_locality = "locality",
    v_dataset_name = "ptarmigan counts") %>%
  select(sn_region, sn_locality, sn_site, e_dd, n_dd, e_utm_33, n_utm33, v_dataset_name) %>%
  arrange(sn_region, sn_locality, sn_site, v_dataset_name)

## format coordinates to suit raster layer data from Thredds 
coord <- cbind.data.frame(lon=coords_test$e_dd, lat=coords_test$n_dd) # only columns with coordinates
coord <- st_as_sf(coord, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84") # convert to spatial object
coord <- st_transform(coord, "+proj=lcc +lat_1=80 +lat_0=80 +lon_0=326 +x_0=-551891.1979581077 +y_0=1031350.2258432541 +R=6367470 +datum=WGS84 +units=m +no_defs")  
coord <- st_coordinates(coord)


## ------------------------------------------------------------------ ##
## 4. DEFINE FUNCTIONS TO CREATE THREDDS FILENAMES AND DOWNLOAD URLS
## ------------------------------------------------------------------ ##

make_filenames <- function(base_name_thredds, frequency, years, months) {
  if (frequency == "annual") {
    tibble(
      year = years,
      month = NA_integer_,
      filename = paste0(base_name_thredds, years, ".nc"))
  } else if (frequency == "monthly") {
    expand.grid(
      year = years,
      month_name = months,
      stringsAsFactors = FALSE) %>%
      mutate(
        month = unname(
          month_numbers[month_name]),
        filename = paste0(base_name_thredds, month_name, ".", year, ".nc")) %>%
      arrange(year, month) %>%
      dplyr::select(year, month, filename)
  } else {
    stop(
      "Unknown frequency: ", frequency, ". Must be 'annual' or 'monthly'.")
  }
}

make_download_url <- function(thredds_path, folder_name_thredds, filename) {
  paste0(thredds_path, "/", folder_name_thredds, "/", filename)}


## --------------------------------------------------------------------- ##
## 5. DEFINE FUNCTION TO EXTRACT THREDDS DATA FOR ONE CONFIGURATION ROW
## --------------------------------------------------------------------- ##

## the function generates all Thredds filenames belonging to a specific configuration row
extract_data <- function(config_row, years, months, coord, coords_metadata, temp_dir) {
  
  base_name_thredds <- as.character(config_row$base_name_thredds[[1]])
  folder_name_thredds <- as.character(config_row$folder_name_thredds[[1]])
  base_name_coat <- as.character(config_row$base_name_coat[[1]])
  frequency <- as.character(config_row$frequency[[1]])
  year_type <- as.character(config_row$year_type[[1]])
  value_name <- as.character(config_row$value_name[[1]])
  
  ## validate configuration
  if (is.na(base_name_thredds) || base_name_thredds == "")
  {stop("base_name_thredds is missing.")}
  if (is.na(folder_name_thredds) || folder_name_thredds == "")
  {stop("folder_name_thredds is missing for ", base_name_thredds)}
  if (is.na(base_name_coat) || base_name_coat == "")
  {stop("base_name_coat is missing for ", base_name_thredds)}
  if (is.na(frequency) || frequency == "")
  {stop("frequency is missing for ", base_name_thredds)}
  if (is.na(year_type) || year_type == "")
  {stop("year_type is missing for ", base_name_thredds)}
  if (is.na(value_name) || value_name == "")
  {stop("value_name is missing for ", base_name_thredds)}
  
  ## generate file names for this configuration row
  file_table <- make_filenames(
    base_name_thredds = base_name_thredds,
    frequency = frequency,
    years = years,
    months = months)
  
  ## storage
  output_list <- vector("list", nrow(file_table))
  
  ## process every file belonging to this configuration
  for (i in seq_len(nrow(file_table))) {
    
    filename <- file_table$filename[i]
    message(
      "\nProcessing configuration row: ",
      "base_name_thredds = ", base_name_thredds,
      ", value_name = ", value_name,
      ", frequency = ", frequency,
      "\nFile: ", filename)
    
    ## temporary file
    local_file <- file.path(temp_dir, filename)
    
    ## build url using this configuration row
    url <- make_download_url(
      thredds_path = thredds_path,
      folder_name_thredds = folder_name_thredds,
      filename = filename)
    cat("\nDownload URL:\n", url, "\n")
    cat("\nLocal destination:\n", local_file, "\n")
    
    ## download
    download_status <- tryCatch({
      download.file(url = url, destfile = local_file, mode = "wb", method = "libcurl")
    }, error = function(e) {
      message("\nDownload failed: ", conditionMessage(e))
      NA_integer_
    })
    
    ## check downloaded file
    if (
      is.na(download_status) ||
      download_status != 0 ||
      !file.exists(local_file) ||
      is.na(file.info(local_file)$size) ||
      file.info(local_file)$size == 0) {
      message("\nSkipping unavailable file: ", filename,
              "\nContinuing with next download...")
      if (file.exists(local_file)) {file.remove(local_file)}
      next
    }
    
    ## open NetCDF raster
    dat_raster <- tryCatch({raster::raster(local_file)}, error = function(e) {
      stop("\nCould not read NetCDF file:\n", local_file, "\nError: ", conditionMessage(e))})
    
    ## extract values at coordinates using bilinear method
    dat <- raster::extract(dat_raster, coord, method = "bilinear")
    
    ## remove temporary NetCDF
    file.remove(local_file)
    
    ## create output data
    out_dat <- coords_metadata %>%
      mutate(
        t_year = file_table$year[i],
        t_month = file_table$month[i])
    
    ## add variable
    out_dat[[value_name]] <- dat
    
    ## add configuration metadata
    out_dat$base_name_coat <- base_name_coat
    
    ## store data
    output_list[[i]] <- out_dat
  }
  
  ## combine all files for this configuration
  bind_rows(output_list)
}


## -------------------------------------------------------------------- ##
## 6. MAIN LOOP: EXTRACT THREDDS DATA FOR ALL SELECTED STATE VARIABLES 
## -------------------------------------------------------------------- ##

## The outer loop is over configuration table rows
## The function make_filenames() within extract_data() generates the file names belonging to each row

all_extracted <- vector("list", nrow(configuration_table))

for (config_i in seq_len(nrow(configuration_table))) {
  
  message(
    "\n\n============================================",
    "\nCONFIGURATION ", config_i, " OF ", nrow(configuration_table),
    "\n============================================")
  
  ## select current configuration row
  config_row <- configuration_table[config_i, ,drop = FALSE]
  
  ## extract data for current configuration
  all_extracted[[config_i]] <- extract_data(
    config_row = config_row,
    years = years,
    months = months,
    coord = coord,
    coords_metadata = coords_test %>%
      dplyr::select(sn_region, sn_locality, sn_site),
    temp_dir = temp_dir)
}

## combine extracted data from all configuration rows
all_extracted <- bind_rows(all_extracted)

## combine data value columns for the same base_name_coat
configuration_groups <- split(all_extracted, all_extracted$base_name_coat)


## ------------------------------------------------------------------ ##
## 7. WRITE ANNUAL DATA FILES
## ------------------------------------------------------------------ ##

## store information about files for later uploading
upload_overview <- data.frame(
  base_name_coat = character(),
  year_type = character(),
  file_type = character(),
  file_name = character(),
  file_path = character(),
  stringsAsFactors = FALSE)

for (coat_name in names(configuration_groups)) {
  
  message(
    "\n============================================",
    "\nWriting output for: ", coat_name,
    "\n============================================"  )
  
  coat_data <- configuration_groups[[coat_name]]
  
  ## get frequency and year_type from configuration_table
  config_info <- configuration_table %>%
    filter(base_name_coat == coat_name)
  
  if (nrow(config_info) == 0) {
    stop("No matching configuration found for: ", coat_name)}
  
  frequencies <- unique(config_info$frequency)
  if (length(frequencies) != 1 || !frequencies %in% c("annual", "monthly")) {
    stop("Invalid or inconsistent frequency for: ", coat_name)}
  frequency <- frequencies[1]
  
  year_types <- unique(config_info$year_type)
  if (length(year_types) != 1 || !year_types %in% c("whole_year", "winter_season", "summer_season")) {
    stop("Invalid or inconsistent year_type for: ", coat_name)}
  year_type <- year_types[1]
  
  ## define identifying columns
  id_columns <- c("sn_region", "sn_locality", "sn_site", "t_year")
  
  ## month is only an identifying column for monthly data
  if (frequency == "monthly") {id_columns <- c(id_columns, "t_month")}
  
  ## only keep value_name columns belonging to this base_name_coat
  value_columns <- unique(as.character(config_info$value_name))
  
  ## check that all expected value columns exist
  missing_columns <- setdiff(value_columns, names(coat_data))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing value columns for ", coat_name, ": ",
      paste(missing_columns, collapse = ", "))
  }
  
  ## keep identifying columns and variables specific to the current base_name_coat only
  coat_data <- coat_data %>%
    select(all_of(c(id_columns, value_columns)))
  
  ## round value columns according to configuration
  for (i in seq_len(nrow(config_info))) {
    
    value_name <- as.character(config_info$value_name[i])
    decimals <- as.integer(config_info$decimals_number[i])
    if (is.na(decimals) || decimals < 0) {
      stop("Invalid decimals_number for variable: ", value_name," in ", coat_name)}
    coat_data[[value_name]] <- round(coat_data[[value_name]], digits = decimals)
  }
  
  ## combine rows belonging to the same location and time
  group_columns <- id_columns
  
  coat_data <- coat_data %>%
    group_by(across(all_of(group_columns))) %>%
    summarise(
      across(all_of(value_columns),
             ~ {x <- .x[!is.na(.x)]
             if (length(x) == 0) {NA_real_
             } else {x[1]}}),
      .groups = "drop") %>%
    mutate()
  
  ## sort data
  sort_columns <- c("sn_region", "sn_locality", "sn_site", "t_year")
  if (frequency == "monthly") {sort_columns <- c(sort_columns, "t_month")}
  
  coat_data <- coat_data %>%
    arrange(across(all_of(sort_columns)))
  
  ## write one file per year
  years_to_write <- sort(unique(coat_data$t_year))
  for (year in years_to_write) {
    
    year_data <- coat_data %>%
      filter(t_year == year)
    if (year_type == "winter_season") {
      year_data <- year_data %>%
        rename(t_year_winter = t_year)
    }
    
    capitalized_name <- paste0(toupper(substr(coat_name, 1, 1)), substr(coat_name, 2, nchar(coat_name)))
    file_name <- paste0(capitalized_name, "_", year, ".txt")
    output_file <- file.path(temp_dir, file_name)
    write.table(year_data, file = output_file, sep = ";", row.names = FALSE,
                quote = which(vapply(year_data, is.character, logical(1))), na = "NA")
    
    message("Written: ", output_file)
    
    ## add annual data file to upload overview
    upload_overview <- rbind(
      upload_overview,
      data.frame(
        base_name_coat = coat_name,
        year_type = year_type,
        file_type = "data",
        file_name = file_name,
        file_path = output_file,
        stringsAsFactors = FALSE))
  }
}

## save upload review and write message
overview_file <- file.path(temp_dir, "upload_overview.txt")
write.table(upload_overview, file = overview_file, sep = ";", row.names = FALSE, quote = FALSE)

message(
  "\n============================================",
  "\nExtraction and file writing complete.",
  "\nOutput directory: ",
  temp_dir,
  "\nUpload overview: ",
  overview_file,
  "\n============================================")


## ------------------------------------------------------------------------------ ##
## 8. DEFINE FUNCTION FOR CREATING NEW STATE VARIABLE VERSION AND UPLOADING DATA
## ------------------------------------------------------------------------------ ##

upload_new_version_data_portal <- function(coat_name, upload_overview) {
  
  ## get files belonging to this state variable dataset
  coat_files <- upload_overview %>%
    filter(base_name_coat == coat_name)
  
  state_name <- coat_name
  
  ## find current state variable version on COAT data portal
  search_result <- package_search(
    q = state_name,
    fq = "{!collapse field=base_name max=version_i}",
    include_private = TRUE,
    include_drafts = TRUE)
  
  if (length(search_result$results) == 0) {stop("COAT state variable dataset not found: ", state_name)}
  
  pkg_state <- search_result[["results"]][[1]]
  state_version <- as.integer(pkg_state[["version"]])
  
  ## get names of resources already in this package
  if (is.null(pkg_state$resources) || length(pkg_state$resources) == 0) {
    filenames_state <- character(0)
  } else {
    filenames_state <- vapply(
      pkg_state$resources,
      function(x) {
        if (is.null(x$name)) NA_character_ else as.character(x$name)
      }, character(1))
    
    filenames_state <- filenames_state[!is.na(filenames_state)]
  }
  
  message("COAT state variable dataset: ", state_name)
  message("Existing resources: ", paste(filenames_state, collapse = ", "))
  
  ## settings for the new version of the state variable
  state_version_new <- state_version + 1  # new version
  state_name_full <- paste(state_name, "_v", state_version, sep = "")
  state_name_new <- sub(state_version, state_version_new, state_name_full)  # name of the new version
  
  end_new <- if (year_type == "winter_season") {
    paste0(years[1], "-04-30")
  } else if (year_type == "summer_season") {
    paste0(years[1], "-10-31")
  } else {
    paste0(years[1], "-12-31")
  }
  #embargo_new <- ""  # write here the new embargo end date
  
  ## modify tags (necessary to avoid a validation error)
  new_tags <- c()
  for (i in 1:length(pkg_state$tags)){
    new_tags[[i]] <- list(name = pkg_state$tags[[i]]$name)
  }
  
  ## creating the new version of the state variable
  package_create(name = state_name_new,
                 title = pkg_state$title,
                 private = TRUE,  # this is default
                 tags = new_tags,
                 author = pkg_state$author,
                 author_email = pkg_state$author_email,
                 license_id = pkg_state$license_id, 
                 notes = pkg_state$notes, 
                 version = as.character(state_version_new), 
                 owner_org = pkg_state$owner_org, 
                 state = "active", 
                 type = "state-variable",
                 extras = list(topic_category = pkg_state$topic_category, 
                               position = pkg_state$position,
                               publisher = pkg_state$publisher,
                               associated_parties = pkg_state$associated_parties,
                               persons = pkg_state$persons,
                               temporal_start = pkg_state$temporal_start,
                               temporal_end = end_new,
                               location = pkg_state$location,
                               scripts = pkg_state$scripts,
                               protocol = pkg_state$protocol,
                               bibliography = pkg_state$bibliography,
                               funding = pkg_state$funding#,
                               #embargo = embargo_new
                 ))
  
  ## the resources (data files) of the old version have to be added to the new version of the dataset
  
  ## get the new version of the package
  pkg_state_new<-package_search(q = list(paste("name:", state_name_new, sep = "")), fq = list(paste("version:", state_version_new, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]]
  
  ## add all resources
  if (length(pkg_state$resources) > 0) {
    for (i in 1:length(pkg_state$resources)) {
      resource_create(pkg_state_new$id,
                      rcurl = pkg_state$resources[[i]]$url, 
                      name = pkg_state$resources[[i]]$name, 
                      http_method = "POST", 
                      description = pkg_state$resources[[i]]$description)
    }
  }
  
  ## upload annual data files, if they do not already exist
  data_files <- coat_files %>%
    filter(file_type == "data")
  
  for (i in seq_len(nrow(data_files))) {
    
    data_name <- data_files$file_name[i]
    data_path <- data_files$file_path[i]
    
    if (!file.exists(data_path)) {
      stop("Data file does not exist locally: ", data_path)
    }
    if (data_name %in% filenames_state) {
      message("Already on data portal - skipping data file: ", data_name)
    } else {
      message("Uploading data file: ", data_name)
      
      resource_create(
        package_id = pkg_state_new$id,
        description = NULL,
        upload = data_path,
        name = data_name,
        http_method = "POST")
      
      filenames_state <- c(filenames_state, data_name)
    }
  }
  message("Finished uploading data for: ", coat_name)
}


## ------------------------------------------------------------------ ##
## 9. CHECK UPLOAD SUMMARY AND FILES TO BE UPLOADED
## ------------------------------------------------------------------ ##

upload_check <- upload_overview %>%
  group_by(base_name_coat) %>%
  summarise(
    upload_year = as.integer(sub(".*_([0-9]{4})\\.txt$", "\\1", file_name[file_type == "data"])),
    n_data_files = sum(file_type == "data"), .groups = "drop") %>%
  arrange(base_name_coat)

## retrieve current and new versions from the data portal
upload_check$state_version_old <- NA_integer_
upload_check$state_version_new <- NA_integer_
upload_check$state_name_new <- NA_character_

for (i in seq_len(nrow(upload_check))) {
  state_name <- upload_check$base_name_coat[i]
  search_result <- package_search(
    q = state_name,
    fq = "{!collapse field=base_name max=version_i}",
    include_private = TRUE,
    include_drafts = TRUE)
  
  if (length(search_result$results) == 0) {stop("COAT state variable not found: ", state_name)}
  pkg_state <- search_result$results[[1]]
  state_version_old <- as.integer(pkg_state$version)
  state_version_new <- state_version_old + 1
  upload_check$state_version_old[i] <- state_version_old
  upload_check$state_version_new[i] <- state_version_new
  upload_check$state_name_new[i] <- paste0(state_name, "_v", state_version_new)
}

## view upload check and files in temporary directory
View(upload_check)
browseURL(tempdir())


## ------------------------------------------------------------------ ##
## 10. CREATE NEW STATE VARIABLE VERSIONS AND UPLOAD FILES
## ------------------------------------------------------------------ ##

## run function for all state variables in the upload overview
for (coat_name in unique(upload_overview$base_name_coat)) {
  upload_new_version_data_portal(coat_name, upload_overview)
}

