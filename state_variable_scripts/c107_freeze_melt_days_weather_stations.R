#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C107_freeze_melt_days_weather_stations
### Date: 15.04.2026
### Author: Guro Bang Synnes
#### --------------------------------------------------------------------------------------------------------- ####

## ------------------------------------------------------------------ ##
## WORKFLOW
## ------------------------------------------------------------------ ##
# This script calculates the number of freeze-melt days during winter seasons for weather stations, and also number of freeze-melt days when there is snow.
# It generates aux and coordinate files containing information about when stations have been in use and their coordinates, respectively.
# Freeze-melt days are defined as days where the minimum temperature is below 0 degrees Celsius and maximum temperature is above 0 degrees Celsius.
# Freeze-melt days when there is snow is calculated in the same way, but only for days where snow thickness is above 0 cm.
# The winter season includes all days between November 1st and April 30th. Here, t_year_winter refers to the winter season ending in April that year.
# The script downloads three elements from Frost, namely min(air_temperature P1D) and max(air_temperature P1D) with timeOffset PT0H (preferred) or PT18H, and surface_snow_thickness with timeResolution P1D.
# The fmd function (sourced from GitHub) is used to calculate the number of freeze-melt days and also freeze-melt days when snow in a winter season for a weather station if the data coverage from the elements that are required is at least 80% for winter season days.


## ------------------------------------------------------------------ ##
## SETUP
## ------------------------------------------------------------------ ##

rm(list = ls())

## load libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(jsonlite)
library(sf)
library(ckanr)
library(purrr)
library(stringi)

Sys.setlocale(category = "LC_ALL", locale = "no_NB.utf8")


## define a list with IDs of all weather stations that should be included in the dataset
sources <- c("SN98974",  # Bergebydalen
             "SN98976",  # Torvhaugdalen
             "SN98978",  # Reinhaugen
             "SN98640",  # Finnesvatnet  
             "SN98642",  # Komagdalen
             "SN98645",  # Hubehytta
             "SN98648",  # Ragarokk
             "SN96890",  # Korgåsen
             "SN99600",  # Farkollen
             "SN98790",  # Vadsø Lufthavn
             "SN98550",  # Vardø Radio
             "SN98400",  # Makkaur Fyr
             "SN98360",  # Båtsfjord - Straumnesaksla
             "SN98090",  # Berlevåg Lufthavn
             #"SN98265",  # FV890 Gednje
             #"SN98630",  # E75 Komagvær
             "SN96850",  # Tana bru
             "SN96931",  # Polmak Tollsted
             #"SN99090",  # E6 Gandvikbakken
             "SN99770",  # Istjørndalen
             "SN99763",  # Reindalspasset
             "SN99875",  # Janssonhaugen
             "SN99874",  # Janssonhaufen Vest
             "SN99882",  # Nedre Sassendalen
             "SN99884",  # Klauva
             "SN99810",  # Revnosa-Agardh
             "SN99811",  # Inglefieldbukta
             "SN99840",  # Svalbard Lufthavn
             "SN99790",  # Isfjord Radio
             "SN99895",  # Kvadehuken II
             "SN99890",  # Kaffiøyra
             "SN99910",  # Ny-ålesund
             "SN99370",  # Kirkenes Lufthavn
             "SN99460",  # Pasvik-Svanvik
             "SN99540",  # Nyrud
             "SN90450",  # Tromsø MET
             "SN90400",  # Tromsø Holt
             "SN90490"   # Tromsø Langnes
)

## define other parameters to download weather station data from frost.met.no
client_id <- Sys.getenv("frost_client_id")  # create a user account here: https://frost.met.no/auth/requestCredentials.html; then write here your client id
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")  # move to downloading

## annual series of daily minimum (tn) and  daily maximum (tx) temperature series
elements_min <- "min(air_temperature P1D)" # daily temperature minimum
elements_max <- "max(air_temperature P1D)" # daily temperature maximum
elements_snow <- "surface_snow_thickness" # daily snow depth

#start_date <- "2004-01-01"  # specify start date when processing a single year
end_date <- "2026-04-30"
#referenceTime <- paste(start_date, end_date, sep = "/")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()

## get functions for calculating freeze-melt days from GitHub (fmd and fmds functions used later in this script)
source("https://github.com/COATnor/climate_module/blob/main/functions/functions_climate_state_variables.R?raw=TRUE")

## set up the connection to the COAT dataportal 
ckanr_setup(url =  "https://data.coat.no", 
            key = Sys.getenv("api_COAT"))  # write here your API key (e.g. "asdf123af123")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD INFO ABOUT THE WEATHER STATIONS AND MAKE A COORDINATE AND AUX FILE
## ---------------------------------------------------------------------------- ##

## build the url to frost
url_sources <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?ids=", paste(sources, collapse = ","), "&types=SensorSystem")

## download information about the weather stations
station_info_raw <- try(fromJSON(URLencode(url_sources),flatten=T))$data

## format data, remember to check that special characters are converted to aa/ae/o
station_info <- station_info_raw %>% 
  select(id, name, country, masl, validFrom, county, masl, geometry.coordinates, stationHolders) %>% 
  rename(v_station_id = id) %>%
  mutate(v_station_name = tolower(name)) %>%
  mutate(v_station_name = str_replace_all(v_station_name, c("\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"))) %>% #try ["å" = "aa", "æ" = "ae", "ø" = "o"] first, then with unicode ["\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"] if it doesn't work with special letters
  mutate(v_station_name = str_replace_all(v_station_name, "kommagdalen", "komagdalen")) %>%
  mutate(date_first_temperature = substr(validFrom, 1, 10)) %>% 
  mutate(sn_region = ifelse(county == "SVALBARD", "svalbard",
                            ifelse(county == "FINNMARK", "varanger", 
                                   ifelse(county == "TROMS", "troms", NA)))) %>% 
  mutate(v_meter_above_sea_level = masl) %>% 
  mutate(e_dd = map_dbl(geometry.coordinates, ~ .x[1]),
         n_dd = map_dbl(geometry.coordinates, ~ .x[2])) %>% 
  mutate(v_owner = tolower(sapply(stationHolders, paste, collapse = ", "))) %>%
  mutate(v_owner = recode(v_owner,
                          "statens vegvesen" = "statens_vegvesen",
                          "norsk polarinstitutt" = "norsk_polarinstitutt",
                          "avinor, met.no" = "met.no_avinor",
                          "nilu – norsk institutt for luftforskning, met.no, nibio" = "nibio_nilu_met.no",
                          "met.no, alfred-wegener-institut fur polar- und meeresforschung" = "alfred_wegener_institut_met.no")) %>% 
  arrange(sn_region, v_station_name)

## make the aux file
aux <- station_info %>% 
  mutate(date_last_temperature = NA, date_last_snow = NA, v_comment = NA) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_temperature, date_last_temperature, date_last_snow, v_comment)

## make coordinate file
coord <- station_info %>% 
  select(v_station_id, e_dd, n_dd)

## convert coordinates to utm33
dd <- cbind.data.frame(lon=coord$e_dd, lat=coord$n_dd) # only columns with coordinates
dd <- st_as_sf(dd, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84") # convert to spatial object
utm33 <- st_transform(dd, crs = 32633)  
utm33 <- st_coordinates(utm33)

## add coordinates in utm33 to coordinate file
coord$e_utm33 = round(utm33[,1])
coord$n_utm33 = round(utm33[,2])


## ---------------------------------------------------------------------------- ##
## CHECK IF THE DATA IS AVAILABLE
## ---------------------------------------------------------------------------- ##

## build url to frost
url_available <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?sources=",  paste(sources, collapse = ","))

## download information about which data is available
element_info_raw <- try(fromJSON(URLencode(url_available),flatten=T))$data

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### check minimum temperature data for the aux file

## format data
element_info_min <- filter(element_info_raw, elementId == elements_min & timeOffset %in% c("PT0H", "PT18H")) %>% 
  select(sourceId, validFrom, validTo, timeOffset) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if temperature data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info_min$v_station_id] # add comment if there is no data available
missing_station_min <- aux$v_station_name[!aux$v_station_id %in% element_info_min$v_station_id]

## check if there are any gaps
gaps_min <- element_info_min %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id, timeOffset) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps_min  # add comment if there are stations with gaps

## add comments
aux$v_comment[aux$v_station_name %in% missing_station_min]
aux$v_comment[aux$v_station_name %in% missing_station_min] <- "no temperature data available from this station"

## use the date when data is available as date_first_temperature
date_first_min <- element_info_min %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first_temperature = min(validFrom))

aux <- aux %>% select(-date_first_temperature) %>% 
  left_join(date_first_min) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_temperature, date_last_temperature, date_last_snow, v_comment)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### check maximum temperature data for the aux file

## format data
element_info_max <- filter(element_info_raw, elementId == elements_max & timeOffset %in% c("PT0H", "PT18H")) %>% 
  select(sourceId, validFrom, validTo, timeOffset) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if temperature data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info_max$v_station_id] # add comment if there is no data available
missing_station_max <- aux$v_station_name[!aux$v_station_id %in% element_info_max$v_station_id]

## check if there are any gaps
gaps_max <- element_info_max %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id, timeOffset) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps_max  # add comment if there are stations with gaps

## add comments
aux$v_comment[aux$v_station_name %in% missing_station_max]
#aux$v_comment[aux$v_station_name %in% missing_station_max] <- "no temperature data available from this station"

## use the date when data is available as date_first_temperature
date_first_max <- element_info_max %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first_temperature_max = min(validFrom))

aux <- aux %>% 
  left_join(date_first_max) %>%
  mutate(
    date_first_temperature = if_else(as.Date(date_first_temperature) >= as.Date(date_first_temperature_max), date_first_temperature, date_first_temperature_max)) %>%
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_temperature, date_last_temperature, date_last_snow, v_comment)

## check cases where min and max don't have the same start dates
date_first_comparison <- date_first_min %>%
  left_join(date_first_max) %>%
  select(v_station_id, date_first_temperature, date_first_temperature_max) %>%
  filter(date_first_temperature != date_first_temperature_max)

## add comment if first date of temperature data are differing
aux$v_comment[aux$v_station_id %in% date_first_comparison$v_station_id] <- "first date of data is differing for daily minimum and maximum temperature datasets"


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### check snow depth data for the aux file

## format data
element_info_snow <- filter(element_info_raw, elementId == elements_snow & timeResolution == "P1D") %>% 
  select(sourceId, validFrom, validTo) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if snow depth data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info_snow$v_station_id] # add comment if there is no data available
missing_station_snow <- aux$v_station_name[!aux$v_station_id %in% element_info_snow$v_station_id]

## check if there are any gaps
gaps_snow <- element_info_snow %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps_snow  # add comment if there are stations with gaps

## add comments
no_snow_data_rows <- aux$v_station_name %in% missing_station_snow
no_snow_data_comments <- aux$v_comment[no_snow_data_rows]
comment_no_snow_data <- "no snow depth data available from this station"
aux$v_comment[no_snow_data_rows] <- ifelse(is.na(no_snow_data_comments), comment_no_snow_data, paste(no_snow_data_comments, comment_no_snow_data, sep = ", "))

## use the date when data is available as date_first_snow
date_first_snow <- element_info_snow %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first_snow = min(validFrom))

aux <- aux %>% 
  left_join(date_first_snow) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_temperature, date_last_temperature, date_first_snow, date_last_snow, v_comment)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, "C107_freeze_melt_days_weather_stations_aux.txt", sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, "C107_freeze_melt_days_weather_stations_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

data_list_min <- c()
data_list_max <- c()
data_list_snow <- c()


for (i in 1:nrow(aux)) {
  
  if (is.na(aux$date_first_temperature[i])) next
  if (aux$date_first_temperature[i] > end_date) next
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## minimum temperature
  
  ## build the url to Frost
  url_data_min <- paste0(
    "https://",
    client_id, 
    "@frost.met.no/observations/v0.jsonld",
    "?",
    "sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first_temperature[i], end_date, sep = "/"),
    "&elements=", elements_min
  )
  
  ## Issue an HTTP GET request and extract JSON data
  xs_min <- try(fromJSON(URLencode(url_data_min),flatten=T))
  
  ## Check if the request worked, print out any errors
  if (class(xs_min) != 'try-error') {
    raw_dat_min <- unnest(xs_min$data, cols = c(observations))
    print(paste("Minimum temperature data retrieved from frost.met.no for ", aux$v_station_name[i], "!", sep = ""))
  } else {
    print("Error: the minimum temperature data retrieval was not successful for ", aux$v_station_name[i], "!")
    next
  }
  
  ## format data
  data_list_min[[i]] <- raw_dat_min %>% filter(timeOffset %in% c("PT0H", "PT18H")) %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date), month = month(t_date), day = day(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_date, value, timeOffset) %>% 
    rename(v_temperature_day_min = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_date, v_temperature_day_min, timeOffset) %>%
    group_by(v_station_id, t_date) %>%
    arrange(case_when(timeOffset == "PT0H" ~ 1, timeOffset == "PT18H" ~ 2), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  ## add missing dates with NA
  first_date_min <- min(data_list_min[[i]]$t_date)
  last_date_min <- max(data_list_min[[i]]$t_date)
  all_dates_min <- seq(as.Date(first_date_min), as.Date(last_date_min), by = "day")
  missing_dates_min <- all_dates_min[!all_dates_min %in% data_list_min[[i]]$t_date]
  
  data_list_min[[i]] <- data_list_min[[i]] %>% 
    add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i], t_year = year(missing_dates_min), t_date = missing_dates_min, v_temperature_day_min = NA) %>% 
    arrange(t_date)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### maximum temperature
  
  ## build the url to Frost
  url_data_max <- paste0(
    "https://",
    client_id, 
    "@frost.met.no/observations/v0.jsonld",
    "?",
    "sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first_temperature[i], end_date, sep = "/"),
    "&elements=", elements_max
  )
  
  ## Issue an HTTP GET request and extract JSON data
  xs_max <- try(fromJSON(URLencode(url_data_max),flatten=T))
  
  ## Check if the request worked, print out any errors
  if (class(xs_max) != 'try-error') {
    raw_dat_max <- unnest(xs_max$data, cols = c(observations))
    print(paste("Maximum temperature data retrieved from frost.met.no for ",aux$v_station_name[i],"!", sep = ""))
  } else {
    print(paste("Error: the maximum temperature data retrieval was not successful for ",aux$v_station_name[i],"!", sep = ""))
    next
  }
  
  data_list_max[[i]] <- raw_dat_max %>% filter(timeOffset %in% c("PT0H", "PT18H")) %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date), month = month(t_date), day = day(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_date, value, timeOffset) %>% 
    rename(v_temperature_day_max = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_date, v_temperature_day_max, timeOffset) %>%
    group_by(v_station_id, t_date) %>%
    arrange(case_when(timeOffset == "PT0H" ~ 1, timeOffset == "PT18H" ~ 2), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  ## add missing dates with NA
  first_date_max <- min(data_list_max[[i]]$t_date)
  last_date_max <- max(data_list_max[[i]]$t_date)
  all_dates_max <- seq(as.Date(first_date_max), as.Date(last_date_max), by = "day")
  missing_dates_max <- all_dates_max[!all_dates_max %in% data_list_max[[i]]$t_date]
  
  data_list_max[[i]] <- data_list_max[[i]] %>% 
    add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i], t_year = year(missing_dates_max), t_date = missing_dates_max, v_temperature_day_max = NA) %>% 
    arrange(t_date)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### snow depth
  
  if (is.na(aux$date_first_snow[i])) next
  if (aux$date_first_snow[i] > end_date) next
  
  
  ## build the url to Frost
  url_data_snow <- paste0(
    "https://",
    client_id,
    "@frost.met.no/observations/v0.jsonld",
    "?sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first_snow[i], end_date, sep = "/"),
    "&elements=", elements_snow
  )
  
  ## Try downloading everything at once
  xs_snow <- try(fromJSON(URLencode(url_data_snow), flatten = TRUE),
                 silent = TRUE)
  
  if (class(xs_snow) != "try-error") {
    
    raw_dat_snow <- unnest(xs_snow$data, cols = c(observations))
    
    print(paste(
      "Snow data retrieved from frost.met.no for ",
      aux$v_station_name[i], "!", sep = ""))
    
  } else {
    
    print(paste("Full request failed for", aux$v_station_name[i],
      "- trying 10 chunks..."))
    
    start_date <- as.Date(aux$date_first_snow[i])
    end_date2  <- as.Date(end_date)
    
    ## Create 11 break points -> 10 chunks
    breaks <- seq(start_date, end_date2, length.out = 11)
    
    chunk_list <- list()
    
    for (j in 1:10) {
      
      chunk_start <- as.Date(breaks[j])
      
      ## avoid overlap between chunks
      if (j < 10) {
        chunk_end <- as.Date(breaks[j + 1] - days(1))
      } else {
        chunk_end <- end_date2}
      
      url_chunk <- paste0(
        "https://",
        client_id,
        "@frost.met.no/observations/v0.jsonld",
        "?sources=", aux$v_station_id[i],
        "&referencetime=", paste(chunk_start,
                                 chunk_end,
                                 sep = "/"),
        "&elements=", elements_snow
      )
      
      xs_chunk <- try(
        fromJSON(URLencode(url_chunk), flatten = TRUE),
        silent = TRUE)
      
      if (class(xs_chunk) != "try-error" &&
          !is.null(xs_chunk$data)) {
        
        chunk_list[[length(chunk_list) + 1]] <-
          unnest(xs_chunk$data, cols = c(observations))
        
        print(paste("Chunk", j, "downloaded successfully."))
        
      } else {
        
        print(paste("Chunk", j, ": failed to download data between",
          as.character(chunk_start), "and", as.character(chunk_end)))}}
    
    ## Merge all successful chunks
    if (length(chunk_list) > 0) {
      
      raw_dat_snow <- bind_rows(chunk_list)
      
      print(paste(
        "Snow data retrieved in chunks for ",
        aux$v_station_name[i], "!", sep = ""))
      
    } else {
      
      print(paste(
        "Error: all chunk downloads failed for ",
        aux$v_station_name[i], "!", sep = ""))
      
      next
    }
  }
  
  ## Save result
  data_list_snow[[i]] <- raw_dat_snow %>% filter(timeResolution == "P1D") %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date), month = month(t_date), day = day(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_date, value) %>% 
    rename(v_snow_depth_day = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_date, v_snow_depth_day)
  
  ## add missing dates with NA
  first_date_snow <- min(data_list_snow[[i]]$t_date)
  last_date_snow <- max(data_list_snow[[i]]$t_date)
  all_dates_snow <- seq(as.Date(first_date_snow), as.Date(last_date_snow), by = "day")
  missing_dates_snow <- all_dates_snow[!all_dates_snow %in% data_list_snow[[i]]$t_date]
  
  data_list_snow[[i]] <- data_list_snow[[i]] %>% 
    add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i], t_year = year(missing_dates_snow), t_date = missing_dates_snow, v_snow_depth_day = NA) %>% 
    arrange(t_date)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## combine to one dataset for min, max and snow
data_all_min <- bind_rows(data_list_min) %>% select(-timeOffset)
data_all_max <- bind_rows(data_list_max) %>% select(-timeOffset)
data_all_snow <- bind_rows(data_list_snow)


## merge the datasets by v_station_id and date
data_all <- data_all_min %>%
  full_join(
    data_all_max,
    by = c("sn_region", "v_station_name", "v_station_id", "t_year", "t_date")) %>%
  full_join(
    data_all_snow,
    by = c("sn_region", "v_station_name", "v_station_id", "t_year", "t_date")) %>%
  arrange(v_station_id, t_date)

## check that there is only one row per year and date
multiple_rows <- data_all[duplicated(data_all[, c("v_station_id", "t_date")]), ]
nrow(multiple_rows)

## check NA values for weather stations in years
na_counts <- data_all %>%
  group_by(v_station_name, t_year) %>%
  summarise(
    na_count_min = sum(is.na(v_temperature_day_min)),
    na_counts_max = sum(is.na(v_temperature_day_max)),
    na_counts_snow = sum(is.na(v_snow_depth_day)))


## calculate state variable
state_var_all <- fmd(data_all) %>%
  left_join(
    fmds(data_all),
    by = c("sn_region", "v_station_name", "v_station_id", "t_year_winter")) %>%
  arrange(sn_region, v_station_name, t_year_winter)


## save data to a temporary directory
file_name <- paste0("C107_freeze_melt_days_weather_stations_", min(state_var_all$t_year_winter), "_", max(state_var_all$t_year_winter), ".txt")
file_name

write.table(state_var_all, "...C107_freeze_melt_days_weather_stations_1920_2025.txt", row.names = FALSE, sep = ";")
write.table(aux, file = "...C107_freeze_melt_days_weather_stations_1920_2025.txt", row.names = FALSE, sep = ";")
write.table(coord, file = "...C107_freeze_melt_days_weather_stations_1920_2025.txt", row.names = FALSE, sep = ";")


#write.table(state_var_all, paste(temp_dir, file_name, sep = "/"), row.names = FALSE, sep = ";")


## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## search for your state variable
state_name <- "c107_freeze_melt_days_weather_stations_v1"
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the file names
filenames_state # are there any files

## upload the aux file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C107_freeze_melt_days_weather_stations_aux.txt", sep = "/"),
  name = "C107_freeze_melt_days_weather_stations_aux.txt",
  http_method = "POST"
)

## upload the coordinate file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C107_freeze_melt_days_weather_stations_coordinates.txt", sep = "/"),
  name = "C107_freeze_melt_days_weather_stations_coordinates.txt",
  http_method = "POST"
)

## upload the data files
for (i in file_name) {
  resource_create(
    package_id = pkg_state$id,
    description = NULL,
    upload = paste(tempdir(), i, sep = "/"),
    name = i,
    http_method = "POST"
  )
}


## ---------------------------------- ##
## UPDATE THE STATE VARIABLE
## ---------------------------------- ##

## here you can update the metadata of the sate variable
## you can for example change the sate from 'draft' to 'active (this is necessary if you created the state variable on data.coat.no and then added the datafiles via R)
## you can also change the visibility from private to public

pkg_state$name # check that the name is correct

## save metadata of the package as a list (as = table -> but the object will be a list)
pkg_updated <- package_show(pkg_state$id, as = "table", http_method = "POST")

## do the necessary modifications of the metadata
names(pkg_updated) # show the names of all metadata fields that can be updated
pkg_updated$private <- TRUE # set private = FALSE to publish a dataset
pkg_updated$state <- "active"

## discard empty metadata fields (they will cause a validation error)
pkg_updated <- discard(pkg_updated, is.null)

## update the package
package_update(pkg_updated, pkg_state$id, http_method = "POST")

