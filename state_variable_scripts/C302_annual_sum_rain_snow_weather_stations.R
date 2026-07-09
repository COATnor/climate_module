#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C302_annual_sum_rain_snow_weather_stations
### Date: 29.06.2026
### Author: Guro Bang Synnes
#### --------------------------------------------------------------------------------------------------------- ####

## ------------------------------------------------------------------ ##
## WORKFLOW
## ------------------------------------------------------------------ ##
# This script calculates the annual sum of precipitation and the fraction falling as rain and snow for weather stations.
# It also generates aux and coordinate files containing information about when stations have been in use and their coordinates, respectively.
# It downloads the elements sum(precipitation_amount P1Y) with timeOffset PT6H, and mean(air_temperature P1D) from Frost with timeOffset PT0H (preferred) or PT6H.
# The frs function (sourced from GitHub) is used to calculate the fractions of the precipitation falling as rain and snow based on temperatures,
# but the annual sum of precipitation is taken directly from sum(precipitation_amount P1Y).


## ------------------------------------------------------------------ ##
## SETUP
## ------------------------------------------------------------------ ##

rm(list = ls())

## load libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
library(sf)
library(ckanr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(janitor)

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

elements_prec <- "sum(precipitation_amount P1Y)" # daily precipitation amount
elements_temp <- "mean(air_temperature P1D)" # daily mean temperature

#start_date <- "2004-01-01"  # specify start date when processing a single year
end_date <- "2025-12-31"
#referenceTime <- paste(start_date, end_date, sep = "/")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()

## get functions for calculating the fraction of precipitation falling as rain from GitHub ("rr.year.rain" function used later in this script)
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

## format data, remember to check that special characters are converted to aa/ae/o and that "nibio_nilu_met.no" is converted correctly
station_info <- station_info_raw %>% 
  select(id, name, country, masl, validFrom, county, masl, geometry.coordinates, stationHolders) %>% 
  rename(v_station_id = id) %>% 
  mutate(v_station_name = tolower(name)) %>% 
  mutate(v_station_name = str_replace_all(v_station_name, c("\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"))) %>% #try ["å" = "aa", "æ" = "ae", "ø" = "o"] first, then with unicode ["\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"] if it doesn't work with special letters
  mutate(date_first_precipitation = substr(validFrom, 1, 10)) %>% 
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
                          "alfred-wegener-institut fur polar- und meeresforschung, met.no" = "alfred_wegener_institut_met.no")) %>% 
  arrange(sn_region, v_station_name)

## make the aux file
aux <- station_info %>% 
  mutate(date_last_precipitation = NA, date_last_temperature = NA, v_comment = NA) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_precipitation, date_last_precipitation, date_last_temperature, v_comment)

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

### check precipitation data for the aux file

## format data
element_info_prec <- filter(element_info_raw, elementId == elements_prec & timeOffset == "PT6H") %>% 
  select(sourceId, validFrom, validTo, timeOffset) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if precipitation data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info_prec$v_station_id] # add comment later if there is no data available
missing_station_prec <- aux$v_station_name[!aux$v_station_id %in% element_info_prec$v_station_id]

## check if there are any gaps
gaps_prec <- element_info_prec %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id, timeOffset) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps_prec  # add comment if there are stations with gaps

## add comments
aux$v_comment[aux$v_station_name %in% missing_station_prec]
aux$v_comment[aux$v_station_name %in% missing_station_prec] <- "no precipitation data available from this station"

## use the date when data is available as date_first_precipitation
date_first_prec <- element_info_prec %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first_precipitation = min(validFrom))

aux <- aux %>% select(-date_first_precipitation) %>%
  left_join(date_first_prec) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_last_temperature, date_first_precipitation, date_last_precipitation, v_comment)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### check temperature data for the aux file

## format data
element_info_temp <- filter(element_info_raw, elementId == elements_temp & timeOffset %in% c("PT0H", "PT6H")) %>% 
  select(sourceId, validFrom, validTo) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if temperature data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info_temp$v_station_id] # add comment later if there is no data available
missing_station_temp <- aux$v_station_name[!aux$v_station_id %in% element_info_temp$v_station_id]

## check if there are any gaps
gaps <- element_info_temp %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps  # add comment if there are stations with gaps

## add comments
no_temp_data_rows <- aux$v_station_name %in% missing_station_temp
no_temp_data_comments <- aux$v_comment[no_temp_data_rows]
comment_no_temp_data <- "no temperature data available from this station"
aux$v_comment[no_temp_data_rows] <- ifelse(is.na(no_temp_data_comments), comment_no_temp_data, paste(no_temp_data_comments, comment_no_temp_data, sep = ", "))

## use the date when data is available as date_first
date_first_temp <- element_info_temp %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first_temperature = min(validFrom))

aux <- aux %>% 
  left_join(date_first_temp) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first_precipitation, date_last_precipitation, date_first_temperature ,date_last_temperature, v_comment)


## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, "C302_annual_sum_rain_snow_weather_stations_aux.txt", sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, "C302_annual_sum_rain_snow_weather_stations_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

data_list_prec <- c()
data_list_temp <- c()


for (i in 1:nrow(aux)) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### precipitation
  
  if (is.na(aux$date_first_precipitation[i])) next
  if (aux$date_first_precipitation[i] > end_date) next
  
  ## build the url to Frost
  url_data_prec <- paste0(
    "https://",
    client_id, 
    "@frost.met.no/observations/v0.jsonld",
    "?",
    "sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first_precipitation[i], end_date, sep = "/"),
    "&elements=", elements_prec
  )
  
  ## Issue an HTTP GET request and extract JSON data
  xs_prec <- try(fromJSON(URLencode(url_data_prec),flatten=T))
  
  ## Check if the request worked, print out any errors
  if (class(xs_prec) != 'try-error') {
    raw_dat_prec <- unnest(xs_prec$data, cols = c(observations))
    print(paste("Precipitation data retrieved from frost.met.no for ", aux$v_station_name[i],"!", sep = ""))
  } else {
    print(paste("Error: the precipitation data retrieval was not successful for ", aux$v_station_name[i],"!", sep = ""))
    next
  }
  
  data_list_prec[[i]] <- raw_dat_prec %>% filter(timeOffset == "PT6H") %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_date, value, timeOffset) %>% 
    rename(v_precipitation_year_mm = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_date, v_precipitation_year_mm)

  ## add missing years with NA
  first_year_prec <- min(data_list_prec[[i]]$t_year)
  last_year_prec <- max(data_list_prec[[i]]$t_year)
  all_years_prec <- seq(first_year_prec, last_year_prec)
  missing_years_prec <- all_years_prec[!all_years_prec %in% data_list_prec[[i]]$t_year]
  
  if (length(missing_years_prec) > 0) {
  data_list_prec[[i]] <- data_list_prec[[i]] %>% 
    add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i], t_year = missing_years_prec, v_precipitation_year_mm = NA) %>% 
    arrange(t_year)
  }
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ### temperature
  
  if (is.na(aux$date_first_temperature[i])) next
  if (aux$date_first_temperature[i] > end_date) next
  
  ## build the url to Frost
  url_data_temp <- paste0(
    "https://",
    client_id, 
    "@frost.met.no/observations/v0.jsonld",
    "?",
    "sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first_temperature[i], end_date, sep = "/"),
    "&elements=", elements_temp
  )
  
  ## Issue an HTTP GET request and extract JSON data
  xs_temp <- try(fromJSON(URLencode(url_data_temp),flatten=T))
  
  ## Check if the request worked, print out any errors
  if (class(xs_temp) != 'try-error') {
    raw_dat_temp <- unnest(xs_temp$data, cols = c(observations))
    print(paste("Temperature data retrieved from frost.met.no for ", aux$v_station_name[i], "!", sep = ""))
  } else {
    print("Error: the temperature data retrieval was not successful for ", aux$v_station_name[i], "!")
    next
  }
  
  ## format data
  data_list_temp[[i]] <- raw_dat_temp %>% filter(timeOffset %in% c("PT0H", "PT6H")) %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_date, value, timeOffset) %>% 
    rename(v_temperature_day = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_date, v_temperature_day, timeOffset) %>%
    group_by(v_station_id, t_year, t_date) %>%
    arrange(case_when(timeOffset == "PT0H" ~ 1, timeOffset == "PT6H" ~ 2), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  ## add missing dates with NA
  first_date_temp <- min(data_list_temp[[i]]$t_date)
  last_date_temp <- max(data_list_temp[[i]]$t_date)
  all_dates_temp <- seq(as.Date(first_date_temp), as.Date(last_date_temp), by = "day")
  missing_dates_temp <- all_dates_temp[!all_dates_temp %in% data_list_temp[[i]]$t_date]
  
  if (length(missing_dates_temp) > 0) {
  data_list_temp[[i]] <- data_list_temp[[i]] %>% 
    add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i], t_year = year(missing_dates_temp), t_date = missing_dates_temp, v_temperature_day = NA) %>% 
    arrange(t_date)
  }
}

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## combine to one dataset for precipitation and temperature
data_all_prec <- bind_rows(data_list_prec)
data_all_temp <- bind_rows(data_list_temp)


## find annual fractions of precipitation falling as rain and snow
data_fractions_rain_snow <- frs(data_all_temp)


## merge the precipitation and fraction datasets
state_var_all <- data_all_prec %>%
  left_join(
    data_fractions_rain_snow,
    by = c("sn_region", "v_station_name", "v_station_id", "t_year")) %>%
    group_by(sn_region, v_station_name, v_station_id) %>%
  complete(
    t_year = seq(min(t_year), max(t_year)),
    fill = list(
      v_precipitation_year_mm = NA_real_,
      v_fraction_rain = NA_real_,
      v_fraction_snow = NA_real_)) %>%
  ungroup() %>%
  mutate(
    v_fraction_rain = if_else(is.na(v_precipitation_year_mm), NA_real_, v_fraction_rain),
    v_fraction_snow = if_else(is.na(v_precipitation_year_mm), NA_real_, v_fraction_snow)) %>%
  select(-t_date) %>%
  arrange(sn_region, v_station_name, t_year)

check_duplicates <-  state_var_all %>%
  get_dupes(v_station_id, t_year)


## save data
file_name <- paste0("C302_annual_sum_rain_snow_weather_stations_", min(state_var_all$t_year), "_", max(state_var_all$t_year), ".txt")
file_name

write.table(state_var_all, file = "", row.names = FALSE, sep = ";")
write.table(aux, file = "", row.names = FALSE, sep = ";")
write.table(coord, file = "", row.names = FALSE, sep = ";")



## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## serach for your state variable
state_name <- "c302_annual_sum_rain_snow_weather_stations_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

## upload the aux file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C302_annual_sum_rain_snow_weather_stations_aux.txt", sep = "/"),
  name = "C302_annual_sum_rain_snow_weather_stations_aux.txt",
  http_method = "POST"
)

## upload the coordinate file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C302_annual_sum_rain_snow_weather_stations_coordinates.txt", sep = "/"),
  name = "C302_annual_sum_rain_snow_weather_stations_coordinates.txt",
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
