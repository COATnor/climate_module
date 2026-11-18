#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C102_monthly_mean_air_temperature_weather_stations
### Date: 09.04.2026
### Author: Ingrid Paulsen
#### ------------------------------------------------------------------------------------------------------------ ####

## ------------------------------------------------------------------ ##
## SETUP
## ------------------------------------------------------------------ ##

## load libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
library(sf)
library(ckanr)

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
client_id <- Sys.getenv("FROST_KEY")  # create a user account here: https://frost.met.no/auth/requestCredentials.html; then write here your client id
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")  # move to downloading
elements <- "mean(air_temperature P1M)" # monthly mean temperature
#start_date <- "2004-01-01"  # specify start date when processing a single year
end_date <- "2025-12-31"
#referenceTime <- paste(start_date, end_date, sep = "/")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
#temp_dir <- tempdir()

## set up the connection to the COAT dataportal 
ckanr_setup(url =  "https://data.coat.no", 
            key = Sys.getenv("COAT_KEY"))  # write here your API key (e.g. "asdf123af123")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD INFO ABOUT THE WEATHER STATIONS AND MAKE A COORDINATE AND AUX FILE
## ---------------------------------------------------------------------------- ##

## build the url to frost
url_sources <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?ids=", paste(sources, collapse = ","), "&types=SensorSystem")

## download information about the weather stations
station_info_raw <- try(fromJSON(URLencode(url_sources),flatten=T))$data

## format data
station_info <- station_info_raw %>% 
  select(id, name, country, masl, validFrom, county, masl, geometry.coordinates, stationHolders) %>% 
  rename(v_station_id = id) %>% 
  mutate(v_station_name = tolower(name)) %>% 
  mutate(v_station_name = str_replace_all(v_station_name, c("å" = "aa", "æ" = "ae", "ø" = "o"))) %>% 
  mutate(date_first = substr(validFrom, 1, 10)) %>% 
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
                          "met.no, avinor" = "met.no_avinor",
                          "nibio, nilu – norsk institutt for luftforskning, met.no" = "nibio_nilu_met.no",
                          "alfred-wegener-institut fur polar- und meeresforschung, met.no" = "alfred_wegener_institut_met.no")) %>% 
  arrange(sn_region, v_station_name)

## make the aux file
aux <- station_info %>% 
  mutate(date_last = NA, v_comment = NA) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first, date_last, v_comment)

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

## needs to be finished!!

## build url to frost
url_available <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?sources=",  paste(sources, collapse = ","))

## download information about which data is available
element_info_raw <- try(fromJSON(URLencode(url_available),flatten=T))$data

## format data
element_info <- filter(element_info_raw, elementId == elements & timeOffset == "PT0H") %>% 
  select(sourceId, validFrom, validTo) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

#In the Frost API, timeOffset: PT0H denotes that daily data (P1D) covers the exact 24-hour period from midnight to midnight (to UTC) on the reference date. 

## check if temperature data is available for all stations
aux$v_station_name[!aux$v_station_id %in% element_info$v_station_id] # add comment if there is no data available
missing_station <- aux$v_station_name[!aux$v_station_id %in% element_info$v_station_id]

## check if there are any gaps
gaps <- element_info %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps  # add comment if there are stations with gaps

## add comments
aux$v_comment[aux$v_station_name %in% missing_station]
aux$v_comment[aux$v_station_name %in% missing_station] <- "no temperature data available from this station"

## use the date when data is available as date_first
date_first <- element_info %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first = min(validFrom))

aux <- aux %>% select(-date_first) %>% 
  left_join(date_first) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first, date_last, v_comment)


## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, "C102_seasonal_mean_air_temperature_weather_stations_aux.txt", sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, "C102_seasonal_mean_air_temperature_weather_stations_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

## janssonhaugen gives an error -> temp data first available in 2025

state_var_list <- c()

for (i in 1:nrow(aux)) {
  
  if (is.na(aux$date_first[i])) next
  if (aux$date_first[i] > end_date) next
  
  ## build the url to Frost
  url_data <- paste0(
    "https://",
    client_id, 
    "@frost.met.no/observations/v0.jsonld",
    "?",
    "sources=", aux$v_station_id[i],
    "&referencetime=", paste(aux$date_first[i], end_date, sep = "/"),
    "&elements=", elements
  )
  
  ## Issue an HTTP GET request and extract JSON data
  xs <- try(fromJSON(URLencode(url_data),flatten=T))
  
  ## Check if the request worked, print out any errors
  if (class(xs) != 'try-error') {
    raw_dat <- unnest(xs$data, cols = c(observations))
    print("Data retrieved from frost.met.no!")  ## improve error message
  } else {
    print("Error: the data retrieval was not successful!")
    next
  }
  
  ## format data
  state_var_list[[i]] <- raw_dat %>% filter(timeOffset == "PT0H") %>%   
    mutate(date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(date), t_month = month(date), day = day(date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_month, value) %>% 
    rename(v_temperature_month = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_month,v_temperature_month)
  
  ## add missing months with NA
  first_year <- min(state_var_list[[i]]$t_year, na.rm = TRUE)
  last_year <- max(state_var_list[[i]]$t_year, na.rm = TRUE)
  
  # Generate a complete sequence of year-month combinations
  all_year_months <- expand.grid(
    t_year = seq(first_year, last_year),
    t_month = 1:12
  )
  
  # Identify missing year-month combinations
  existing_year_months <- state_var_list[[i]] %>% 
    select(t_year, t_month)
  missing_year_months <- anti_join(all_year_months, existing_year_months, by = c("t_year", "t_month"))
  
  # Add missing rows with NA for v_temperature_month
  state_var_list[[i]] <- state_var_list[[i]] %>% 
    bind_rows(
      missing_year_months %>% 
        mutate(
          sn_region = aux$sn_region[i],
          v_station_name = aux$v_station_name[i],
          v_station_id = aux$v_station_id[i],
          v_temperature_month = NA
        )
    ) %>% 
    arrange(t_year, t_month)
  
}

## combine to one dataset
state_var_all <- bind_rows(state_var_list)

## save data to a temporary directory

file_name <- paste0("C102_seasonal_mean_air_temperature_weather_stations_", min(state_var_all$t_year), "_", max(state_var_all$t_year), ".txt")
file_name

write.table(state_var_all, paste(temp.dir,file_name,sep="/"), row.names = FALSE, sep = ";")

## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## search for your state variable
state_name <- "c102_seasonal_mean_air_temperature_weather_stations_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

## upload the aux file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C101_annual_mean_air_temperature_weather_stations_aux.txt", sep = "/"),
  name = "C101_annual_mean_air_temperature_weather_stations_aux.txt",
  http_method = "POST"
)

## upload the coordinate file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C101_annual_mean_air_temperature_weather_stations_coordinates.txt", sep = "/"),
  name = "C101_annual_mean_air_temperature_weather_stations_coordinates.txt",
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






