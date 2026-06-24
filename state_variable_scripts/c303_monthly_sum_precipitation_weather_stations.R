#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C303_monthly_sum_precipitation_weather_stations
### Date: 24.04.2026
### Author: Guro Bang Synnes
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
library(dplyr)

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

elements <- "sum(precipitation_amount P1M)" # monthly sum of precipitation
#start_date <- "2004-01-01"  # specify start date when processing a single year
end_date <- "2025-12-31"
#referenceTime <- paste(start_date, end_date, sep = "/")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()

## set up the connection to the COAT dataportal 
ckanr_setup(url =  "https://data.coat.no", 
            key = Sys.getenv("api_COAT"))  # write here your API key (e.g. "asdf123af123")

## load rr.month (monthly sum of precipiation) function from GitHub, not used in this version of the script
#source("https://github.com/COATnor/climate_module/blob/main/functions/functions_climate_state_variables.R?raw=TRUE")


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
  mutate(v_station_name = str_replace_all(v_station_name, c("\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"))) %>% #try ["å" = "aa", "æ" = "ae", "ø" = "o"] first, then with unicode ["\u00e5" = "aa", "\u00e6" = "ae", "\u00f8" = "o"] if it doesn't work with special letters
  mutate(v_station_name = str_replace_all(v_station_name, "kommagdalen", "komagdalen")) %>%
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
                          "avinor, met.no" = "met.no_avinor",
                          "nilu – norsk institutt for luftforskning, met.no, nibio" = "nibio_nilu_met.no",
                          "alfred-wegener-institut fur polar- und meeresforschung, met.no" = "alfred_wegener_institut_met.no")) %>% 
  arrange(sn_region, v_station_name)

## make the aux file, check that v_owner looks correct after recoding
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
## CHECK IF THE DATA IS AVAILABE
## ---------------------------------------------------------------------------- ##

## build url to frost
url_available <- paste0("https://", client_id, "@frost.met.no/observations/availableTimeSeries/v0.jsonld?sources=",  paste(sources, collapse = ","))

## download information about which data is available
element_info_raw <- try(fromJSON(URLencode(url_available),flatten=T))$data

## format data
element_info <- filter(element_info_raw, elementId == elements & timeOffset %in% c("PT6H", "PT18H")) %>% 
  select(sourceId, validFrom, validTo) %>% 
  mutate(sourceId = sub(":0", "", sourceId)) %>%  
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>% 
  rename(v_station_id = sourceId) %>% 
  left_join(aux)

## check if precipitation data is available for all stations
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
aux$v_comment[aux$v_station_name %in% missing_station] <- "no precipitation data available from this station"

## use the date when data is available as date_first
date_first <- element_info %>% 
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>% 
  group_by(v_station_id) %>%
  summarise(date_first = min(validFrom))

aux <- aux %>% select(-date_first) %>% 
  left_join(date_first) %>% 
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first, date_last, v_comment)


## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, "C303_monthly_sum_precipitation_weather_stations_aux.txt", sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, "C303_monthly_sum_precipitation_weather_stations_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

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
    print(paste("Precipitation data retrieved from frost.met.no for ",aux$v_station_name[i],"!", sep = ""))
  } else {
    print(paste("Error: the precipitation data retrieval was not successful for ",aux$v_station_name[i],"!", sep = ""))
    next
  }
  
  ## format data
  state_var_list[[i]] <- raw_dat %>% filter(timeOffset %in% c("PT6H", "PT18H")) %>%   
    mutate(t_date = ymd(as.Date(referenceTime))) %>% 
    mutate(t_year = year(t_date), t_month = month(t_date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, t_year, t_month, t_date, value, timeOffset) %>% 
    rename(v_precipitation_month_mm = value) %>% 
    mutate(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i], v_station_id = aux$v_station_id[i]) %>% 
    select(sn_region, v_station_name, v_station_id, t_year, t_month, t_date, v_precipitation_month_mm, timeOffset) %>%
    group_by(v_station_id, t_year, t_month) %>%
    arrange(case_when(timeOffset == "PT6H" ~ 1, timeOffset == "PT18H" ~ 2), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  # add missing months with NA
  first_date <- min(state_var_list[[i]]$t_date)
  last_date  <- max(state_var_list[[i]]$t_date)
  
  all_months <- seq(floor_date(first_date, "month"), floor_date(last_date, "month"), by = "month")
  missing_months <- all_months[!all_months %in% state_var_list[[i]]$t_date]
  
  if (length(missing_months) > 0) {
    state_var_list[[i]] <- state_var_list[[i]] %>%
      add_row(sn_region = aux$sn_region[i], v_station_name = aux$v_station_name[i],
              v_station_id = aux$v_station_id[i], t_year = year(missing_months),
        t_month = month(missing_months), t_date = missing_months, v_precipitation_month_mm = NA) %>%
      arrange(t_date)
  }
}

## combine to one dataset
state_var_all <- bind_rows(state_var_list) %>%
  select(sn_region, v_station_name, v_station_id, t_year, t_month, v_precipitation_month_mm) %>%
  arrange(sn_region, v_station_name, t_year, t_month)

## save data to a temporary directory

file_name <- paste0("C303_monthly_sum_precipitation_weather_stations_", min(state_var_all$t_year), "_", max(state_var_all$t_year), ".txt")
file_name

write.table(state_var_all, paste(temp_dir, file_name, sep = "/"), row.names = FALSE, sep = ";")


## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## serach for your state variable
state_name <- "c303_monthly_sum_precipitation_weather_stations_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

## upload the aux file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C303_monthly_sum_precipitation_weather_stations_aux.txt", sep = "/"),
  name = "C303_monthly_sum_precipitation_weather_stations_aux.txt",
  http_method = "POST"
)

## upload the coordinate file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C303_monthly_sum_precipitation_weather_stations_coordinates.txt", sep = "/"),
  name = "C303_monthly_sum_precipitation_weather_stations_coordinates.txt",
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







