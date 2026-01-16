#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C101_annual_mean_air_temperature_weather_stations
### Date: 08.12.2025
### Author: Hanna Böhner
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

## load functions
source("https://github.com/COATnor/climate_module/blob/main/functions/functions_climate_state_variables.R?raw=TRUE")

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
             "SN98265",  # FV890 Gednje
             "SN98630",  # E75 Komagvær
             "SN96850",  # Tana bru
             "SN96931",  # Polmak Tollsted
             "SN99090",  # E6 Gandvikbakken
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
             "SN99540"   # Nyrud
             )

## define other parameters to download weather station data from frost.met.no
client_id <- Sys.getenv("frost_client_id")  # create a user account here: https://frost.met.no/auth/requestCredentials.html; then write here your client id
endpoint <- paste0("https://", client_id, "@frost.met.no/observations/v0.jsonld")  # move to downloading
elements <- "mean(air_temperature P1D)" # annual mean temperature
#start_date <- "2004-01-01"  # use start date from aux file when processing all year, specify start date when processing a single year
end_date <- "2024-12-31"
#referenceTime <- paste(start_date, end_date, sep = "/")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()

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

## format data
station_info <- station_info_raw %>% 
  select(id, name, country, masl, validFrom, county, geometry.coordinates) %>% 
  rename(v_station_id = id) %>% 
  mutate(v_station_name = tolower(name)) %>% 
  mutate(v_station_name = str_replace_all(v_station_name, c("å" = "aa", "æ" = "ae", "ø" = "o"))) %>% 
  mutate(date_first = substr(validFrom, 1, 10)) %>% 
  mutate(sn_region = ifelse(county == "SVALBARD", "svalbard",
                           ifelse(county == "FINNMARK", "varanger", NA))) %>% 
  mutate(e_dd = map_dbl(geometry.coordinates, ~ .x[1]),
         n_dd = map_dbl(geometry.coordinates, ~ .x[2])) %>% 
  arrange(sn_region, v_station_name)

## make the aux file
aux <- station_info %>% 
  mutate(date_last = NA, v_comment = NA) %>% 
  select(sn_region, v_station_name, v_station_id, date_first, date_last, v_comment)

## add comments to the aux file
aux$v_comment[aux$v_station_id %in% c("SN98265", "SN98630", "SN99090")] <- "road weather station, uncertain quality"

## make coordinate file
coord <- station_info %>% 
  select(v_station_id, e_dd, n_dd)

## convert coordinates to utm33
dd <- cbind.data.frame(lon=coord$e_dd, lat=coord$n_dd) # only columns with coordinates
dd <- st_as_sf(dd, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84") # convert to spatial object
utm33 <- st_transform(dd, crs = 32633)  
utm33 <- st_coordinates(utm33)

## add coordinates in utm33 to coordinate file
coord$e_utm33 = utm33[,1]
coord$n_utm33 = utm33[,2]

## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, "C101_annual_mean_air_temperature_weather_stations_aux.txt", sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, "C101_annual_mean_air_temperature_weather_stations_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## CHECK IF THE DATA IS AVAILABE
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

length(unique(element_info_raw$sourceId))


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

## janssonhaugen gives an error -> temp data first available in 2025
## no temperature data for polmak -> gives an error

state_var_list <- c()

for (i in 1:nrow(aux)) {
  
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
    print("Data retrieved from frost.met.no!")
  } else {
    print("Error: the data retrieval was not successful!")
  }
  
  ## format data
  dat <- raw_dat %>% filter(timeOffset == "PT0H") %>%   
    mutate(date = ymd(as.Date(referenceTime))) %>% 
    mutate(year = year(date), month = month(date), day = day(date)) %>% 
    mutate(station_id = substr(sourceId, 1, 7)) %>% 
    select(station_id, date, month, day, year, value) %>% 
    group_by(year) %>% 
    filter(min(month) == 1) # keep only complete years
  
  ## discard station that don't have a complete year
  if(nrow(dat) == 0) next
  
  ## calculate annual mean (function from Ole Einar)
  years <- unique(dat$year)
  
  values <- c()
  for (j in 1:length(unique(dat$year))) {
    filter_dat <- filter(dat, year == years[j])
    values[j] <- tm.ses(tg = filter_dat$value, dato = filter_dat$date, ses = "ann")
  }
  
  # format data for COAT data portal
  state_var_list[[i]] <- data.frame(sn_region = aux$sn_region[i],
                                    v_station_name = aux$v_station_name[i],
                                    v_station_id = aux$v_station_id[i], 
                                    t_year = years,  
                                    v_temperature_year = values) %>% 
                                    select(sn_region, v_station_name, v_station_id, t_year, v_temperature_year)
  
  ## calculate annual mean (tidyverse)
  # state_var_list[[i]] <- dat %>% group_by(year) %>% 
  #   dplyr::summarise(v_temperature = round(mean(value, na.rm = TRUE), digits = 1)) %>% 
  #   mutate(sn_site = stations[i], v_station_id = sources[i]) %>% 
  #   mutate(sn_region = ifelse(sn_site %in% c("torvhaugdalen", "reinhaugen"), "varanger", "svalbard")) %>% 
  #   rename(t_year = year) %>% 
  #   select(sn_region, sn_site, t_year, v_station_id, v_temperature)
  #   
  
}

## combine to one dataset
state_var_all <- bind_rows(state_var_list)

## split data in one file for each year and save it to a temporary directory
years <- sort(unique(state_var_all$t_year))

for (i in 1:length(years)) {
  state_var_year <- filter(state_var_all, t_year == years[i])
  out_names[i] <- paste0("C101_annual_mean_air_temperature_weather_stations_", years[i], ".txt")
  write.table(state_var_year, paste(temp_dir, out_names[i], sep = "/"), row.names = FALSE, sep = ";")
}


## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## serach for your state variable
state_name <- "c101_annual_mean_air_temperature_weather_stations_v1" # write here the name including the version of the state variable you want to add data to
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
for (i in out_names) {
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







test <- map_dfr(paste(temp_dir, out_names, sep = "/"), read.table, header = TRUE, sep = ";")

means <-test %>%  group_by(sn_region, t_year) %>% 
  summarise(temp = mean(v_temperature_year))

means %>% ggplot(aes(x = t_year, y = temp, color = sn_region)) +
  geom_line() +
  geom_smooth(method = "lm", aes(group = sn_region, fill = sn_region), linetype = "dashed", size = 1, alpha = 0.15, se = TRUE)

