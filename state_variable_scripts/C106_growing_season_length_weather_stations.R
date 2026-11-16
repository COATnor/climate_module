#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C106_growing_season_length_weather_stations
### Date: 16.11.2026
### Author: Julia Mikhailova
#### --------------------------------------------------------------------------------------------------------- ####

## ------------------------------------------------------------------ ##
## WORKFLOW
## ------------------------------------------------------------------ ##
# This script calculates the length of the meteorological growing season per weather station and year,
# for two temperature thresholds (0 and 5 degrees Celsius), stored as two columns in the same data file.
# It also generates aux and coordinate files containing information about when stations have been in use and their coordinates, respectively.
# It downloads the element mean(air_temperature P1D) from Frost, with timeOffset PT0H (preferred) or PT6H.
#
# Definition of the growing season (per station, year and threshold):
# - only days in May-October are considered (season_months)
# - season start = first day of the first period of at least 5 consecutive days with daily mean temperature > threshold
# - season end   = last day of the last period of at least 5 consecutive days with daily mean temperature > threshold
# - season length = number of days from start to end (colder days in between are included)
# - the length is 0 if there is no such period, and NA if
#     a) less than 80% of the days in May-October have data, or
#     b) missing days could change the start or end of the season (the season is calculated twice, treating
#        missing days as cold and as warm; if the two results differ, the season length is set to NA)


## ------------------------------------------------------------------ ##
## SETUP
## ------------------------------------------------------------------ ##

## fix problems with ø æ å if present in dataset
set_output_locale <- function() {
  candidate_locales <- c("nb_NO.UTF-8", "nb_NO.utf8", "nn_NO.UTF-8", "nn_NO.utf8", "no_NO.UTF-8", "no_NO.utf8", "C.UTF-8")
  for (locale_name in candidate_locales) {
    result <- try(Sys.setlocale("LC_CTYPE", locale_name), silent = TRUE)
    if (!inherits(result, "try-error") && !is.na(result) && nzchar(result)) {
      return(invisible(result))
    }
  }
  warning("Could not set a UTF-8 Norwegian locale; continuing with the current locale.")
  invisible(NULL)
}

set_output_locale()

## load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {install.packages("tidyverse")};library("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) {install.packages("lubridate")};library("lubridate")
if (!requireNamespace("jsonlite", quietly = TRUE)) {install.packages("jsonlite")};library("jsonlite")
if (!requireNamespace("ckanr", quietly = TRUE)) {install.packages("ckanr")};library("ckanr")

## convert coordinates from WGS84 (decimal degrees) to UTM zone 33N without external spatial dependencies
utm33_from_wgs84 <- function(lon, lat) {
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)

  a <- 6378137.0
  f <- 1 / 298.257223563
  e2 <- f * (2 - f)
  ep2 <- e2 / (1 - e2)
  k0 <- 0.9996
  lon0 <- 15 * pi / 180

  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  delta_lon <- lon_rad - lon0

  sin_lat <- sin(lat_rad)
  cos_lat <- cos(lat_rad)
  tan_lat <- tan(lat_rad)

  n <- a / sqrt(1 - e2 * sin_lat^2)
  t <- tan_lat^2
  c <- ep2 * cos_lat^2
  a_term <- cos_lat * delta_lon

  m <- a * ((1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256) * lat_rad -
    (3 * e2 / 8 + 3 * e2^2 / 32 + 45 * e2^3 / 1024) * sin(2 * lat_rad) +
    (15 * e2^2 / 256 + 45 * e2^3 / 1024) * sin(4 * lat_rad) -
    (35 * e2^3 / 3072) * sin(6 * lat_rad))

  easting <- k0 * n * (a_term + (1 - t + c) * a_term^3 / 6 + (5 - 18 * t + t^2 + 72 * c - 58 * ep2) * a_term^5 / 120) + 500000
  northing <- k0 * (m + n * tan_lat * (a_term^2 / 2 + (5 - t + 9 * c + 4 * c^2) * a_term^4 / 24 + (61 - 58 * t + t^2 + 600 * c - 330 * ep2) * a_term^6 / 720))

  cbind(easting = easting, northing = northing)
}

## find the first and last day of warm periods of at least min_run consecutive days (one station and year)
find_season <- function(date, warm, min_run) {
  runs <- rle(warm)
  run_end <- cumsum(runs$lengths)
  run_start <- run_end - runs$lengths + 1
  qualifying <- which(runs$values & runs$lengths >= min_run)
  if (length(qualifying) == 0) return(as.Date(c(NA, NA)))
  date[c(run_start[min(qualifying)], run_end[max(qualifying)])]
}

## calculate the length of the growing season per station and year for one threshold
## (input: one row per station and day, with complete years and NA for missing days)
calc_gs_length <- function(data, thres_val, min_run = 5, months = 5:10, min_coverage = 0.8) {
  data %>%
    filter(t_month %in% months) %>%
    arrange(v_station_id, date) %>%
    group_by(sn_region, v_station_name, v_station_id, t_year) %>%
    group_modify(~ {
      temp <- .x$v_temperature_day
      # season if every missing day was cold vs. if every missing day was warm
      season_na_cold <- find_season(.x$date, !is.na(temp) & temp > thres_val, min_run)
      season_na_warm <- find_season(.x$date,  is.na(temp) | temp > thres_val, min_run)
      determined <- identical(as.numeric(season_na_cold), as.numeric(season_na_warm))
      enough_data <- mean(!is.na(temp)) >= min_coverage

      if (!enough_data || !determined) {
        season <- as.Date(c(NA, NA)); len <- NA_real_
      } else if (is.na(season_na_cold[1])) {
        season <- as.Date(c(NA, NA)); len <- 0            # enough data, but no growing season
      } else {
        season <- season_na_cold; len <- as.numeric(season[2] - season[1]) + 1
      }
      tibble(v_gs_start = season[1], v_gs_end = season[2], v_gs_length = len)
    }) %>%
    ungroup()
}

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

## define the growing season settings
thresholds <- c(above0 = 0, above5 = 5)   # temperature thresholds in degrees C; the names are used in the column names
season_months <- 5:10                     # months in which the growing season can start and end (May-October)
min_run <- 5                              # minimum number of consecutive days above the threshold
min_coverage <- 0.8                       # minimum proportion of days in season_months with data

## define other parameters to download weather station data from frost.met.no
client_id <- Sys.getenv("frost_client_id")  # create a user account here: https://frost.met.no/auth/requestCredentials.html; then save your client id as frost_client_id in your .Renviron file
elements <- "mean(air_temperature P1D)" # daily mean temperature
end_date <- "2025-12-31"  # last complete year

## define the file names
sv_name <- "C106_growing_season_length_weather_stations"
file_aux <- paste0(sv_name, "_aux.txt")
file_coord <- paste0(sv_name, "_coordinates.txt")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()

## set up the connection to the COAT dataportal
ckanr_setup(url =  "https://data.coat.no",
            key = Sys.getenv("api_COAT"))  # save your API key as api_COAT in your .Renviron file


## ---------------------------------------------------------------------------- ##
## DOWNLOAD INFO ABOUT THE WEATHER STATIONS AND MAKE A COORDINATE AND AUX FILE
## ---------------------------------------------------------------------------- ##

## build the url to frost
url_sources <- paste0("https://", client_id, "@frost.met.no/sources/v0.jsonld?ids=", paste(sources, collapse = ","), "&types=SensorSystem")

## download information about the weather stations
station_info_raw <- try(fromJSON(URLencode(url_sources),flatten=T))$data

## format data, remember to check that special characters are converted to aa/ae/o and that v_owner is converted correctly
station_info <- station_info_raw %>%
  select(id, name, country, masl, validFrom, county, geometry.coordinates, stationHolders) %>%
  rename(v_station_id = id) %>%
  mutate(v_station_name = tolower(name)) %>%
  mutate(v_station_name = str_replace_all(v_station_name, c("å" = "aa", "æ" = "ae", "ø" = "o"))) %>% #try ["å" = "aa", "æ" = "ae", "ø" = "o"] first, then with unicode ["å" = "aa", "æ" = "ae", "ø" = "o"] if it doesn't work with special letters
  mutate(v_station_name = case_when(
      v_station_name == "kommagdalen" ~ "komagdalen", TRUE ~ v_station_name)) %>%
  mutate(date_first = substr(validFrom, 1, 10)) %>%
  mutate(sn_region = ifelse(county == "SVALBARD", "svalbard",
                            ifelse(county == "FINNMARK", "varanger",
                                   ifelse(county == "TROMS", "troms", NA)))) %>%
  mutate(v_meter_above_sea_level = masl) %>%
  mutate(e_dd = map_dbl(geometry.coordinates, ~ .x[1]),
         n_dd = map_dbl(geometry.coordinates, ~ .x[2])) %>%
  mutate(v_owner = tolower(sapply(stationHolders, paste, collapse = ", "))) %>%
  mutate(v_owner = case_when(
        grepl("nibio", v_owner) & grepl("nilu", v_owner) & grepl("met.no", v_owner)  ~ "nibio_nilu_met.no",
        grepl("alfred-wegener", v_owner) & grepl("met.no", v_owner)                  ~ "alfred_wegener_institut_met.no",
        grepl("met.no", v_owner) & grepl("avinor", v_owner)                          ~ "met.no_avinor",
        grepl("statens vegvesen", v_owner)                                           ~ "statens_vegvesen",
        grepl("norsk polarinstitutt", v_owner)                                       ~ "norsk_polarinstitutt",
        TRUE ~ v_owner # keeps the original value if none of the above match
        )) %>%
  arrange(sn_region, v_station_name)

## make the aux file
aux <- station_info %>%
  mutate(date_last = NA, v_comment = NA) %>%
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first, date_last, v_comment)

## make coordinate file
coord <- station_info %>%
  select(v_station_id, e_dd, n_dd)

## convert coordinates to utm33
utm33 <- utm33_from_wgs84(coord$e_dd, coord$n_dd)

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

## format data, keep timeOffset PT0H (daily mean from midnight to midnight UTC) and PT6H (06 to 06 UTC)
element_info <- filter(element_info_raw, elementId == elements & timeOffset %in% c("PT0H", "PT6H")) %>%
  select(sourceId, validFrom, validTo, timeOffset) %>%
  mutate(sourceId = sub(":0", "", sourceId)) %>%
  mutate(validFrom = substr(validFrom, 1, 10), validTo = substr(validTo, 1, 10)) %>%
  rename(v_station_id = sourceId) %>%
  left_join(aux, by = "v_station_id")

## check if temperature data is available for all stations
missing_station <- aux$v_station_name[!aux$v_station_id %in% element_info$v_station_id]
missing_station # add comment if there is no data available

## check if there are any gaps
gaps <- element_info %>%
  mutate(validFrom = ymd(validFrom), validTo = ymd(validTo)) %>%
  group_by(v_station_id, timeOffset) %>%
  arrange(validFrom, .by_group = TRUE) %>%
  mutate(gap = as.numeric(validFrom - lag(validTo, default = first(validFrom))) > 1) %>%
  filter(gap == TRUE)
gaps  # add comment in aux if there are stations with gaps

## add comments
aux$v_comment[aux$v_station_name %in% missing_station] <- "no temperature data available from this station"

## use the date when data is available as date_first
date_first <- element_info %>%
  mutate(validFrom = ymd(validFrom)) %>%
  group_by(v_station_id) %>%
  summarise(date_first = min(validFrom))

aux <- aux %>% select(-date_first) %>%
  left_join(date_first, by = "v_station_id") %>%
  select(sn_region, v_station_name, v_station_id, v_owner, v_meter_above_sea_level, date_first, date_last, v_comment)

## save aux and coordinate file to a temporary directory for uploading them to the COAT data portal later
write.table(aux, paste(temp_dir, file_aux, sep = "/"), row.names = FALSE, sep = ";")
write.table(coord, paste(temp_dir, file_coord, sep = "/"), row.names = FALSE, sep = ";")


## ---------------------------------------------------------------------------- ##
## DOWNLOAD AND PROCESS DATA FROM FROST
## ---------------------------------------------------------------------------- ##

## janssonhaugen gives an error -> temp data first available in 2025

data_list <- list()

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
  if (!inherits(xs, "try-error")) {
    raw_dat <- unnest(xs$data, cols = c(observations))
    print(paste0("Data retrieved from frost.met.no for ", aux$v_station_name[i], "!"))
  } else {
    print(paste0("Error: the data retrieval was not successful for ", aux$v_station_name[i], "!"))
    next
  }

  ## format data, use PT0H if available and PT6H otherwise, keep one value per day
  station_dat <- raw_dat %>%
    filter(timeOffset %in% c("PT0H", "PT6H")) %>%
    mutate(date = as.Date(ymd_hms(referenceTime))) %>%
    select(date, value, timeOffset) %>%
    group_by(date) %>%
    arrange(case_when(timeOffset == "PT0H" ~ 1, timeOffset == "PT6H" ~ 2), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    rename(v_temperature_day = value) %>%
    select(date, v_temperature_day)

  ## add missing dates with NA to make complete years
  all_dates <- tibble(date = seq(floor_date(min(station_dat$date), "year"),
                                 ceiling_date(max(station_dat$date), "year") - 1,
                                 by = "day"))

  data_list[[aux$v_station_id[i]]] <- all_dates %>%
    left_join(station_dat, by = "date") %>%
    mutate(sn_region = aux$sn_region[i],
           v_station_name = aux$v_station_name[i],
           v_station_id = aux$v_station_id[i],
           t_year = year(date),
           t_month = month(date),
           t_day = day(date)) %>%
    select(sn_region, v_station_name, v_station_id, t_year, t_month, t_day, date, v_temperature_day)
}

## stations without data in the output
aux$v_station_name[!aux$v_station_id %in% names(data_list)]

## combine to one dataset
data_all <- bind_rows(data_list)


## ---------------------------------------------------------------------------- ##
## CALCULATE THE STATE VARIABLE
## ---------------------------------------------------------------------------- ##

## calculate start, end and length of the growing season for each threshold
gs_detail <- bind_rows(lapply(names(thresholds), function(thres_name) {
  calc_gs_length(data_all, thres_val = thresholds[[thres_name]], min_run = min_run,
                 months = season_months, min_coverage = min_coverage) %>%
    mutate(threshold = thres_name)
}))

## check: seasons that start on the first or end on the last day of season_months may be cut off
## (if this happens often, consider extending season_months)
window_first <- function(year) make_date(year, min(season_months), 1)
window_last  <- function(year) ceiling_date(make_date(year, max(season_months), 1), "month") - 1

gs_detail %>%
  filter(v_gs_start == window_first(t_year) | v_gs_end == window_last(t_year)) %>%
  count(threshold, v_station_name)

## check: number of years with NA per station and threshold
gs_detail %>%
  group_by(threshold, v_station_name) %>%
  summarise(n_years = n(), n_na = sum(is.na(v_gs_length)), .groups = "drop") %>%
  filter(n_na > 0)

## make the final dataset, one column per threshold
state_var_all <- gs_detail %>%
  select(sn_region, v_station_name, v_station_id, t_year, threshold, v_gs_length) %>%
  pivot_wider(names_from = threshold, values_from = v_gs_length,
              names_prefix = "v_growing_season_length_") %>%
  arrange(sn_region, v_station_name, t_year)

## save data to a temporary directory
file_name <- paste0(sv_name, "_", min(state_var_all$t_year), "_", max(state_var_all$t_year), ".txt")
file_name

write.table(state_var_all, paste(temp_dir, file_name, sep = "/"), row.names = FALSE, sep = ";")


## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## set to TRUE when the files in temp_dir have been checked and you are ready to upload
upload_to_portal <- FALSE
if (!upload_to_portal) stop("Upload stopped: check the output, then set upload_to_portal <- TRUE.")

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## search for your state variable
state_name <- "c106_growing_season_length_weather_stations_v1" # write here the name including the version of the state variable you want to add data to (check the exact name on data.coat.no)
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name # check that this is the correct state variable before uploading
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

## upload the aux file, the coordinate file and the data file
for (i in c(file_aux, file_coord, file_name)) {
  resource_create(
    package_id = pkg_state$id,
    description = NULL,
    upload = paste(temp_dir, i, sep = "/"),
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
