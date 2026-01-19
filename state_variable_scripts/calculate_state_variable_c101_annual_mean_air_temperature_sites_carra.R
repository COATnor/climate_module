#### --------------------------------------------------------------------------------------------------------- ####
### STATE VARIABLE - C101_annual_mean_air_temperature_sites
### Date: 05.12.2025
### Author: Hanna Böhner
#### ------------------------------------------------------------------------------------------------------------ ####

## ------------------------------------------------------------------ ##
## SETUP
## ------------------------------------------------------------------ ##

## load libraries
library(ckanr)
library(tidyverse)
library(purrr)
library(leaflet)
library(htmlwidgets)
library(terra)
library(sf)

## set up the connection to the COAT Data Portal 
ckanr_setup(url =  "https://data.coat.no", 
            key = Sys.getenv("api_COAT"))  # write here your API key (e.g. "asdf123af123")

 ## set up the connection to thredds.met.no
thredds_path <- "https://thredds.met.no/thredds/fileServer/coat/gridded_indicators_beta/carra"

## set the name of the gridded dataset on thredds.met.no
dataset_name <- "tg"

## load functions from GitHub
source("https://github.com/COATnor/climate_module/blob/main/functions/function_get_coat_coordinates.R?raw=TRUE")

## make a temporary directory (the files will be stored there temporary before they are uploaded to the COAT dataportal)
temp_dir <- tempdir()


## ------------------------------------------------------------------ ##
## FETCH AND PREPARE COORDINATES FROM THE COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## download all coordinates from the COAT dataportal (IMPORTANT: the coordinates and the functions have to be checked carefully once before the state variables are updated)
coords <- get_coat_coordinates()

## convert coordinates to projection of the gridded dataset
coord <- cbind.data.frame(lon=coords$e_dd, lat=coords$n_dd) # only columns with coordinates
coord <- st_as_sf(coord, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84") # convert to spatial object
coord <- st_transform(coord, "+proj=lcc +lat_1=80 +lat_0=80 +lon_0=326 +x_0=-551891.1979581077 +y_0=1031350.2258432541 +R=6367470 +datum=WGS84 +units=m +no_defs")  
coord <- st_coordinates(coord)

## save coordinate file to a temporary directory for uploading it to the COAT data portal later
write.table(coords, paste(temp_dir, "C101_annual_mean_air_temperature_sites_carra_coordinates.txt", sep = "/"), row.names = FALSE, sep = ";")

## ------------------------------------------------------------------ ##
## GET GRIDDED DATA FROM THREDDS AND EXTRACT VALUES FOR ALL COORDINATES
## ------------------------------------------------------------------ ##

## years that should be downloaded
years <- 1991:2024

## month that should be downloaded
#month <- c("jan", "feb", "mar", "apr", "mai", "06", "07", "08", "09", "10", "11", "12")

## make file names
filenames <- paste(dataset_name, "carra.ann", years, "nc", sep = ".")

## loop to extract values for all coordinates from gridded dataset

out_names <- c()

for (i in 1:length(filenames)) {
  
  ## download netcdf-file to a temporary directory
  download.file(url = paste(thredds_path, dataset_name, filenames[i], sep = "/"), 
                destfile = paste(temp_dir, filenames[i], sep = "/"), mode = "wb")
  
  ## load netcdf-file from temporary directory
  carra_dat <- raster::raster(paste(temp_dir, filenames[i], sep = "/"))
  
  ## extract climate data for all coordinates
  dat <- raster::extract(carra_dat, coord, method = "bilinear")
  
  ## delete netcdf-file from temporary directory
  file.remove(paste(temp_dir, filenames[i], sep = "/"))
  
  ## check the extrackted data
  head(dat)
  
  ## make a dataframe
  out_dat <- data.frame(sn_region = coords$sn_region,
                        sn_locality = coords$sn_locality, ## include coordinates??
                        sn_site = coords$sn_site,
                        t_year = str_extract(filenames[i], "\\d{4}"),
                        v_temperature_year = dat) 
  
  ## reorder
  out_dat <- arrange(out_dat, sn_region, sn_locality, sn_site)
  
  ## create a name for the output file
  out_names[i] <- paste0("C101_annual_mean_air_temperature_sites_carra_",str_extract(filenames[i], "\\d{4}"), ".txt")
  
  ## save output file to a temporary directory
  write.table(out_dat, paste(temp_dir, out_names[i], sep = "/"), row.names = FALSE, sep = ";")
  
}


## ------------------------------------------------------------------ ##
## UPLOAD DATA FILES TO COAT DATA PORTAL
## ------------------------------------------------------------------ ##

## state variable has to be created (separate step -> either manually or with script) -> manually at the moment
## create new version of the state variable (include step in this script or manually) when updating the state variable

## serach for your state variable
state_name <- "c101_annual_mean_air_temperature_sites_carra_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files


## upload the coordinate file
resource_create(
  package_id = pkg_state$id,
  description = NULL,
  upload = paste(tempdir(), "C101_annual_mean_air_temperature_sites_carra_coordinates.txt", sep = "/"),
  name = "C101_annual_mean_air_temperature_sites_carra_coordinates.txt",
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
