## Function to download all coordinate files from the COAT data portal
## Hanna Böhner 2026-01-15

## packages that need to be loaded: ckanr, tidyverse, purrr

## the connection to the COAT dataportal has to be set up with ckanr::ckanr_setup(url =  "https://data.coat.no", key = "your-api-key") 

## the function downloads all coordinate files from the coat dataportal (files that end with _coordinates.txt)
## combines all coordinates, removes duplicated sites and reformats the files

## IMPORTANT:the coordinate file should be chekced carefully before the function is used to update climate state variables


## ----------------------------------------------------------------------------------------------------------------------------------------

get_coat_coordinates <- function() {
  

  ## get all modules
  modules <- organization_list(as = "table")$name
  
  ## get all datasets (only most recent version)
  pkg_list <- c()
  
  for (i in 1:length(modules)) {
    pkg_list[[i]] <- package_search(q = paste0("organization:", modules[i]), rows = 1000, include_private = TRUE, as = "table")$results %>% 
      mutate(status = ifelse(private, "private", "public")) %>% 
      select(name, version, type, status) %>% 
      arrange(name) %>% 
      mutate(module = modules[i]) %>% 
      filter(type == "dataset")  
  }
  
  all_pkg <- bind_rows(pkg_list)
  
  ## get all data files
  res_list <- c()
  
  for (i in 1:nrow(all_pkg)) {
    res_list[[i]] <- package_search(q = list(paste("name:", all_pkg$name[i], sep = "")), fq = list(paste("version:", all_pkg$version[i], sep = "")), include_private = TRUE, as = "table")$results$resources[[1]]
    
    if (length(res_list[[i]]) != 0) res_list[[i]] <- select(res_list[[i]], id, name, package_id, url)
    if (length(res_list[[i]]) != 0) res_list[[i]] <- mutate(res_list[[i]], module = all_pkg$module[i])
  }
  
  ## select only coordinate files
  coord_overview <- res_list %>% discard(~length(.) == 0) %>% 
    bind_rows() %>%
    slice(grep("coordinates", name)) %>% 
    filter(!duplicated(name)) %>% 
    filter(name != "S_grubbing_transects_coordinates.txt") %>%  # use coordinates from plant data
    filter(name != "V_snow_structure_intensive_coordinates.txt")  # "wrong" coordinate for ko_ko_hn_a
  
  
  ## download all coordinate files
  coord_list <- c()
  
  for (i in 1:nrow(coord_overview)) {
    coord_list[[i]] <- ckan_fetch(coord_overview$url[i],
                                  sep = ";",
                                  header = TRUE,
                                  format = "txt"
    )
    
    if ("sn_group_of_sites_spatial" %in% colnames(coord_list[[i]])) coord_list[[i]]$sn_group_of_sites_spatial <- as.character(coord_list[[i]]$sn_group_of_sites_spatial)
    
    coord_list[[i]]$dataset_name <- coord_overview$name[i]
  }
  
  
  ## keep only unique rows
  coords <- bind_rows(coord_list) %>% 
    mutate(sn_region = ifelse(substr(dataset_name, 1, 1) == "S", "svalbard",
                              ifelse(substr(dataset_name, 1, 1) == "V", "varanger",
                                     ifelse(substr(dataset_name, 1, 1) == "T", "troms", NA)))) %>% 
    filter(!is.na(sn_site)) %>% 
    select(sn_region, sn_locality, sn_site, e_dd, n_dd, e_utm33, n_utm33) %>% 
    arrange(sn_site) %>% 
    mutate(e_dd = round(e_dd, digits = 5),
           n_dd = round(n_dd, digits = 5),
           e_utm33 = round(e_utm33, digits = 0),
           n_utm33 = round(n_utm33, digits = 0)) %>% 
    distinct() %>%
    filter(!is.na(e_dd))
  
  ## check if there are still duplicated sites in the dataframe
  which(duplicated(coords$sn_site))
  duplicated_sites <- coords$sn_site[duplicated(coords$sn_site)]  
  table(coords$sn_site)
  
  duplicates <- coords %>% filter(sn_site %in% duplicated_sites) %>% 
    arrange(sn_site)
  
  ## delete the duplicated sites
  coords <- filter(coords, !duplicated(sn_site))
  
  ## add sn_locality
  coords$sn_locality <- ifelse(substr(coords$sn_site, 1, 2) == "ko", "komagdalen",
                               ifelse(substr(coords$sn_site, 1, 2) == "vj", "vestre_jakobselv",
                                      ifelse(substr(coords$sn_site, 1, 2) == "if", "ifjordfjellet",
                                             ifelse(substr(coords$sn_site, 1, 2) == "KI", "kirkenes", 
                                                    ifelse(substr(coords$sn_site, 1, 2) == "TA", "tana", 
                                                           ifelse(substr(coords$sn_site, 1, 2) == "a_", "austertana", 
                                                                  ifelse(substr(coords$sn_site, 1, 2) == "b_", "bugoyfjord", 
                                                                         ifelse(substr(coords$sn_site, 1, 2) == "t_", "tana", 
                                                                                ifelse(substr(coords$sn_site, 1, 4) == "aark", "aaroy_kontinent", 
                                                                                       ifelse(substr(coords$sn_site, 1, 4) == "aaro", "aaroy", 
                                                                                              ifelse(substr(coords$sn_site, 1, 4) == "daaf", "daafjord", 
                                                                                                     ifelse(substr(coords$sn_site, 1, 4) == "karl", "karlsoy", 
                                                                                                            ifelse(substr(coords$sn_site, 1, 4) == "rein", "reinoy", 
                                                                                                                   ifelse(substr(coords$sn_site, 1, 4) == "rekv", "rekvik", 
                                                                                                                          ifelse(substr(coords$sn_site, 1, 4) == "skul", "skulsfjord", 
                                                                                                                                 ifelse(substr(coords$sn_site, 1, 4) == "trom", "tromvik", 
                                                                                                                                        ifelse(substr(coords$sn_site, 1, 4) == "tuss", "tussoy", 
                                                                                                                                               ifelse(substr(coords$sn_site, 1, 4) == "tusk", "tussoy_kontinent", 
                                                                                                                                                      ifelse(substr(coords$sn_site, 1, 4) == "veng", "vengsoy", 
                                                                                                                                                             ifelse(substr(coords$sn_site, 1, 4) == "daav", "daavoy", 
                                                                                                                                                                    ifelse(substr(coords$sn_site, 1, 2) == "bf", "bekkarfjord", 
                                                                                                                                                                           ifelse(substr(coords$sn_site, 1, 2) == "ho", "nordkynn", 
                                                                                                                                                                                  ifelse(substr(coords$sn_site, 1, 2) == "st", "stjernevann",
                                                                                                                                                                                         ifelse(substr(coords$sn_site, 1, 2) %in% c("f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8"), "komagdalen", 
                                                                                                                                                                                                ifelse(substr(coords$sn_site, 1, 2) %in% c("f_", "n_"), "polmak",
                                                                                                                                                                                                       ifelse(substr(coords$sn_site, 1, 3) %in% c("k50"), "kvaloysletta",
                                                                                                                                                                                                              ifelse(substr(coords$sn_site, 1, 4) %in% c("k100", "k170", "k240"), "kvaloysletta",
                                                                                                                                                                                                                     ifelse(substr(coords$sn_site, 1, 3) %in% c("r30", "r10", "r17", "r24"), "reinoy",
                                                                                                                                                                                                                            ifelse(substr(coords$sn_site, 1, 3) %in% c("s50", "s10", "s17", "s24"), "skogsfjord",
                                                                                                                                                                                                                                   coords$sn_locality
                                                                                                                                                                                                                            ))))))))))))))))))))))))))))) 
  
  coords$sn_locality[coords$sn_site %in% paste0("k", 1:14)] <- "komagdalen"
  
  unique(coords$sn_locality)
  
  return(coords)
  
  ## make a map
  # map <- leaflet(data = coords) %>% 
  #   #addProviderTiles(providers$OpenTopoMap) %>% 
  #   addTiles() %>% 
  #   setView(lng = 29.3, lat = 69.8, zoom = 5) %>% 
  #   addCircleMarkers(~e_dd, ~n_dd, popup = ~sn_site)
  # map
  
  ## convert coordinates to projection of the gridded dataset
  # coord <- cbind.data.frame(lon=coords$e_dd, lat=coords$n_dd) # only columns with coordinates
  # coord <- st_as_sf(coord, coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84") # convert to spatial object
  # coord <- st_transform(coord, "+proj=lcc +lat_1=80 +lat_0=80 +lon_0=326 +x_0=-551891.1979581077 +y_0=1031350.2258432541 +R=6367470 +datum=WGS84 +units=m +no_defs")  
  # coord <- st_coordinates(coord)  
}

