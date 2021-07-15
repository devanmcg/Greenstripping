pacman::p_load(tidyverse, sf, foreach, doSNOW)

load('./Greenstripping/objects/gb_perims.Rdata')

# same loaded as sf object
samp_sf <- gb_perims %>%
            st_transform(4326)
#
# RAP parameters 
#
  # Specify the data type to retrieve: 
     Type = 'VegCover'  # Annual vegetation by cover class
    #  Type = 'BiomassA'   # Annual NPP 
  
  # Specify bands to fetch:
    vc_bands = c('1')
     ab_bands = c('1', '2')
    
    bands = vc_bands

  # RAP URL to query
    index_url = 
      ifelse(
        Type == 'VegCover', 
        'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-cover/v2/vegetation-cover-v2-', 
        'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-biomass/v2/vegetation-biomass-v2-'
      )
    
{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  GBafgc <- 
    foreach(f=1:length(unique(samp_sf$Event_ID)), 
            .combine='rbind', 
            .errorhandling = 'remove', 
            .packages=c('tidyverse', 'sf')) %dopar% {
      # Subset feature of interest 
        unit_sf <- filter(samp_sf, Event_ID == unique(samp_sf$Event_ID)[f])
      # URL for RAP type + year                   
        rap_url = paste0('/vsicurl/', index_url, unit_sf$VegYear, '.tif')
     
      # Extract RAP data from URL by feature subset 
        terra::extract(terra::rast(rap_url), 
                       terra::vect(as_Spatial(unit_sf)), 
                       fun = 'mean', 
                       list = FALSE,
                       na.rm = TRUE, 
                       df = TRUE) %>%
        as_tibble() %>%
          rename(point = ID) %>%
        pivot_longer(names_to = 'col', 
                     values_to = 'value', 
                     - point) %>%
        separate(col, c('type', 'band'), sep = '_') %>%
        filter(band %in% bands) %>%
        mutate(Event_ID = unique(samp_sf$Event_ID)[f],
               VegYear = unit_sf$VegYear ) %>%
        select(Event_ID, VegYear, value) %>%
          rename(AFGC = value)
            }
  stopCluster(cl)
  Sys.time() - begin 
}

# save(GBafgc, file = './Greenstripping/objects/GBafgc.Rdata')
    
samp_sf <- 
    gb_perims %>%
        filter(Event_ID %in% filter(GBafgc, AFGC > 60)$Event_ID ) %>%
        st_transform(4326)

# Productivity data (for fuel loads)

{
  begin = Sys.time()
  cores = length(unique(samp_sf$Event_ID))
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  # Specify bands to fetch:
    bands = c('1', '2')
  
  # RAP URL to query
    index_url = 'http://rangeland.ntsg.umt.edu/data/rap/rap-vegetation-biomass/v2/vegetation-biomass-v2-'
  
  GBprod <- 
    foreach(f=1:length(unique(samp_sf$Event_ID)), 
            .combine='rbind', 
            .errorhandling = 'remove', 
            .packages=c('tidyverse', 'sf')) %dopar% {
              # Subset feature of interest 
              unit_sf <- filter(samp_sf, Event_ID == unique(samp_sf$Event_ID)[f])
              # URL for RAP type + year                   
              rap_url = paste0('/vsicurl/', index_url, unit_sf$VegYear, '.tif')
              
              # Extract RAP data from URL by feature subset 
              terra::extract(terra::rast(rap_url), 
                             terra::vect(as_Spatial(unit_sf)), 
                             fun = 'mean', 
                             list = FALSE,
                             na.rm = TRUE, 
                             df = TRUE) %>%
                as_tibble() %>%
                rename(point = ID) %>%
                pivot_longer(names_to = 'col', 
                             values_to = 'value', 
                             - point) %>%
                separate(col, c('type', 'band'), sep = '_') %>%
                filter(band %in% bands) %>%
                mutate(Event_ID = unique(samp_sf$Event_ID)[f],
                       VegYear = unit_sf$VegYear ) %>%
                select(Event_ID, VegYear, value) %>%
                rename(biomass = value)
            }
  stopCluster(cl)
  Sys.time() - begin 
        }

# save(GBprod, file = './Greenstripping/objects/GBprod.Rdata')