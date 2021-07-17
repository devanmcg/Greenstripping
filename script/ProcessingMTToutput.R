pacman::p_load(tidyverse, sf, stars, foreach, doSNOW)

fuels <- raster::raster('./FB/MTT/inputs/fuels.asc')
raster::crs(fuels)



fuels_stars <- st_as_stars(fuels)
raster::crs(ros_ras) <- raster::crs(fuels)

# Process ROS output

{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
ros_fp = './FB/MTT/outputs/ROS'
files <- list.files('./FB/MTT/outputs/completed') 
ros_files <- files[substr(files, nchar(files)-6, nchar(files)) == "ROS.asc" ] 

ros_results <- tibble() 
ros_results <- 
foreach(f=1:length(ros_files), 
        .combine = bind_rows, 
        .errorhandling = 'stop', 
        .inorder = FALSE, 
        .packages = c('tidyverse')) %dopar% {

  ros_ras <- raster::raster(file.path(ros_fp, ros_files[f]))
  
  ros_ras[c(raster::cellFromRowColCombine(ros_ras, 
                                          c(1:165), 
                                          c(1:1040)))] %>% 
    as_tibble() %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    filter(value > 0) %>%
    summarize(prop = n() / c(165*1040), 
              ros = mean(value)) %>%
    mutate(scenario = gsub("\\+.*","", ros_files[f])) %>%
    bind_rows(ros_results) 
        }
  stopCluster(cl)
  Sys.time() - begin
}

ros_results %>%
  select(scenario, prop, ros) %>%
  separate(scenario, c('LM', 'width', 'units', 
                       'TL', 'FD', 'LF'), sep = '_') %>%
  select(-units) %>% 
  mutate(across(c(LM:LF), ~sub("^\\D+", "", .)) )

ros_stars <- st_as_stars(ros_ras)

ros_stars <- st_crop(ros_stars, fuels_stars)

ggplot() + theme_bw() + 
  geom_stars(data = fuels_stars) +
  geom_stars(data = ros_stars) +
  scale_fill_continuous(low="thistle2", high="darkred", 
                        guide="colorbar", na.value="transparent")


# Arrival times 

art_ras <- raster::raster('./FB/MTT/outputs/tests/NoStrip_ArrivalTime.asc')
raster::plot(art_ras)
