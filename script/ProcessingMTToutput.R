{
pacman::p_load(tidyverse, sf, stars, foreach, doSNOW)
pacman::p_load_gh("devanmcg/wesanderson")
}

fuels <- raster::raster('./FB/MTT/inputs/fuels.asc')
raster::crs(fuels)

fuelbed <- 
  as.character(fuels[c(raster::cellFromRowColCombine(fuels, 
                                          c(1:nrow(fuels)), 
                                          c(1:ncol(fuels))))]) 
fb_ras <- terra::rast(fuelbed)

st_as_stars(fb_ras)
fuels_stars <- st_as_stars(fuels)
raster::crs(ros_ras) <- raster::crs(fuels)

# Process ROS output

{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  ros_fp = 'C:/Users/devan.mcgranahan/OneDrive - USDA/Research/MTT'
  # ros_fp = 'S:/DevanMcG/FireModeling/MTToutput'
  files <- list.files(ros_fp) 
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
  beepr::beep() 
  Sys.time() - begin
}

# Plotting

ros_results %>%
  select(scenario, prop, ros) %>%
  separate(scenario, c('LM', 'width', 'units', 
                       'TL', 'FD', 'LF'), sep = '_') %>%
  select(-units) %>% 
  # filter(TL %in% c(min(TL), max(TL))) %>%
  mutate(across(c(LM:LF), ~sub("^\\D+", "", .)), 
         `Fine:Coarse\nfuel ratio` = FD,
         # LF = as.character(as.numeric(LF) * 100), 
         `Total load` = recode(as.character(TL), 
                     '0.1' = '\n0.24 t/ha', 
                     '0.4' = '\n0.91 t/ha', 
                     '0.7' = '\n1.57 t/ha', 
                     '1' = '\n2.24 t/ha'), 
         LM = as.numeric(LM)) %>%
  ggplot() + theme_bw(14) + 
    geom_line(aes(y = prop*100, 
                  x = LM, 
                  color = LF, 
                  group = LF), 
              size = 1.1, 
              position = position_dodge(width = 1)) +
  geom_point(aes(y = prop*100, 
                x = LM, 
                fill = LF, 
                shape = LF), 
            size = 2.5, 
            stroke = 1, 
            color = "lightgrey", 
            position = position_dodge(width = 1)) +
  labs(x = 'Live fuel moisture content (%)', 
       y = 'Protected area burned (%)') +
  scale_shape_manual("Live:Dead\nfuel ratio",
                     values = c(23, 22, 21, 24)) +
  scale_fill_manual("Live:Dead\nfuel ratio",
                     values = wes_palette("FuelMoisture4")) +
  scale_color_manual("Live:Dead\nfuel ratio",
                     values = wes_palette("FuelMoisture4")) +
  scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                     labels = seq(30, 120, length.out = 4)) +
  facet_grid(`Fine:Coarse\nfuel ratio` ~`Total load`, 
             labeller = label_both) +
  theme(strip.text.y = element_text(angle = 0), 
        panel.grid.minor.x = element_blank() )

# Diagnostics 
# Missing scenarios
ros_results %>%
  select(scenario) %>%
  separate(scenario, c('LM', 'width', 'units', 
                       'TL', 'FD', 'LF'), sep = '_') %>%
  select(-units, -width) %>%
  # filter(LM == 'LM=120', 
  #        TL == 'TotalLoad=0.1') %>%
  group_by(LM, TL) %>%
  summarize(count = n()) # missing LM=120, TotalLoad=0.1, PropFine != 1, 0.7

# Scenarios that require more strips
NonZeroScenariosR1 <-
  ros_results %>%
  filter(prop != 0) %>%
  select(scenario)

# save(NonZeroScenariosR1, file = "./FB/MTT/inputs/NonZeroScenariosR1.Rdata")

# Raster business

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
