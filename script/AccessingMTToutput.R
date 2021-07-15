pacman::p_load(tidyverse, sf, stars)

fuels <- raster::raster('./FB/MTT/inputs/fuels.asc')

fuels_stars <- st_as_stars(fuels)

# TO compare two
mtt_paths <- 
  bind_rows(
  read_sf('./FB/MTT/outputs/tests', 
          'NoStrip_MTTMajorPaths', 
          crs = 26911) %>%
    mutate(Strip = "No strip"), 
  read_sf('./FB/MTT/outputs/tests', 
          'StripTest_MTTMajorPaths', 
          crs = 26911) %>%
    mutate(Strip = "Default GR2") )

# To just see one
  mtt_paths <-
    read_sf('./FB/MTT/outputs/tests', 
          'StripTest_MTTMajorPaths', 
          crs = 26911)

ggplot() + theme_bw() + 
  geom_stars(data = fuels_stars) +
  geom_rect(data = ig_bbox, 
            aes(xmin = xmin, 
                xmax = xmax,
                ymin = ymin, 
                ymax = ymax), 
            fill = NA, 
            color = "white", 
            size = 1) +
  geom_sf(data = ignitions, 
          pch = 21, 
          fill = 'yellow', 
          color = 'orange') + 
  geom_sf(data = mtt_paths, 
          color = 'red' ) 



NoStrip_ROS <- raster::raster('./FB/MTT/outputs/tests/NoStrip_ROS.asc')
GR2_ROS <- raster::raster('./FB/MTT/outputs/tests/StripTest_ROS.asc')
raster::plot(GR2_ROS)

CRS(GR2_ROS) = "+proj=longlat +datum=WGS84"

ros_stars <- st_as_stars(ros_ras, 
                         crs = st_crs(26911)) 

ros_stars %>%
  st_crs(26911) 
              st_crop(fuels_stars)


ggplot() + theme_bw() + 
  geom_stars(data = fuels_stars) +
  geom_stars(data = ros_stars) 


art_ras <- raster::raster('./FB/MTT/outputs/tests/NoStrip_ArrivalTime.asc')
raster::plot(art_ras)
