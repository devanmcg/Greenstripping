pacman::p_load(raster, tidyverse, sf, stars)

gb_sf <- read_sf('S:/DevanMcG/GIS/SpatialData/US/EPAecoregions/L3', 
                 'us_eco_l3_state_boundaries')  %>%
            filter(US_L3CODE %in% c('13', '14', '80'))

albersEAC <- st_crs(gb_sf)

gb_perims <- read_sf('S:/DevanMcG/GIS/SpatialData/US/mtbs', 
                     'mtbs_perims_DD') %>%
              st_transform(albersEAC) %>%
              st_intersection(gb_sf) %>%
              filter(Incid_Type == "Wildfire")

gb_perims <-
  gb_perims %>%
    select(Event_ID, Ig_Date) %>%
    mutate(date = as.POSIXct(Ig_Date, '%Y-%m-%d'), 
           VegYear = as.numeric(format(date, '%Y')) - 1) %>%
    filter(VegYear >= 1986)

save(gb_perims, file = './objects/gb_perims.Rdata')

load('./objects/gb_perims.Rdata')

gb_perims %>%
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1:6) %>%
  summarize(mean = mean(area)) %>%
  st_centroid() %>%
  ggplot() + 
  geom_sf() 

rgbif::elevation(latitude = 41.194343, 
                 longitude = -115.92551, 
                 username = 'devanmcg')

scale_factor = 0.33
lcp_bbox <- 
  tibble(
      ymin = 2193941 - (39414*scale_factor), 
      ymax = 2193941 + (39414*scale_factor), 
      xmin = -1645980 - (39414*(scale_factor*2)), 
      xmax = -1645980 + (39414*(scale_factor*2))
      ) 

# Create the basic (empty) raster for simulation landscape
lcp_ras <- raster::raster(resolution = 50, 
                          xmn=lcp_bbox$xmin, 
                          xmx=lcp_bbox$xmax, 
                          ymn=lcp_bbox$ymin, 
                          ymx=lcp_bbox$ymax, 
                          crs = '+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs')
# Create fuel model theme inputs for FlamMap .lcp file 
fuels<- lcp_ras
  raster::values(fuels) = 14

  # Greenstrip fuel codes
  max_width = 5 
  codes = seq(15, 14+max_width, 1) 
  for(r in 1:length(codes)) {
    fuels[c(raster::cellFromRow(fuels, c((scale_factor*500)+r, (scale_factor*500)+r)), 
            raster::cellFromCol(fuels, c(0, ncol(fuels))))] <- codes[r]
  }
  
  raster::writeRaster(fuels, './FB/MTT/inputs/fuels.asc', overwrite = T)
  
 fuels_stars <- st_as_stars(fuels)

  ggplot() + theme_bw() + 
    geom_stars(data = fuels_stars) 
  
# Create ignitions 
  # Define zone 
  
  ig_bbox <- 
    tibble(
      ymin = 2193941 - (39414*(scale_factor-0.05)+1500), 
      ymax = 2193941 + (39414*(scale_factor-0.05)-15000), 
      xmin = -1645980 - (39414*(scale_factor+0.05)+5000), 
      xmax = -1645980 + (39414*(scale_factor+0.05)+5000)
    ) 
  
  # Random ignition points
  
  ig_num = 5
  ignitions <-  
    tibble(lat = runif(ig_num, ig_bbox$ymin, ig_bbox$ymax), 
           lon = runif(ig_num, ig_bbox$xmin, ig_bbox$xmax)) %>%
    st_as_sf(coords = c('lon', 'lat'), 
             crs = 26911) 
  
  ignitions %>%
    write_sf('./FB/MTT/inputs/TestIgnitions5.shp', append = F)
  
  ggplot() + theme_bw() + 
    geom_stars(data = fuels_stars) +
    geom_rect(data = ig_bbox, 
              aes(xmin = xmin, 
                  xmax = xmax,
                  ymin = ymin, 
                  ymax = ymax), 
              fill = NA, 
              color = "red", 
              size = 1) +
    geom_sf(data = ignitions, 
            pch = 21, 
            size = 3, 
            stroke = 1.25, 
            fill = 'yellow', 
            color = 'orange')
 




    

