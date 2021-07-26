pacman::p_load(tidyverse, sf, climateR) 

load('./objects/GBafgc.Rdata')
load('./objects/gb_perims.Rdata')

GBafgc %>%
  ggplot(aes(x = AFGC)) + theme_bw() +
 geom_histogram() 


gb_ll <- gb_perims %>%
          filter(Event_ID %in% filter(GBafgc, AFGC > 60)$Event_ID ) %>%
         st_centroid() %>%
          st_transform(4326)


{
  begin = Sys.time()
  cl <- makeSOCKcluster(20) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  elements = c(
    'daily_minimum_relative_humidity',
    'daily_maximum_temperature',
    'daily_mean_wind_speed', 
    'dead_fuel_moisture_100hr')
  
  params <- filter(param_meta$gridmet, description %in% elements )$common.name

  DayOfWx <- 
    foreach(f=1:length(unique(gb_ll$Event_ID) ), 
            .combine = rbind, 
            .errorhandling = 'remove', 
            .inorder = FALSE, 
            .packages = c('tidyverse', 'sf', 'climateR')) %dopar% {
              fire = gb_ll %>%
                        filter(Event_ID == unique(Event_ID)[f]) 
              climateR::getGridMET(
                AOI = fire, 
                param = params,
                startDate = fire$Ig_Date )  %>% 
                mutate(Fmax = frost::convert.temperature(from = "K", to = "F", tmax), 
                       FDFM = fmoist_100 - 2) %>%
                select(-source, -lat, -lon, -tmax, -fmoist_100) %>% 
                pivot_longer(names_to = "param", 
                             values_to = "DayOfValue", 
                             -date) %>%
                mutate(event = fire$Event_ID) 
            } 
  stopCluster(cl)
  Sys.time() - begin
          }

# save(DayOfWx, file = './Greenstripping/objects/DayOfWx.Rdata')

DayOfWx %>%
  ggplot() + theme_bw() + 
    geom_boxplot(aes(x = param, 
                     y = DayOfValue), 
                 outlier.shape = NA) +
  coord_flip() +
  labs(x = '') + 
  facet_wrap(~ param, scales = "free") +
  theme(panel.grid.major.y = element_blank(), 
        axis.text.y = element_blank() , 
        axis.ticks.y = element_blank() )


