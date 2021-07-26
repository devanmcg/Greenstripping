pacman::p_load(tidyverse, foreach, doSNOW)
library(Rothermel, attach.required = F)

data("SFM_metric")
load('./objects/GBprod.Rdata')
load('./objects/DayOfWx.Rdata')

# Extreme fuel & weather parameters from historical fires
  cheat_params <-
    bind_rows(
      GBprod %>%
        group_by(Event_ID) %>%
        summarize(FuelLoad = sum(biomass), 
                  .groups = 'drop') %>%
        summarize(Mean = mean(FuelLoad), 
                  SD = sd(FuelLoad), 
                  lwr = quantile(FuelLoad, 0.05), 
                  upr = quantile(FuelLoad, 0.95))  %>%
        mutate(param = 'FuelLoad'),
      DayOfWx %>%
        group_by(param) %>%
        summarize(Mean = mean(DayOfValue), 
                  SD = sd(DayOfValue), 
                  lwr = quantile(DayOfValue, 0.05), 
                  upr = quantile(DayOfValue, 0.95), 
                  .groups = 'drop') )  %>%
        mutate( extreme = case_when(
                 param == 'Fmax' ~ upr, 
                 param == 'FuelLoad' ~ upr * 0.000453592, 
                 param == 'wind_vel' ~ upr * 3.6,
                 param %in% c("FDFM", 'rhmin') ~ lwr )) %>%
        select(param, Mean, SD, extreme)
  
  cheat <- cheat_params %>%
    select(param, extreme) %>%
    pivot_wider(names_from = 'param', 
                values_from = "extreme")

# Environmental parameters 
  wind = cheat$wind_vel
  slope = 0
  
# Cheatgrass fuel model parameters 
  # for ros
  cheat_mt = SFM_metric["A1", "Fuel_Model_Type"]
  cheat_w <- c(cheat$FuelLoad, 0, 0, 0, 0)
  cheat_s <- as.vector(SFM_metric ["A1", 7:11])
  cheat_delta <- SFM_metric ["A1", "Fuel_Bed_Depth"]
  cheat_mx.dead <- SFM_metric ["A1", "Mx_dead"]
  cheat_h <- SFM_metric ["A1", 14:18]
  cheat_m <- c(cheat$FDFM, 0, 0, 0, 0)
  
 # for .fmd
  fmd <- 
  tibble(
    FMNum = 14, 
    FMCode = "cheat", 
    H1 = cheat_w[1], 
    H10 = cheat_w[2],
    H100 = cheat_w[3], 
    LiveH = cheat_w[4], 
    LiveW = cheat_w[5], 
    FMtype = cheat_mt, 
    SAV1H = cheat_s[[1]] / 100, 
    LiveHSAV = cheat_s[[4]] / 100, 
    LiveWSAV = cheat_s[[5]] / 100, 
    depth = cheat_delta, 
    XtMoist = cheat_mx.dead, 
    DHt = cheat_h[[1]], 
    LHt = cheat_h[[4]], 
    name = "Typical cheatgrass stand"
  ) 
  
  write_tsv(fmd, './FB/MTT/inputs/CustomFuels.fmd', col_names=F, append=F)
  
# Greenstrip fuel  model runs 

{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  mod = 'GR2' # Base fuel model
  # Custom parameters: 
      green_load <- seq(0.1, 1, length.out = 5)
      green_ratio <- seq(0.1, 0.9, length.out = 5)
      prop_herb <- seq(0.1, 1, length.out = 5)
      green_H2O <- seq(30, 120, length.out = 8)
  
  GreenROS <- 
    foreach(l=1:length(green_load), 
            .combine = bind_rows, 
            .errorhandling = 'remove', 
            .inorder = TRUE) %:%
      foreach(r=1:length(green_ratio), 
              .combine = rbind, 
              .inorder = TRUE,
              .errorhandling = 'remove') %:% 
      foreach(h=1:length(prop_herb), 
              .combine = rbind, 
              .inorder = TRUE,
              .errorhandling = 'remove') %:% 
      foreach(m=1:length(green_H2O), 
              .combine = rbind, 
              .errorhandling = 'stop', 
              .inorder = TRUE, 
              .packages = c('tidyverse')) %dopar% {

    green_m <- c(cheat$FDFM, cheat$FDFM+2, 0, green_H2O[m], green_H2O[m])
    
    tibble(
      TotalLoad = green_load[l], 
      PropFine = prop_herb[h], 
      PropLive = green_ratio[r],
      LiveMoisture = green_H2O[m], 
      ros = as.numeric(
              Rothermel::ros (
                modeltype = SFM_metric [mod, "Fuel_Model_Type"], 
                w = c((green_load[l]*(1-green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
                      (green_load[l]*(1-green_ratio[r]))*(1-prop_herb[h]), 
                      0, 
                      (green_load[l]*(green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
                      (green_load[l]*(green_ratio[r]))*(1-prop_herb[h])), 
                s = SFM_metric [mod, 7:11], 
                delta = SFM_metric [mod, "Fuel_Bed_Depth"], 
                mx.dead = SFM_metric [mod, "Mx_dead"], 
                h = SFM_metric [mod, 14:18], 
                m = green_m, 
                u = wind, 
                slope = 0)[15] ) 
    )
              }
  stopCluster(cl)
  Sys.time() - begin
  }
  
  GreenROS %>%
    filter(TotalLoad %in% c(min(TotalLoad), max(TotalLoad)), 
           PropFine %in% c(min(PropFine), max(PropFine))) %>%
    mutate(PropLive = as.factor(PropLive)) %>%
  ggplot( ) + theme_bw(14) + 
    geom_line(aes(x = LiveMoisture, 
                  y = ros, 
                  color = PropLive, 
                  group = PropLive)) +
  facet_grid(PropFine~TotalLoad) +
    scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                       labels = seq(30, 120, length.out = 4)) 
  