pacman::p_load(tidyverse, foreach, doSNOW)

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
cheat_mt = SFM_metric ["A1", "Fuel_Model_Type"]
cheat_w <- c(cheat$FuelLoad, 0, 0, 0, 0)
cheat_s <- SFM_metric ["A1", 7:11]
cheat_delta <- SFM_metric ["A1", "Fuel_Bed_Depth"]
cheat_mx.dead <- SFM_metric ["A1", "Mx_dead"]
cheat_h <- SFM_metric ["A1", 14:18]
cheat_m <- c(cheat$FDFM, 0, 0, 0, 0)

# Build .fmd files
{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  mod = 'GR2' # Base dynamic fuel model
  # Custom parameters: 
  green_load <- seq(0.24, 2.24, length.out = 4)
  green_ratio <- seq(0.1, 0.9, length.out = 4)
  prop_herb <- seq(0.1, 1, length.out = 4)
  
  # Cheatgrass .fmd row
  cheat_eng <- 
    tibble(
      FMNum = 14, 
      FMCode = "cheat", 
      H1 = cheat_w[1] * 2.2417, 
      H10 = cheat_w[2] * 2.2417,
      H100 = cheat_w[3] * 2.2417, 
      LiveH = cheat_w[4] * 2.2417, 
      LiveW = cheat_w[5] * 2.2417, 
      FMtype = cheat_mt, 
      SAV1H = 3500, 
      LiveHSAV = 1500, 
      LiveWSAV = 1500, 
      depth = cheat_delta * 0.033, 
      XtMoist = cheat_mx.dead, 
      DHt = 8000, 
      LHt = 8000, 
      name = "Typical cheatgrass stand")  %>%
    mutate(across(H1:LiveW, ~round(., 2)))
  
  # Loop through fuel scenarios
  foreach(l=1:length(green_load), 
          .errorhandling = 'remove', 
          .inorder = FALSE) %:%
    foreach(r=1:length(green_ratio), 
            .inorder = FALSE,
            .errorhandling = 'remove') %:% 
    foreach(h=1:length(prop_herb), 
            .inorder = FALSE, 
            .errorhandling = 'stop', 
            .packages = c('tidyverse', 'Rothermel')) %dopar% {
      data(SFM_metric)
      # Define fuel model parameters 
      W = c((green_load[l]*(1-green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
            (green_load[l]*(1-green_ratio[r]))*(1-prop_herb[h]), 
            0, 
            (green_load[l]*(green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
            (green_load[l]*(green_ratio[r]))*(1-prop_herb[h])) 
      S = SFM_metric [mod, 7:11] 
      mt = SFM_metric [mod, "Fuel_Model_Type"]
      D = SFM_metric [mod, "Fuel_Bed_Depth"] 
      Mx = SFM_metric [mod, "Mx_dead"] 
      H = SFM_metric [mod, 14:18]

      # build tibble for Rothermel::ros() using metric values
      
      ModStats = paste0( 'TotalLoad=', green_load[l], 
                         '_PropFine=', prop_herb[h], 
                         '_PropLive=', green_ratio[r])
        tibble(
          scenario = ModStats,
          H1 = W[1],
          H10 = W[2],
          H100 = W[3],
          LiveH = W[4],
          LiveW = W[5] ,
          FMtype = mt,
          SAV1H = 2000,
          LiveHSAV = 1800,
          LiveWSAV = 1500,
          depth = D ,
          XtMoist = Mx,
          DHt = 8001,
          LHt = 8000) %>%
        mutate(across(H1:LiveW, ~round(., 3))) %>%
      write_tsv('./FB/ros/inputs.txt',
                col_names=F, append=T)