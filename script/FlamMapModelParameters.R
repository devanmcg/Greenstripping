pacman::p_load(tidyverse, foreach, doSNOW)

load('./Greenstripping/objects/GBprod.Rdata')
load('./Greenstripping/objects/DayOfWx.Rdata')

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
    green_load <- seq(0.24, 2.24, length.out = 5)
    green_ratio <- seq(0.1, 0.9, length.out = 5)
    prop_herb <- seq(0.1, 1, length.out = 5)
 
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
  #
  # Table compilation
  #
    CellStats = paste0( 'TotalLoad=', green_load[l], 
                        '_PropFine=', prop_herb[h], 
                        '_PropLive=', green_ratio[r])
  # build tibble for .fmd using standard English values
    strip_eng <- 
      tibble(
        FMNum = 15, 
        FMCode = "greenstrip", 
        H1 = W[1] * 2.2417, 
        H10 = W[2] * 2.2417,
        H100 = W[3] * 2.2417, 
        LiveH = W[4] * 2.2417, 
        LiveW = W[5] * 2.2417, 
        FMtype = mt, 
        SAV1H = 2000, 
        LiveHSAV = 1800, 
        LiveWSAV = 1500, 
        depth = D * 0.033, 
        XtMoist = Mx, 
        DHt = 8000, 
        LHt = 8000, 
        name = "Custom greenstrip") %>%
    mutate(across(H1:LiveW, ~round(., 3)))
  # build tibble for Rothermel::ros() using metric values
      tibble(
        scenario = CellStats, 
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
    # Create .fmd for strip scenarios
    # One strip
      bind_rows(cheat_eng, 
                strip_eng, 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '16', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '17', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '18', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '19', FMNum)))) %>%
      write_tsv(paste0('./Greenstripping/FB/MTT/inputs/FuelModels/', 
                       'Strips1_eng_', 
                       CellStats, 
                       '.fmd'), 
                       col_names=F, append=F)
    # Two strips
      bind_rows(cheat_eng, 
                strip_eng, 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '16', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '17', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '18', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '19', FMNum)))) %>%
        write_tsv(paste0('./Greenstripping/FB/MTT/inputs/FuelModels/', 
                         'Strips2_eng_', 
                         CellStats, 
                         '.fmd'), 
                  col_names=F, append=F)
      # Three strips
      bind_rows(cheat_eng, 
                strip_eng, 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '16', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '17', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '18', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '19', FMNum)))) %>%
        write_tsv(paste0('./Greenstripping/FB/MTT/inputs/FuelModels/', 
                         'Strips3_eng_', 
                         CellStats, 
                         '.fmd'), 
                  col_names=F, append=F)
      # Four strips
      bind_rows(cheat_eng, 
                strip_eng, 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '16', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '17', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '18', FMNum))), 
                mutate(cheat_eng, FMNum = as.numeric(chartr('14', '19', FMNum)))) %>%
        write_tsv(paste0('./Greenstripping/FB/MTT/inputs/FuelModels/', 
                         'Strips4_eng_', 
                         CellStats, 
                         '.fmd'), 
                  col_names=F, append=F)
      # Five strips
      bind_rows(cheat_eng, 
                strip_eng, 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '16', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '17', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '18', FMNum))), 
                mutate(strip_eng, FMNum = as.numeric(chartr('15', '19', FMNum)))) %>%
        write_tsv(paste0('./Greenstripping/FB/MTT/inputs/FuelModels/', 
                         'Strips5_eng_', 
                         CellStats, 
                         '.fmd'), 
                  col_names=F, append=F)
              }
  stopCluster(cl)
  Sys.time() - begin
  }
  
  
# Run Rothermel::ros 
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

#  save(GreenROS, file = "./Greenstripping/FB/ros/GreenROS.Rdata")
  
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
  