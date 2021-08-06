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

#
# Create ros inputs file
#

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
  green_H2O <- seq(30, 120, length.out = 8)

  GreenROS <- 
  # Loop through fuel scenarios
  foreach(l=1:length(green_load), 
          .errorhandling = 'remove',            
          .combine = rbind,
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
            .packages = c('tidyverse', 'Rothermel')) %dopar% {
      data(SFM_metric)
      # Define fuel model parameters 
      W = c((green_load[l]*(1-green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
            (green_load[l]*(1-green_ratio[r]))*(1-prop_herb[h]), 
            0, 
            (green_load[l]*(green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
            (green_load[l]*(green_ratio[r]))*(1-prop_herb[h])) 


          green_m <- c(cheat$FDFM, cheat$FDFM+2, 0, green_H2O[m], green_H2O[m])
              
              tibble(
                scenario = paste0( 'TL=', round(green_load[l], 2), 
                                   '_PF=', round(prop_herb[h], 1), 
                                   '_PL=', round(green_ratio[r], 1), 
                                   '_LM=', round(green_H2O[m], 0)), 
                ros = as.numeric(
                  ros (
                    modeltype = SFM_metric [mod, "Fuel_Model_Type"], 
                    w = c(W[1], 
                          W[2], 
                          W[3], 
                          W[4], 
                          W[5] ), 
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
  separate(scenario, c('TL', 'PF', 
                       'PL', 'LM'), sep = '_') %>%
  mutate(across(c(TL:LM), ~sub("^\\D+", "", .)), 
         LM = as.numeric(LM)) %>%
  filter(TL %in% c(min(TL), max(TL)), 
         PF %in% c(min(PF), max(PF))) %>%
 mutate(PL = as.factor(PL)) %>%
  ggplot( ) + theme_bw(14) + 
  geom_line(aes(x = LM, 
                y = ros, 
                color = PL, 
                group = PL)) +
  facet_grid(PF ~ TL) +
  scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                     labels = seq(30, 120, length.out = 4))
