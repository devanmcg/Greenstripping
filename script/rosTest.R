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

{
 begin = Sys.time()
 cores = parallel::detectCores()
 cl <- makeSOCKcluster(cores) 
 registerDoSNOW(cl)
 clusterCall(cl, function(x) .libPaths(x), .libPaths())
 
   # Custom parameters: 
   green_load <- seq(0.24, 2.24, length.out = 5)
   green_ratio <- seq(0.1, 0.9, length.out = 5)
   prop_herb <- seq(0.1, 1, length.out = 5)
 
   # Loop through fuel scenarios
   foreach(l=1:length(green_load), 
           .errorhandling = 'remove', 
           .inorder = TRUE) %:%
   foreach(r=1:length(green_ratio), 
             .inorder = TRUE,
             .errorhandling = 'remove') %:% 
   foreach(h=1:length(prop_herb), 
             .inorder = TRUE, 
            .errorhandling = 'stop', 
           .packages = c('tidyverse', 'Rothermel')) %dopar% {
   data(SFM_metric)
   # Define fuel model parameters 
     W = c((green_load[l]*(1-green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
                      (green_load[l]*(1-green_ratio[r]))*(1-prop_herb[h]), 
                      0, 
                       (green_load[l]*(green_ratio[r]))*(prop_herb[h]),  # 1 hr (dead)
                       (green_load[l]*(green_ratio[r]))*(1-prop_herb[h])) 
     S = SFM_metric ['GS2', 7:11] 
     D = SFM_metric ['GR2', "Fuel_Bed_Depth"] 
     Mx = SFM_metric ['GR2', "Mx_dead"] 
     H = SFM_metric ['GR2', 14:18]

       # build tibble for Rothermel::ros() using metric values
       
         tibble(
            scenario = paste0( 'TL=', green_load[l], 
                               '_PF=', prop_herb[h], 
                             '_PL=', green_ratio[r]),
          type = SFM_metric [mod, "Fuel_Model_Type"],
           H1 = W[1],
           H10 = W[2],
           H100 = W[3],
           LiveH = W[4],
           LiveW = W[5] ,
           SAV1H = S[1], 
           SAV10H = S[2], 
           SAV100H = S[3], 
           SAVLH = S[4], 
           SAVLW = S[5],
           depth = D ,
           XtMoist = Mx,
           Heat_1h = H[1],
           Heat_10h = H[2],
           Heat_100h = H[3],
           Heat_LH = H[4], 
           Heat_LW = H[5],
           DHt = 8000,
           LHt = 8000) %>%
         mutate(across(H1:LiveW, ~round(., 3))) %>%
      write_tsv('./FB/ros/FuelInputs.txt',
                   col_names=F, append=T)
           }
           stopCluster(cl)
           Sys.time() - begin
       }


# Calculate Rate of Spread 
  # Load fuel parameters
   inputs <- read_tsv('./FB/ros/FuelInputs.txt', 
                      col_names = c(
                          'scenario',
                          'type',
                          'H1',
                          'H10',
                          'H100',
                          'LiveH',
                          'LiveW' ,
                          'SAV1H', 
                          'SAV10H', 
                          'SAV100H',
                          'SAVLH', 
                          'SAVLW',
                          'depth'  ,
                          'XtMoist',
                          'Heat_1h',  
                          'Heat_10h',
                          'Heat_100h',  
                          'Heat_LH' , 
                          'Heat_LW' ,
                          'DHt' ,
                          'LHt' )   )
  # Run Rothermel::ros
   {
     begin = Sys.time()
     cores = parallel::detectCores()
     cl <- makeSOCKcluster(cores) 
     registerDoSNOW(cl)
     clusterCall(cl, function(x) .libPaths(x), .libPaths())
       
       green_H2O <- seq(30, 120, length.out = 8)
     
    GreenROS <- 
     foreach(r=1:length(inputs$scenario), 
             .combine = rbind, 
             .inorder = FALSE,
             .errorhandling = 'stop') %:% 
       foreach(m=1:length(green_H2O), 
               .combine = rbind, 
               .errorhandling = 'stop', 
               .inorder = FALSE, 
               .packages = c('tidyverse')) %dopar% {
                 fuel = inputs %>%
                          filter(scenario == scenario[r])
                 green_m <- c(cheat$FDFM, 
                              cheat$FDFM+2, 
                              0, 
                              green_H2O[m], 
                              green_H2O[m]+30)
                 tibble(
                   scenario = paste0(fuel$scenario, 
                                      '_LM=', 
                                     round(green_H2O[m], 0)), 
                   H1 = fuel$H1, 
                   H10 = fuel$H10, 
                   LF = fuel$LiveH, 
                   LC = fuel$LiveW,
                   TL = sum(H1, H10, LF, LC), 
                   LM = green_m[4],
                   ros = as.numeric(
                     Rothermel::ros (
                       modeltype = fuel$type, 
                       w = select(fuel, H1:LiveW), 
                       s = select(fuel, SAV1H:SAVLW), 
                       delta = fuel$depth, 
                       mx.dead = fuel$XtMoist, 
                       h = select(fuel, Heat_1h:Heat_LW), 
                       m = green_m, 
                       u = wind, 
                       slope = 0)[15] ) 
                 )
               }
     stopCluster(cl)
     Sys.time() - begin
  }

 #  save(GreenROS, file = "./Greenstripping/FB/ros/GreenROS.Rdata")
 
 GreenROS %>% View() 
   # separate(scenario, c('TL', 'PF', 
   #                      'PL', 'LM'), sep = '_') %>%
   # mutate(across(c(TL:LM), ~sub("^\\D+", "", .)), 
   #        LM = as.numeric(LM)) %>%
   # filter(TL %in% c(min(TL), max(TL)), 
   #        H10 %in% c(min(H10), max(H10))) %>%

   mutate(across(H1:TL, ~as.factor(.))) %>%
   ggplot( ) + theme_bw(14) + 
   geom_line(aes(x = LM, 
                 y = ros, 
                 color = LC, 
                 group = LC)) +
   facet_grid(H10 ~ TL) +
   scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                      labels = seq(30, 120, length.out = 4)) 
   
   GreenROS %>% 
     select(scenario, ros) %>%
   separate(scenario, c('TL', 'PF',
                        'PL', 'LM'), sep = '_') %>%
   mutate(across(c(TL:LM), ~sub("^\\D+", "", .)),
          LM = as.numeric(LM)) %>%
   filter(TL %in% c(min(TL), max(TL)),
         PF %in% c(min(PF), max(PF))) %>%
     ggplot( ) + theme_bw(14) + 
     geom_line(aes(x = LM, 
                   y = ros, 
                   color = PL, 
                   group = PL)) +
     facet_grid(PF ~ TL) +
     scale_x_continuous(breaks = seq(30, 120, length.out = 4), 
                        labels = seq(30, 120, length.out = 4)) 
   
   inputs %>%
     filter(scenario == 'TL=2.24_PF=1_PL=0.9') %>%
     pivot_longer(cols = H1:LHt) 
   
   library(Rothermel)
   library(dplyr)
   data("SFM_metric")
   
   Rothermel::ros (
     modeltype = 'D', 
     w = c(0.224, # 1 hr
           0,     # 10 hr
           0,     # 100 hr
           2.02,  # Live fine
           0 ),   # Live coarse
     s = SFM_metric ['GS2', 7:11], 
     delta = 30, 
     mx.dead = 15, 
     h = SFM_metric ['GS2', 14:18], 
     m = c(2.33, 4.33, 0, 120, 150), 
     u = 16.92, 
     slope = 0)[15] # 1.41
   

     Rothermel::ros (
       modeltype = 'D', 
       w = c(0.024, # 1 hr
             0,     # 10 hr
             0,     # 100 hr
             0.216, # Live fine
             0 ),   # Live coarse
       s = SFM_metric ['GS2', 7:11], 
       delta = 30, 
       mx.dead = 15, 
       h = SFM_metric ['GS2', 14:18], 
       m = c(2.33, 4.33, 0, 120, 150), 
       u = 16.92, 
       slope = 0)[15] # 1.41
     
     S =  
     D = SFM_metric ['GR2', "Fuel_Bed_Depth"] 
     Mx = SFM_metric ['GR2', "Mx_dead"] 
     H = 
 