pacman::p_load(tidyverse, foreach, doSNOW)

# Batch-create .input files
{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  cpu_num = 6
  fmd <- list.files('./FB/MTT/inputs/FuelModels') %>%
    tools::file_path_sans_ext()
  lhfm = seq(30, 120, length.out = 4)
  
  foreach(f=1:length(fmd), 
          .inorder = FALSE,
          .errorhandling = 'remove') %:% 
  foreach(m=1:length(lhfm), 
          .errorhandling = 'stop', 
          .inorder = FALSE) %dopar% {

    writeLines(c("ShortTerm-Inputs-File-Version-1",
                 "FUEL_MOISTURES_DATA: 7",
                 "0 2.33 4 5 10 30",
                 "14 2.33 4 5 10 30",
                 paste0("15 2.33 4 5 ", lhfm[m], " ", lhfm[m]+30),
                 paste0("16 2.33 4 5 ", lhfm[m], " ", lhfm[m]+30),
                 paste0("17 2.33 4 5 ", lhfm[m], " ", lhfm[m]+30),
                 paste0("18 2.33 4 5 ", lhfm[m], " ", lhfm[m]+30),
                 paste0("19 2.33 4 5 ", lhfm[m], " ", lhfm[m]+30),
                 paste0("CUSTOM_FUELS_FILE: D:\\GitHubProjects\\Greenstripping\\FB\\MTT\\inputs\\FuelModels\\", fmd[f], ".fmd"),
                 "WIND_SPEED: 37.8",
                 "WIND_DIRECTION: 180",
                 "GRIDDED_WINDS_GENERATE: No",
                 paste0("NUMBER_PROCESSORS: ", cpu_num),
                 "MTT_RESOLUTION: 50",
                 "MTT_SIM_TIME: 60",
                 "MTT_TRAVEL_PATH_INTERVAL: 100",
                 "MTT_SPOT_PROBABILITY: 0.0"), 
               file(paste0('./FB/MTT/inputs/MoistureScenarios/', 
                           substr(fmd[f], 1, 7),
                            '/LM=',
                           lhfm[m], 
                           "_",
                           fmd[f], 
                           '.input')) ) 
            }
  stopCluster(cl)
  Sys.time() - begin
}

# Create Cmd file 
# 1 strip
  inputs = list.files('./FB/MTT/inputs/MoistureScenarios/Strips1')
  strips = 1 
# 2 Strips 
  strips = 2
  inputs = list.files('./FB/MTT/inputs/MoistureScenarios/Strips2') 
  load("./FB/MTT/inputs/NonZeroScenariosR1.Rdata")
  nz.input <- NonZeroScenariosR1 %>% 
                mutate(scenario = str_replace(scenario, "Strips1",  "Strips2"), 
                       scenario = paste0(scenario, '.input'))
  inputs <- inputs[inputs %in% nz.input$scenario]
# 3 Strips 
  strips = 3
  inputs = list.files('./FB/MTT/inputs/MoistureScenarios/Strips3') 
  load("./FB/MTT/inputs/NonZeroScenariosR2.Rdata")
  nz.input <- NonZeroScenariosR2 %>% 
    mutate(scenario = str_replace(scenario, "Strips2",  "Strips3"), 
           scenario = paste0(scenario, '.input'))
  inputs <- inputs[inputs %in% nz.input$scenario]
# 4 Strips 
  strips = 4
  inputs = list.files('./FB/MTT/inputs/MoistureScenarios/Strips4') 
  load("./FB/MTT/inputs/NonZeroScenariosR3.Rdata")
  nz.input <- NonZeroScenariosR3 %>% 
                mutate(scenario2 = scenario) %>%
                separate(scenario2, c('LM', 'width', 'units', 
                                     'TL', 'FD', 'LF'), sep = '_') %>%
                filter(width == 'Strips3') %>%
                select(scenario) %>%
                mutate(scenario = str_replace(scenario, "Strips3",  "Strips4"), 
                       scenario = paste0(scenario, '.input')) 
  inputs <- inputs[inputs %in% nz.input$scenario]
# 5 Strips 
  strips = 5
  inputs = list.files('./FB/MTT/inputs/MoistureScenarios/Strips5') 
  load("./FB/MTT/inputs/NonZeroScenariosR4.Rdata")
  nz.input <- NonZeroScenariosR4 %>% 
    mutate(scenario2 = scenario) %>%
    separate(scenario2, c('LM', 'width', 'units', 
                          'TL', 'FD', 'LF'), sep = '_') %>%
    filter(width == 'Strips4') %>%
    select(scenario) %>%
    mutate(scenario = str_replace(scenario, "Strips4",  "Strips5"), 
           scenario = paste0(scenario, '.input')) 
  inputs <- inputs[inputs %in% nz.input$scenario]

for(i in 1:length(inputs)) {
  call = noquote(paste0('D:\\GitHubProjects\\Greenstripping\\FB\\MTT\\inputs\\greenstrip.lcp ', 
                        'D:\\GitHubProjects\\Greenstripping\\FB\\MTT\\inputs\\MoistureScenarios\\Strips', strips, '\\',
                        inputs[i],
                        ' D:\\GitHubProjects\\Greenstripping\\FB\\MTT\\inputs\\ignitions.shp 0',
                        ' S:\\DevanMcG\\FireModeling\\MTToutput\\',
                        tools::file_path_sans_ext(inputs[i]), 
                        '+ 1') )
  readr::write_lines(call, 
                     file(paste0('./FB/MTT/Strips', strips, 'BatchCmd.txt')), 
                     append = T)
  }
