pacman::p_load(tidyverse, foreach, doSNOW)

# Batch-create .input files
{
  begin = Sys.time()
  cores = parallel::detectCores()
  cl <- makeSOCKcluster(cores) 
  registerDoSNOW(cl)
  clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  cpu_num = cores
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
                 paste0("15 2.33 4 5 ", lhfm[m], " ", lhfm[m]-5),
                 paste0("16 2.33 4 5 ", lhfm[m], " ", lhfm[m]-5),
                 paste0("17 2.33 4 5 ", lhfm[m], " ", lhfm[m]-5),
                 paste0("18 2.33 4 5 ", lhfm[m], " ", lhfm[m]-5),
                 paste0("19 2.33 4 5 ", lhfm[m], " ", lhfm[m]-5),
                 paste0("CUSTOM_FUELS_FILE: D:\\GitHubProjects\\Greenstripping\\FB\\MTT\\inputs\\FuelModels\\", fmd[f], ".fmd"),
                 "WIND_SPEED: 37.8",
                 "WIND_DIRECTION: 180",
                 "GRIDDED_WINDS_GENERATE: No",
                 paste0("NUMBER_PROCESSORS: ", cpu_num),
                 "MTT_RESOLUTION: 50",
                 "MTT_SIM_TIME: 60",
                 "MTT_TRAVEL_PATH_INTERVAL: 100",
                 "MTT_SPOT_PROBABILITY: 0.0",
                 "BURNPROBABILITY:",
                 "FLAMELENGTH:",
                 "SPREADRATE:",
                 "INTENSITY:" ), 
               file(paste0('./FB/MTT/inputs/MoistureScenarios/LM=',
                           lhfm[m], 
                           "_",
                           fmd[f], 
                           '.input')) ) 
            }
  stopCluster(cl)
  Sys.time() - begin
}

# Create Cmd file 

inputs = list.files('./FB/MTT/inputs/MoistureScenarios') 

for(i in 1:length(inputs)) {
  call = noquote(paste0('.\\MTT\\inputs\\greenstrip.lcp ', 
                        '.\\MTT\\inputs\\MoistureScenarios\\',
                        inputs[i],
                        ' .\\MTT\\inputs\\ignitions.shp 0',
                        ' .\\MTT\\outputs\\',
                        tools::file_path_sans_ext(inputs[i]), 
                        '+ 1') )
  readr::write_lines(call, 
                     file('./FB/MTT/BatchCmd.txt'), 
                     append = T)
  }
