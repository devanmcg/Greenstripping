cpu_num = '4'
fmd <- list.files('./FB/MTT/inputs/FuelModels') %>%
  tools::file_path_sans_ext()
lhfm = seq(30, 120, length.out = 4)

writeLines(c("ShortTerm-Inputs-File-Version-1",
             "FUEL_MOISTURES_DATA: 7",
             "0 2.33 4 5 10 30",
             "14 2.33 4 5 10 30",
             paste0("15 2.33 4 5 ", lhfm[m], " 30"),
             paste0("16 2.33 4 5 ", lhfm[m], " 30"),
             paste0("17 2.33 4 5 ", lhfm[m], " 30"),
             paste0("18 2.33 4 5 ", lhfm[m], " 30"),
             paste0("19 2.33 4 5 ", lhfm[m], " 30"),
             paste0("CUSTOM_FUELS_FILE: C:\\Users\\devan.mcgranahan\\GithubProjects\\Greenstripping\\FB\\MTT\\inputs\\FuelModels\\", fmd[f], ".fmd"),
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
           file(paste0('./Greenstripping/FB/MTT/inputs/MoistureScenarios/LM=',
                       lhfm[m], 
                       "_",
                       fmd[f], 
                       '.txt')) ) 
