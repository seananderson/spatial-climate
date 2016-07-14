library(dplyr)
fram_characteristics <- readr::read_csv(file.path("..", "data-raw", 
    "fram-characteristics.csv"))
names(fram_characteristics) <- tolower(names(fram_characteristics))
names(fram_characteristics) <- gsub(" ", "_", names(fram_characteristics))
names(fram_characteristics) <- gsub("\\(", "", names(fram_characteristics))
names(fram_characteristics) <- gsub("\\)", "", names(fram_characteristics))
fram_characteristics <- select(fram_characteristics,
  haul_id, temperate_at_gear_c, temperature_at_surface_c,
  sea_floor_depth_m) %>% 
    rename(temperature_bottom = temperate_at_gear_c, 
      temperature_surface = temperature_at_surface_c, 
      floor_depth = sea_floor_depth_m) %>% 
    mutate(temperature_bottom = as.numeric(temperature_bottom),
      temperature_surface = as.numeric(temperature_surface),
      floor_depth = as.numeric(floor_depth))

rock_characteristics <- readxl::read_excel(file.path("..", "data-raw",
    "RockfishHaulCatchIndivData2003To2014_20150826Fnl.xlsx"),
  sheet = 2, skip = 8)

