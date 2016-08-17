library(dplyr)
fram_characteristics <- readr::read_csv(file.path("data-raw", 
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

rock_characteristics <- readxl::read_excel(file.path("data-raw",
    "RockfishHaulCatchIndivData2003To2014_20150826Fnl.xlsx"),
  sheet = 2, skip = 8)

rock_characteristics <- rename(rock_characteristics,
  haul_id = `Haul Identifier`)

names(rock_characteristics) <- tolower(names(rock_characteristics))
names(rock_characteristics) <- gsub(" ", "_", names(rock_characteristics))
names(rock_characteristics) <- gsub("\\(", "", names(rock_characteristics))
names(rock_characteristics) <- gsub("\\)", "", names(rock_characteristics))

rock_characteristics <- left_join(rock_characteristics,
  fram_characteristics, by = "haul_id")

# number of observations missing bottom temperature:
sum(is.na(rock_characteristics$temperature_bottom)) %>% message

saveRDS(rock_characteristics, file = file.path("data-generated",
    "rock-characteristics.rds"))

trawl <- rock_characteristics %>% 
  select(-starts_with("sebas"), -cancer_magister) %>% 
  rename(latitude = haul_latitude_decimal_degrees,
    longitude = haul_longitude_decimal_degrees) %>% 
  as.data.frame()

saveRDS(trawl, "data-generated/trawl.rds")

readr::write_csv(trawl, path = "data-generated/trawl.csv")