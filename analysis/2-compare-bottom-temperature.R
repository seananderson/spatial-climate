library(dplyr)
library(viridis)
library(ggplot2)

trawl <- readRDS("../data-generated/rock-characteristics.rds") %>% 
  select(-starts_with("sebas")) %>% 
    rename(latitude = haul_latitude_decimal_degrees,
      longitude = haul_longitude_decimal_degrees)

atlas <- readRDS("../data-generated/woa.rds")

p <- ggplot(trawl, aes(longitude, latitude, color = temperature_bottom)) +
  geom_point() + scale_color_viridis()
print(p)

atlas <- atlas %>% group_by(latitude, longitude) %>% 
  filter(depth == max(depth)) %>% 
    as_data_frame() %>% 
      filter(depth < 1600, 
        longitude < max(trawl$longitude),
        longitude > min(trawl$longitude),
        latitude < max(trawl$latitude),
        latitude > min(trawl$latitude)
        )

p <- ggplot(atlas, aes(longitude, latitude, color = temperature)) +
  geom_point() + scale_color_viridis()
print(p)

# hist(atlas$depth)
# hist(trawl$haul_depth_meters)
