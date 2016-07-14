library(dplyr)
library(viridis)
library(ggplot2)

trawl <- readRDS("../data-generated/rock-characteristics.rds") %>% 
  select(-starts_with("sebas")) %>% 
  rename(latitude = haul_latitude_decimal_degrees,
    longitude = haul_longitude_decimal_degrees)

atlas <- readRDS("../data-generated/woa.rds")


atlas <- atlas %>% group_by(latitude, longitude) %>% 
  filter(depth == max(depth)) %>% 
    as_data_frame() %>% 
  filter(depth < 1600, 
    longitude <= max(trawl$longitude),
    longitude >= min(trawl$longitude),
    latitude <= max(trawl$latitude),
    latitude >= min(trawl$latitude))  %>% 
  rename(temperature_bottom = temperature)

p1 <- ggplot(trawl, aes(longitude, latitude, color = temperature_bottom)) +
  geom_point() + scale_color_viridis()
p2 <- ggplot(atlas, aes(longitude, latitude, color = temperature_bottom)) +
  geom_point() + scale_color_viridis()
gridExtra::grid.arrange(p1, p2)

res <- min(diff(sort(unique(atlas$latitude))))
atlas_lat <- seq(min(atlas$latitude), max(atlas$latitude), res)
atlas_lon <- seq(min(atlas$longitude), max(atlas$longitude), res)

trawl <- mutate(trawl, longitude = atlas_lon[findInterval(longitude, atlas_lon, all.inside = TRUE)])
trawl <- mutate(trawl, latitude = atlas_lat[findInterval(latitude, atlas_lat, all.inside = TRUE)])

trawl_bin <- group_by(trawl, longitude, latitude) %>% 
  summarize(mean_temperature = mean(temperature_bottom, na.rm = TRUE),
    mean_haul_depth = mean(haul_depth_meters, na.rm = TRUE),
    mean_bottom_depth = mean(floor_depth, na.rm = TRUE)) %>% 
  as_data_frame()

trawl_bin <- inner_join(trawl_bin, atlas)

p1 <- ggplot(trawl_bin, aes(longitude, latitude, color = mean_haul_depth - mean_bottom_depth)) +
  geom_point() + scale_colour_gradient2()
print(p1)
