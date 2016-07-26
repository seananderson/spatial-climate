bath <- marmap::getNOAA.bathy(
  lon1 = -130,
  lon2 = -115,
  lat1 = 25,
  lat2 = 55,
  resolution = 1,
  keep = TRUE)
# plot(bath)

# check against trawl data:
trawl <- readRDS("data-generated/rock-characteristics.rds")
trawl$noaa_bath <- -1 * marmap::get.depth(bath,
  dplyr::select(trawl,
    haul_longitude_decimal_degrees,
    haul_latitude_decimal_degrees),
  locator = FALSE)$depth

library(ggplot2)
ggplot(trawl, aes(haul_depth_meters, noaa_bath)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_fixed()

saveRDS(bath, file = "data-generated/noaa-bathymetry.rds")
