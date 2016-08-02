bath <- marmap::getNOAA.bathy(
  lon1 = -130,
  lon2 = -115,
  lat1 = 25,
  lat2 = 55,
  resolution = 1,
  keep = TRUE)
# plot(bath)
saveRDS(bath, file = "data-generated/noaa-bathymetry.rds")

bath2 <- marmap::readGEBCO.bathy(
  file = "data-raw/gebco/GEBCO_2014_2D_-136.0_36.0_-105.0_54.0.nc", 
  resolution = 1)
saveRDS(bath2, file = "data-generated/gebco-bathymetry.rds")

# check against trawl data:
trawl <- readRDS("data-generated/rock-characteristics.rds")
trawl$noaa_bath <- -1 * marmap::get.depth(bath,
  dplyr::select(trawl,
    haul_longitude_decimal_degrees,
    haul_latitude_decimal_degrees),
  locator = FALSE)$depth

trawl$gebco_bath <- -1 * marmap::get.depth(bath2,
  dplyr::select(trawl,
    haul_longitude_decimal_degrees,
    haul_latitude_decimal_degrees),
  locator = FALSE)$depth

library(ggplot2)

########## noaa
p1 <- ggplot(trawl, aes(haul_depth_meters, noaa_bath)) + geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_fixed()

p2 <- ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
  colour = haul_depth_meters - noaa_bath)) + geom_point(alpha=0.5) +
  scale_color_gradient2()

trawl$diff_abs <- abs(trawl$haul_depth_meters - trawl$noaa_bath)
p3 <- ggplot(filter(trawl, diff_abs > 150), 
  aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
  colour = haul_depth_meters - noaa_bath)) + geom_point(alpha=0.7) +
  scale_color_gradient2()

pdf("figs/check-depth-trawl-noaa.pdf", width = 6, height = 12)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

########## gebco
p1 <- ggplot(trawl, aes(haul_depth_meters, gebco_bath)) + geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_fixed()

p2 <- ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
  colour = haul_depth_meters - gebco_bath)) + geom_point(alpha=0.5) +
  scale_color_gradient2()

trawl$diff_abs <- abs(trawl$haul_depth_meters - trawl$gebco_bath)
p3 <- ggplot(filter(trawl, diff_abs > 150), 
  aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
    colour = haul_depth_meters - gebco_bath)) + geom_point(alpha=0.7) +
  scale_color_gradient2()

pdf("figs/check-depth-trawl-gebco.pdf", width = 6, height = 12)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

############## ensemble
trawl$ens_bath <- (trawl$gebco_bath + trawl$noaa_bath)/2
p1 <- ggplot(trawl, aes(haul_depth_meters, ens_bath)) + geom_point(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  coord_fixed()

p2 <- ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
  colour = haul_depth_meters - ens_bath)) + geom_point(alpha=0.5) +
  scale_color_gradient2()

trawl$diff_abs <- abs(trawl$haul_depth_meters - trawl$ens_bath)
p3 <- ggplot(filter(trawl, diff_abs > 150), 
  aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees, 
    colour = haul_depth_meters - ens_bath)) + geom_point(alpha=0.7) +
  scale_color_gradient2()

pdf("figs/check-depth-trawl-ensemble.pdf", width = 6, height = 12)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()
