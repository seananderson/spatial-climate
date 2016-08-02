library(RNetCDF)
library(dplyr)
file <- "thetao_Omon_HadGEM2-ES_rcp45_r1i1p1_200512-201511.nc"
fid <- open.nc(file.path("data-raw", file))
print.nc(fid)
d <- read.nc(fid)
d$lon <- d$lon - 360
# image(d$lon, d$lat, d$thetao[,,2,1])
lat_i <- which(d$lat > 28 & d$lat < 52)
lon_i <- which(d$lon > -132 & d$lon < -112)
temp <- d$thetao
dim(temp)
temp <- temp[lon_i, lat_i, , ]
dim(temp)
temp <- temp - 273.15
# dims: lon, lat, depth, time
temp <- apply(temp, c(1, 2, 4), function(x) {
   x_nna <- !is.na(x)
   if (sum(x_nna) > 0) {
     depth_i <- max(which(x_nna))
     x[depth_i]
   } else {
     NA
   }
})

te <- reshape2::melt(temp)
names(te) <- c("lon", "lat", "time", "temp")
te$lon <- d$lon[lon_i[te$lon]]
te$lat <- d$lat[lat_i[te$lat]]
times <- as.Date(d$time, origin = "1859-12-01")
te$time <- times[te$time]
library(ggplot2)
filter(te, grepl("-05-", as.character(time))) %>%
  mutate(time_ = time) %>%
  na.omit() %>%
  ggplot(aes(lon, lat, colour = temp)) +
    geom_point() + facet_wrap(~time_)

class(lubridate::ymd(te$time))

trawl <- readRDS("data-generated/rock-characteristics.rds")
trawl$trawl_date <- lubridate::ymd(trawl$trawl_date)

unique_cmip5_date <- unique(te$time)
ids <- sapply(trawl$trawl_date, function(x) 
    which.min(abs(x - unique_cmip5_date)))
trawl$trawl_date_cmip5 <- unique_cmip5_date[ids]

interp_cmip <- function(trawl_dat) {
  cmip_dat <- filter(te, time == trawl_dat$trawl_date_cmip5[1]) %>% na.omit()
  ii <- apply(trawl_dat, 1, function(x) {
    diag(akima::interp(cmip_dat$lon, cmip_dat$lat, cmip_dat$temp, 
      xo = x["haul_longitude_decimal_degrees"], 
      yo = x["haul_latitude_decimal_degrees"])$z)})
  data.frame(trawl_dat, temp_cmip = ii)
}

trawl <- trawl %>% plyr::ddply("trawl_date_cmip5", function(x) {interp_cmip(x)})
trawl$year = substr(trawl$trawl_date, 1, 4)

pdf("figs/HadGEM2-trawl-diff.pdf", width = 10, height = 10)
library(ggplot2)
filter(te, grepl("-06-", as.character(time))) %>%
  mutate(time_ = time) %>%
  na.omit() %>%
  ggplot(aes(lon, lat, colour = temp)) +
    geom_point() + facet_wrap(~time_)

ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees,
  colour = temp_cmip)) +
  geom_point() + facet_wrap(~year) + scale_colour_gradient(limits=c(2, 15))

ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees,
  colour = temperature_bottom)) +
  geom_point() + facet_wrap(~year)  + scale_colour_gradient(limits=c(2, 15))

ggplot(trawl, aes(haul_longitude_decimal_degrees, haul_latitude_decimal_degrees,
  colour = temp_cmip - temperature_bottom)) +
  geom_point() + facet_wrap(~year) + scale_color_gradient2()
dev.off()
