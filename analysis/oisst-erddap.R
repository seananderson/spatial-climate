# devtools::install_github("ropensci/rerddap")
# install.packages("rerddap")

library(dplyr)
rerddap::ed_search(query = "oisst") %>% .$info

# SST, Daily Optimum Interpolation (OI), AVHRR Only, 
# Version 2, Final+Preliminary
(out <- rerddap::info("ncdcOisst2Agg"))

(res <- rerddap::griddap(out,
  time = c("2016-09-19T00:00:00Z", "2016-09-19T00:00:01Z"),
  latitude = c(-89.875, 89.875),
  longitude = c(0.125, 359.875),
  fields = "sst"
))
d <- res$data
unique(d$time)

library(ggplot2)
ggplot(d, aes(lon, lat, fill = sst)) + 
  geom_raster()
