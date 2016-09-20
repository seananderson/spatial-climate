# devtools::install_github("ropensci/rerddap")
# install.packages("rerddap")

library(dplyr)

rerddap::ed_search(query = "bloom") %>% .$info

(out <- rerddap::info("osuBloomsModisChla"))
(res <- rerddap::griddap(out,
  time = c("2015-06-21T00:00:00Z", "2015-06-21T00:00:01Z")))
library(ggplot2)
d <- res$data
filter(d, prc_chla < 300) %>%
  ggplot(aes(lon, lat, fill = prc_chla)) + 
  geom_raster() +
  scale_fill_gradient2()