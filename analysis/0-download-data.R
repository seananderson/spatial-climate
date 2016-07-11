# Download data from World Oceans Atlas
# URL http://data.nodc.noaa.gov/woa/WOA13/

base_url <- "http://data.nodc.noaa.gov/woa/WOA13/DATAv2/temperature/csv/"
resolution <- "0.25/"
prefix <- "woa13_"
decades <- c("8594", "95a4", "A5B2")
suffix <- "_t15an04v2.csv.gz" # season 15 (July-September), objectively analyzed 
dir.create("../data-raw", showWarnings = FALSE)

lapply(1:3, function(i) {
  file_name <- paste0(prefix, decades[i], suffix)
  this_url <- paste0(base_url, decades[i], "/", resolution, file_name)
  destination <- paste0("../data-raw/", file_name)
  if (!file.exists(destination))
    download.file(this_url, destfile = destination)
})
