# Download data from World Oceans Atlas
# URL http://data.nodc.noaa.gov/woa/WOA13/

base_url <- "http://data.nodc.noaa.gov/woa/WOA13/DATAv2/temperature/csv/"
resolution <- "0.25/"
prefix <- "woa13_"
decades <- c("8594", "95A4", "A5B2")
suffix <- "_t15an04v2.csv.gz" # season 15 (July-September), objectively analyzed 
dir.create("../data-raw", showWarnings = FALSE)
dir.create("../data-generated", showWarnings = FALSE)
depths <- c(seq(0, 100, 5), seq(125, 500, 25), seq(550, 2000, 50), 
  seq(2100, 5500, 100))
column_names <- c(c("latitude", "longitude"), paste0("d", depths))
bounding_lat <- sort(c(30, 55))
bounding_long <- sort(c(-140, -122))

out <- lapply(seq_along(decades), function(i) {
  file_name <- paste0(prefix, decades[i], suffix)
  this_url <- paste0(base_url, decades[i], "/", resolution, file_name)
  destination <- paste0("../data-raw/", file_name)

  if (!file.exists(destination))
    download.file(this_url, destfile = destination)

  uncompressed_file <- substring(destination, 1, nchar(destination) - 3)
  if (!file.exists(uncompressed_file))
    system(paste("gzip -ndkf", destination))

  # pad the ragged csv:
  ncol <- length(column_names)
  system(paste0("gawk -F, -vOFS=, '{$", ncol, "=$", ncol, "}1' ", 
      uncompressed_file, " > temp.csv"))

  d_output <- readr::read_csv("temp.csv", comment = "#", col_names = FALSE)
  names(d_output) <- column_names
  unlink("temp.csv")

  # switch to long format:
  d_output <- tidyr::gather_(d_output, key_col = "depth", 
    value_col = "temperature", gather_cols = column_names[-c(1:2)])

  d_output$depth <- as.numeric(substring(d_output$depth, 2))
  d_output <- dplyr::filter(d_output, !is.na(temperature))

  d_output <- dplyr::filter(d_output, 
    longitude >= bounding_long[1] & longitude <= bounding_long[2],
    latitude >= bounding_lat[1] & latitude <= bounding_lat[2])
  d_output$temperature <- as.numeric(d_output$temperature)

  d_output
})

out <- dplyr::bind_rows(out)
saveRDS(out, file = file.path("..", "data-generated", "woa.rds"))
