# Download data from World Oceans Atlas
# URL http://data.nodc.noaa.gov/woa/WOA13/

base_url <- "http://cses.washington.edu/rocinante/WRF/ECHAM5_A1B/sfc_vars/"
library(ncdf4)
library(RNetCDF)
library(dplyr)
library(PBSmapping)
# Unfortunately ncdf4 doesn't work with URLs, so seems like we have to
# 1. identify dates w/trawl survey (month / day / year)
# 2. download only relevant files + process

# Pull in trawl data to identify unique dates
trawlDat = read.csv("/users/eric.ward/dropbox/data for sean/_Eulachon Biomass w substrate bathymetry temperature DATA 2003 - 2012.csv")
trawlDates = unique(trawlDat$Trawl_date)
trawl.year = substr(trawlDates, 1, 4)
trawl.month = substr(trawlDates, 5, 6)
trawl.day = substr(trawlDates, 7, 8)

# Start with just one year for simplicity -- 2003
# download 

this.year = 2003
this.month = trawl.month[trawl.year==this.year][1]
this.day = trawl.day[trawl.year==this.year][1]
file.desc = paste0(this.year, "/", "wrfoutp_d02_",this.year,"-",this.month,"-",this.day,"_12:00:00")
fileName <- paste0(base_url, file.desc)
localName = paste0("/users/eric.ward/downloads/",file.desc)
download.file(url=fileName, destfile=localName)

# load with RNetCDF function
fid<-open.nc(localName)
dat<-read.nc(fid)
dat$SST = as.data.frame(dat$SST) - 273.15 # convert to C
# 162 x 123, on 12km grid
CEN_LAT = 5058170.29# 45.6427993774414
CEN_LON = 281932.21 #-119.798309326172

colnames(dat$SST) = CEN_LAT + seq(-61,61)*12000
rownames(dat$SST) = CEN_LON + seq(-81,80)*12000

xydata = expand.grid("X"=as.numeric(rownames(dat$SST)), "Y"=as.numeric(colnames(dat$SST)))
xydata$PID = 1
xydata$POS = seq(1,nrow(xydata))
attr(xydata, "zone") = 11
attr(xydata, "projection") = "UTM"

# convert grid to lat - lon
ll.grid = convUL(xydata, km=FALSE)

data(nepacLL)

plotMap(nepacLL, xlim=c(-140, -100), ylim=c(36, 55))
points(ll.grid$X, ll.grid$Y, cex=0.1, col="red")
# add trawl data
points(trawlDat$Lon_mid, trawlDat$Lat_mid, cex=0.1, col="blue")
