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

# Read in the longitude latitude that was output from the
# NCAR IDV viewer. Projection attributes in lambert, with parameters
# shown in print.nc() command
fid = open.nc("analysis/lambert_latlon.nc")
lon_lambert = read.nc(fid)$x
lat_lambert = read.nc(fid)$y
print.nc(fid)

# convert 
xy <- expand.grid("X"=lon_lambert, "Y"=lat_lambert)
library(sp)
library(rgdal)
crs <- CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=45.66558 +lon_0=-121 +datum=WGS84 +units=km")
p <- SpatialPoints(xy, proj4string=crs)
xy.ll <- as.data.frame(coordinates(spTransform(p, CRS("+proj=longlat +datum=WGS84"))))
names(xy.ll) = c("lon", "lat")
xy.ll$sst = c(as.matrix(dat$SST))
  
data(nepacLL)

plotMap(nepacLL, xlim=c(-135, -105), ylim=c(36, 55))
points(xy.ll$lon, xy.ll$lat, cex=0.1, col="red")
# add trawl data
points(trawlDat$Lon_mid, trawlDat$Lat_mid, cex=0.1, col="blue")

# Getting closer on projections
plotMap(nepacLL, xlim=c(-135, -105), ylim=c(36, 55))
image(xy.ll$lon[xy$Y == min(xy$Y)], xy.ll$lat[xy$X == min(xy$X)], as.matrix(dat$SST), add=T)
for(i in 1:length(unique(nepacLL$PID))) {
  polygon(x = nepacLL$X[nepacLL$PID == unique(nepacLL$PID)[i]], y = nepacLL$Y[nepacLL$PID == unique(nepacLL$PID)[i]])
}