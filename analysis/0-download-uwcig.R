# Import example ncdf file from UW CIG model projections
# and convert WRF model to proper coordinates

# Note: for precise mappign as possible, here's what Eric Salathe recommends: "you’ll get the 
# best result with Lambert Conformal Conic map centered on -120, 45.6 and Std Par 1 49.55 
# Std Par 2 41.450, Height 13"

base_url <- "http://cses.washington.edu/rocinante/WRF/ECHAM5_A1B/sfc_vars/"
library(RNetCDF)
library(dplyr)
library(PBSmapping)
library(sp)
library(rgdal)
library(mgcv)
library(akima)

# Create empty data folders if needed:
dir.create(file.path("..", "data-raw", "wrf"), showWarnings = FALSE)
sapply(1970:2069, function(x) {
  dir.create(file.path("..", "data-raw", "wrf", x), showWarnings = FALSE)
})

# Unfortunately ncdf4 doesn't work with URLs, so seems like we have to
# 1. identify dates w/trawl survey (month / day / year)
# 2. download only relevant files + process

# Pull in trawl data to identify unique dates
trawlDat = read.csv(paste0("/Users/", Sys.info()[["user"]], 
    "/Dropbox/data for sean/_Eulachon Biomass w substrate bathymetry ",
    "temperature DATA 2003 - 2012.csv"))
trawlDat$month = substr(trawlDat$Trawl_date, 5, 6)
trawlDat$day = substr(trawlDat$Trawl_date, 7, 8)
trawlDates = unique(trawlDat$Trawl_date)
trawl.year = substr(trawlDates, 1, 4)
trawl.month = substr(trawlDates, 5, 6)
trawl.day = substr(trawlDates, 7, 8)
trawlDat$X = trawlDat$Lon_mid
trawlDat$Y = trawlDat$Lat_mid
trawlDat$PID = 1
trawlDat$POS = seq(1,nrow(trawlDat))
attr(trawlDat,"zone")=7
attr(trawlDat,"projection")="LL"
trawlDat = convUL(trawlDat)

# Start with just one year for simplicity -- 2003
this.year = 2003
this.month = "07"#trawl.month[trawl.year==this.year][1]
this.day = "01"#trawl.day[trawl.year==this.year][1]
file.desc = paste0(this.year, "/", "wrfoutp_d02_",this.year,"-",this.month,"-",this.day,"_12:00:00")
fileName <- paste0(base_url, file.desc)
localName = file.path("..", "data-raw", file.desc)
download.file(url=fileName, destfile=localName)

# load with RNetCDF function
fid<-open.nc(localName)
dat<-read.nc(fid)
dat$SST = as.data.frame(dat$SST) - 273.15 # convert to C

# Read in the longitude latitude that was output from the
# NCAR IDV viewer. Projection attributes in lambert, with parameters
# shown in print.nc() command
#fid = open.nc("analysis/lambert_latlon.nc")
#lon_lambert = read.nc(fid)$x
#lat_lambert = read.nc(fid)$y
#print.nc(fid)

fid = open.nc("terrain_d02.nc")
print.nc(fid)

# grid of all possible values
xy.ll <- data.frame("X"=c(read.nc(fid)$XLONG), "Y"=c(read.nc(fid)$XLAT))

#crs <- CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=45.66558 +lon_0=-121 +datum=WGS84 +units=km")
#p <- SpatialPoints(xy, proj4string=crs)
#xy.ll <- as.data.frame(coordinates(spTransform(p, CRS("+proj=longlat +datum=WGS84"))))
#names(xy.ll) = c("X", "Y")
xy.ll$sst = c(as.matrix(dat$SST))
xy.ll$PID = 1
xy.ll$POS = seq(1,nrow(xy.ll))
attr(xy.ll, "zone") = 7
attr(xy.ll, "projection") = "LL"
xy.utm = convUL(xy.ll)  

### PLOTS 
data(nepacLL)
plotMap(nepacLL, xlim=c(-135, -105), ylim=c(36, 55))
points(xy.ll$X, xy.ll$Y, cex=0.2, col = rgb(0, 0, xy.ll$sst, alpha=xy.ll$sst, maxColorValue=25))
# add trawl data
points(trawlDat$Lon_mid, trawlDat$Lat_mid, cex=0.1, col=rgb(1,0,0,0.3))

# Plot same map in UTM
plotMap(convUL(nepacLL), xlim = range(xy.utm$X), ylim = range(xy.utm$Y))
points(xy.utm$X, xy.utm$Y, cex=0.2, col = rgb(0, 0, xy.ll$sst, alpha=xy.ll$sst, maxColorValue=25))

# Getting closer on projections -- something was just not right with the image() call below
plotMap(nepacLL, xlim=c(-135, -105), ylim=c(36, 55))
points(xy.ll$lon, xy.ll$lat, col = rgb(0, 0, xy.ll$sst, alpha=xy.ll$sst, maxColorValue=25), cex=0.2)
#image(xy.ll$lon[xy$Y == min(xy$Y)], xy.ll$lat[xy$X == min(xy$X)], as.matrix(dat$SST), add=T)
#for(i in 1:length(unique(nepacLL$PID))) {
#  polygon(x = nepacLL$X[nepacLL$PID == unique(nepacLL$PID)[i]], y = nepacLL$Y[nepacLL$PID == unique(nepacLL$PID)[i]])
#}

### Now we can try to match up temp interpolation for 2003 trawl data

get_wrf = function(this.year, this.month, this.day) {
  file.desc = paste0(this.year, "/", "wrfoutp_d02_",this.year,"-",this.month,"-",this.day,"_12:00:00")
  fileName <- paste0(base_url, file.desc)
  localName = paste0("../data-raw/",file.desc)
  download.file(url=fileName, destfile=localName)
  
  # load with RNetCDF function
  fid<-open.nc(localName)
  dat<-read.nc(fid)
  dat$SST = as.data.frame(dat$SST) - 273.15 # convert to C
  
  fid = open.nc("terrain_d02.nc")
  print.nc(fid)
  
  # grid of all possible values
  xy.ll <- data.frame("X"=c(read.nc(fid)$XLONG), "Y"=c(read.nc(fid)$XLAT))
  
  #crs <- CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=45.66558 +lon_0=-121 +datum=WGS84 +units=km")
  #p <- SpatialPoints(xy, proj4string=crs)
  #xy.ll <- as.data.frame(coordinates(spTransform(p, CRS("+proj=longlat +datum=WGS84"))))
  #names(xy.ll) = c("X", "Y")
  xy.ll$sst = c(as.matrix(dat$SST))
  xy.ll$PID = 1
  xy.ll$POS = seq(1,nrow(xy.ll))
  attr(xy.ll, "zone") = 7
  attr(xy.ll, "projection") = "LL"
  xy.utm = convUL(xy.ll)  
  return(list("UTM"=xy.utm, "SST"=dat$SST))
}

####################
# Interpolate SST at trawl data locations for 2003 -- create model to validate bottom temp - SST relationship
####################
attr(nepacLL, "zone")=7
nepacUTM = convUL(nepacLL)

trawlDat$sst = NA

# Cycle over unique month-day combinations
for(i in 1:length(trawl.month[trawl.year==this.year])) {

g = get_wrf(2003, trawl.month[trawl.year==this.year][i], trawl.day[trawl.year==this.year][i])
# get rid of points on land
#if(exists("pip")==FALSE) pip = point.in.polygon(g$UTM$X, g$UTM$Y, pol.x = nepacUTM$X[nepacUTM$PID==1], pol.y = nepacUTM$Y[nepacUTM$PID==1])
#g$UTM = g$UTM[pip==0 & g$UTM$X < 2200,]

# use simple 2D gam to do interpolation -- probably worth also looking into interp()
#gam.fit = gam(sst ~ s(X, Y), data = g$UTM) 
predict.loc = which(trawlDat$Year==2003 & trawlDat$month==trawl.month[trawl.year==this.year][i] & trawlDat$day==trawl.day[trawl.year==this.year][i])
#trawlDat$sst[predict.loc] = predict(gam.fit, newdata=trawlDat[predict.loc,])

trawlDat$sst[predict.loc] = diag(interp(g$UTM$X, g$UTM$Y, g$UTM$sst, xo = trawlDat$X[predict.loc], yo = trawlDat$Y[predict.loc])$z)
}

# Look at pred vs obs -- pretty good fit
predicted.bottom_temp = lm(log(G.temp.all) ~ log(sst) + as.numeric(month) +I(as.numeric(month)^2) + SRTM_depth_m + I(SRTM_depth_m^2), data=trawlDat[trawlDat$Year==2003,])
plot(exp(predicted.bottom_temp$fitted.values), exp(predicted.bottom_temp$fitted.values + predicted.bottom_temp$residuals), xlab="Predicted", ylab="Observed")
abline(0,1, lwd=3, col="red")

