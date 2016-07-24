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
dir.create(file.path("data-raw", "wrf"), showWarnings = FALSE)
sapply(1970:2069, function(x) {
  dir.create(file.path("data-raw", "wrf", x), showWarnings = FALSE)
})

# Unfortunately ncdf4 doesn't work with URLs, so seems like we have to
# 1. identify dates w/trawl survey (month / day / year)
# 2. download only relevant files + process

# Pull in trawl data to identify unique dates
trawlDat <- readRDS("data-generated/rock-characteristics.rds")
trawlDat$month = substr(trawlDat$trawl_date, 6, 7)
trawlDat$year = substr(trawlDat$trawl_date, 1, 4)
trawlDat$day = substr(trawlDat$trawl_date, 9, 10)
trawlDates = unique(trawlDat$trawl_date)
trawl.year = substr(trawlDates, 1, 4)
trawl.month = substr(trawlDates, 6, 7)
trawl.day = substr(trawlDates, 9, 10)
trawlDat$X = trawlDat$haul_longitude_decimal_degrees
trawlDat$Y = trawlDat$haul_latitude_decimal_degrees
trawlDat$PID = 1
trawlDat$POS = seq(1,nrow(trawlDat))
attr(trawlDat,"zone")=7
attr(trawlDat,"projection")="LL"
trawlDat = convUL(trawlDat)
# Start with just one year for simplicity -- 2003
this.year = 2003
this.month = trawl.month[trawl.year==this.year][1]
this.day = trawl.day[trawl.year==this.year][1]
file.desc = paste0(this.year, "/", "wrfoutp_d02_",this.year,"-",
  this.month,"-",this.day,"_12:00:00")
fileName <- paste0(base_url, file.desc)
localName = file.path("data-raw", "wrf", file.desc)
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

fid = open.nc("data-raw/terrain_d02.nc")
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
points(trawlDat$haul_longitude_decimal_degrees,
  trawlDat$haul_latitude_decimal_degrees, cex=0.1, col=rgb(1,0,0,0.3))

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
  localName = paste0("data-raw/", "wrf/", file.desc)
  if (!file.exists(localName)) {
    Sys.sleep(2)
    download.file(url=fileName, destfile=localName)
  }
  
  # load with RNetCDF function
  fid<-open.nc(localName)
  dat<-read.nc(fid)
  dat$SST = as.data.frame(dat$SST) - 273.15 # convert to C
  
  fid = open.nc("data-raw/terrain_d02.nc")
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

# was getting errors with R version, so:
unique.trawl.year <- unique(trawl.year)
sapply(unique.trawl.year, function(y) {
  Sys.sleep(200)
  oldwd <- getwd()
  setwd(paste0("data-raw/wrf/", y))
  system(paste0(
      "wget --user-agent=Mozilla --no-directories ",
      "--wait=2 --accept='*12:00:00*' -r -l 1 ", 
      "http://cses.washington.edu/rocinante/WRF/ECHAM5_A1B/sfc_vars/", y, "/"))
  setwd(oldwd)
})

# Cycle over unique year-month-day combinations
g <- list()
for(j in seq_along(unique.trawl.year)) {
  g[[j]] <- list()
  for(i in 1:length(trawl.month[trawl.year==unique.trawl.year[j]])) {
    g[[j]][[i]] = get_wrf(
      unique.trawl.year[j], 
      trawl.month[trawl.year==unique.trawl.year[j]][i], 
      trawl.day[trawl.year==unique.trawl.year[j]][i])
    # get rid of points on land
    #if(exists("pip")==FALSE) pip = point.in.polygon(g$UTM$X, g$UTM$Y, pol.x = nepacUTM$X[nepacUTM$PID==1], pol.y = nepacUTM$Y[nepacUTM$PID==1])
    #g$UTM = g$UTM[pip==0 & g$UTM$X < 2200,]
  }
}
  
for(i in 1:length(trawl.month[trawl.year==this.year])) {
  # use simple 2D gam to do interpolation -- probably worth also looking into interp()
  #gam.fit = gam(sst ~ s(X, Y), data = g$UTM) 
  message(i)
  predict.loc = which(trawlDat$year==2003 & 
    trawlDat$month==trawl.month[trawl.year==this.year][i] & 
    trawlDat$day==trawl.day[trawl.year==this.year][i])
  #trawlDat$sst[predict.loc] = predict(gam.fit, newdata=trawlDat[predict.loc,])
  
  trawlDat$sst[predict.loc] = diag(interp(g[[i]]$UTM$X, g[[i]]$UTM$Y, g[[i]]$UTM$sst, 
      xo = trawlDat$X[predict.loc], yo = trawlDat$Y[predict.loc])$z)
}

saveRDS(trawlDat, file = "data-generated/trawl-with-uwcig-sst.rds")

# Look at pred vs obs -- pretty good fit
predicted.bottom_temp = lm(log(temperature_bottom) ~ log(sst) + as.numeric(month) +I(as.numeric(month)^2) + floor_depth + I(floor_depth^2), data=trawlDat[trawlDat$year==2003,])
plot(exp(predicted.bottom_temp$fitted.values), exp(predicted.bottom_temp$fitted.values + predicted.bottom_temp$residuals), xlab="Predicted", ylab="Observed")
abline(0,1, lwd=3, col="red")

library(caret)
dat <- trawlDat[trawlDat$year==2003]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 20)
m_gbm1 <- train(log(temperature_bottom) ~ sst + as.numeric(month) + floor_depth,
  data = dat,
  method = "gbm", trControl = fitControl, verbose = FALSE)
print(m_gbm1)

m_gam1 <- gam(log(temperature_bottom) ~ s(sst) + 
  s(as.numeric(month), k = 4) + s(floor_depth),
  data = dat)
par(mfrow = c(1, 3))
plot(m_gam1)

# m_gam1 <- gamclass::CVgam(log(temperature_bottom) ~ s(sst) + 
#   s(as.numeric(month), k = 3) + s(floor_depth),
#   data = dat)
# names(m_gam1)

m_lm1 <- train(log(temperature_bottom) ~ log(sst) + as.numeric(month) +
  I(as.numeric(month)^2) + floor_depth + I(floor_depth^2),
  data = dat,
  method = "lm", trControl = fitControl)
print(m_lm1)
