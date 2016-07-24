trawlDat <- readRDS("data-generated/trawl-with-uwcig-sst.rds")

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
