library(dplyr)
trawlDat <- readRDS("data-generated/trawl-with-uwcig-sst.rds")
names(trawlDat)

# Look at pred vs obs -- pretty good fit
dat <- trawlDat[trawlDat$year=="2003",] %>%
  select(temperature_bottom, month, floor_depth, sst, X, Y, PID, POS, day) %>%
  na.omit()
predicted.bottom_temp = lm(log(temperature_bottom) ~ 
  (log(sst) + as.numeric(month) + I(as.numeric(month)^2) + 
    floor_depth + I(floor_depth^2))^2, 
  data=dat)
plot(exp(predicted.bottom_temp$fitted.values), exp(predicted.bottom_temp$fitted.values + predicted.bottom_temp$residuals), xlab="Predicted", ylab="Observed")
abline(0,1, lwd=3, col="red")

dat$temp_bottom_lm <- predict(predicted.bottom_temp) %>% exp

library(caret)
# dat <- trawlDat
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 4),
                        n.trees = c(1000, 2000),
                        shrinkage = c(0.01),
                        n.minobsinnode = 10)
m_gbm1 <- train(temperature_bottom ~ sst + as.numeric(month) + floor_depth +
  X + Y,
  data = dat,
  tuneGrid = gbmGrid,
  method = "gbm", trControl = fitControl, verbose = FALSE)
print(m_gbm1)
dat$temp_bottom_gbm <- predict(m_gbm1)

m_gam1 <- mgcv::gam(temperature_bottom ~ s(sst) + 
  s(as.numeric(month), k = 4) + s(I(floor_depth/100)) + 
    ti(I(X/1000)) + ti(I(Y/1000)) +
    # te(I(X/1000),I(Y/1000)),
    ti(I(X/1000),I(Y/1000)),
    # s(I(X/1000)) + s(I(Y/1000)),
  data = dat)

mgcv::gam.check(m_gam1)
par(mfrow = c(1, 6))
plot(m_gam1)
dat$temp_bottom_gam <- predict(m_gam1)
AIC(m_gam1)
summary(m_gam1)

# m_gam1 <- gamclass::CVgam(log(temperature_bottom) ~ s(sst) + 
#   s(as.numeric(month), k = 3) + s(floor_depth),
#   data = dat)
# names(m_gam1)

m_lm1 <- train(log(temperature_bottom) ~ log(sst) + as.numeric(month) +
  I(as.numeric(month)^2) + floor_depth + I(floor_depth^2),
  data = dat,
  method = "lm", trControl = fitControl)
print(m_lm1)

# m_rf1 <- train(log(temperature_bottom) ~ sst + as.numeric(month) + floor_depth,
#   data = dat, method = "rf", trControl = fitControl)
# print(m_rf1)

# Plot map in UTM
library(PBSmapping)
library(ggplot2)
library(viridis)
data(nepacLL)
dat2 = dat
xlim <- range(dat2$X)
ylim <- range(dat2$Y)
ggplot(convUL(nepacLL), aes(X, Y, group = PID)) + geom_polygon() +
  geom_point(data = dat2, aes(X, Y, 
    colour = temperature_bottom - temp_bottom_gam)) +
    # colour = temperature_bottom - temp_bottom_gbm)) +
    # colour = temperature_bottom - temp_bottom_lm)) +
  # scale_color_viridis() + 
  scale_colour_gradient2() +
  coord_equal() + xlim(xlim) + ylim(ylim)
