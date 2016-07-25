library(dplyr)
trawlDat <- readRDS("data-generated/trawl-with-uwcig-sst.rds")
# trawlDat_orig <- readRDS("data-generated/rock-characteristics.rds")
# trawlDat$sst_trawl <- trawlDat_orig$temperature_surface
dir.create("figs", showWarnings = FALSE)

# Look at pred vs obs -- pretty good fit
dat <- trawlDat[trawlDat$year=="2012",] %>%
# dat <- trawlDat %>%
  select(temperature_bottom, year, month, floor_depth, sst, X, Y, 
    PID, POS, day) %>%
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

library(gbm)
m_gbm2 <- gbm(temperature_bottom ~ sst + as.numeric(month) + floor_depth +
  I(X/1000) + I(Y/1000), interaction.depth = 3, shrinkage = 0.01, n.trees = 3000,
  data = dat)
summary(m_gbm2)
pdf("figs/gbm-bottom.pdf", width = 9, height = 4)
par(mfrow=c(1,5))
sapply(seq_len(5), function(i) plot(m_gbm2, i))
dev.off()

m_gam1 <- mgcv::gam(temperature_bottom ~ s(sst) + 
  s(as.numeric(month), k = 4) + #s(I(floor_depth/100)) + 
    ti(I(X)) + ti(I(Y)) +
    # te(I(X/1000),I(Y/1000)),
    ti(I(X),I(Y)),
    # s(I(X/1000)) + s(I(Y/1000)),
  data = dat)

pdf("figs/gam-bottom.pdf", width = 6, height = 5)
mgcv::gam.check(m_gam1)
par(mfrow = c(2, 3))
plot(m_gam1)
dev.off()
dat$temp_bottom_gam <- predict(m_gam1)
AIC(m_gam1)
summary(m_gam1)

m_gamm1 <- mgcv::gamm(temperature_bottom ~ s(sst) + 
  s(as.numeric(month), k = 4) + s(I(floor_depth/100)) + 
    ti(I(X/1000)) + ti(I(Y/1000)) +
    # te(I(X/1000),I(Y/1000)),
    ti(I(X/1000),I(Y/1000)),
    random=list(year=~1),
    # s(I(X/1000)) + s(I(Y/1000)),
  data = dat)
pdf("figs/gamm-bottom.pdf", width = 6, height = 5)
mgcv::gam.check(m_gamm1$gam)
par(mfrow = c(2, 3))
plot(m_gamm1)
dev.off()
dat$temp_bottom_gam <- predict(m_gamm1$gam)

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
dat2$resid <- dat2$temperature_bottom - dat2$temp_bottom_gam
xlim <- range(dat2$X)
ylim <- range(dat2$Y)

common <- ggplot(convUL(nepacLL), aes(X, Y, group = PID)) + geom_polygon() +
  coord_equal() + xlim(xlim) + ylim(ylim)

p3 <- common +
  geom_point(data = dat2, aes(X, Y, colour = resid)) +
  scale_colour_gradient2()

p1 <- common +
  geom_point(data = dat2, aes(X, Y, colour = temperature_bottom)) +
  scale_color_viridis(guide = guide_legend(title = "bot.obs"))

p2 <- common +
  geom_point(data = dat2, aes(X, Y, colour = sst)) +
  scale_color_viridis()

p4 <- common +
  geom_point(data = dat2, aes(X, Y, colour = temp_bottom_gam)) +
  scale_color_viridis(guide = guide_legend(title = "bot.gam"))

pdf("figs/bottom-temp-residuals.pdf", width = 11, height = 5)
gridExtra::grid.arrange(p2, p1, p4, p3, nrow = 1)
dev.off()
