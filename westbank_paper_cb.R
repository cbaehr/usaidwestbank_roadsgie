
library(plotrix)

## run this script once you have obtained regression coefficients for the time-to-treatment model

out <- read.csv("/Users/christianbaehr/Desktop/example.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]
out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)
out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)
out$year <- c(-24:-1, 1:24)
out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)
out <- out[order(out$year),]
plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)
# dev.off()


#################################################################################



out <- read.csv("/Users/christianbaehr/Desktop/example.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]
out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)
out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)
out$year <- c(-24:-1, 1:24)
out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)
out <- out[order(out$year),]
plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)
# dev.off()





















###

## run this script once you have obtained regression coefficients for the time-to-treatment model

out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_1km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]


out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-24:-1, 1:24)

out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]


# png(file="/Users/christianbaehr/Desktop/eventstudy_1km.png")

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 1km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)

# dev.off()

###


out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_2km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]


out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-24:-1, 1:24)

out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]


# png(file="/Users/christianbaehr/Desktop/eventstudy_1km.png")

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 2km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)

###


out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_3km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]
out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)
out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)
out$year <- c(-24:-1, 1:24)
out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)
out <- out[order(out$year),]

# png(file="/Users/christianbaehr/Desktop/eventstudy_1km.png")

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 3km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)









#####################################################################
library(plotrix)

## run this script once you have obtained regression coefficients for the time-to-treatment model

out <- read.csv("/Users/christianbaehr/Desktop/example.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]


out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-23:24)


out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)

############################################################



out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_1km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]

out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-23:24)

out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 1km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)

###



out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_2km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]

out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-23:24)

out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 2km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)


###

out <- read.csv("/Users/christianbaehr/Desktop/eventstudy_3km.csv", stringsAsFactors = F)
out <- out[-1,]
out <- out[-49,]

out$ci1 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][1]})
out$ci1 <- as.numeric(out$ci1)

out$ci2 <- sapply(out$X.1, FUN = function(x) {strsplit(x, ",")[[1]][2]})
out$ci2 <- as.numeric(out$ci2)

out$year <- c(-23:24)

out[49, ] <- c(NA,
               mean(as.numeric(out$est1[out$year %in% c(-1, 1)])),
               NA,
               mean(out$ci1[out$year %in% c(-1, 1)]),
               mean(out$ci2[out$year %in% c(-1, 1)]),
               0)

out <- out[order(out$year),]

plot(out$year, out$est1, type='l', xlim = c(-24, 24), ylim = c(min(out$ci1), max(out$ci2)), col="blue", lwd=2, xaxt="n",
     xlab = "Months to/from treatment", ylab = "Treatment Effects on NTL",
     main = "Time to treatment, 3km bin", cex.lab=1.12)
# axis(side = 1, at = c(-9, -6, -3, 0, 3, 6, 9))
axis(side = 1, at = seq(-24, 24, 6))
# axis(side = 2, at = c())
lines(out$year, out$ci1, type='l', col="blue", lty=2, lwd=2)
lines(out$year, out$ci2, type='l', col="blue", lty=2, lwd=2)
abline(v=0, col = "black", lwd=3)
ablineclip(reg = lm(est1~year,data=out[out$year<=0,]),x2=0, col = "red", lwd=2)
ablineclip(reg = lm(est1~year,data=out[out$year>=0,]),x1=0, col = "red", lwd=2)





