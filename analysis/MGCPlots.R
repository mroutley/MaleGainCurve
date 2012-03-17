# Code for "Quantifying the male gain curve" seminar
# Generates example & empirical gain curves
graphics.off()
rm(list=ls())
# b = 1 linear
# b > 1 accelerating
# b < 1 decelerating
allocation <- seq(0.01, 1, 0.01)
curve <- function(a, b) {a*allocation^b}
lineWeight <- 5
plotCurve <- function(male, female) {
	maleCurve <- curve(male[1], male[2])
	maleCurve <- maleCurve/max(maleCurve)
	femaleCurve <- 1-curve(female[1], female[2])
	femaleCurve <- femaleCurve/max(femaleCurve)
	totalCurve <- maleCurve + femaleCurve
	totalCurve <- totalCurve/max(totalCurve)
	quartz(width=10, height=8, family=("Optima-Regular"), pointsize=18)
	plot(allocation, totalCurve, axes=FALSE, xlab="Allocation to male function", ylab="Fitness", type="l", col="orange", lwd=lineWeight, ylim=range(0, max(totalCurve)))
	axis(1, labels = c("low","","","","","high"))
	axis(2, las=1, labels = c("low","","","","","high"))
	lines(allocation, femaleCurve, col="red", lwd=lineWeight)
	lines(allocation, maleCurve, col="blue", lwd=lineWeight)
}
# Hermaphrodites
male <- c(1, 0.4)
female <- c(1, 1.4)
plotCurve(male, female)
# Dioecy
male <- c(1, 1.4)
female <- c(1, 0.4)
plotCurve(male, female)
# Empirical assuming linear female curve
# empiricalMale_linearFemale.pdf
male <- c(0.016, -0.310257)
female <- c(1, 1)
plotCurve(male, female)
# Single curves
singleCurve <-function(curveValues) {
	plotValues <- curve(curveValues[1], curveValues[2])
	quartz(width=10, height=8, family=("Optima-Regular"), pointsize=18)
	plot(allocation, plotValues, axes=FALSE, xlab="Allocation to function", ylab="Fitness", type="l", col="orange", lwd=lineWeight, ylim=range(0, max(curveValues)))
	axis(1, labels = c("low","","","high"))
	axis(2, las=1, labels = c("low","","","high"))
}
#Linear
curveValues <-c(1, 1)
singleCurve(curveValues)
#Decelerating
curveValues <-c(1, 0.5)
singleCurve(curveValues)
#Acelerating
curveValues <-c(1, 1.5)
singleCurve(curveValues)
#Family of curves
linear <- curve(1,1)
acc1 <- curve(1, 1.2)
acc2 <- curve(1, 1.4)
dec1 <- curve(1, 0.8)
dec2 <- curve(1, 0.6)
quartz(width=10, height=8, family=("Optima-Regular"), pointsize=18)
plot(allocation, linear, axes=FALSE, xlab="Allocation to function", ylab="Fitness", type="l", col="orange", lwd=lineWeight, ylim=range(0, max(linear)))
axis(1, labels = c("low","","","","","high"))
axis(2, las=1, labels = c("low","","","","","high"))
lines(allocation, acc1, col="blue", lwd=lineWeight)
lines(allocation, acc2, col="blue", lwd=lineWeight)
lines(allocation, dec1, col="red", lwd=lineWeight)
lines(allocation, dec2, col="red", lwd=lineWeight)

linear <- curve(1,1)
acc1 <- curve(1.2, 1.2)
acc2 <- curve(1.4, 1.4)
dec1 <- curve(0.8, 0.8)
dec2 <- curve(0.6, 0.6)
quartz(width=10, height=8, family=("Optima-Regular"), pointsize=18)
plot(allocation, linear, axes=FALSE, xlab="Allocation to function", ylab="Fitness", type="l", col="orange", lwd=lineWeight, ylim=range(0, max(linear)))
axis(1, labels = c("low","","","","","high"))
axis(2, las=1, labels = c("low","","","","","high"))
lines(allocation, acc1, col="blue", lwd=lineWeight)
lines(allocation, acc2, col="blue", lwd=lineWeight)
lines(allocation, dec1, col="red", lwd=lineWeight)
lines(allocation, dec2, col="red", lwd=lineWeight)
