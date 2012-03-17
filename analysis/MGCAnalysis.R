# ==========
# = Title: Analysis of the canola male gain curve data =
# = Author: Matthew Routley - matthew.routley@gmail.com =
# ==========
# =================
# = Global values =
# =================
library(MASS)
library(RSQLite)
library(lattice)
base <- "/Users/mroutley/Documents/MaleGainCurve/" # Change as appropriate
averagePollenProduction <- 13000
# ===================
# = Gain curve data =
# ===================
# ======================================
# = Connect to server and extract data =
# ======================================
driver <- dbDriver("SQLite")
connection <- dbConnect(driver, dbname = paste(base, "db/MGCData.db", sep=""))
pollenReceipt <- dbGetQuery(connection, statement = "SELECT flower.array_id, array.anther_number, array.flower_number, flower.plant_number, array.replicate, flower.position, flower.stigma_pollen FROM flower, array WHERE flower.array_id = array.array_id")
# seedSet <-  dbGetQuery(connection, statement = "SELECT flower.array_id, array.anther_number, array.flower_number, flower.plant_number, array.replicate, flower.position, flower.seed_set FROM flower, array WHERE flower.array_id = array.array_id")
pollenRemaining <- dbGetQuery(connection, statement = "SELECT pollenRemaining.array_id, array.anther_number, array.flower_number, pollenRemaining.plant_number, array.replicate, pollenRemaining.anther_pollen FROM pollenRemaining, array WHERE pollenRemaining.array_id = array.array_id")
pollinators <- dbGetQuery(connection, statement = "SELECT pollinators.array_id, array.anther_number, array.flower_number, pollinators.plant_number, array.replicate, pollinators.flower_visits FROM pollinators, array WHERE pollinators.array_id = array.array_id")
selfing <- dbGetQuery(connection, statement = "SELECT selfing.array_id, array.anther_number, array.flower_number, selfing.gus_pollen FROM array, selfing WHERE array.array_id = selfing.array_id")
seedSet <- dbGetQuery(connection, statement = "SELECT seedSet.array_id, array.anther_number, array.flower_number, seedSet.seeds, seedSet.aborted, seedSet.ovules FROM array, seedSet WHERE array.array_id = seedSet.array_id")
dbDisconnect(connection)
dbUnloadDriver(driver)
rm(driver)
rm(connection)
# ==================
# = Summarize data
# ==================
gaincurve <- merge(pollenReceipt, pollenRemaining, all.x=TRUE, sort=TRUE)
gaincurve <- merge(gaincurve, pollinators, all.x=TRUE, sort=TRUE)
seedSet$propSeed <- seedSet$seeds / (seedSet$seeds + seedSet$aborted + seedSet$ovules)
seedSet <- aggregate(seedSet[,c(7)], by=list(array=seedSet$array_id, anthers=seedSet$anther_number, flowers=seedSet$flower_number), mean, na.rm = TRUE)
names(seedSet)[4] <- "propSeed"
# gaincurve <- merge(gaincurve, seedSet, all.x=TRUE, sort=TRUE)
# merged.data <- merge(merged.data, selfing, all.x=TRUE, sort=TRUE)
rm(pollenReceipt, pollenRemaining, pollinators)
# Modify classes of variables
gaincurve <- transform(gaincurve, array_id = as.factor(gaincurve$array_id), plant_number = as.factor(gaincurve$plant_number), anther_number = as.integer(gaincurve$anther_number), flower_number = as.integer(gaincurve$flower_number), position = as.factor(gaincurve$position))
gaincurve$anther_pollen[gaincurve$position == "top"] <- NA
gaincurve$flower_visits[gaincurve$position == "top"] <- NA
attach(gaincurve)
# Summarize values
stigmaPollen <- aggregate(gaincurve[,c(7)], by=list(array=array_id, anthers=anther_number, flowers=flower_number, replicate=replicate), sum, na.rm = TRUE)
antherPollen <- aggregate(gaincurve[,c(8)], by=list(array=array_id, anthers=anther_number, flowers=flower_number, replicate=replicate), sum, na.rm = TRUE)
flowerVisits <- aggregate(gaincurve[,c(9)], by=list(array=array_id, anthers=anther_number, flowers=flower_number, replicate=replicate), sum, na.rm = TRUE)
gaincurve <- cbind(stigmaPollen, antherPollen[,5], flowerVisits[,5])
names(gaincurve)[5:7] <- c("stigmaPollen", "antherPollen", "flowerVisits")
gaincurve$selfing <- NA
for(array in selfing$array_id) {
	gaincurve$selfing[array] <- selfing$gus_pollen[selfing$array==array]
}
rm(antherPollen, selfing, flowerVisits, array, stigmaPollen)
detach(gaincurve)
# ===========================
# = Calculate export values =
# ===========================
antherNumber <- 30
attach(gaincurve)
gaincurve$probStigmaDeposition <- stigmaPollen / (antherNumber * averagePollenProduction)
# gaincurve$probPollenRemoval <- (antherPollen - averagePollenProduction) / averagePollenProduction
detach(gaincurve)
gaincurve <- transform(gaincurve, anthers = as.integer(gaincurve$anthers), flowers = as.integer(gaincurve$flowers))
badArrays <- c(34, 33)
gaincurve <- gaincurve[-badArrays, ]
rm(badArrays, antherNumber)
# ======================
# = Pollen shadow data =
# ======================
# =====================================
# = Extract data from SQLite database =
# =====================================
# base <- "/Users/mroutley/Documents/MaleGainCurve/" # Change as appropriate
# library(RSQLite)
driver <- dbDriver("SQLite")
connection <- dbConnect(driver, dbname = paste(base, "db/MGCPollenShadowData.db", sep=""))
selfing <- dbGetQuery(connection, statement = "SELECT gus_stigmas.id AS array_id, arrays.donor_anthers, arrays.recipient_anthers, gus_stigmas.gus_pollen, gus_stigmas.wt_pollen FROM gus_stigmas, arrays WHERE gus_stigmas.id = arrays.id")
outcrossing <- dbGetQuery(connection, statement = "SELECT wt_stigmas.array_id, arrays.donor_anthers, arrays.recipient_anthers, wt_stigmas.gus_pollen, wt_stigmas.position FROM wt_stigmas, arrays WHERE wt_stigmas.array_id = arrays.id")
visits <- dbGetQuery(connection, statement = "SELECT visits.array_id, arrays.donor_anthers, arrays.recipient_anthers, visits.plant_id, visits.bout, visits.arrive, visits.depart FROM visits, arrays WHERE visits.array_id = arrays.id")
pollen <- dbGetQuery(connection, statement = "SELECT pollen.array_id, arrays.donor_anthers, arrays.recipient_anthers, pollen.time, pollen.anther_pollen FROM pollen, arrays WHERE pollen.array_id = arrays.id")
dbDisconnect(connection)
dbUnloadDriver(driver)
rm(connection, driver)
detach("package:RSQLite")
# ============
# = Distance =
# ============
shadow <- data.frame(array = outcrossing$array_id, donorAnthers = outcrossing$donor_anthers, recipientAnthers = outcrossing$recipient_anthers, position = outcrossing$position, export = outcrossing$gus_pollen)
shadow <- transform(shadow, background = as.factor(ifelse(shadow$donorAnthers > shadow$recipientAnthers, "smaller", "larger")))
shadow$distance <- NA
shadow$distance[shadow$position %in% c(1, 4, 21, 24)] <- sqrt(3^2 + 3^2)
shadow$distance[shadow$position %in% c(2, 3, 22, 23)] <- sqrt(1^2 + 3^2)
shadow$distance[shadow$position %in% c(5, 7, 18, 20)] <- sqrt(2^2 + 2^2)
shadow$distance[shadow$position %in% c(6, 12, 13, 19)] <- 2
shadow$distance[shadow$position %in% c(8, 11, 14, 17)] <- sqrt(3^2 + 1^2)
shadow$distance[shadow$position %in% c(9, 10, 15, 16)] <- sqrt(1^2 + 1^2)
shadow$edge[shadow$position %in% c(1:4, 21:24, 8, 14, 11, 17)] <- "yes"
shadow$edge[is.na(shadow$edge)] <- "no"
shadow.ag <- aggregate(shadow$export, by=list(array=shadow$array, donorAnthers=shadow$donorAnthers, background=shadow$background, distance=shadow$distance), sum, na.rm = TRUE)
names(shadow.ag)[5]	<- "export"
# shadow.array <- aggregate(shadow$export, by=list(array=shadow$array, donorAnthers=shadow$donorAnthers, background=shadow$background), sum, na.rm = TRUE)
# names(shadow.array)[4] <- "export"
# summary(aov(export ~ as.factor(donorAnthers), data=shadow.array))
# bwplot(export ~ donorAnthers, data=shadow.array)
shadow.edge <- aggregate(shadow$export, by=list(array=shadow$array, donorAnthers=shadow$donorAnthers, edge=shadow$edge), sum, na.rm = TRUE)
names(shadow.edge)[4] <- "export"
bwplot(export ~ donorAnthers|edge, data=shadow.edge)
edge.model <- lm(export ~ donorAnthers*edge, data=shadow.edge)
summary(aov(edge.model))
# ==========
# = Visits =
# ==========
visits$duration <- strptime(visits$depart, format="%H:%M:%S") - strptime(visits$arrive, format="%H:%M:%S")
duration.total <- tapply(as.integer(visits$duration), visits$array_id, sum, na.rm = TRUE)
focal.duration <- tapply(as.integer(visits$duration[visits$plant_id==25]), visits$array_id[visits$plant_id==25], sum, na.rm = TRUE) # Plant 25 is the focal plant
# ===============
# = Outcrossing =
# ===============
outcrossing <- tapply(outcrossing$gus_pollen, outcrossing$array_id, sum, na.rm = TRUE) # Total pollen export
# =================
# = Array summary =
# =================
pollenshadow <- data.frame(array = 1:26, donorAnthers = selfing$donor_anthers, recipientAnthers = selfing$recipient_anthers, export = outcrossing, pollenProduction = pollen$anther_pollen[pollen$time=="before"], pollenRemaining = pollen$anther_pollen[pollen$time=="after"])
pollenshadow <- transform(pollenshadow, probExport = pollenshadow$export / (averagePollenProduction * pollenshadow$donorAnthers), background = as.factor(ifelse(pollenshadow$donorAnthers > pollenshadow$recipientAnthers, "smaller", "larger")), focalSelfing = selfing$gus_pollen, focalOutcrossing = selfing$wt_pollen, totalVisits = duration.total, focalVisits = focal.duration)
# pollenshadow <- transform(pollenshadow, probExport = pollenshadow$export / (pollenshadow$pollenProduction * pollenshadow$donorAnthers), background = as.factor(ifelse(pollenshadow$donorAnthers > pollenshadow$recipientAnthers, "smaller", "larger")), focalSelfing = selfing$gus_pollen, focalOutcrossing = selfing$wt_pollen, totalVisits = duration.total, focalVisits = focal.duration)
pollenshadow <- pollenshadow[pollenshadow$array!=8 & pollenshadow$array!=6, ]
rm(outcrossing, duration.total, focal.duration, selfing, visits, averagePollenProduction, pollen)
# ===============
# = Gain curves =
# ===============
# Gain curve
# model
gaincurve.model <- lm(log(probStigmaDeposition) ~ log(anthers)*flowers, data=gaincurve)
gaincurve.step <- stepAIC(gaincurve.model, scope=list(upper= ~ log(anthers)*flowers*flowerVisits*antherPollen, lower = ~1))
dropterm(gaincurve.step, test="F")
gaincurve.aov <- aov(log(probStigmaDeposition) ~ log(anthers) + flowers + flowerVisits + flowers:flowerVisits, data=gaincurve)
summary(gaincurve.aov)
# plot

xyplot(propSeed ~ anther_number, data=seedSet, groups=flower_number, type="a")

xyplot(probStigmaDeposition ~ anthers, data=gaincurve, groups=flowers, type="a")

, panel = "panel.superpose",
	panel = function(x, y, groups) {
		panel.xyplot(x,y)
		panel.llines
	},
	scales=list(log=TRUE)
)

xyplot(probStigmaDeposition ~ anthers|flowers, data=gaincurve, scales=list(log=TRUE))

pointType <- c(21, 25, 23)
pointColour <- c("white", "black")
lineType <- c("dashed", "dotdash", "solid")
legendLabels <- c("One flower", "Three flowers", "Five flowers")
attach(gaincurve, warn.conflicts = FALSE)
plot(as.integer(anthers), probStigmaDeposition, type="n", xlab="Anther number", ylab="Probability of export", axes=FALSE, log="xy")#, ylim=range(0, 0.03))
axis(1)
axis(2)
points(anthers, probStigmaDeposition, pch=pointType[flowers], bg=pointColour[replicate])
for (flowerNumber in 1:3) {
	lines(1:6, tapply(probStigmaDeposition, list(flowers, anthers), mean)[flowerNumber,], lty=lineType[flowerNumber])
}
legend(4, 0.04, legendLabels, pch=pointType, pt.bg="black", bty="n", pt.cex=1.5)
detach(gaincurve)
# Pollen shadow
# models
pollenshadow.model <- lm(log(probExport) ~ log(donorAnthers)*background, data=pollenshadow)
pollenshadow.step <- stepAIC(pollenshadow.model, scope=list(upper= ~ log(donorAnthers)*background*focalVisits*totalVisits*pollenRemaining, lower = ~1))
dropterm(pollenshadow.step, test="F")
pollenshadow.aov <- aov(log(probExport) ~ log(donorAnthers), data=pollenshadow)
summary(pollenshadow.aov)
# plot
pointType <- c(24, 25) #Triangles up & down
attach(pollenshadow)
plot(donorAnthers, probExport, pch=pointType[background], xlab="Donor anthers", ylab="Probabilty of pollen export", axes=FALSE)
axis(1, at=2:5, labels=2:5)
axis(2)
conditions <- c(donorAnthers > recipientAnthers)
lines(2:5, tapply(probExport[conditions], donorAnthers[conditions], mean), lty=lineType[1])
conditions <- c(donorAnthers < recipientAnthers)
lines(2:5, tapply(probExport[conditions], donorAnthers[conditions], mean), lty=lineType[2])
legend(4, 0.006, c("Larger background", "Smaller background"), pch=pointType)
detach(pollenshadow)
rm(pointType, pointColour, legendLabels, conditions, flowerNumber, lineType)
# ======================
# = Pollen shadow plot =
# ======================
shadow <- shadow[order(shadow$array, shadow$distance),]
xyplot(export ~ distance | donorAnthers, data=shadow, groups=array, panel = panel.superpose, type="b")
bwplot(distance ~ export | donorAnthers, data=shadow)
dotplot(distance ~ export | donorAnthers, data=shadow)
# Aggregated data
xyplot(export ~ distance | donorAnthers, data=shadow.ag, groups=array, panel = panel.superpose, type="b")
bwplot(export ~ distance, data=shadow.ag)
bwplot(distance ~ export | donorAnthers, data=shadow.ag)
# ===============
# = Discounting =
# ===============
# gaincurve$selfing  [1]  NA  NA 165 214 124 221 150 156 173  34 261 157 459 268  NA  NA 172 156  NA  NA 165 214
# [23] 124 221 150 156 173  34 261 157 459 268 172 156 117 268
# gain curve
xyplot(selfing ~ I(stigmaPollen-selfing), data=gaincurve, groups=flowers, auto.key=TRUE)
# plot(gaincurve$selfing, (gaincurve$stigmaPollen-gaincurve$selfing), xlab="Self-pollen deposition", ylab="Cross-pollen export")#, log="xy")
gaincurve.discount <- lm(selfing ~ I(stigmaPollen-selfing), data=gaincurve)
gaincurve.discount.step <- stepAIC(gaincurve.discount, scope=list(upper= ~ I(stigmaPollen-selfing)*as.factor(flowers)*flowerVisits*antherPollen, lower = ~1))
dropterm(gaincurve.discount.step, test="F")
gaincurve.discount.aov <- aov(selfing ~ I(stigmaPollen - selfing)*as.factor(flowers), data=gaincurve)
summary(gaincurve.discount.aov)
coef(gaincurve.discount.aov)
# pollen shadow
xyplot(export ~ focalSelfing, data=pollenshadow, groups=background, auto.key=TRUE)
# plot(pollenshadow$focalSelfing , pollenshadow$export, xlab="Self-pollen deposition", ylab="Total pollen export")#, log="xy")
pollenshadow.discount <- lm(export ~ focalSelfing, data=pollenshadow)
pollenshadow.discount.step <- stepAIC(pollenshadow.discount, scope=list(upper= ~ focalSelfing*totalVisits*focalVisits*background, lower = ~1))

xyplot(focalOutcrossing ~ focalSelfing, data=pollenshadow, groups=background, auto.key=TRUE)
# plot(pollenshadow$focalSelfing , pollenshadow$focalOutcrossing, xlab="Self-pollen deposition", ylab="Cross-pollen deposition")#, log="xy")
pollenshadow.discount <- lm(focalOutcrossing ~ focalSelfing, data=pollenshadow)
pollenshadow.discount.step <- stepAIC(pollenshadow.discount, scope=list(upper= ~ focalSelfing*totalVisits*focalVisits*background, lower = ~1))


xyplot(focalSelfing ~ focalVisits, data=pollenshadow, groups=background, auto.key=TRUE, scales=list(log=TRUE))

# Anthers & flower visits

xyplot(selfing ~ anthers, data=gaincurve, groups=flowers, auto.key=TRUE)

gaincurve.discount <- lm(selfing ~ anthers*as.factor(flowers), data=gaincurve)
gaincurve.discount.step <- stepAIC(gaincurve.discount, scope=list(upper= ~ anthers*as.factor(flowers)*flowerVisits*antherPollen, lower = ~1))
dropterm(gaincurve.discount.step, test="F")
gaincurve.discount.aov <- aov(selfing ~ anthers * as.factor(flowers), data=gaincurve)

plot(gaincurve$anthers, gaincurve$meanFlowerVisits, pch=c(21, 25, 23)[gaincurve$flowers])
summary(lm(meanFlowerVisits ~ anthers*flowers, data=gaincurve))
plot(pollenshadow$donorAnthers, pollenshadow$meanVisit)
summary(lm(meanVisit ~ donorAnthers, data=pollenshadow))
plot(pollenshadow$donorAnthers, pollenshadow$focalVisits)
summary(lm(focalVisits ~ donorAnthers, data=pollenshadow))
# Import & selfing
plot(gaincurve$meanStigmaPollen, gaincurve$selfing)
summary(lm(meanStigmaPollen ~ selfing, data=gaincurve))
plot(pollenshadow$outcrossing, pollenshadow$pollenImport)
summary(lm(pollenImport ~ selfing, data=pollenshadow))
