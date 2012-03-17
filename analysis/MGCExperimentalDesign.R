# Generates experimental runs for male gain curve experiments
graphics.off()
rm(list=ls())
maxAnthersList <- c(30)
antherNumberList <- 1:6
flowerNumberList <- c(1, 3, 5)
replicates <- 1
visitObservations <- c("0-15", "15-30", "30-45", "45-60")
experimentDesign <- NULL
experimentDesign$antherNumber <- rep(antherNumberList, length(flowerNumberList) * replicates)
experimentDesign <- as.data.frame(experimentDesign)
experimentDesign$flowerNumber <- rep(flowerNumberList, each=length(antherNumberList) * replicates)
attach(experimentDesign)
experimentDesign$plantNumber <- maxAnthersList / antherNumber
experimentDesign$plantMarkerType <- sample(c("WT", "GUS"), length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$visit1 <- sample(visitObservations, length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$visit2 <- sample(visitObservations, length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$visit3 <- sample(visitObservations, length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$visit4 <- sample(visitObservations, length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$visit5 <- sample(visitObservations, length(experimentDesign$antherNumber), replace=TRUE)
experimentDesign$run <- sample(1:length(experimentDesign$antherNumber), length(experimentDesign$antherNumber), replace=FALSE)
attach(experimentDesign)
experimentDesign <- experimentDesign[order(experimentDesign$run),]
experimentDesign
write.table(experimentDesign, file="gainCurveArrays.txt", row.names=FALSE, sep="\t", quote=FALSE)
# Generates a random number table for anther harvesting
rows <- 45
antherList <- 2:6
antherList <- c(antherList, antherList)
randomTable <- NULL
for(antherNumber in antherList) {
randomTable <- cbind(randomTable, sample(1:antherNumber, (rows/(antherNumber-1)), replace=TRUE))
}
randomTable <- as.data.frame(randomTable)
names(randomTable) <- c("2", "3", "4", "5", "6", "2", "3", "4", "5", "6")
print(randomTable)
write.table(randomTable, file="randomTable.txt", row.names=FALSE, sep="\t", quote=FALSE)
# Generates random anther selections
antherSetup <- function(plantNumber) {
	anthers <- NULL
	anthers$length <- sample(c(rep("short", round(1/3 * plantNumber)), rep("long", round(2/3 * plantNumber))))
	anthers <- as.data.frame(anthers)
	attach(anthers)
	anthers$number <- NA
	for (i in 1:plantNumber) {
		anthers[i,2] <- round(ifelse(anthers$length[i] == "short", runif(1, 1, 2), runif(1, 1, 4)))
	}
	print(anthers)
}
antherSetup(30)