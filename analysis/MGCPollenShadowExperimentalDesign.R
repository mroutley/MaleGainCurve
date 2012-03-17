# Randomized designs for the male gain curve, pollen shadow experiment
donorAnthers <- 2:5
offset <- 17 # Increment for each replicate
experiment <- NULL
experiment$donor <- rep(donorAnthers, 2)
experiment <- as.data.frame(experiment)
experiment$recipient <- c(1:4, 3:6)
experiment$run <- sample(offset:(dim(experiment)[1]+offset-1), dim(experiment)[1])
experiment <- experiment[order(experiment$run),]
experiment
write.table(experiment, file="gainCurveArrays.txt", row.names=FALSE, sep="\t", quote=FALSE)