test<-function(dimensions, runs, cecFunctionNumber){
	
	source("/home/preston/Pulpit/MEUM_projekt/meum/de.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/cec.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/config.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/selection.r")
	library(Hmisc)
	
	range <- getRange(cecFunctionNumber)
  evaluation <- getEvaluation(cecFunctionNumber)
  populationSize <- getPopulationSize()
  iterations <- getIterations()
  
	baseResult <- list()
	medianResult <- list()
  threshResult <- list()
	
	for (loop in 1:runs){
		print(paste ("number:", loop, "/", runs))
		
    meanPoint <- c(runif(dimensions,-range,range))
		
    base <- differentialEvolution(meanPoint, dimensions, range, populationSize, normalSelection, evaluation, iterations)
		median <- differentialEvolution(meanPoint, dimensions, range, populationSize, medianSelection, evaluation, iterations)
		thresh <- differentialEvolution(meanPoint, dimensions, range, populationSize, threshSelection, evaluation, iterations)

		baseResult <- c(baseResult, base)
		medianResult <- c(medianResult, median)
    threshResult <- c(threshResult, thresh)
	}
	
	# zapisz empiryczną dystrybuantę danych
	ecdfFileBase <- "/home/preston/Pulpit/MEUM_projekt/plots/ecdf_cec2005_" 
	ecdfFilename <- paste(ecdfFileBase, cecFunctionNumber, "dim", dimensions,".jpg")
	jpeg(ecdfFilename)
	Ecdf(unlist(baseResult), col = 'blue', lwd = 2, lty = 1, xlab = "Blue=BaseDE, Red=MedianDE, Black=ThreshDE")
	Ecdf(unlist(medianResult), add = TRUE, col = 'red', lwd = 2, lty = 1)
	Ecdf(unlist(threshResult), add = TRUE, col = 'black', lwd = 2, lty = 1)
	dev.off()
}


