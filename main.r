## Normal selection
normalSelection<-function(population, model)
{
  return (sample(population, 1)[[1]])
}

## Weird trick for named list access in R
## Stackoverflow magic :)
`%$%` <- function(x, n)sapply(x, `[[`, n)


## Median selection
medianSelection<-function(population, model)
{
  probability <- (model$limit - model$iter) / model$limit
  med <- median(population %$% "quality")
  
  repeat {
    selectedPoint <- sample(population, 1)[[1]]
    if (selectedPoint$quality > med || runif(1,0,1) < med)
      return (selectedPoint)
  }
}

## Thresh selection
threshSelection<-function(population, model)
{  
  sortedQualities <- sort(population %$% "quality")
  
  ps <- length(population)
  i <- model$limit
  # linear function approximation
  a <- (1 - ps) / (1 - i)
  b <- (ps - i) / (1 - i)
  threshIndex <- round(a * model$iter + b)
  # upper limit
  if (threshIndex > ps - 5)
    threshIndex <- ps - 5
  thresh <- sortedQualities[threshIndex]
  
  repeat {
    selectedPoint <- sample(population, 1)[[1]]
    if (selectedPoint$quality >= thresh)
      return (selectedPoint)
  }
}


test_de<-function(dimensions, runs, cecFunctionNumber){
	
	source("/home/preston/Pulpit/MEUM_projekt/meum/de.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/cec.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/config.r")
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
	ecdfFileBase <- "/home/preston/Pulpit/MEUM_projekt/plots/ecdf_cec2005_1_dimensions" 
	ecdfFilename <- paste( ecdfFileBase, dimensions,".jpg")
	jpeg(ecdfFilename)
	Ecdf(unlist(baseResult), col = 'blue', lwd = 2, lty = 1, xlab = "Blue=BaseDE, Red=MedianDE, Black=ThreshDE")
	Ecdf(unlist(medianResult), add = TRUE, col = 'red', lwd = 2, lty = 1)
	Ecdf(unlist(medianResult), add = TRUE, col = 'black', lwd = 2, lty = 1)
	dev.off()
}


