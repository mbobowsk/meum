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
  probability <- (500 - model$iter) / 500
  med <- median(population %$% "quality")
  repeat {
    selectedPoint <- sample(population, 1)[[1]]
    if (selectedPoint$quality > med || runif(1,0,1) < med)
      return (selectedPoint)
  }
  
  print(sort(population$quality))
  return (sample(population, 1)[[1]])
}


test_de<-function(dimensions, runs, cecFunctionNumber){
	
	source("/home/preston/Pulpit/MEUM_projekt/meum/de.r")
	source("/home/preston/Pulpit/MEUM_projekt/meum/cec.r")
	library(Hmisc)
	
	range <- getRange(cecFunctionNumber)
  evaluation <- getEvaluation(cecFunctionNumber)
	baseResult <- list()
	medianResult <- list()
  populationSize <- 10
	
	for (loop in 1:runs){
		print(paste ("number:", loop, "/", runs))
		
    meanPoint <- c(runif(dimensions,-range,range))
		
    base <- differentialEvolution(meanPoint, dimensions, range, populationSize, normalSelection, evaluation)
		median <- differentialEvolution(meanPoint, dimensions, range, populationSize, medianSelection, evaluation)

		baseResult <- c(baseResult, base)
		medianResult <- c(medianResult, median)
	}
	
	# zapisz empiryczną dystrybuantę danych
	ecdfFileBase <- "/home/preston/Pulpit/MEUM_projekt/plots/ecdf_cec2005_1_dimensions" 
	ecdfFilename <- paste( ecdfFileBase, dimensions,".jpg")
	jpeg(ecdfFilename)
	Ecdf(unlist(baseResult), col = 'blue', lwd = 2, lty = 1, xlab = "Blue=BaseDE, Red=MedianDE")
	Ecdf(unlist(medianResult), add = TRUE, col = 'red', lwd = 2, lty = 1)
	dev.off()
}


