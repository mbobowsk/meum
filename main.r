test_de<-function(dimensions, runs){
	
	source("/home/preston/Pulpit/MEUM_projekt/de.r")
	library(Hmisc)
	
	border <- 100
	allResults <- list()
  populationSize <- 10
	
	for (loop in 1:runs){
		print(paste ("number:", loop, "/", runs))
    
		startPoints <- c(runif(dimensions,-border,border))
		
    result <- differentialEvolution(startPoints, dimensions)
    
		allResults <- c(allResults, result)
	}
	
	# zapisz empiryczną dystrybuantę danych
	ecdfFileBase <- "/home/preston/Pulpit/MEUM_projekt/plots/ecdf_cec2005_1_dimensions" 
	ecdfFilename <- paste( ecdfFileBase, dimensions,".jpg")
	jpeg(ecdfFilename)
	Ecdf(unlist(allResults), col = 'blue', lwd = 2, lty = 1, xlab = "BaseDE")
	dev.off()
}


