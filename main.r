test_de<-function(dimensions, runs){
	
	source("/home/preston/Pulpit/MEUM_projekt/meum/de.r")
	library(Hmisc)
	
	range <- 100
	allResults <- list()
  populationSize <- 10
	
	for (loop in 1:runs){
		print(paste ("number:", loop, "/", runs))
		
    meanPoint <- c(runif(dimensions,-range,range))
		
    result <- differentialEvolution(meanPoint, dimensions, range, populationSize)
    #print(result[[1]])
		#allResults <- c(allResults, result)
	}
	
	# zapisz empiryczną dystrybuantę danych
	#ecdfFileBase <- "/home/preston/Pulpit/MEUM_projekt/plots/ecdf_cec2005_1_dimensions" 
	#ecdfFilename <- paste( ecdfFileBase, dimensions,".jpg")
	#jpeg(ecdfFilename)
	#Ecdf(unlist(allResults), col = 'blue', lwd = 2, lty = 1, xlab = "BaseDE")
	#dev.off()
}


