## Model is a list of:
# iter - current iteration number
# limit - iteration limit
initModel<-function(limit)
{
	return (list(iter=1, limit=limit))
}


## Termination condition
termination<-function(model)
{
	if ( model$iter > model$limit )
		return (TRUE)
	else
		return (FALSE)
}

## Update model
modelUpdate<-function(model)
{
  return (list(iter = model$iter + 1, limit = model$limit))
}

## Tournament
tournament<-function(point1, point2)
{
  if (point1$quality > point2$quality)
    return (point1)
  else
    return (point2)
}

## Crossover
crossover<-function(point1, point2)
{
  newCoordinates <- c()
  for (i in 1:length(point1$coordinates) ) {
    if (runif(1,0,1) > 0.5)
      newCoordinates[i] <- point1$coordinates[i]
    else
      newCoordinates[i] <- point2$coordinates[i]
  }
  return (list(coordinates=newCoordinates, quality=evaluation(newCoordinates)))
}

## Prevents coordinates from exceeding maximum and minimum
rangeCorrection<-function(coordinates, range)
{
  for ( i in 1:length(coordinates) ) {
    if ( coordinates[i] > range )
      coordinates[i] <- 2*range - coordinates[i]
    else if ( coordinates[i] < -range )
      coordinates[i] <- -2*range - coordinates[i]
  }
  return (coordinates)
}

## Differential evolution mutation
## f is a constant factor
mutation<-function(base, point1, point2, range)
{
  diff <- point2$coordinates - point1$coordinates
  f <- 0.8
  newCoords <- base$coordinates + f*diff
  newCoords <- rangeCorrection(newCoords, range)
  return (list(coordinates=newCoords, quality=evaluation(newCoords)))
}

## Select random point from population
simpleSelection<-function(population)
{
  return (sample(population, 1)[[1]])
}

## An aggregated operator generates new population and model
aggregatedOperator<-function(oldModel, range, dimensions, oldPopulation, selection)
{
  newPopulation <- list()

  for (i in 1:length(oldPopulation)) {
    xi <- oldPopulation[[i]]
    xj <- selection(oldPopulation, oldModel)
    xk <- simpleSelection(oldPopulation)
    xl <- simpleSelection(oldPopulation)
    y <- mutation(xj, xk, xl, range)
    z <- crossover(xi,y)
    newPopulation[[i]] <- tournament(xi, z)
  }
  
	newModel<-modelUpdate(oldModel)
	return (list(newPopulation=newPopulation, newModel=newModel))
}

## The main loop of a metaheuristic.
## Returns the last population
metaheuristicRun<-function(startPoints, termination, evaluation, range, dimensions, selection, iterations)
{
	model<-initModel(iterations)
  population <- startPoints
  
	repeat {
		aa<-aggregatedOperator(model, range, dimensions, population, selection)
		population<-aa$newPopulation
		model<-aa$newModel
    
    if (termination(model))
      break;
	}
	return(aa$newPopulation)
}

# Returns quality of population's center point
getPopulationCenter<-function(population)
{
  meanCoords <- population[[1]]$coordinates
  for (i in 2:length(population)) {
    meanCoords <- meanCoords + population[[i]]$coordinates
  }
  return (evaluation(meanCoords / length(population)))
}

`%$%` <- function(x, n)sapply(x, `[[`, n)

## Returns the best element from final population
## startPoint - meanPoint of the first population
## dim - dimensions (2, 10, 30 or 50 for CEC05)
## range - range of objective function arguments
## popSize - size of population
differentialEvolution<-function(meanPoint, dim, range, popSize, selection, evaluation, iterations)
{
  
  startPoints <- list()
  
  for (i in 1:popSize){
    modifier <- c(rnorm(dimensions,0,(range/10)))
    coords <- meanPoint+modifier
    newPoint <- list(coordinates=coords, quality=evaluation(coords))
    startPoints[[i]] <- newPoint
  }
  
	result <- metaheuristicRun(startPoints, termination, evaluation, range, dim, selection, iterations)
	
  #best <- getPopulationCenter(result)
  best <- max(result %$% "quality")
  
  print(paste("result: ", best))
	return(best)
	
}

