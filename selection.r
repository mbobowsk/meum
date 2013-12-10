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
  probability <- probability - 0.2
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
