## This file contains information about cec benchmark
library(cec2005benchmark)

getEvaluation<-function(number)
{
  if (number == 1)
    return (evaluation1)
  else
    stop(paste("Illegal CEC benchmark function number: ", number))
}

getRange<-function(number)
{
  if (number == 1)
    return (100)
  else
    stop(paste("Illegal CEC benchmark function number: ", number))
}

evaluation1<-function(coordinates)
{
  return (-cec2005benchmark1(coordinates))
}