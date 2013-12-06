#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

library(cec2005benchmark)

## Obliczenie wartości funkcji celu w punkcie
## '-' bo zadanie maksymalizacji
evaluation<-function(coordinates)
{
		return (-cec2005benchmark1(coordinates))
}

## Procedura inicjalizacyjna - zamienia punkt startowy w listę z punktem startowym :)
initialization<-function(startPoints)
{
	return (list(startPoints))
}

## Inicjalizacja modelu
# Zakładam, że model jest listą zawierającą:
# currentPoint - aktualny punkt
# t - temperaturę
initModel<-function(startPoints)
{
	return (list(currentPoint=startPoints[[1]], iter=1))
}


## Warunek zakonczenia - na razie niech będzie jakieś ograniczenie temperatury
termination<-function(history,model)
{
	if ( model$iter > 5000 )
		return (TRUE)
	else
		return (FALSE)
}




#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
# zwraca listę jednoelementową zawierającą ostatnio wygenerowany punkt
selection<-function(history, model)
{
	selectedPoint <- tail(history,1)
	return(selectedPoint)
}

#update of a model based on a LIST of points
# na wejście dostaje listę jednoelementową z ostatnio wygenerowanym punktem
# sprawdza czy punkt lepszy i ewentualnie podmienia, zmniejsza temperature
modelUpdate<-function(selectedPoints, oldModel)
{
	# operujemy na konkretnym punkcie, a nie na liście
	selectedPoint <- selectedPoints[[1]]
	# wybór nowego punktu roboczego
	newQuality <- selectedPoint$quality
	oldQuality <- oldModel$currentPoint$quality
  i <- oldModel$iter + 1
  
	if ( newQuality > oldQuality )
		newModel <- list(currentPoint=selectedPoint, iter=i)
	else {
			newModel <- list(currentPoint=oldModel$currentPoint, iter=i)
	}
	return (newModel)
}

#generation of a LIST of new points
# generuje losowy punkt z otoczenia punktu z modelu
# zwraca jednoelementową listę punktów
variation<-function(selectedPoints, model, border, dimensions)
{
	modifier <- c(rnorm(dimensions,0,(border/6)))
	coordinates <- selectedPoints[[1]]$coordinates + modifier
	# odbijanie od ograniczenia
	for ( i in 1:dimensions ) {
		if ( coordinates[i] > border )
			coordinates[i] <- 2*border - coordinates[i]
		else if ( coordinates[i] < -border )
			coordinates[i] <- -2*border - coordinates[i]
	}
	newPoints <- list(quality=evaluation(coordinates),coordinates=coordinates)
	return (list(newPoints))
}



#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
#To jest w zasadzie konstruktor klasy reprezentującej operator zagregowany
#Operator zagregowany jest lista zawierajaca punkty i model
aggregatedOperator<-function(history, oldModel, border, dimensions)
{
	selectedPoints<-selection(history, oldModel)
	newModel<-modelUpdate(selectedPoints, oldModel)
	newPoints<-variation(selectedPoints, newModel, border, dimensions)
	return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation, border, dimensions)
{
	history<-initialization(startPoints)
	history<-evaluateList(history,evaluation)
	model<-initModel(history)
	while (!termination(history,model))
	{
		aa<-aggregatedOperator(history, model, border, dimensions)
		aa$newPoints<-evaluateList(aa$newPoints, evaluation)
		history<-historyPush(history,aa$newPoints)
		model<-aa$newModel
	}
	return(history)
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
	newHistory<-c(oldHistory,newPoints)
	return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
	stop=length(history)
	start=max(stop-number+1,1)
	return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{  
	for (i in 1:length(points))
		points[[i]]$quality<-evaluation(points[[i]]$coordinates)
	return (points) 
}


# result - lista punktów z historii (log)
differentialEvolution<-function(coordinates, dim){
	startPoints <- list(quality=evaluation(coordinates),coordinates=coordinates)

	result <- metaheuristicRun(initialization, startPoints, termination, evaluation, 100, dim)
	best <- result[[1]]$quality
	coordinates <- NULL
	
	for ( i in 1:length(result) ) {
		if ( result[[i]]$quality > best ) {
			best <- result[[i]]$quality
			coordinates <- result[[i]]$coordinates
			result[[i]]<-result[[i]]$quality
		}
		else{
			result[[i]]<-best
		}

	}
	print(paste("result: ", best))
	return(best)
}

