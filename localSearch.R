localSearch <- function(sites,paths,capacity, megaSolution){
  maxIter <- 250
  
  invisible(capture.output( orgCost <- solutionCheck(sites,paths,megaToNormalSolution(megaSolution),capacity) ))
  
  cat("Original solution cost:", orgCost,"\n")
  
  totalCost <- 0
  
  operation <- list("Shift(1,0)","Swap(1,1)")
  operationCost <- c(NULL,NULL,NULL,NULL)
  operationSolution <- c(NULL,NULL,NULL,NULL) 
  
  for(type in c(1,2,3)){
    typeSolutions <- megaSolution[[type]]
    pathSolutions <- typeSolutions[[1]] #aka the initial solution
    
    #Cost of initial solution for type
    invisible(capture.output( bestCost <- solutionCheckType(sites,paths,pathSolutions, capacity, type) ))
    best <- pathSolutions
    
    cat(" Initial best cost for type", type, ": ", bestCost, "\n")
    
    #For stats
    invalidAttempt <- 0
    worseAttempt <- 0
    randomTimedOut <- 0
    
    i <- 1
    while(1){
      #Random shift(1,0)
      operationSolution[1] <- randomShift10(sites,paths,capacity,typeSolutions)
      if(is.null(operationSolution[1])){
        randomTimedOut <- 1
        #break
      }
      
      #Random swap(1,1)
      operationSolution[2] <- randomSwap11(sites,paths,capacity,typeSolutions)
      if(is.null(operationSolution[2])){
        randomTimedOut <- randomTimedOut + 1
      }
      
      #Check validity and cost of newly constructed solutions
      invisible(capture.output( operationCost[1] <- solutionCheckType(sites,paths,operationSolution[[1]],capacity,type) ))
      invisible(capture.output( operationCost[2] <- solutionCheckType(sites,paths,operationSolution[[2]],capacity,type) ))
      
      #Compare different operation results, pick best
      costNew <- min(operationCost)
      indexMinNew <- match(costNew,operationCost)
      solNew <- operationSolution[indexMinNew]

      #If the cost is better, replace best known solution
      if(costNew < bestCost){
        costDecrease <- bestCost - costNew
        cat("Better solution found ", costDecrease," decrease in cost (",bestCost,"->",costNew,"). OP(",operation[[indexMinNew]],"). Iter(",i,"/",maxIter,")\n")
        bestCost <- costNew
        best <- solNew
        
        #Reset iteration counter to 1
        i <- 1
      } else{
        if(is.infinite(costNew)) invalidAttempt <- invalidAttempt + 1
        else worstAttempt <- worseAttempt + 1  
      }
      
      
      i <- i + 1
      if(i == maxIter){
        cat("No more changes for type", type,". Invalid Attempts:",invalidAttempt,"Worse Attempts:", worseAttempt,"\n")
        break
      }
    }
    
    megaSolution[[type]] <- best
    totalCost <- totalCost + bestCost
  }
  
  cat("Toal cost after execution: ", totalCost, " Change of: ",(totalCost-orgCost),"\n")
  megaSolution
}