localSearch <- function(sites,paths,capacity, megaSolution, printIT = FALSE){
  maxIter <- 50
  
  invisible(capture.output( orgCost <- solutionCheck(sites,paths,megaToNormalSolution(megaSolution),capacity) ))
  
  cat("Original solution cost:", orgCost,"\n")
  
  totalCost <- 0
  
  operation <- list("Shift(1,0)","Swap(1,1)","Shift(2,0)")
  operationCost <- list()
  operationSolution <- list()
  operationDo <- list(1,1,1)
  
  for(type in c(1,2,3)){
    typeSolutions <- megaSolution[[type]]
    pathSolutions <- typeSolutions[[1]] #aka the initial solution
    
    #Cost of initial solution for type
    invisible(capture.output( bestCost <- solutionCheckType(sites,paths,pathSolutions, capacity, type) ))
    best <- megaSolution[[type]]
    
    cat("************** [TYPE",type,"] START: Initial cost: ", bestCost, " **************\n")
    
    #For stats
    invalidAttempt <- 0
    worseAttempt <- 0
    randomTimedOut <- 0
    
    operationDo <- c(1,1,1)
    
    i <- 1
    totalIt <- 0
    improvements <- 0
    
    while(1){
      if(printIT)cat(i," ")
      
      if(i >= maxIter){
        if(printIT) cat("\n")
        cat("No more improvements\n")
        totalIt <- totalIt + i -1
        cat("[TYPE",type,"] END. Total iter:",totalIt,"Improvements:",improvements,"Worse Attempts:", worseAttempt,"Invalid Attempts:",invalidAttempt,"\n\n")
        break
      }
      
      #Random shift(1,0)
      if(operationDo[[1]] == 1){
        sol1 <- randomShift10NEW(sites,paths,capacity,typeSolutions)
        operationSolution[[1]] <- sol1
        if(is.null(sol1)){
          if(printIT) cat("(Sh10_stop)")
          randomTimedOut <- 1
          operationSolution[[1]] <- NA
          operationCost[1] <- Inf
          #break
        } else {
          invisible(capture.output( operationCost[1] <- solutionCheckType(sites,paths,operationSolution[[1]][[1]],capacity,type) ))
        }
      }
      
      #Random swap(1,1)
      if(operationDo[[2]] == 1){
        sol2 <- randomSwap11(sites,paths,capacity,typeSolutions)
        operationSolution[[2]] <- sol2
        
        if(is.null(sol2)){
          if(printIT) cat("(Sw11_stop)")
          randomTimedOut <- randomTimedOut + 1
          operationSolution[[2]] <- NA
          operationCost[[2]] <- Inf
        } else {
          invisible(capture.output( operationCost[2] <- solutionCheckType(sites,paths,operationSolution[[2]][[1]],capacity,type) ))
        }
      }
      
      #Random shift(2,0)
      if(operationDo[[3]] == 1){
        sol3 <- randomShift20(sites,paths,capacity,typeSolutions)
        operationSolution[[3]] <- sol2
        
        if(is.null(sol3)){
          if(printIT) cat("(Sh20_stop)")
          randomTimedOut <- randomTimedOut + 1
          operationSolution[[3]] <- NA
          operationCost[[3]] <- Inf
          operationDo[[3]] <- 0
        } else {
          invisible(capture.output( operationCost[3] <- solutionCheckType(sites,paths,operationSolution[[3]][[1]],capacity,type) ))
        }
      }
      
      #If no operation thinks it can continue
      if(!any(operationDo > 0)){
        if(printIT) cat("\n")
        cat("All moves finished\n")
        cat("[TYPE ",type,"] END. Invalid Attempts:",invalidAttempt,"Worse Attempts:", worseAttempt,"\n")
        break
      }
      
      #Compare different operation results, pick best
      costNew <- min(unlist(operationCost))
      indexMinNew <- match(costNew,operationCost)
      solNew <- operationSolution[[indexMinNew]]

      #If the cost is better, replace best known solution
      if(costNew < bestCost){
        costDecrease <- bestCost - costNew
        if(printIT) cat("\n")
        cat("Better solution found ", costDecrease," decrease in cost (",bestCost,"->",costNew,"). OP(",operation[[indexMinNew]],"). Iter(",i,"/",maxIter,")\n")
        bestCost <- costNew
        best <- solNew
        
        #Do all operations again
        operationDo <- c(1,1,1)
        #Reset iteration counter to 1
        totalIt <- totalIt + i
        improvements <- improvements + 1
        i <- 1
        next
      } else{
        if(is.infinite(costNew))
          {invalidAttempt <- invalidAttempt + 1}
        else
          {worseAttempt <- worseAttempt + 1}
      }
      
      
      i <- i + 1
    }
    
    megaSolution[[type]] <- best
    totalCost <- totalCost + bestCost
  }
  
  cat("Toal cost after execution: ", totalCost, " Change of: ",(totalCost-orgCost),"\n")
  megaSolution
}