simulatedAnnealing <- function(sites,paths,capacity, megaSolution, printIT = FALSE){
  maxIter <- 50
  tempInitial <- 1000
  tempFinal <- 1
  alpha <- 0.9
  
  invisible(capture.output( orgCost <- solutionCheck(sites,paths,capacity,megaToNormalSolution(megaSolution)) ))
  
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
    invisible(capture.output( startCost <- solutionCheckType(sites,paths,pathSolutions, capacity, type) ))
    
    #New in simulatedAnnealing
    scurrent <- typeSolutions
    sbest <- typeSolutions
    temperature <- tempInitial #TODO::INITIAL
    temperatureFinal <- 1 ##TODO::INIT THIS
    
    cost_current <- startCost
    cost_best <- startCost
    
    cat("************** [TYPE",type,"] START: Initial cost: ", cost_best, " **************\n")
    
    #For stats
    invalidAttempt <- 0
    worseAttempt <- 0
    randomTimedOut <- 0
    
    operationDo <- c(1,1,1)
    
    i <- 1
    totalIt <- 0
    improvements <- 0
    
    while(temperature > temperatureFinal){
      if(printIT)cat(temperature," ")
      
      if(i >= maxIter){
        if(printIT) cat("\n")
        cat("No more improvements\n")
        totalIt <- totalIt + i -1
        cat("[TYPE",type,"] END. Total iter:",totalIt,"Improvements:",improvements,"Worse Attempts:", worseAttempt,"Invalid Attempts:",invalidAttempt,"\n\n")
        break
      }
      
      #Random shift(1,0)
      if(operationDo[[1]] == 1){
        sol1 <- randomShift10(sites,paths,capacity,typeSolutions)
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
      
      
      costDiff <- costNew - cost_current
      #If the cost of new solution is better than the cost of current solution, update current
      if(costDiff <= 0){
        if(printIT) cat("\n")
        cat("Current solution updated[-]", costDiff," decrease in cost (",cost_current,"->",costNew,"). OP(",operation[[indexMinNew]],")\n")
        cost_current <- costNew
        scurrent <- solNew
        
        #If the new current solution is better than the best known solution, update best
        bestCostDiff <- cost_current - cost_best
        if(bestCostDiff < 0){
          if(printIT) cat("\n")
          cat("Best solution updated[!]", bestCostDiff," decrease in cost (",cost_best,"->",cost_current,"). OP(",operation[[indexMinNew]],")\n")
          cost_best <- cost_current
          sbest <- scurrent
        }
        
        #Do all operations again
        operationDo <- c(1,1,1)
      } else{
        if(is.infinite(costNew))
        {invalidAttempt <- invalidAttempt + 1}
        else
        {
          worseAttempt <- worseAttempt + 1
          #Pick the worse solution with probability
          if(acceptWorseSolution(costDiff,temperature)){
            if(printIT) cat("\n")
            cat("Current solution updated[+]", costDiff," increase in cost (",cost_current,"->",costNew,"). OP(",operation[[indexMinNew]],")\n")
            cost_current <- costNew
            scurrent <- solNew
          }
        }
      }
      
      temperature = temperature * alpha
    }
    
    megaSolution[[type]] <- sbest
    totalCost <- totalCost + cost_best
  }
  
  cat("Toal cost after execution: ", totalCost, " Change of: ",(totalCost-orgCost),"\n")
  megaSolution
}

acceptWorseSolution <- function(difference,temperature){
  boltzman <- exp(-difference/temperature)
  return(boltzman > runif(1, min=0, max=1))
}