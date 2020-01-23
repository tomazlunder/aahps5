randomSwap11 <- function(sites, paths, capacity, megaSolution){
  pathSolutions <- megaSolution[[1]]
  nodeSolutions <- megaSolution[[2]]
  totalLoads <- megaSolution[[3]]
  
  type <- pathSolutions[[1]][1]
  typeIndex <- getTypeIndex(colnames(sites),type)
  
  viablePicks <- sapply(nodeSolutions, function(i) length(i) >= 2)
  viablePicks <- nodeSolutions[viablePicks]
  
  #No viable picks (all nodes are len 1 or only one len 2)
  if(length(viablePicks) < 2){
    return(NULL)
  }
  #Pick random lines 1 and 2
  #This is cheap and reapeated until a valid candidates are picked or max iterations is reached
  i <- 1
  maxIter <- 15
  while(1){
    if(i >= maxIter) return(NULL)
    
    randomLinesInds <- sample(1:length(viablePicks),2)
    
    viablePick1index <- randomLinesInds[[1]]
    viablePick2index <- randomLinesInds[[2]]
    ns1 <- viablePicks[viablePick1index]
    ns2 <- viablePicks[viablePick2index]
    
    randomLine1index <- match(ns1,nodeSolutions)
    randomLine2index <- match(ns2,nodeSolutions)
    
    ns1 <- nodeSolutions[[randomLine1index]]
    ns2 <- nodeSolutions[[randomLine2index]]
    
    validSwaps <- list()
    
    foundSomething <- FALSE
    
    j <- 1
    #Try a few times
    while(j < 10){
      #Pick a random node from random line 1
      if(length(ns1) == 1){
        randomNode1 <- ns1[1]
      }
      else{
        randomNode1 <- sample(ns1[1:(length(ns1))],1)
      }
      
      #Pick a random node from line 2
      if(length(ns2) == 1){
        randomNode2 <- ns2[1]
      }
      else{
        randomNode2 <- sample(ns2[1:(length(ns2))],1)
      }
      
      newLoad1 <- (totalLoads[[randomLine1index]] - sites[randomNode1,typeIndex]) + sites[randomNode2,typeIndex]
      newLoad2 <- (totalLoads[[randomLine2index]] - sites[randomNode2,typeIndex]) + sites[randomNode1,typeIndex]
      
      #Found something we can swap
      if(newLoad1 < capacity && newLoad2 < capacity){
        foundSomething <- TRUE
        break;
      }
      
      j <- j+1
    }
    
    if(foundSomething) break
    i <- i + 1
  }
  
  #Remove selected nodes from lines
  index1 <- match(randomNode1, ns1)
  index2 <- match(randomNode2, ns2)
  ns1new <- ns1[-index1]
  ns2new <- ns2[-index2]
  
  ns1new <- insertNodeIntoBestPlace(sites,paths,capacity,ns1new,randomNode2,type)
  ns2new <- insertNodeIntoBestPlace(sites,paths,capacity,ns2new,randomNode1,type)
  
  pathSolutions[[randomLine1index]] <- constructFullSolution(sites,paths,ns1new,type)
  pathSolutions[[randomLine2index]] <- constructFullSolution(sites,paths,ns2new,type)
  
  nodeSolutions[[randomLine1index]] <- list(ns1new)
  nodeSolutions[[randomLine2index]] <- list(ns2new)
  
  totalLoads[[randomLine1index]] <- newLoad1
  totalLoads[[randomLine2index]] <- newLoad2
  
  list(pathSolutions,nodeSolutions, totalLoads)
}
