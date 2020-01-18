removeAndReinsert2 <- function(sites, paths, capacity, bothSolutions){
  pathSolutions <- bothSolutions[[1]]
  nodeSolutions <- bothSolutions[[2]]
  
  #Pick random lines 1 and 2
  randomLinesInds <- sample(1:length(pathSolutions),2)

  randomLine1index <- randomLinesInds[[1]]
  randomLine2index <- randomLinesInds[[2]]

  type <- pathSolutions[[randomLine1index]][1]
  
  ns1 <- nodeSolutions[[randomLine1index]]
  ns2 <- nodeSolutions[[randomLine2index]]
  
  #Pick a random node from random line 1
  if(length(ns1) == 1){
    randomNode <- ns1[1]
  }
  else{
    randomNode <- sample(ns1[1:(length(ns1))],1)
  }
  
  #Insert into random line 2
  ns2new <- insertNodeIntoBestPlace(sites,paths,ns2,randomNode,capacity,type)
  
  pathSolutions[[randomLine2index]] <- constructFullSolution(sites,paths,ns2new,type)
  nodeSolutions[[randomLine2index]] <- ns2new
  
  #Rebuild or remove line 1 if necesary 
  if(length(ns1) == 1){
    pathSolutions[[randomLine1index]] <- NULL
    nodeSolutions[[randomLine1index]] <- NULL
  }
  else{
    index <- match(randomNode, ns1)
    ns1new <- ns1[-index]
    
    #Repair the path without the lost node
    pathSolutions[[randomLine1index]] <- constructFullSolution(sites,paths,ns1new, type)
    nodeSolutions[[randomLine1index]] <- list(ns1new)
  }
  
  list(pathSolutions,nodeSolutions)
}

insertNodeIntoBestPlace <- function(sites,paths,nodes,node, capacity, type){
  bestPlace <- 1
  bestCost <- Inf
  best <- NULL
  for(place in 1:length(nodes)+1){
    nodesNew <- c(nodes[1:place],node)
    
    nodesNew <- c(nodes[0:(place-1)],node)
    if(place <= length(nodes)) nodesNew <- c(nodesNew, nodes[place:length(nodes)])
    
    repaired <- constructFullSolution(sites,paths,nodesNew,type)
    cost <- lineCost(sites,paths,capacity,repaired,nodesNew,type)
    
    if(cost < bestCost){
      bestCost <- cost
      best <- nodesNew
    }
    
  }
  best
}