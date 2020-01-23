randomShift10 <- function(sites, paths, capacity, megaSolution){
  pathSolutions <- megaSolution[[1]]
  nodeSolutions <- megaSolution[[2]]
  totalLoads <- megaSolution[[3]]
  
  #Type
  type <- pathSolutions[[1]][1]
  typeIndex <- getTypeIndex(colnames(sites),type)
  
  #Space remaining on lines, max space remaining
  space <- sapply(totalLoads, function(x) x<- (capacity - x))
  maxAcceptableLoad <- max(space)
  
  #Calculating the minimum load node of each path
  donorsMinGive <- c()
  i <- 1
  for(each in nodeSolutions){
    ns <- nodeSolutions[[i]]
    
    loadsNS <- sapply(ns,function(x) x <- sites[ns[match(x,ns)],typeIndex])
    donorsMinGive <- c(donorsMinGive,min(loadsNS))
    
    i <- i + 1
  }
  
  totalMinGive <- min(donorsMinGive)
  
  #Select a viable receiver
  viableReceivers <- sapply(totalLoads, function(x) x + totalMinGive <= capacity)
  viableReceivers <- nodeSolutions[viableReceivers]
  
  numTries <- 10
  i <- 1
  while(1){
    if(i > numTries) return(NULL)
    
    viableReceiver <- sample(viableReceivers,1)
    randomLine2index <- match(viableReceiver,nodeSolutions)
    ns2 <- nodeSolutions[[randomLine2index]]
    line2load <- totalLoads[[randomLine2index]]
    
    #Remove the unviable donor candidates
    viableDonors <- nodeSolutions
    
    viableDonorsNarrowed <- sapply(donorsMinGive, function(x) x + line2load <= capacity )
    viableDonors <- viableDonors[viableDonorsNarrowed]
    
    #The only availabe receiver was self
    if(length(viableDonors) == 1){
      #If this was the only option - terminate
      if(length(viableReceivers) == length(viableDonors)){
        return(NULL)
      }
      #Else try a few more times
      i <- i + 1
      next
    }
    
    break
  }
  
  #Select one of the viable donors
  viableDonor <- sample(viableDonors,1)
  randomLine1index <- match(viableDonor,nodeSolutions)
  ns1 <- nodeSolutions[[randomLine1index]]
  
  #Select which node to donate
  viableNodes <- sapply(ns1,function(x) x <- (sites[x,typeIndex] + line2load) <=  capacity)
  bla <- ns1[viableNodes]
  
  if(length(bla) == 1){bla <- list(bla)}
  randomViable <- sample(bla,1)
  selectedNode <- unlist(randomViable)
  
  #Insert into random line 2
  ns2new <- insertNodeIntoBestPlace(sites,paths,capacity,ns2,selectedNode,type)
  
  pathSolutions[[randomLine2index]] <- constructFullSolution(sites,paths,ns2new,type)
  nodeSolutions[[randomLine2index]] <- ns2new
  totalLoads[[randomLine2index]] <- totalLoads[[randomLine2index]] + sites[selectedNode,typeIndex]
  
  #Rebuild or remove line 1 if necesary 
  if(length(ns1) == 1){
    pathSolutions[[randomLine1index]] <- NULL
    nodeSolutions[[randomLine1index]] <- NULL
    totalLoads[[randomLine1index]] <- NULL
  }
  else{
    index <- match(selectedNode, ns1)
    ns1new <- ns1[-index]
    
    #Repair the path without the lost node
    pathSolutions[[randomLine1index]] <- constructFullSolution(sites,paths,ns1new, type)
    nodeSolutions[[randomLine1index]] <- list(ns1new)
    totalLoads[[randomLine1index]] <- totalLoads[[randomLine1index]] - sites[selectedNode,typeIndex]
  }
  
  list(pathSolutions,nodeSolutions, totalLoads)
}

randomShift20 <- function(sites, paths, capacity, megaSolution){
  pathSolutions <- megaSolution[[1]]
  nodeSolutions <- megaSolution[[2]]
  totalLoads <- megaSolution[[3]]
  
  #Type
  type <- pathSolutions[[1]][1]
  typeIndex <- getTypeIndex(colnames(sites),type)
  
  #Space remaining on lines, max space remaining
  space <- sapply(totalLoads, function(x) x<- (capacity - x))
  maxAcceptableLoad <- max(space)
  
  #Viable donors based on length (atleast 2 nodes)
  viableDonors <- sapply(nodeSolutions, function(i) length(i) >= 2)
  viableDonors <- nodeSolutions[viableDonors]
  
  #No paths with 2 or more nodes, so none can give to away
  if(length(viableDonors) == 0){
    return (NULL)
  }
  
  #Calculating the minimum givable set of 2 nodes load
  donorsMinGive <- c()
  for(each in viableDonors){
    lineIndex <- match(list(each),nodeSolutions)
    ns <- nodeSolutions[[lineIndex]]
    
    loadsAB <- sapply(ns[1:(length(ns)-1)],function(a) a <- sites[ns[match(a,ns)],typeIndex] + sites[ns[match(a,ns)+1],typeIndex])
    donorsMinGive <- c(donorsMinGive,min(loadsAB))
  }
  
  minAB <- min(donorsMinGive)
  
  #Which lines would be able to receive atleast some load
  viableReceivers <- sapply(totalLoads, function(x) x + minAB <= capacity)
  viableReceivers <- nodeSolutions[viableReceivers]
  
  #If no line could receive it
  if(length(viableReceivers) == 0){
    return(NULL)
  }
  
  ### Checks complete, from here on we should be getting a viable solution
  
  #Select a viable receiver
  viableReceiver <- sample(viableReceivers,1)
  randomLine2index <- match(viableReceiver,nodeSolutions)
  ns2 <- nodeSolutions[[randomLine2index]]
  line2load <- totalLoads[[randomLine2index]]
  
  
  #Remove the unviable donor candidates
  viableDonorsNarrowed <- sapply(donorsMinGive, function(x) x + line2load <= capacity )
  viableDonors <- viableDonors[viableDonorsNarrowed]
  
  #Select one of the viable donors
  viableDonor <- sample(viableDonors,1)
  randomLine1index <- match(viableDonor,nodeSolutions)
  ns1 <- nodeSolutions[[randomLine1index]]
  
  #Select, which two nodes to donate
  loadsAB <- sapply(ns1[1:(length(ns1)-1)],function(a) a <- sites[ns1[match(a,ns1)],typeIndex] + sites[ns1[match(a,ns1)+1],typeIndex])
  viableAB <- sapply(loadsAB,function(x) x + line2load <=  capacity)
  bla <- loadsAB[viableAB]
  
  if(length(bla) == 1){bla <- list(bla)}
  oneViable <- sample(bla,1)
  aIndex <- unlist(match(unlist(oneViable),loadsAB))
  bIndex <- aIndex+1
  
  a <- ns1[aIndex]
  b <- ns1[bIndex]
  
  #Insert the two selected nodes into line 2
  ns2new <- insertNodeIntoBestPlace(sites,paths,capacity,ns2,a,type)
  ns2new <- insertNodeIntoBestPlace(sites,paths,capacity,ns2,b,type)
  
  pathSolutions[[randomLine2index]] <- constructFullSolution(sites,paths,ns2new,type)
  nodeSolutions[[randomLine2index]] <- ns2new
  totalLoads[[randomLine2index]] <- totalLoads[[randomLine2index]] + sites[a,typeIndex] + sites[b,typeIndex]
  
  #Rebuild or remove line 1 if necesary 
  if(length(ns1) == 2){
    pathSolutions[[randomLine1index]] <- NULL
    nodeSolutions[[randomLine1index]] <- NULL
    totalLoads[[randomLine1index]] <- NULL
  }
  else{
    indexA <- match(a, ns1)
    indexB <- match(b, ns1)
    
    ns1new <- ns1[-c(indexA,indexB)]
    
    #Repair the path without the lost nodes
    pathSolutions[[randomLine1index]] <- constructFullSolution(sites,paths,ns1new, type)
    nodeSolutions[[randomLine1index]] <- ns1new
    totalLoads[[randomLine1index]] <- ((totalLoads[[randomLine1index]] - sites[a,typeIndex]) - sites[b,typeIndex])
  }
  
  list(pathSolutions,nodeSolutions, totalLoads)
}

randomSwap11 <- function(sites, paths, capacity, megaSolution){
  pathSolutions <- megaSolution[[1]]
  nodeSolutions <- megaSolution[[2]]
  totalLoads <- megaSolution[[3]]
  
  #Pick random lines 1 and 2
  #This is cheap and reapeated until a valid candidates are picked or max iterations is reached
  i <- 1
  maxIter <- 500
  while(1){
    if(i == maxIter) return(NULL)
    
    randomLinesInds <- sample(1:length(pathSolutions),2)
    
    randomLine1index <- randomLinesInds[[1]]
    randomLine2index <- randomLinesInds[[2]]
    
    
    type <- pathSolutions[[randomLine1index]][1]
    typeIndex <- getTypeIndex(colnames(sites),type)
    
    ns1 <- nodeSolutions[[randomLine1index]]
    ns2 <- nodeSolutions[[randomLine2index]]
    
    #If both lines service just one node, no change would happen, retry
    if(length(ns1) == 1 & length(ns2) == 1){
      i <- i + 1
      next;
    }
    
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
      break;
    }
    
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

insertNodeIntoBestPlace <- function(sites,paths, capacity, nodes,node, type){
  bestPlace <- 1
  bestCost <- Inf
  best <- NULL
  for(place in 0:(length(nodes))){
    if(place == 0){
      nodesNew <- c(node, nodes)
    }
    else{
      nodesNew <- c(nodes[1:place],node)
      
      if(place < length(nodes)){
        nodesNew <- c(nodesNew, nodes[(place+1):(length(nodes))])
      }
    }
    
    repaired <- constructFullSolution(sites,paths,nodesNew,type)
    cost <- lineCost(sites,paths,capacity,repaired,nodesNew,type)
    
    if(cost < bestCost){
      bestCost <- cost
      best <- nodesNew
    }
    
  }
  best
}


