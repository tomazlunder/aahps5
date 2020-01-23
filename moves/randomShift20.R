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