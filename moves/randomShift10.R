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
