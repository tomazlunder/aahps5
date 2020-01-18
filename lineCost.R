source("common.R")

lineCost <- function(sites,paths,maxLoad,line,servicedNodes,type){
  error <- 0
  
  if(line[1] %in% c(1,2,3)){
    line <- line[2:length(line)]
  }

  typeIndex = getTypeIndex(colnames(sites),type)
  
  time <- 0.5 #Starting time due to unloading
  cost <- 10 #Starting fixed cost
  distance <- 0
  load <- 0
  
  alreadyServiced <- c()
  
  last <- 1
  for(site in line[2:length(line)]){
    bestPath <- getBestPathToNeighbor(paths, last, site, load)
    if(bestPath == Inf){
      error <- 1
    }
    distance <- distance + bestPath
    
    #Organic
    if(site %in% servicedNodes & !(site%in% alreadyServiced)){
      load <- load + sites[sites$ID==site,typeIndex]
      alreadyServiced <- c(alreadyServiced, site)
    }
    
    last <- site  
  }
  
  #Fuel cost
  fuelCost <- (distance*0.1)
  cost <- cost + fuelCost
  
  #Time due to pickup
  time <- time + (0.2 * length(servicedNodes))
  
  #Time due to travel
  time <- time + (distance/50)
  
  
  #Worker cost
  if(time < 8){
    workerCost <- time * 10 
  } else {
    workerCost <-  8 * 10
    workerCost <- workerCost + (time - 8) * 20
  }
  cost <- cost + workerCost
  
  #cat(" -Fuel:",fuelCost," for ",distance," km\n")
  #cat(" -Work:",workerCost," for ",time," hours\n")
  
  if(error == 1){
    return(Inf)
  }
  
  return(cost)
}