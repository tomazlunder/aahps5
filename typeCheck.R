source("common.R")

solutionCheckType <- function(sites, paths, solution, maxLoad, type){
  printMid <- 0
  totalCost <- 0
  lineIndex <-1
  
  error <- 0
  notStartingAtDepot <- c()
  notEndingAtDepot <- c()
  
  typeIndex = getTypeIndex(colnames(sites),type)
  
  for(line in solution){
    time <- 0.5 #Starting time due to unloading
    cost <- 10 #Starting fixed cost
    distance <- 0
    load <- 0

    if(line[2] != 1){
      cat("Line ", line, " does not start at depot! [ERROR]")
      error <- 1
    }
    
    last <- 1
    for(site in line[3:length(line)]){
      bestPath <- getBestPathToNeighbor(paths, last, site, load)
      if(bestPath == Inf){
        #cat("ERROR: Valid path [",last,"->",site,"] from solution line[",lineIndex,"] not found!\n")
        error <- 1
        return(Inf)
      }
      distance <- distance + bestPath
      
      #Load
      if((sites[sites$ID==site,typeIndex]+load) <= maxLoad &
         sites[sites$ID==site,typeIndex] > 0){
        load <- load + sites[sites$ID==site,typeIndex]
        sites[sites$ID==site,typeIndex] <- 0 
        time <- time + 0.2
      }
      
      last <- site  
    }
    
    if(last != 1){
      notEndingAtDepot <- append(notEndingAtDepot,lineIndex)
      error <- 1
      return(Inf)
    }
    
    #Time due to travel
    time <- time + (distance/50)
    
    #Fuel cost
    fuelCost <- (distance*0.1)
    
    if(printMid) cat(" -Fuel:",fuelCost," for ",distance," km\n")
    cost <- cost + fuelCost
    
    #Worker cost
    if(time < 8){
      workerCost <- time * 10 
    } else {
      workerCost <-  8 * 10
      workerCost <- workerCost + (time - 8) * 20
    }
    
    if(printMid) cat(" -Work:",workerCost," for ",time," hours\n")
    cost <- cost + workerCost
    
    #Add total line cost to total cost
    #cat("Line ",lineIndex,"cost: ",cost,"\n")
    totalCost <- totalCost + cost
    
    lineIndex <- lineIndex + 1
  }
  
  cat("Total cost:",totalCost," [RESULT]\n")
  
  #IF PROGRAM GOT TO THIS POINT, THE LINES WERE OK
  cat("All paths taken by all lines were valid, following one-way and load rules. [OK]\n")
  
  #ERROR CHECK - NOT STARTING/ENDING AT DEPOT
  for(e in notStartingAtDepot){
    cat("Line ",e," doesn't start at depot! [ERROR]\n")
  }
  if(length(notStartingAtDepot) == 0){
    cat("All lines start at the depot! [OK]\n")
  }
  
  for(e in notEndingAtDepot){
    cat("Line ",e," doesn't end at depot! [ERROR]\n")
  }
  if(length(notEndingAtDepot) == 0){
    cat("All lines end at the depot! [OK]\n")
  }
  
  #ERROR CHECK - ALL GARBAGE COLLECTED
  uncollected <- sites[sites[,typeIndex] >0,]
  
  if(nrow(uncollected) >0){
    cat("Uncollected garbage detected! [FAIL]\n")
    error <-  1
    return(Inf)
    #print(uncollected)
  } else {
    cat("All garbage was collected! [OK]\n")
  }
  
  if(error) return(Inf)
  
  return(totalCost)
}

