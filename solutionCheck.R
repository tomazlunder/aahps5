source("common.R")

solutionCheck <- function(sites, paths, solution, maxLoad, printMid = FALSE){
  totalCost <- 0
  lineIndex <-1
  
  error <- 0
  notStartingAtDepot <- c()
  notEndingAtDepot <- c()
  
  for(line in solution){
    time <- 0.5 #Starting time due to unloading
    cost <- 10 #Starting fixed cost
    distance <- 0
    load <- 0
    type <- line[1]

    typeIndex <- getTypeIndex(colnames(sites),type)
    
    if(line[2] != 1){
      cat("Line ", line, " does not start at depot! [ERROR]")
      notStartingAtDepot <- append(notStartingAtDepot,lineIndex)
      error <- 1
    }
    
    last <- 1
    for(site in line[3:length(line)]){
      bestPath <- getBestPathToNeighbor(paths, last, site, load)
      if(bestPath == Inf){
        cat("ERROR: Path [",last,"->",site,"] from solution line[",lineIndex,"] not found!\n",
        "(Could also be breaking the road capacity rule) Terminating... [FAIL]\n")
        error <- 1
        stop()
      }
      distance <- distance + bestPath
      

      #IF pick up
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
    cat("Line ",lineIndex,"cost: ",cost,"\n")
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
  uncollected <- sites[which(sites$Organic>0 |
                               sites$Plastic>0 |
                                sites$Paper>0),]
  
  if(nrow(uncollected) >0){
    cat("Uncollected garbage detected! [FAIL]\n")
    error <-  1
    print(uncollected)
  } else {
    cat("All garbage was collected! [OK]\n")
  }
  
  if(error == 1) return(Inf)
  
  return(totalCost)
}

