source("common.R")
library(dplyr)

initialSolver <- function(sites, paths, capacity, types = c(1,2,3)){
  #Used to find closest unserved neighbor
  sp <- shortestPathsUnderLoad(sites,paths,0);
  cat("Initial solver: created shortest paths under load")
  
  ### SOLVER
  start_time <- Sys.time()
  
  i <- 1
  megaSolution <- list()
  for(type in types){
    typeIndex = getTypeIndex(colnames(sites),type)
    
    it <- 1
    #Each type creates a solution and midSolution list
    solutionType = list() #A solution containing the full path (final solution)
    midSolution = list() #A list of nodes in order of service (without un-serviced nodes)
    solutionLoad = list() #A list of final loads for solutions
    
    #Unserviced nodes
    unserviced <- sites[sites[,typeIndex] > 0,]
    #Unserviced nodes ordered by distance from depot
    distances <- sp$length[1,sites$ID]
    unserviced <- mutate(unserviced, distanceFromDepot = (distances[ID]))
    unserviced <- unserviced[order(unserviced$distanceFromDepot),]
    
    it <- 1
    while(nrow(unserviced) > 0){
      current <- 1
      myPath <- c()
      myServiced <- c()
      load <- 0
      
      #Get path from closest unserviced
      closest <- unserviced[1,1]
      myPath <- append(myPath,extractPath(sp,current,closest))
      
      #Serve the node
      current <- closest
      load <- load + sites[sites$ID==current,typeIndex]
      sites[current,typeIndex] <- 0 
      myServiced <- c(myServiced, current) #For midSolution
      unserviced <- unserviced[-1,] #Remove 
      
      #Update current position
      current <- closest
      
      #Check if any neighbors can be served
      sn <- servableNeighbors(sites, paths, capacity, current, load, typeIndex)
      while(nrow(sn) > 0){
        #Get path to the closest of unserved neighbors, add it to own path
        closest <- nearestOf(paths,current,sn,load)
        myPath <- append(myPath, closest)
        
        #Serve the node
        current <- site <- myPath[length(myPath)]
        load <- load + sites[sites$ID==current,typeIndex]
        sites[current,typeIndex] <- 0     
        myServiced <- c(myServiced, current)
        unserviced <- unserviced[unserviced$ID != current,];
        
        #Update servable neighbors and try again
        sn <- servableNeighbors(sites, paths, capacity, current, load, typeIndex)
      }
      
      #When no more neighobrs can be serviced, the last serviced node was reach
      #Only thing left is to return to depot
      pathToDepot <- shortestToFromUnderLoad(paths,current,1,load)
      myPath <- append(myPath, pathToDepot[2:length(pathToDepot)])
      
      #Add the line to the list of solutions for the type
      solutionType[[it]] <- c(type,myPath)
      midSolution[[it]] <- myServiced
      solutionLoad[[it]] <- load

      it <- it+1
    }
    
    #Adds the full and mid sollution for current type to the mega solution
    megaSolution[[type]] <- list(solutionType,midSolution,solutionLoad)
  }
  end_time <- Sys.time()
  cat("Initial solution generated in",(end_time - start_time),"\n")

  megaSolution

}

#Helper functions
servableNeighbors <- function(sites, paths, capacity, site, load, typeIndex){
  neighbors <- getNeighbors(paths,site, load)
  select <- sites[which(sites$ID %in% neighbors &
                          sites[,typeIndex] > 0 &
                          sites[,typeIndex] < (capacity-load)),]
  
  select
}

servedNeighbors <- function(sites, paths, site,load,typeIndex){
  neighbors <- getNeighbors(paths,site,load)
  
  select <- sites[which(sites$ID %in% neighbors &
                          sites[,typeIndex] == 0),]
  
  select
}

nearestOf <- function(paths, site, options, load){
  if(nrow(options) == 0){
    print("nearestOf - no options given [ERR]")
    return()
  }
  
  shortest <- NULL
  dist <- Inf
  
  for(option in options$ID){
    optLen <- getBestPathToNeighbor(paths,site, option, load)
    if(optLen < dist){
      dist <- optLen
      shortest <- option
    }
  }
  
  shortest
}
