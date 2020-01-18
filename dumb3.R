source("common.R")

dumbSolver3 <- function(sites, paths, capacity, types = c(1,2,3)){
  mySites <- sites
  
  #Solver function 1
  findFirstUnserved <- function(myPath, load, capacity, typeIndex){
    site <- myPath[length(myPath)]
    searched<<-append(searched, site)
    
    sn <- servableNeighbors(mySites, paths, site, load, capacity, typeIndex)
    if(nrow(sn) > 0){
      closest <- nearestOf(paths, site,sn,load)
      myPath <- append(myPath, closest)
      
      #Serve the node
      load <- load + mySites[closest,typeIndex]
      mySites[closest,typeIndex] <<- 0 
      myServed <<- c(myServed, closest)
      
      return (findNextUnserved(myPath,load, capacity, typeIndex))
    } 
    else{
      asn <- servedNeighbors(mySites,paths,site,load,typeIndex)
      for(e in asn$ID){
        if(e %in% searched) next
        
        myPath <- append(myPath, e)
        temp <- findFirstUnserved(myPath,load, capacity, typeIndex)
        if(found == 1){
          return(temp)
        }
        else{
          myPath <- myPath[1:length(myPath)-1]
        }
      }
      return -1 #Not found (something went wrong)
    }
  }
  
  #Solver function 2
  findNextUnserved <- function(myPath, load, capacity, typeIndex){
    site <- myPath[length(myPath)]
    
    sn <- servableNeighbors(mySites, paths, site, load, capacity, typeIndex)
    if(nrow(sn) > 0){
      closest <- nearestOf(paths, site,sn,load)
      myPath <- append(myPath, closest)
      
      #Serve the node
      load <- load + mySites[mySites$ID==closest,typeIndex]
      mySites[closest,typeIndex] <<- 0     
      myServed <<- c(myServed, closest)
      
      return(findNextUnserved(myPath,load, capacity, typeIndex))
    } 
    else{
      return(returnToDepot(myPath, load, capacity, typeIndex))
    }
  }
  
  #Solver function 3
  returnToDepot <- function(myPath, load, capacity, typeIndex){
    found <<- 1
    site <- myPath[length(myPath)]
    
    sp <- shortestPathsUnderLoad(sites,paths,load)
    pathToDepot <- extractPath(sp, site, 1)
    
    #If he can pick up something on the way back
    if(length(pathToDepot) > 2){
      midNodes <- pathToDepot[2:(length(pathToDepot)-1)]
      i <- 1
      for(node in midNodes){
        if(mySites[mySites$ID==node,typeIndex] > 0 & mySites[mySites$ID==node,typeIndex] <(capacity-load)){
          
          #Serve the node
          load <- load + mySites[mySites$ID==node,typeIndex]
          mySites[node,typeIndex] <<- 0     
          myServed <<- c(myServed, site)
          
          myPath<- append(myPath, midNodes[1:i])
          
          return(returnToDepot(myPath,load,capacity,typeIndex))
        }
        i <- i + 1
      }
    }
    
    #Else, go home :)
    myPath<- append(myPath, pathToDepot[2:length(pathToDepot)])
    myPath
  }
  
  
  ### SOLVER
  start_time <- Sys.time()
  
  i <- 1
  megaSolution <- list()
  for(type in types){
    typeIndex = getTypeIndex(colnames(sites),type)
    
    it <- 1
    solution = list()
    midSolution = list()
    
    #While left to serve > 0
    while(nrow(mySites[mySites[,typeIndex] > 0,]) != 0){
      myPath <- c(1)
      myServed <- c() #For mid-solution
      
      found <- 0
      searched <- c()
      route <- findFirstUnserved(myPath, 0, capacity, typeIndex)
      #cat("[",type,",",it,"] ",route,"\n")
      solution[[it]] <- c(type,route)
      midSolution[[it]] <- myServed
      
      it <- it + 1
      i <- i + 1
    }
    
    megaSolution[[type]] <- list(solution,midSolution)
    
    
  }
  end_time <- Sys.time()
  print(end_time - start_time)
  
  megaSolution
}

#Helper functions
servableNeighbors <- function(sites, paths, site, load, capacity, typeIndex){
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
