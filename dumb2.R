dumbSolver2 <- function(sites, paths, capacity){
  mySites <- sites
  
  #Solver function 1
  findFirstUnserved <- function(myPath, load, capacity, typeIndex){
    site <- myPath[length(myPath)]
    searched<<-append(searched, site)
    
    sn <- servableNeighbors(mySites, paths, site, load, capacity, typeIndex)
    if(nrow(sn) > 0){
      closest <- nearestOf(paths, site,sn,load)
      myPath <- append(myPath, closest)
      
      load <- load + mySites[closest,typeIndex]
      mySites[closest,typeIndex] <<- 0 
      
      return (findNextUnserved(myPath,load, capacity, typeIndex))
    } 
    else{
      asn <- servedNeighbors(mySites,paths,site,load,typeIndex)
      for(e in asn$ID){
        if(e %in% searched | e %in% exhausted) next
        
        myPath <- append(myPath, e)
        temp <- findFirstUnserved(myPath,load, capacity, typeIndex)
        if(found == 1){
          return(temp)
        }
        else{
          myPath <- myPath[1:length(myPath)-1]
          exhausted <<- e
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
      
      load <- load + mySites[mySites$ID==closest,typeIndex]
      mySites[closest,typeIndex] <<- 0     
      return(findNextUnserved(myPath,load, capacity, typeIndex))
    } 
    else{
      return(returnToDepot(myPath, load, capacity))
    }
  }
  
  #Solver function 3
  returnToDepot <- function(myPath, load, capacity){
    found <<- 1
    site <- myPath[length(myPath)]
    
    sp <- shortestPathsUnderLoad(sites,paths,load)
    pathToDepot <- extractPath(sp, site, 1)
    myPath<- append(myPath, pathToDepot[2:length(pathToDepot)])
    myPath
  }
  
  
  ### SOLVER
  start_time <- Sys.time()
  
  solution = list()
  
  types = c(1,2,3)
  i <- 1
  for(type in types){
    if(type == 1) typeName <- "Organic"
    else if(type == 2) typeName <- "Plastic"
    else if(type == 3) typeName <- "Paper"
    typeIndex = grep(typeName, colnames(sites))
    
    it <- 1
    #While left to serve > 0
    while(nrow(mySites[mySites[,typeIndex] > 0,]) != 0){
      myPath <- c(1)
      found <- 0
      
      searched <- c()
      exhausted <- c()
      
      route <- findFirstUnserved(myPath, 0, capacity, typeIndex)
      #cat("[",type,",",it,"] ",route,"\n")
      solution[[i]] <- c(type,route)
      i <- i + 1
      it <- it + 1
    }
  }
  end_time <- Sys.time()
  print(end_time - start_time)
  
  solution
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
