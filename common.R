library('e1071')

getBestPathToNeighbor <- function(paths,from,to,load){
  op1 <- paths[which(
    paths$ID1==from & 
      paths$ID2==to &
      paths$Capacity >= load), ]
  
  op2 <- paths[which(
    paths$ID1==to & 
      paths$ID2==from &
      paths$OneWay== 0 &
      paths$Capacity >= load), ]
  
  total <- rbind(op1,op2)
  
  if(nrow(total) == 0){
    return(Inf)
  }
  
  min(total$Distance)
}

shortestPathsUnderLoad <- function(sites,paths,load){
  x <- matrix(NA, nrow(sites), nrow(sites))
  diag(x) <- 0
  for(site in sites$ID){
    neighbors <- getNeighbors(paths,site,load)
    #print(neighbors)
    
    for(neighbor in neighbors){
      x[site,neighbor] <- getBestPathToNeighbor(paths,site,neighbor,load)
    }
  }
  
  z <- allShortestPaths(x)
  #print(z)
  z
}

getNeighbors <- function(paths, site, load){
  #load <- as.character(load)
  op1 <- paths[which(
    paths$ID1==site &
      paths$Capacity >= load), ]
  
  op2 <- paths[which(
    paths$ID2==site &
      paths$OneWay== 0 &
      paths$Capacity >= load), ]
  
  neighbors <- c(op1$ID2, op2$ID1)
  #cat("site ",site,": neighbors:",neighbors,"\n") #DBG
  neighbors
}