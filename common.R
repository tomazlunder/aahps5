library('e1071') #sp all
library('igraph') #sp from to


getTypeIndex <- function(colnames,type){
  if(type == 1) typeName <- "Organic"
  else if(type == 2) typeName <- "Plastic"
  else if(type == 3) typeName <- "Paper"
  typeIndex = grep(typeName, colnames(sites))
}

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

shortestToFromUnderLoad <- function(paths,from,to,load){
  ipaths <- paths
  tw <- paths[paths$OneWay == 0,]
  tw <- tw[c("ID2","ID1","Distance","OneWay","Capacity")]
  colnames(tw) = c("ID1","ID2","Distance","OneWay","Capacity")
  ipaths <- rbind(ipaths, tw)
  ipaths <- ipaths[ipaths$Capacity >= capacity,]
  
  
  ipaths <- ipaths[c("ID1","ID2","Distance")]
  colnames(ipaths) <- c("ID1","ID2","weight")
  
  ipaths <- aggregate(weight ~ ID1 + ID2, ipaths, FUN = min)
  
  g <- graph_from_data_frame(ipaths, directed = TRUE)
  is_weighted(g)
  
  aa <- shortest_paths(g, from = as.character(from), to= as.character(to), mode = c("out"), weights = ipaths$weight,
                       predecessors = FALSE, inbound.edges = FALSE)
  
  cc <- as.character(aa[[1]])
  bb <- strsplit(cc,"`")
  bb <- as.numeric(bb[[1]][c(FALSE,TRUE)])
  
  return(bb)
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

constructFullSolution <- function(sites, paths, midSolution, type){
  load <- 0
  path <- c(1)
  cur <- 1
  
  typeIndex = getTypeIndex(colnames(sites),type)
  
  for(node in midSolution){
    #sp <- shortestPathsUnderLoad(sites,paths,load)
    #curToNode <- extractPath(sp,cur,node)
    curToNode <- shortestToFromUnderLoad(paths,cur,node,load)
    curToNode <- curToNode[2:length(curToNode)]
    
    load <- load + sites[sites$ID==node,typeIndex]
    
    path <- c(path, curToNode)
    cur <- node
  }
  
  path <- c(type,path, 1)
  path
}

megaToNormalSolution <- function(megaSolution){
  pathSolutions <- list()
  for(e in megaSolution){
    pathSolutions <- append(pathSolutions, e[[1]])
  }
  pathSolutions
}