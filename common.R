library('e1071') #sp all
library('igraph') #sp from to

getTypeIndex <- function(colnames,type){
  if(type == 1) typeName <- "Organic"
  else if(type == 2) typeName <- "Plastic"
  else if(type == 3) typeName <- "Paper"
  typeIndex = grep(typeName, colnames)
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
  ipaths <- ipaths[ipaths$Capacity >= load,]
  
  
  ipaths <- ipaths[c("ID1","ID2","Distance")]
  colnames(ipaths) <- c("ID1","ID2","weight")
  
  if(nrow(ipaths) == 0){
    a<-3
    #Wtf
  }
  
  ipaths <- aggregate(weight ~ ID1 + ID2, ipaths, FUN = min)
  
  g <- graph_from_data_frame(ipaths, directed = TRUE)
  is_weighted(g)
  
  aa <- shortest_paths(g, from = as.character(from), to= as.character(to), mode = c("out"), weights = ipaths$weight,
                       predecessors = FALSE, inbound.edges = FALSE)
  
  #cc <- as.character(aa[[1]])
  #bb <- strsplit(cc,"`")
  lol <- names(unlist(aa[[1]]))
  lol <- as.numeric(lol)
  #bb <- as.numeric(bb[[1]][c(FALSE,TRUE)])
  
  return(lol)
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

normalToMegaSolution <- function(sites, paths, capacity, solution){
  totalCost <- 0

  solutionType = list() 
  midSolution = list()
  solutionLoad = list()
  
  megaSolution = list()
  
  lastType <- 1
  
  
  it <- 1
  for(line in solution){
      load <- 0
      myServiced <- c()

      type <- line[1]
      
      if(type != lastType){
        megaSolution[[lastType]] <- list(solutionType, midSolution, solutionLoad)
        solutionType = list() 
        midSolution = list()
        solutionLoad = list()

        lastType <- type
        it <- 1
      }

      
      typeIndex <- getTypeIndex(colnames(sites),type)
      
      for(site in line[3:length(line)]){
        #IF pick up
        if((sites[sites$ID==site,typeIndex]+load) <= capacity &
           sites[sites$ID==site,typeIndex] > 0){
          
          load <- load + sites[sites$ID==site,typeIndex]
          sites[sites$ID==site,typeIndex] <- 0 
          myServiced <- c(myServiced, site)
        }
      }
      
      solutionType[[it]] <- line
      midSolution[[it]] <- myServiced
      solutionLoad[[it]] <- load
      
      it <- it + 1
  }
  megaSolution[[type]] <- list(solutionType, midSolution, solutionLoad)
  
  return(megaSolution)
}

insertNodeIntoBestPlace <- function(sites,paths, capacity, nodes,node, type){
  bestPlace <- 1
  bestCost <- Inf
  best <- NULL
  for(place in 0:(length(nodes))){
    if(place == 0){
      nodesNew <- c(node, nodes)
    }
    else{
      nodesNew <- c(nodes[1:place],node)
      
      if(place < length(nodes)){
        nodesNew <- c(nodesNew, nodes[(place+1):(length(nodes))])
      }
    }
    
    repaired <- constructFullSolution(sites,paths,nodesNew,type)
    cost <- lineCost(sites,paths,capacity,repaired,nodesNew,type)
    
    if(cost < bestCost){
      bestCost <- cost
      best <- nodesNew
    }
    
  }
  best
}