source("solutionCheck.R")
source("readFile.R")
source("common.R")
source("dumb1.R")
source("dumb2.R")
source("dumb3.R")
source("mutation.R")
source("mutators.R")
source("typeCheck.R")
source("lineCost.R")

install.packages("dplyr")
library(dplyr)

#Reading file (readFile.R)
file <- "input/Problem1.txt"

capSitesPaths <- readFile(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]

sp <- shortestPathsUnderLoad(sites,paths,capacity)
extractPath(sp,4,3)
ep <- shortestToFromUnderLoad(paths,1,4,capacity)

#sol1 <- dumbSolver(sites,paths,capacity)
#solutionCheck(sites,paths,sol1[[1]],capacity,1)

#lineCost(sites,paths,capacity,sol1[[1]][[1]],sol1[[2]][[1]])

#midSolution <- sol2[[2]]


### GRAPHING (VISUALIZATION)

##install.packages("igraph")
library('igraph')
ipaths <- paths
tw <- paths[paths$OneWay == 0,]
tw <- tw[c("ID2","ID1","Distance","OneWay","Capacity")]
colnames(tw) = c("ID1","ID2","Distance","OneWay","Capacity")
ipaths <- rbind(ipaths, tw)
#ipaths <- ipaths[ipaths$Capacity >= capacity,]


ipaths <- ipaths[c("ID1","ID2","Distance")]
colnames(ipaths) <- c("ID1","ID2","weight")

test <- ipaths %>% group_by("ID1", "ID2") %>%
  slice(which.min(ipaths$weight))

test <- aggregate(weight ~ ID1 + ID2, ipaths, FUN = min)


g <- graph_from_data_frame(ipaths, directed = TRUE, vertices = sites)
plot(g)

aa <- shortest_paths(g, "1", to = "4", mode = c("out"), weights = ipaths$weight,
                     predecessors = FALSE, inbound.edges = FALSE)


mutate2(sites,paths,capacity,type1)
removeAndReinsert2(sites,paths,capacity,type1)
solutionCheckType(sites,paths,type1[[1]], capacity, 1)

sol3 <- dumbSolver3(sites,paths,capacity)
solutionCheck(sites,paths,megaToNormalSolution(sol3),capacity)
res <- mutate2(sites,paths,capacity,sol3)
solutionCheck(sites,paths,res[[1]],capacity)

dummySol <- list(c(1,1,4,1),c(1,1,3,1),c(1,1,5,1),c(1,1,2,1),c(2,1,4,2,3,1),c(2,1,5,1),c(3,1,4,2,5,1),c(3,1,3,1))
dummySol <- list(c(1,1,4,2,3,1),c(1,1,5,1),c(1,1,2,1),c(2,1,4,2,3,1),c(2,1,5,1),c(3,1,4,2,5,1),c(3,1,3,1))

solutionCheck(sites,paths,dummySol,capacity)

mutate2 <- function(sites,paths,capacity, megaSolution){
  invisible(capture.output( orgCost <- solutionCheck(sites,paths,megaToNormalSolution(megaSolution),capacity) ))

  cat("Original solution cost:", orgCost,"\n")
  
  totalCost <- 0
  
  for(type in c(1,2,3)){
    typeSolutions <- megaSolution[[type]]
    pathSolutions <- typeSolutions[[1]] #aka the initial solution
    
    #Cost of initial solution for type
    invisible(capture.output( bestCost <- solutionCheckType(sites,paths,pathSolutions, capacity, type) ))
    best <- pathSolutions
    
    cat(" Initial best cost for type", type, ": ", bestCost, "\n")
    
    #For stats
    invalidAttempt <- 0
    worseAttempt <- 0
    
    i <- 1
    while(1){
      solNew <- removeAndReinsert2(sites,paths,capacity,typeSolutions)
      #Check validity and cost of newly constructed solution
      invisible(capture.output( res <- solutionCheckType(sites,paths,solNew[[1]],capacity,type) ))
      
      #If the cost is better, replace best known solution
      if(res < bestCost){
        costDecrease <- bestCost - res
        cat("New best found ", costDecrease, " decrease in cost!!! Prev: (",bestCost,")\n")
        bestCost <- res
        best <- solNew
        
        #Reset iteration counter to 1
        i <- 1
      } else{
        if(is.infinite(res)) invalidAttempt <- invalidAttempt + 1
        else worstAttempt <- worseAttempt + 1  
      }
      
      
      i <- i + 1
      if(i == 500){
        cat("No more changes for type", type,". Invalid Attempts:",invalidAttempt,"Worse Attempts:", worseAttempt,"\n")
        break
      }
    }
    
    totalCost <- totalCost + bestCost
  }
  
  cat("Toal cost after execution: ", totalCost, " Change of: ",(totalCost-orgCost),"\n")
  solNew
}