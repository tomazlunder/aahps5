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

test <- aggregate(weight ~ ID1 + ID2, ipaths, FUN = min)


g <- graph_from_data_frame(ipaths, directed = TRUE, vertices = sites)
plot(g)

aa <- shortest_paths(g, "1", to = "4", mode = c("out"), weights = ipaths$weight,
                     predecessors = FALSE, inbound.edges = FALSE)


mutate2(sites,paths,capacity,type1)
removeAndReinsert2(sites,paths,capacity,type1)
solutionCheckType(sites,paths,type1[[1]], capacity, 1)

dummySol <- list(c(1,1,4,1),c(1,1,3,1),c(1,1,5,1),c(1,1,2,1),c(2,1,4,2,3,1),c(2,1,5,1),c(3,1,4,2,5,1),c(3,1,3,1))
dummySol <- list(c(1,1,4,2,3,1),c(1,1,5,1),c(1,1,2,1),c(2,1,4,2,3,1),c(2,1,5,1),c(3,1,4,2,5,1),c(3,1,3,1))