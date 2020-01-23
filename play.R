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

