source("solutionCheck.R")
source("readFile.R")
source("common.R")
source("dumb1.R")

#Reading file (readFile.R)
file <- "input/Problem1.txt"

capSitesPaths <- readFile(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]

sp <- shortestPathsUnderLoad(sites,paths,capacity)

sol1 <- dumbSolver(sites,paths,100)


### GRAPHING (VISUALIZATION)

##install.packages("igraph")
library('igraph')
g <- graph_from_data_frame(paths, directed = FALSE, vertices = sites)
plot(g)
