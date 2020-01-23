source("common.R")
source("readProblem.R")

source("solutionCheck.R")
source("typeCheck.R")
source("lineCost.R")

source("initialSolver.R")
source("localSearch.R")
source("mutators.R")

#Reading file
file <- "input/Problem2.txt"

capSitesPaths <- readProblem(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]

initialSolution <- initialSolver(sites,paths,capacity)
solutionCheck(sites,paths,megaToNormalSolution(initialSolution),capacity)
newSolution <- localSearch(sites,paths,capacity,initialSolution, printIT = TRUE)
solutionCheck(sites,paths,megaToNormalSolution(newSolution),capacity)
