source("common.R")
source("readProblem.R")

source("solutionCheck.R")
source("typeCheck.R")
source("lineCost.R")

source("initialSolver.R")
source("localSearch.R")
source("simulatedAnnealing.R")
source("readWriteSolution.R")

#Reading file
file <- "input/Problem3.txt"

capSitesPaths <- readProblem(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]

initialSolution <- initialSolver(sites,paths,capacity)
solutionCheck(sites,paths,capacity,megaToNormalSolution(initialSolution))
newSolution <- localSearch(sites,paths,capacity,initialSolution, printIT = TRUE)
newSolution <- simulatedAnnealing(sites,paths,capacity,initialSolution, printIT = TRUE)
solutionCheck(sites,paths,capacity,megaToNormalSolution(newSolution))

writeSolution(newSolution,"output/manual/Problem2_.txt")
loaded <- readSolution("output/manual/a.txt")
solutionCheck(sites,paths,capacity,loaded)