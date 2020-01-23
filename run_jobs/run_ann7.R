setwd("D:/Projects/R Studio/naloga5")
source("common.R")
source("readProblem.R")

source("solutionCheck.R")
source("typeCheck.R")
source("lineCost.R")

source("initialSolver.R")
source("localSearch.R")
source("simulatedAnnealing.R")
source("mutators.R")

source("readWriteSolution.R")

#Reading file
file <- "input/Problem7.txt"

capSitesPaths <- readProblem(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]

initialSolution <- initialSolver(sites,paths,capacity)
solutionCheck(sites,paths,capacity,megaToNormalSolution(initialSolution))
newSolution <- simulatedAnnealing(sites,paths,capacity,initialSolution, printIT = TRUE)
solutionCheck(sites,paths,capacity,megaToNormalSolution(newSolution))

writeSolution(newSolution,"output/manual/annProblem7.txt")