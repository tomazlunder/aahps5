source("common.R")
source("readProblem.R")

source("solutionCheck.R")
source("typeCheck.R")
source("lineCost.R")

source("initialSolver.R")
source("localSearch.R")
source("mutators.R")

source("readWriteSolution.R")

files <- list("Problem1.txt", "Problem2.txt", "Problem3.txt", "Problem4.txt", "Problem5.txt",
              "Problem6.txt", "Problem7.txt", "Problem8.txt", "Problem9.txt", "Problem10.txt")

res <- list()

for(filename in files){
  path <- paste("input/",filename,sep="")
  
  capSitesPaths <- readProblem(path)
  capacity <- capSitesPaths[[1]]
  sites <- capSitesPaths[[2]]
  paths <- capSitesPaths[[3]]
  
  initialSolution <- initialSolver(sites,paths,capacity)
  solutionCheck(sites,paths,capacity,megaToNormalSolution(initialSolution))
  newSolution <- localSearch(sites,paths,capacity,initialSolution)
  #solutionCheck(sites,paths,capacity,megaToNormalSolution(newSolution))
  
  #Write to file
  path <- paste("output/alg1/",filename,sep="")
  writeSolution(newSolution,path)
  
  res <- append(res,newSolution)
}

res
