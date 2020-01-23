source("common.R")
source("readProblem.R")

source("solutionCheck.R")
source("typeCheck.R")
source("lineCost.R")

source("initialSolver.R")
source("localSearch.R")
source("mutators.R")

files <- list("Problem1.txt", "Problem2.txt", "Problem3.txt", "Problem4.txt", "Problem5.txt",
              "Problem6.txt", "Problem7.txt", "Problem8.txt", "Problem9.txt", "Problem10.txt")

res <- list()

for(file in files){
  path <- paste("input/",file,sep="")
  
  capSitesPaths <- readProblem(path)
  capacity <- capSitesPaths[[1]]
  sites <- capSitesPaths[[2]]
  paths <- capSitesPaths[[3]]
  
  initialSolution <- initialSolver(sites,paths,capacity)
  solutionCheck(sites,paths,megaToNormalSolution(initialSolution),capacity)
  newSolution <- localSearch(sites,paths,capacity,initialSolution)
  
  res <- append(res,newSolution)
}
