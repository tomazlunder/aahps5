# Tests based on Problem1.txt ---
source("solutionCheck.R")
source("readFile.R")

file = "input/Problem1.txt"
capSitesPaths <- readFile(file)
capacity <- capSitesPaths[[1]]
sites <- capSitesPaths[[2]]
paths <- capSitesPaths[[3]]


# Test1: OK Solution
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1),c(1,1,5,1),
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,2,1),c(3,1,3,1),c(3,1,4,1),c(3,1,5,1))
solutionCheck(sites,paths,testSol,capacity)

# Test2: Solution with missing path
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1),c(1,1,5,4,1), #<- (5->4) doesn't exist
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,2,1),c(3,1,3,1),c(3,1,4,1),c(3,1,5,1))
solutionCheck(sites,paths,testSol,capacity)

# Test3: Solution with garbage left over
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1), #<- (Organic from 5 not collected)
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,2,1),c(3,1,3,1),c(3,1,4,1),c(3,1,5,1))
solutionCheck(sites,paths,testSol,capacity)

# Test4: Solution trying to use a road with load limit, while over it
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1),c(1,1,5,1),
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,3,1),c(3,1,5,2,4,1)) #<- (1,5,2,4 - fills up plastic so much, that the (4->1 road cant handle it))
solutionCheck(sites,paths,testSol,capacity)

# Test5: One line doesn't end at depot
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1),c(1,1,5), #<- (1,5) - doesn't end at depot (1)
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,2,1),c(3,1,3,1),c(3,1,4,1),c(3,1,5,1))
solutionCheck(sites,paths,testSol,capacity)

# Test6: One line doesn't start at depot
testSol<- list(c(1,1,2,1),c(1,1,3,1),c(1,1,4,1),c(1,5,1), #<- (5,1) - doesn't start at depot (1)
               c(2,1,2,1),c(2,1,3,1),c(2,1,4,1),c(2,1,5,1),
               c(3,1,2,1),c(3,1,3,1),c(3,1,4,1),c(3,1,5,1))
solutionCheck(sites,paths,testSol,capacity)