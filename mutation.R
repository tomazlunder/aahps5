removeAndReinsert <- function(solution){
  originalSolution <- solution
  
  randomLines <- sample(solution,2)
  randomLine1 <- randomLines[[1]]
  randomLine2 <- randomLines[[2]]
  
  type <- randomLine1[1]
  
  rl1 <- randomLine1[2:length(randomLine1)]
  rl2 <- randomLine2[2:length(randomLine2)]

  #Pick a random node
  if(length(rl1) == 3){
    randomNode <- rl1[2]
    solution <- solution[match(list(randomLine1),solution)] <- NULL
    
  }
  else{
    randomNode <- sample(rl1[2:(length(rl1)-1)],1)
    index <- match(randomNode, rl1)
  
    line1new <- rl1[-index]
    solution[match(list(randomLine1),solution)] <- list(line1new)
  }

  randomPlace <- sample(2:length(rl2),1)
  line2new <- c(rl2[1:randomPlace],randomNode)

  if(randomPlace != length(rl2)){
    c(line2new ,rl2[randomPlace+1:length(rl2)])
  }
  
  solution[match(list(randomLine2),solution)] <- list(c(type,line2new))
  
  solution
  #Pick a random line 2
}