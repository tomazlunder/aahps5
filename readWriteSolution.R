writeSolution <- function(megaSolution,filepath){
  fileConn<-file(filepath)
  cat("",file=filepath,sep="")
  
  for(type in c(1,2,3)){
    typeSolution <- megaSolution[[type]]
    pathSolutions <- typeSolution[[1]] #aka the initial solution
  
    for(path in pathSolutions){
      cat(path,"\n",file=filepath,append = TRUE)
    }
  }
  
  close(fileConn)
}

readSolution <- function(filepath){
  solution <- list()
  
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    #print(line)
    nodes <- strsplit(line," ")
    solution <- append(solution, list(as.numeric(unlist(nodes))))
  }
  
  close(con)
  
  solution
}