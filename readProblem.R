#Reads an input file and returns a list containing
# 1: Vehicle capacity
# 2: Site dataframe
# 3: Path dataframe

readProblem <- function(file){
  #Read num sites and truck capacity
  con <- file(file,"r")
  first_line <- readLines(con,n=1)
  close(con)
  
  first_line <- unlist(strsplit(first_line, ","))
  
  NumSites <- first_line[1]
  Capacity <- as.numeric(as.character(first_line[2]))
  #print(NumSites)
  #print(Capacity)
  
  #Read site and path data
  data <- read.csv(file=file, header = FALSE,skip=1)
  
  #Split data into sites and paths
  sites <- data[!is.na(data$V6),]
  paths <- data[is.na(data$V6),]
  
  paths$V6 <- NULL
  rownames(paths) <- NULL
  
  colnames(sites) <- c("ID","X","Y","Organic","Plastic","Paper")
  colnames(paths) <- c("ID1","ID2", "Distance","OneWay","Capacity")
  
  sites$ID <- as.numeric(as.character(sites$ID))
  sites$X <- as.numeric(as.character(sites$X))
  sites$Y <- as.numeric(as.character(sites$Y))
  sites$Organic <- as.numeric(as.character(sites$Organic))
  sites$Plastic <- as.numeric(as.character(sites$Plastic))
  sites$Paper <- as.numeric(as.character(sites$Paper))

  paths$ID1 <- as.numeric(as.character(paths$ID1))
  paths$ID2 <- as.numeric(as.character(paths$ID2))
  
  paths$Distance <- as.numeric(as.character(paths$Distance))
  paths$OneWay <- as.numeric(as.character(paths$OneWay))
  paths$Capacity <- as.numeric(as.character(paths$Capacity))
  
  toReturn <- list(Capacity, sites, paths)
  toReturn
}