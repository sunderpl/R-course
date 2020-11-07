# computes top num hospitals within a state for a given health outcome (heart attack, heart failure or pneumonia)
# default is top 1 (ie the best)
rankhospital <- function(state,outcome,num="best") {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  if (length(data[data[,7]==state,])==0) {
    stop("invalid state")
  }
  if (outcome=="heart attack") {
    choosecol  <- 11
  } else if (outcome=="heart failure") {
    choosecol <- 17
  } else if (outcome=="pneumonia") {
    choosecol <- 23
  } else {
    stop("invalid outcome")
  } 
  
  data <- data[data[,7]==state,c(2,choosecol)] # only keep rows for chosen state and relevant columns
  data <- replace(data, data == "Not Available", NA)
  data <- data[!is.na(data[,2]),] # take out NA values
  data[, 2] <- sapply(data[, 2], as.numeric) # make outcome numeric
  #data <- data[order(data[,2]),] # sort by outcome 
  data <- data[with(data,order(data[,2],data[,1])),] # sort by outcome, then hospital to break ties
  
  lowestrank <- nrow(data)
  if (num=="best") {
    rank <- 1
  } else if (num=="worst") {
    rank <- lowestrank
  } else if (num>lowestrank) {
    return(NA)
  } else {
    rank <- num
  }
  
  rankhospital <- data[rank,1]
}
