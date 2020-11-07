# computes top num hospitals for the entire US for a given health outcome (heart attack, heart failure or pneumonia)
# default is top 1 (ie the best)
rankall <- function(outcome,num="best") {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  if (outcome=="heart attack") {
    choosecol  <- 11
  } else if (outcome=="heart failure") {
    choosecol <- 17
  } else if (outcome=="pneumonia") {
    choosecol <- 23
  } else {
    stop("invalid outcome")
  } 
  
  #data <- data[data[,7]==state,c(2,choosecol)] # only keep rows for chosen state and relevant columns
  data <- data[,c(2,choosecol,7)] # only keep rows for relevant columns
  names(data)[2] <- "outcome" # rename outcome column for easier subsetting
  data <- replace(data, data == "Not Available", NA)
  data <- data[!is.na(data[,2]),] # take out NA values
  data[, 2] <- sapply(data[, 2], as.numeric) # make outcome numeric
  
  data[,3] <- as.factor(data[,3])

  if (num=="best") {
    ordered <- data[with(data,order(data[,3],data[,2],data[,1])),] # sort by state, then outcome ASCENDING, then hospital to break ties
    splitdata <- split(ordered, ordered$State)
    temp1 <- lapply(splitdata,function(x) return(x[1,1]))
  } else if (num=="worst") {
    ordered <- data[with(data,order(data[,3],-data[,2],data[,1])),] # sort by state, then outcome DESCENDING, then hospital to break ties
    splitdata <- split(ordered, ordered$State)
    temp1 <- lapply(splitdata,function(x) return(x[1,1]))
  } else { # this returns NA if num exceeds number of hospitals in state
    ordered <- data[with(data,order(data[,3],data[,2],data[,1])),] # sort by state, then outcome ASCENDING, then hospital to break ties
    splitdata <- split(ordered, ordered$State)
    temp1 <- lapply(splitdata,function(x) return(x[num,1]))
  }

  temp2 <- unlist(temp1)
  temp3 <- names(temp1)
  
  return(data.frame(hospital=temp2, state=temp3, row.names=temp3))
}
