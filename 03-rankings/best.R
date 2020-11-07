# computes best hospital within a state for a given health outcome (heart attack, heart failure or pneumonia)
best <- function(state,outcome) {
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  mystate <- data[data[,7]==state,]
  if (length(mystate)==0) {
    stop("invalid state")
  }
  if (outcome=="heart attack") {
    choosecol  <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome=="heart failure") {
    choosecol <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome=="pneumonia") {
    choosecol <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  } 
  mystate <- mystate[order(mystate$Hospital.Name),] # sort to resolve ties alphabetically below
  myoutcome <- mystate[,choosecol]
  myoutcome <- replace(myoutcome, myoutcome == "Not Available", NA)
  myoutcome <- as.numeric(unlist(myoutcome))
  minval <- min(myoutcome,na.rm=TRUE)
  minidx <- which(myoutcome==minval)
  best <- mystate[minidx[1],2]
}
