# this script computes different rankings of hospitals by health outcomes
# outcome data used is "outcome-of-care-measures.csv"
# functions to rank are best.r, rankhospital.r and rankall.r

setwd("C:/Users/xcb652/Dropbox/Coursera/R_course/assignment3")

outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character")
head(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

# find best hospital in a state for a given outcome
# best: lowest 30 day mortality. 
# state: 2 character
# outcome: heart attack, heart failure, pneumonia. mortality and readmission for each

# returns Hospital.Name. column 2
# parameter 1 is State. column 7
# parameter 2 is col 11 heart attack, col 17 heart failure, col 23 pneumonia

# ---------------------------
source("best.R")
test <- best("TX","heart failure")
test <- best("MD","pneumonia")

# ---------------------------
source("rankhospital.R")
test <- rankhospital("TX","heart failure",4)
test <- rankhospital("MD","heart attack","worst")
test <- rankhospital("MN","heart attack",5000)

# ---------------------------
source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

# -------------------------------
# Quiz

q1 <- best("SC", "heart attack")
q2 <- best("NY", "pneumonia")
q3 <- best("AK", "pneumonia")
q4 <- rankhospital("NC", "heart attack", "worst")
q5 <- rankhospital("WA", "heart attack", 7)
q6 <- rankhospital("TX", "pneumonia", 10)
q7 <- rankhospital("NY", "heart attack", 7)
q8 <- r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
q9 <- r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
q10 <- r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

