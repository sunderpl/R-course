# week 2 assignment
# uses subfolder specdata and functions pollutantmean, complete, and corr
# script evaluates these functions for specific questions that were asked in the course

source("pollutantmean.R")
source("complete.R")
source("corr.R")

# Part 1
mean1 <- pollutantmean("specdata", "sulfate", 1:10)
mean2 <- pollutantmean("specdata", "nitrate", 70:72)
mean3 <- pollutantmean("specdata", "nitrate", 23)
mean4 <- pollutantmean("specdata", "sulfate", 34)
mean5 <- pollutantmean("specdata", "nitrate")

# Part 2
cc1 <- complete("specdata", c(2,4,8,10,12))
cc2 <- complete("specdata", 1)
cc3 <- complete("specdata", 30:25)
cc4 <- complete("specdata", 3)
cc5 <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc5$nobs)
cc6 <- complete("specdata", 54)
print(cc6$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc7 <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc7[use, "nobs"])

# Part 3
cr1 <- corr("specdata",150)
cr2 <- corr("specdata",400)
cr3 <- corr("specdata",5000)
cr4 <- corr("specdata")

head(cr1)
summary(cr1)
head(cr2)
summary(cr2)
head(cr3)
summary(cr3)
head(cr4)
summary(cr4)

cr5 <- corr("specdata")                
cr5 <- sort(cr5)   
RNGversion("3.5.1")
set.seed(868)                
out5 <- round(cr5[sample(length(cr5), 5)], 4)
print(out5)

cr6 <- corr("specdata", 129)                
cr6 <- sort(cr6)                
n <- length(cr6)    
RNGversion("3.5.1")
set.seed(197)                
out6 <- c(n, round(cr6[sample(n, 5)], 4))
print(out6)

cr7 <- corr("specdata", 2000)                
n <- length(cr7)                
cr8 <- corr("specdata", 1000)                
cr8 <- sort(cr8)
print(c(n, round(cr8, 4)))

