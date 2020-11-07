# compute mean over rows id for pollutant in directory
# exclude NaNs
pollutantmean <- function(directory, pollutant, id=1:332) {
  allobs <- c()
  for (i in 1:length(id)) {
    location = paste(directory,"/",sprintf("%03d", id[i]),".csv",sep="")
    data <- read.csv(location)
    # ignore  NaNs
    mycol <- data[,pollutant]
    bad <- is.na(mycol)
    good<- mycol[!bad]
    allobs <- c(allobs,good) # append
  }
  pollutantmean <- mean(allobs)
}
