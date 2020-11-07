# computes correlation between complete cases of sulfate and nitrate pollutants
corr <- function(directory,threshold=0) {
  complete_cases = complete(directory)
  corr <- c()
  for (i in 1:332) {
    if (complete_cases[i,2]>=threshold) {
      location = paste(directory,"/",sprintf("%03d", i),".csv",sep="")
      data <- read.csv(location)
      datacomplete <- data[complete.cases(data),]
      correlation <- cor(datacomplete$sulfate,datacomplete$nitrate)
      if (is.na(correlation)==FALSE) {
        corr <- c(corr,correlation)
      }
    }
  }
  return(corr)
}
