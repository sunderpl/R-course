# computes complete cases over rows id for data in directory
complete <- function(directory, id=1:332) {
  #mylist = list()
  mycol1 <- c()
  mycol2 <- c()
  for(i in 1:length(id)) {
    location = paste(directory,"/",sprintf("%03d", id[i]),".csv",sep="")
    data <- read.csv(location)
    # find complete cases
    n <- nrow(data[complete.cases(data),])
    mycol1 <- c(mycol1,id[i]) 
    mycol2 <- c(mycol2,n) 
  }
  #mylist[[i]] <- c("id"=i,"nobs"=n)
  mylist <- list("id"=mycol1,"nobs"=mycol2)
  complete <- as.data.frame(mylist)
}
