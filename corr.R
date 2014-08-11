source("complete.R")

corr <- function(directory, threshold = 0) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  if (basename(getwd()) != directory) {
    setwd(directory)
  }
  ## Return a numeric vector of correlations
  files <- list.files()    
  totalData <- do.call("rbind", lapply(files, read.csv, header = TRUE))
  ## strip out all of the NA's
  totalData <- totalData[complete.cases(totalData), ]
  
  cData <- complete(directory)
  aboveThresh <- cData[cData$nobs>threshold, ]
  monIDs <- aboveThresh[, 1]
  
  corrData <- vector("numeric", length = 0)
  index <- 1
  for (i in 1:length(monIDs)) {
    
    monID <- monIDs[i]

    ## create x vector to hold the nitrate values
    ## and y vector for corresponding sulfate values
    x <- totalData[totalData$ID == monID, "nitrate"]
    y <- totalData[totalData$ID == monID, "sulfate"]
    ## vector of correlates
    cr <- cor(x,y)
    corrData[index] <- cr
    index <- index + 1
  }
  ## return the correlates
  return(corrData)
}