## Assignment 1 Code

pollutantmean <- function(directory, pollutant, id = 1:332) {

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  if (basename(getwd()) != directory) {
    setwd(directory)
  }
  files <- list.files()    
  
  ## load all of the files into a large data.frame
  totalData <- do.call("rbind", lapply(files, read.csv, header = TRUE))
  ## strip out all of the NA's
  cleanedData <- totalData[complete.cases(totalData), ]
  
  ## filter the data-set by the range passed in using the ID column  
  rData <- cleanedData[cleanedData$ID %in% id, ]
  ## filter according to the pollutant column name and calculate the mean
  mData <- round(mean(rData[, pollutant]), digits = 3)
}