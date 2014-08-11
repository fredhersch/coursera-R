## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  if (basename(getwd()) != directory) {
    setwd(directory)
  }

  files <- list.files()    
  ## load all of the files into a large data.frame
  totalData <- do.call("rbind", lapply(files, read.csv, header = TRUE))
  
  ## create an empty data.frame with cols set
  result <- data.frame('id' = numeric(), 'nobs' = numeric())
  
  ## apply filter
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## filter the data-set by the range passed in using the ID column  
  fData <- totalData[totalData$ID %in% id, ]
  ## strip out all of the NA's
  cData <- fData[complete.cases(fData), ]
  
  r <- vector(mode = "numeric", length = length(id))
  
  for(i in id) {
    newRow <- data.frame(id=i, nobs = nrow(cData[cData$ID == i, ] ))
    result <- rbind(result, newRow)
  }
  setwd('../')
  return(result)
}