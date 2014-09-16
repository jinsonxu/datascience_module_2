# Jinson's week 2 assignments for R Programming module
# of Data Science Specialization



complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
    
  files <- list.files(directory, full.names=TRUE)
  data <- do.call('rbind', lapply(files, read.csv, header=TRUE))
  data$count <- 1
  data <- na.omit(data)
  
  res <- aggregate(x=data$count, by=list(id=data$ID), FUN="sum")
  names(res) <- c('id', 'nobs')

  #now lets loop through the criteria and filter out the rows and merge
  filtered <- data.frame(id = numeric(), nobs=numeric())
  for(i in seq_along(id)) {
    subset <- res[res$id==id[i],]
    filtered <- rbind(filtered, subset)
  }
  row.names(filtered) <- 1:nrow(filtered)
  return(filtered)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)