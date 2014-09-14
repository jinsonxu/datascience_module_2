# Jinson's week 2 assignments for R Programming module
# of Data Science Specialization



pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  getMeasurementsDF <- function(content, colName, rowCriteria) {
    content[content['ID']==rowCriteria, c(colName)]
  }
  
  files <- list.files(directory, full.names=TRUE)
  data <- do.call('rbind', lapply(files, read.csv, header=TRUE))
  
#  write.csv(data[data['ID']==70:72,],"7072.csv")
  mean(getMeasurementsDF(data, pollutant, id), na.rm=TRUE) 
}
pollutantmean("data/specdata", "sulfate", 1:10)

pollutantmean("data/specdata", "nitrate", 70:72)