# Jinson's week 4 3rd programming assignment for R Programming module
# of Data Science Specialization


# clear workspace
rm(list=ls())

#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#ncol(outcome)
#names(outcome)

#hist(as.numeric(outcome[,11]))


best <- function(state = character(), outcome = character()) {
  allowedOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', sep=",", 
#                    colClasses = "character", 
                    stringsAsFactors = FALSE,
                    na.strings=c(".", "NA", "", "?", "Not Available"), 
                    strip.white=TRUE) # trim the whitespace in our values, save alot of trouble later


  ## Check that state and outcome are valid
  if(length(grep(state, outcomeData[,c('State')], perl=TRUE)) == 0) {
    stop('invalid state')
  }

  if(length(grep(outcome, allowedOutcomes, perl=TRUE)) == 0) {
    stop('invalid outcome')
  }


  ## Return hospital name in that state with lowest 30-day death rate
  workingData <- outcomeData[outcomeData['State'] == state, ]  # filter to only the state we're interested in

  ## Get hold of the column number for the metric we're interested in finding.
  colNameToCheck <- paste('^Hospital.30.Day.Death..Mortality..Rates.from.', 
                          gsub('([\\s])', '.', outcome, perl=TRUE), sep='')  # do some col name transformation to match R's auto col name changes on import
  colNum <- grep(colNameToCheck, names(workingData), ignore.case=TRUE)

  ## Obtain the hospital name with the best metric value for the outcome in that state
  res <- workingData[workingData[colNum] == min(workingData[,colNum], na.rm=TRUE) 
                                          & !is.na(workingData[colNum])
                   ,]

  bestHospitals <- sort(as.character(res[,c('Hospital.Name')]))
  return(bestHospitals[1])  # return only the first one via alphabetical order
}


#best('TX', 'heart attack')
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")
#best("NY", "hert attack")