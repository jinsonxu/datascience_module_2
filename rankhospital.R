# Jinson's week 4 3rd programming assignment for R Programming module
# of Data Science Specialization


# clear workspace
rm(list=ls())


## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.
## The num argument can take values "best", "worst", or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function will return NA. Hospitals that do not have data on a particular outcome are 
## excluded from the set of hospitals when deciding the rankings.
rankhospital <- function(state, outcome, num = "best") {
  allowedOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  allowedCharNum <- c('best', 'worst')

  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', sep=",", 
                          stringsAsFactors = FALSE,
                          na.strings=c(".", "NA", "", "?", "Not Available"), 
                          strip.white=TRUE) # trim the whitespace in our values, save alot of trouble later
  
  ## Check that state, outcome, and num are valid
  if(length(grep(state, outcomeData[,c('State')], perl=TRUE)) == 0) {
    stop('invalid state')
  }
  
  if(length(grep(outcome, allowedOutcomes, perl=TRUE)) == 0) {
    stop('invalid outcome')
  }
  
  if(!is.numeric(num) & length(grep(num, allowedCharNum, perl=TRUE)) == 0) {
    stop('invalid num')
  }

  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  workingData <- outcomeData[outcomeData['State'] == state, ]  # filter to only the state we're interested in
  
  ## Get hold of the column number for the metric we're interested in finding.
  colNameToCheck <- paste('^Hospital.30.Day.Death..Mortality..Rates.from.', 
                          gsub('([\\s])', '.', outcome, perl=TRUE), sep='')  # do some col name transformation to match R's auto col name changes on import
  colNum <- grep(colNameToCheck, names(workingData), ignore.case=TRUE)

  ## filter out rows with NAs for that metric
  workingData <- workingData[!is.na(workingData[colNum]),]


  ## Get the hospital names for that metric in that state ordered from lowest to highest
  bestHospitals <- workingData[order(workingData[colNum], workingData[2]), c(2,7,colNum)]
  
  if(is.numeric(num)) {
    if(num > nrow(bestHospitals)) {
      return(NA)
    } else {
      return(bestHospitals[num, 1])      
    }
    
  } else if(num == 'worst') {
    return(bestHospitals[nrow(bestHospitals), 1])
    
  } else {
    return(bestHospitals[1,1])
    
  }

}


#rankhospital("TX", "heart failure", 4)
#rankhospital("MD", "heart attack", "worst")
#rankhospital("MN", "heart attack", 5000)