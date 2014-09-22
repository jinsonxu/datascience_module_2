# Jinson's week 4 3rd programming assignment for R Programming module
# of Data Science Specialization


# clear workspace
rm(list=ls())


## rankall returns the hospitals in each state that have the specified
## ranking in num for the outcome
rankall <- function(outcome, num = "best") {
  allowedOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
  allowedCharNum <- c('best', 'worst')
  
  ## Read outcome data
  outcomeData <- read.csv('outcome-of-care-measures.csv', sep=",", 
                          stringsAsFactors = FALSE,
                          na.strings=c(".", "NA", "", "?", "Not Available"), 
                          strip.white=TRUE) # trim the whitespace in our values, save alot of trouble later
  

  ## Check that outcome, and num are valid
  if(length(grep(outcome, allowedOutcomes, perl=TRUE)) == 0) {
    stop('invalid outcome')
  }
  
  if(!is.numeric(num) & length(grep(num, allowedCharNum, perl=TRUE)) == 0) {
    stop('invalid num')
  }
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Get hold of the column number for the metric we're interested in finding.
  colNameToCheck <- paste('^Hospital.30.Day.Death..Mortality..Rates.from.', 
                          gsub('([\\s])', '.', outcome, perl=TRUE), sep='')  # do some col name transformation to match R's auto col name changes on import
  colNum <- grep(colNameToCheck, names(outcomeData), ignore.case=TRUE)
  

  ## loop within each state
  states <- unique(outcomeData[,c('State')])
  results <- data.frame(hospital = character(), state=character(), row.names=2)

  for(state in states) {
    # for each state, lets get the hospital with that ranking
    data <- outcomeData[outcomeData['State']== state
                        & !is.na(outcomeData[colNum]),]
    orderedStateResults <- data[order(data[colNum], data[2]),]

    if(is.numeric(num)) {
      if(num > nrow(orderedStateResults)) {
        result <- as.data.frame(list('hospital' = NA, 'state' = state))
      } else {
        result <- orderedStateResults[num, c(2,7)]
        names(result) <- c('hospital', 'state')
      }
    
    } else if(num == 'worst') {
      result <- orderedStateResults[nrow(orderedStateResults), c(2,7)]
      names(result) <- c('hospital', 'state')
      
    } else {
      result <- orderedStateResults[1,c(2,7)]
      names(result) <- c('hospital', 'state')
      
    }

    # now bind the result to our results data frame
    results <- rbind(result, results)

  }
  results <- results[order(results[2]),]
  return(results)  
}


#head(rankall("heart attack", 20), 10)
#tail(rankall("pneumonia", "worst"), 3)
#tail(rankall("heart failure"), 10)