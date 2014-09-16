
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(directory, full.names=TRUE)
  data <- do.call('rbind', lapply(files, read.csv, header=TRUE))
  data$count <- 1
  data$isComplete <- complete.cases(data)
  
  
  # filter out data where both pollutants completely observed
  filtered <- data[data$isComplete==TRUE,]
#  filtered <<- data[!is.na(data$sulfate) 
#                   & !is.na(data$nitrate)
#                   & !is.na(data$Date)
#                   & !is.na(data$ID),]
  
  # find out what IDs have cases >= threshold
  agg <- aggregate(x=filtered$count, by=list(id=filtered$ID), FUN="sum")
  passIDs <- agg[agg$x >= threshold, c('id')]
  
  # now loop through the data where id in passIDs vector 
  results <- vector(mode="numeric", length=0)
  for(i in seq_along(passIDs)) {
    subset <- filtered[filtered$ID==passIDs[i],]
    results <- append(results, cor(subset$sulfate, subset$nitrate))
  }
  
  return(results)
  
}



#cr <- corr("specdata", 150)
#head(cr)
#summary(cr)

#cr <- corr("specdata", 400)
#head(cr)
#summary(cr)

#cr <- corr("specdata", 5000)
#summary(cr)
#length(cr)

#cr <- corr("specdata")
#summary(cr)
#length(cr)
