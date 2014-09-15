# Jinson's week 2 assignments for R Programming module
# of Data Science Specialization


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
  
  # filter out data where both pollutants completely observed
  filtered <- data[!is.na(data$sulfate) 
                   & !is.na(data$nitrate)
                   & !is.na(data$Date)
                   & !is.na(data$ID),]

  # find out what IDs have cases >= threshold
  agg <- aggregate(x=data$count, by=list(id=data$ID), FUN="sum")
  passIDs <- agg[agg$x >= threshold, c('id')]
  
  # now loop through the data where id in passIDs vector 
  results <- vector(mode="numeric", length=0)
  for(i in seq_along(passIDs)) {
    subset <- filtered[filtered$ID==passIDs[i],]
    results <- round(append(results, cor(subset$sulfate, subset$nitrate)), digits = 5) 
  }

  return(results)

}
cr <- corr("data/specdata", 400)
summary(cr)     
           
filtered <- test[!is.na(test$sulfate) 
                 & !is.na(test$nitrate)
                 & !is.na(test$Date)
                 & !is.na(test$ID),]
agg <- aggregate(x=filtered$count, by=list(id=filtered$ID), FUN="sum")
passIDs <- agg[agg$x >= 132, c('id')]
results <- vector(mode="numeric", length=0)
for(i in seq_along(passIDs)) {
  subset <- filtered[filtered$ID==passIDs[i],]
  append(results, cor(subset$sulfate, subset$nitrate))  
}

