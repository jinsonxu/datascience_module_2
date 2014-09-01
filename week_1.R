# Jinson's week 1 assignments for R Programming module
# of Data Science Specialization

x <- 4
class(x)

x <-c(4, TRUE)
class(x)

x <- c(1,3, 5)
y <- c(3, 2, 10)
class(rbind(x,y))

x <- list(2, "a", "b", TRUE)
test <- x[[1]]


x <- 1:4
y <- 2:3
test <- x+y

x <- c(17, 14, 4, 5, 13, 12, 10)
x[x>10] <- 4


d <- read.csv('data/hw1_data.csv')
d[1:2,]
nrow(d)
d[152:153,]
d[47,c('Ozone')]
missingOzone <- is.na(d$Ozone)
mean(d$Ozone, na.rm=TRUE)

sub <- d[d$Ozone>31 & d$Temp >90,]
mean(sub$Solar.R, na.rm=TRUE)

mean(d[d$Month==6,c('Temp')])

max(d[d$Month==5,c('Ozone')],na.rm=TRUE)