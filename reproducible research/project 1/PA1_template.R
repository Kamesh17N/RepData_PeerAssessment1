
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")



library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(totalSteps, binwidth=1000, xlab="No. of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)



library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-min interval") +
  ylab("Avg. No. of steps taken")



averages[which.max(averages$steps),]



missing <- is.na(data$steps)

table(missing)



fillVal <- function(steps, interval) 
{
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "Steps"])
  return(filled)
}
filledData <- data
filledData$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)



totalSteps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="No. of steps taken each day")
mean(total.steps)
median(total.steps)



wRw <- function(date)
{
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("Invalid Date")
}
filledData$date <- as.Date(filledData$date)
filledData$day <- sapply(filledData$date, FUN=wRw)



averages <- aggregate(steps ~ interval + day, data=filledData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
