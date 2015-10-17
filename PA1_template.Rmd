# Load data
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

# Summarize data
summary (data)
str(data)

# Convert 'Date' and 'Interval' to readable forms
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)


# What is mean total number of steps taken per day?

NA_data <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_data,]
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)
colnames(steps_each_day) <- c("date", "steps")

### Histogram of the number of steps taken each day

hist(as.numeric(steps_each_day$steps), breaks = 20, col = "blue", 
xlab = "Number of Steps", main= "Histogram of steps taken each day")
mean(steps_each_day$steps)
median(steps_each_day$steps)


# What is the average daily activity pattern?
steps_per_interval<- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)
colnames(steps_per_interval)<-c("interval", "average_steps")

plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l", 
xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern", col="blue")

## 5-minute interval that contains the maximum number of steps:
  
###The maximum number of average steps
max_steps <- max(steps_per_interval$average_steps)
max_steps

###The 5-minute interval that contains the maximum number of steps
interval_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
interval_max_steps


#Imputing missing values

sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))
sum(is.na(as.character(data$interval)))

##finding the indices of missing values (NAs)
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
##Imputing missing values using the mean for that 5-minute interval
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
  steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
}))

## Summarize new data
summary(complete_data)
str(complete_data)

steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
colnames(steps_each_day_complete) <- c("date", "steps")

#Make histogram of the number of steps taken
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps")

mean(steps_each_day_complete$steps)
median(steps_each_day_complete$steps)

# Are there differences in activity patterns between weekdays and weekends?

##Creating a factor variable "day "to store the day of the week:
complete_data$day <- as.factor(weekdays(complete_data$date))

##Creating a logical variable "is_weekday" (weekday=TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 

##Calculating the average number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)

##Calculating the average number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

#Adding columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)
week_data$day <- as.factor(week_data$day)

## Make plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
