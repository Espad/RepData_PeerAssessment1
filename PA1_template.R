library(dplyr)
library(timeDate)
library(lattice)

# unzip dataset to /data directory
unzip(zipfile="./activity.zip",exdir="./data")
df <- read.csv("./data/activity.csv")


#Question 1. What is mean total number of steps taken per day?
steps_each_day <- aggregate(df$steps, by=list(df$date),FUN=sum, na.rm=TRUE)

hist(steps_each_day$x, col = "blue", main = paste("Histogram: Steps per day"), xlab = "Total number of steps per day", ylim = c(0, 30), breaks = 7)

step_mean <- mean(steps_each_day$x,na.rm = TRUE)
step_mean

step_median <- median(steps_each_day$x,na.rm = TRUE)
step_median
#lets add this info on graphic!
abline(v=step_mean, col="red", lwd=3)
abline(v=step_median, col="blue", lwd=3)
hist(steps_each_day$x, col = "blue", main = paste("Histogram: Steps per day"), xlab = "Total number of steps per day", ylim = c(0, 30), breaks = 7)


#Question 2. What is the average daily activity pattern?
steps_activity <- aggregate(df$steps, by=list(df$interval),FUN=mean, na.rm=TRUE)

#this code block convert time period from 0 to 0000 0910 to 09:2015 and etc
steps_activity$Group.1 <- sprintf("%04d", steps_activity$Group.1)
steps_activity$Group.1 <- format(strptime(steps_activity$Group.1, format="%H%M"), format = "%H:%M")
steps_activity$Group.1 <- as.POSIXct(steps_activity$Group.1,format="%H:%M")

plot(x ~ Group.1, data = steps_activity, type = "l", ylim = c(0, 230),las=2, ylab = "Number of steps", xlab = "Hours")

format(steps_activity[which.max(steps_activity$x), ]$Group.1,"%H:%M")

#Question 3. Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
df_filled <- df
sum(is.na(df_filled$steps))

#Devise a strategy for filling in all of the missing values in the dataset. I prefer to use a mean for a 5-minute interval
df_filled <- transform(df_filled, steps = ifelse(is.na(df_filled$steps), steps_activity$x, df_filled$steps))

total_steps <- aggregate(df_filled$steps, by=list(df_filled$date),FUN=sum, na.rm=FALSE)

hist(total_steps$x, col = "red", main = paste("Histogram: Steps per day with filled NA's"), 
     xlab = "Total number of steps per day", ylim = c(0, 40), breaks = 7)

mean(total_steps$x,na.rm = FALSE)

median(total_steps$x,na.rm = FALSE)

#Question 4. Are there differences in activity patterns between weekdays and weekends?

df_days <- df_filled
df_days <- df_days %>% mutate(type_of_day = factor(ifelse(isWeekday(df_days$date,wday = 1:5), "weekday" ,"weekend")))

stepsByDay <- aggregate(df_days$steps ~ df_days$interval + df_days$type_of_day, df_days, mean)

#this code block convert time period from 0 to 0000 0910 to 09:2015 and etc
stepsByDay$`df_days$interval` <- sprintf("%04d", stepsByDay$`df_days$interval`)
stepsByDay$`df_days$interval` <- format(strptime(stepsByDay$`df_days$interval`, format="%H%M"), format = "%H:%M")
stepsByDay$`df_days$interval` <- as.POSIXct(stepsByDay$`df_days$interval`,format="%H:%M")


xyplot(stepsByDay$`df_days$steps` ~ stepsByDay$`df_days$interval` | factor(stepsByDay$`df_days$type_of_day`), 
       data = stepsByDay, aspect = 1/2, type = "l", xlab = "Hours", ylab = "Number of steps", scales=list(x=list(tick.number=9)))

