---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

##Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity <- read.csv("activity.csv")
#Ignore the missing value#
activity1 <- na.omit(activity)
```

##What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day

```r
#Change the date format#
activity1$date <- as.Date(activity1$date, format = "%m/%d/%Y")
#Calculate the total number of steps taken per day#
b <- aggregate(activity1$steps, by = list(date = activity1$date),FUN=sum)
#histogram of the total number of steps taken each day#
hist(b[,2], breaks = 11, xlab = "Steps", main = "Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/withoutNA-1.png)<!-- -->

```r
#Mean of the total number of steps taken per day#
meanb <- round(mean(b[,2]),digits = 0)
#Median of the total number of steps taken per day#
medianb <- median(b[,2])
```
The mean is 1.0766\times 10^{4} and the median is 10765.

##What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
#Make a time series plot#
c <- aggregate(activity1$steps, by = list(interval = activity1$interval),FUN=mean)
plot(c,type = "l",main = "Average number of steps across all days",xlab = "interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->

```r
#Which 5-minutes interval has the highest average steps#
number <- c[which.max(c[,2]),1]
```
The 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps is 835.

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Total number of missing values in the dataset#
nadata <- subset(activity, is.na(steps))
row <- nrow(nadata)
#I plan to use the average steps of each interval to fill the missing value#
for(i in 1:nrow(nadata)){
    for(j in 1:nrow(c)){
        if (nadata[i,3]==c[j,1]){
            nadata[i,1] = c[j,2]
        }
    }
}
nadata$date <- as.Date(nadata$date, format = "%m/%d/%Y")
#Create a new dataset that is equal to the original dataset but with the missing data filled in.#
new <- rbind(nadata,activity1)
new <- new[order(new$date),]
#Calculate the total number of steps taken per day#
newb <- aggregate(new$steps, by = list(date = new$date),FUN=sum)
#histogram of the total number of steps taken each day#
hist(newb[,2], breaks = 11, xlab = "Steps", main = "Histogram of Total Steps per Day (NEW)")
```

![](PA1_template_files/figure-html/NA-1.png)<!-- -->

```r
#Mean of the total number of steps taken per day#
meannewb <- round(mean(newb[,2]),digit = 0)
#Median of the total number of steps taken per day#
mediannewb <- median(newb[,2])
```
The total number of missing value is 2304. I use the mean for that 5-minute interval to fill in the missing value. As a result, the mean is 1.0766\times 10^{4} and the median is 1.0766189\times 10^{4}. The mean value doesn't change, because I use the mean value to fill the NA value, however, the median value is a little bit different than previous one. It is easy to understand that the size of the sample is different, so the median will slightly change.

##Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.#
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
new$category <- factor((weekdays(new$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
#Make a panel plot containing a time series plot#
bb<-aggregate(steps ~ interval + category, data=new, mean)
library(lattice)
xyplot(steps~interval|category, data=bb, type="l",  layout = c(1,2), main = "Average steps based on weekdays and weekend", ylab = "number of steps",xlab = "Interval")
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
