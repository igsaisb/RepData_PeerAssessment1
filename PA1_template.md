---
output: 
  html_document: 
    keep_md: yes
---

#Reproducible Research: Course Project 1

## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?

We create a histogram and calculate the mean total number of daily steps. 

```r
DailySteps <- tapply(activity$steps, activity$date, sum)

hist(DailySteps, xlab = "Number of Steps", main = "Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
DailyMean<- mean(DailySteps, na.rm = TRUE)
DailyMedian <- median(DailySteps, na.rm = TRUE)
DailyMean
```

```
## [1] 10766.19
```

```r
DailyMedian
```

```
## [1] 10765
```

##What is the average daily activity pattern?

We take a look at a plot of average daily activity by interval and calculate which interval on average has the maximum number of steps.



```r
IntervalSteps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(as.numeric(names(IntervalSteps)), 
     IntervalSteps, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Daily Activity Pattern (average)", 
     type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
IntervalMax <- names(sort(IntervalSteps, decreasing = TRUE)[1])
StepsMax <- sort(IntervalSteps, decreasing = TRUE)[1]
```


Calculate the number of missing values in the dataset

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


Creating a new data set to replace the missing data with imputed values. I will fill in missing data with the mean number of steps across all days with available data for that particular interval.



```r
IntervalSteps <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# split activity data by interval
activity.by.interval <- split(activity, activity$interval)
# fill in missing data for each interval
for(i in 1:length(activity.by.interval)){
        activity.by.interval[[i]]$steps[is.na(activity.by.interval[[i]]$steps)] <- IntervalSteps[i]
}
activity.imputed <- do.call("rbind", activity.by.interval)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]


DailySteps.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
hist(DailySteps.imputed, xlab = " Steps", main = "Daily Steps (imputed data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
DailyMean.imputed <- mean(DailySteps.imputed, na.rm = TRUE)
DailyMean
```

```
## [1] 10766.19
```

```r
DailyMedian.imputed <- median(DailySteps.imputed, na.rm = TRUE)
DailyMedian
```

```
## [1] 10765
```
The mean and median didn't change materially as the original calculation ignored them in the first place and replacing them with the average won't move the average. 


We next look at any difference in weekday and weekend activity patterns.

```r
activity.imputed$day <- ifelse(weekdays(as.Date(activity.imputed$date)) == "Saturday" | weekdays(as.Date(activity.imputed$date)) == "Sunday", "weekend", "weekday")

# Calculate average steps per interval for weekends
IntervalSteps.weekend <- tapply(activity.imputed[activity.imputed$day == "weekend" ,]$steps, activity.imputed[activity.imputed$day == "weekend" ,]$interval, mean, na.rm = TRUE)

# Calculate average steps per interval for weekdays
IntervalSteps.weekday <- tapply(activity.imputed[activity.imputed$day == "weekday" ,]$steps, activity.imputed[activity.imputed$day == "weekday" ,]$interval, mean, na.rm = TRUE)

# Set a 2 panel plot
par(mfrow=c(1,2))

# Plot weekday activity
plot(as.numeric(names(IntervalSteps.weekday)), 
    IntervalSteps.weekday, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity Pattern (weekdays)", 
     type = "l")

# Plot weekend activity
plot(as.numeric(names(IntervalSteps.weekend)), 
     IntervalSteps.weekend, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity Pattern (weekends)", 
     type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

