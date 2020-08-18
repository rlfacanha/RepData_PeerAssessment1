---
title: "Reproducible Research: Peer Assessment 1"
author: "rlfacanha"
date: "17/08/2020"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
unzip("activity.zip")
#Read dataset and load into data table object
activity <- read.table("activity.csv", sep = ",",header = TRUE)
```
## What is mean total number of steps taken per day?

```r
library(dplyr)
#Ignoring missing values
activity_na <- activity[complete.cases(activity), ]
#1. Calculate the total number of steps taken per day
totalsteps_perday <- activity_na %>% group_by(date) %>% summarize(totalsteps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
#2. Make a histogram of the total number of steps taken each day
with(totalsteps_perday, hist(totalsteps_perday$totalsteps, xlab = "Total Steps Per Day", col = "red", main = "Histogram of the total number of steps taken each day"))
```

![](PA1_template_files/figure-html/activity_mean_steps-1.png)<!-- -->

```r
dev.copy(png, file = "figures/plot_totalsteps_perday.png")
```

png 
  3 

```r
dev.off()
```

png 
  2 

```r
#3. Calculate and report the mean and median of the total number of steps taken per day
mean(totalsteps_perday$totalsteps)
```

[1] 10766.19

```r
median(totalsteps_perday$totalsteps)
```

[1] 10765

## What is the average daily activity pattern?

```r
#1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
steps_per_interval <- activity_na %>% group_by(interval) %>% summarise(avgsteps = mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(steps_per_interval, plot(steps_per_interval$interval,
                             steps_per_interval$avgsteps, 
                             xlab = "5-minute interval",
                             ylab = "Average Number of Steps",             
                             type='l',
                             main = "Time Series of steps by 5 minute interval"))
```

![](PA1_template_files/figure-html/daily_activity_pattern-1.png)<!-- -->

```r
dev.copy(png, file = "figures/plot_steps_per_interval.png")
```

png 
  3 

```r
dev.off()
```

png 
  2 

```r
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
#steps_per_interval[which.max(steps_per_interval$avgsteps),]
print(paste("Interval: ", steps_per_interval[which.max(steps_per_interval$avgsteps),][1]))
```

[1] "Interval:  835"

## Imputing missing values

```r
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
print(paste("Number of missing values: ", sum(is.na(activity))))
```

[1] "Number of missing values:  2304"

```r
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
print("The strategy will fill the NA values with the number of steps corresponding the mean for the corresponding 5-minute interval.")
```

[1] "The strategy will fill the NA values with the number of steps corresponding the mean for the corresponding 5-minute interval."

```r
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
filled_activity <- activity
for (row in 1:nrow(activity)) {
        if (is.na(activity$steps[row])) {
                steps_to_fill <- steps_per_interval$avgsteps[steps_per_interval$interval == activity$interval[row]]
                filled_activity$steps[row] <- steps_to_fill
        }        
}
#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
filled_totalsteps_perday <- filled_activity %>% group_by(date) %>% summarize(totalsteps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
with(filled_totalsteps_perday, 
     hist(filled_totalsteps_perday$totalsteps, 
          xlab = "Total Steps Per Day", col = "red", 
          main = "Histogram of the total number of steps taken each day"))
```

![](PA1_template_files/figure-html/missing_values-1.png)<!-- -->

```r
mean(filled_totalsteps_perday$totalsteps)
```

[1] 10766.19

```r
median(filled_totalsteps_perday$totalsteps)
```

[1] 10766.19

```r
print("The mean value does not differ and the median differs just slightly. The impact depends on the devised strategy")
```

[1] "The mean value does not differ and the median differs just slightly. The impact depends on the devised strategy"

## Are there differences in activity patterns between weekdays and weekends?

```r
#1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekdays_weekday <- c("segunda-feira","terça-feira","quarta-feira","quinta-feira","sexta-feira")
filled_activity$wDay <- c('weekend', 'weekday')[(weekdays(as.Date(activity$date)) %in% weekdays_weekday)+1L]
#2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(ggplot2)
steps_perweekday <- filled_activity %>% group_by(interval,wDay) %>% summarise(avgsteps = mean(steps, na.rm=TRUE))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
qplot(interval, avgsteps, data=steps_perweekday,
                xlab = "5-minute interval",
                ylab = "Average Number of Steps",                
                geom="line",
                main = "Time Series of steps by 5 minute interval and weekend/weekday",
                facets = wDay ~ .)
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
