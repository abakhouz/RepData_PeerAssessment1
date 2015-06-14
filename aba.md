Reproducible Research, Assesment1, Week2
========================================================
========================================================
========================================================

0.Read.csv() file


```r
data <- read.csv( "activity.csv")
```


1.Make a histogram of the total number of steps taken each day



```r
hist(tapply(data$steps, data$date, sum), xlab = "Total daily steps", breaks = 20, 
     main = "Total of steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

2.Calculate and report the mean and median total number of steps taken per day


```r
total.daily.steps <- as.numeric(tapply(data$steps, data$date, sum))
step.mean <- mean(total.daily.steps, na.rm = TRUE)
step.median <- median(total.daily.steps, na.rm = TRUE)

step.mean
```

```
## [1] 10766.19
```

```r
step.median
```

```
## [1] 10765
```


3.Plot with Average steps 5-minute interval


```r
data$interval <- as.factor(as.character(data$interval))
interval.mean <- as.numeric(tapply(data$steps, data$interval, mean, na.rm = TRUE))
intervals <- data.frame(intervals = as.numeric(levels(data$interval)), interval.mean)
intervals <- intervals[order(intervals$intervals), ]

labels <- c("00:00", "05:00", "10:00", "15:00", "20:00")
labels.at <- seq(0, 2000, 500)
plot(intervals$intervals, intervals$interval.mean, type = "l", main = "Average steps 5-minute interval", 
    ylab = "Average steps", xlab = "Time of day", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

4. The 5-minute interval with the highest average number of steps corresponds to the interval between 8:35 AM and 8:40 AM:

```r
intervals.sorted <- intervals[order(intervals$interval.mean, decreasing = TRUE),    ]
head(intervals.sorted)
```

```
##     intervals interval.mean
## 272       835      206.1698
## 273       840      195.9245
## 275       850      183.3962
## 274       845      179.5660
## 271       830      177.3019
## 269       820      171.1509
```

```r
max.interval <- intervals.sorted$intervals[1[1]]
max.interval
```

```
## [1] 835
```


5. The total number of missing values in the dataset (i.e. the total number of rows with NAs) is 2304:

```r
dim(data[is.na(data$steps), ])[1]
```

```
## [1] 2304
```


The strategy for filling in all of the missing values in the dataset is to change the "NA"s to the mean values for that 5-minute interval.


```r
steps <- vector()
for (i in 1:dim(data)[1]) {
    if (is.na(data$steps[i])) {
        steps <- c(steps, intervals$interval.mean[intervals$intervals == data$interval[i]])
    } else {
        steps <- c(steps, data$steps[i])
    }
}
```


```r
activity.without.missing.data <- data.frame(steps = steps, date = data$date, 
    interval = data$interval)
```
    

```r
  hist(tapply(activity.without.missing.data$steps, activity.without.missing.data$date, 
    sum), xlab = "Total daily steps", breaks = 20, main = "Total of steps taken per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
total.daily.steps <- as.numeric(tapply(activity.without.missing.data$steps, 
    activity.without.missing.data$date, sum))
step.mean <- mean(total.daily.steps)
step.median <- median(total.daily.steps)

step.mean
```

```
## [1] 10766.19
```

```r
step.median
```

```
## [1] 10766.19
```



The new mean and median of total number of steps taken per day are 10766 and 10766 respectively, the median is exactly equal to the mean. Because of the strategy chosen, there is no impact of imputing missing data on the estimates of the total daily number of steps.


    

```r
activity.without.missing.data$day.type <- c("weekend", "weekday", "weekday", 
    "weekday", "weekday", "weekday", "weekend")[as.POSIXlt(activity.without.missing.data$date)$wday + 
    1]
activity.without.missing.data$day.type <- as.factor(activity.without.missing.data$day.type)

weekday <- activity.without.missing.data[activity.without.missing.data$day.type == 
    "weekday", ]
weekend <- activity.without.missing.data[activity.without.missing.data$day.type == 
    "weekend", ]
weekday.means <- as.numeric(tapply(weekday$steps, weekday$interval, mean))
weekend.means <- as.numeric(tapply(weekend$steps, weekend$interval, mean))

intervals.day.type <- data.frame(intervals = as.numeric(levels(data$interval)), 
    weekday.means, weekend.means)
intervals.day.type <- intervals.day.type[order(intervals.day.type$intervals), 
    ]
```
Plot two time series - weekdays and weekends - of the 5-minute intervals and average number of steps taken.


```r
par <- par(mfrow = c(2, 1))
plot(intervals.day.type$intervals, intervals.day.type$weekday.means, type = "l", 
col = "red", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekday", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
plot(intervals.day.type$intervals, intervals.day.type$weekend.means, type = "l", 
col = "blue", ylab = "Average steps", xlab = "Time of day", main = "Average steps 5-minute interval at weekend", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
par(par)
```

It is a bit difficult to compare the two plots. For a better comparison, combine the time series on a single plot.


```r
plot(intervals.day.type$intervals, intervals.day.type$weekday.means, type = "l", col = "red", ylab = "Average steps", xlab = "Time of day", main = "Comparison between weekday and weekend", xaxt = "n")
axis(side = 1, at = labels.at, labels = labels)
lines(intervals.day.type$intervals, intervals.day.type$weekend.means, type = "l", col = "blue")
legend(1500, 230, c("Weekend", "Weekday "), lty = c(1, 1), lwd = c(1, 1), col = c("blue", "red"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
    
