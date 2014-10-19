Reproducible Research: Course Project 1
========================================================

**Loading and preprocessing the data**

```r
library(ggplot2)
library(lattice)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
pj1 <- read.csv("/Users/Z/Documents/Coursera/Reproducible_Research/activity.csv")
pj1$date <- as.Date(pj1$date)
```

**What is mean total number of steps taken per day?**

```r
# Histogram of steps taken per day
hist(pj1$steps, binwidth = 100, xlab="Total Steps Taken Per Day", ylab="Steps", col="red")
```

```
## Warning in title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...):
## "binwidth" is not a graphical parameter
```

```
## Warning in axis(1, ...): "binwidth" is not a graphical parameter
```

```
## Warning in axis(2, ...): "binwidth" is not a graphical parameter
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
total_steps <- tapply(pj1$steps, pj1$date, FUN=sum, na.rm=TRUE)
# Mean and Median steps
mean(total_steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps, na.rm=TRUE)
```

```
## [1] 10395
```

**What is the average daily activity pattern?**

```r
# calculate average number of steps per interal
avg <- aggregate(x=list(steps=pj1$steps), by=list(interval=pj1$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=avg, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Avg Num Steps Per Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

**Imputing missing values**

```r
# Create ne data set of NA values in original.
missing_val <- is.na(pj1$steps)
table(missing_val)
```

```
## missing_val
## FALSE  TRUE 
## 15264  2304
```

```r
# Fill the NA values with the average values in each interval.
fillNA <- pj1
for (i in 1:nrow(fillNA)) {
    if (is.na(fillNA$steps[i])) {
        fillNA$steps[i] <- avg[which(fillNA$interval[i] == avg$interval), ]$steps
    }
}

# plot Total Number of Steps
ggplot(fillNA, aes(date, steps)) + 
    geom_bar(stat = "identity") +
    xlab("Date") +
    ylab("Total Number Of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# Original Mean and Median of the data set.
mean(total_steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total_steps, na.rm=TRUE)
```

```
## [1] 10395
```

```r
# Calculate new Mean and Median after filling NA data with avg.
new_total_steps <- tapply(fillNA$steps, fillNA$date, FUN=sum, na.rm=TRUE)
mean(new_total_steps)
```

```
## [1] 10766.19
```

```r
median(new_total_steps)
```

```
## [1] 10766.19
```

**Are there differences in activity patterns between weekdays and weekends?**

```r
fillNA$weekdays <- factor(format(fillNA$date, "%A"))
levels(fillNA$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(fillNA$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(fillNA$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(fillNA$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
# Average accross all weekdays
avg_step <- aggregate(fillNA$steps, 
                      list(interval = as.numeric(as.character(fillNA$interval)), 
                           weekdays = fillNA$weekdays),
                      FUN = "mean")
names(avg_step)[3] <- "Means"

# Panel plot of time series data.
xyplot(avg_step$Means ~ avg_step$interval | avg_step$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
       

