# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

Load the Data and process date

```r
data <- read.csv("activity.csv")
data$Date <- as.Date(data$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Calculate total number of steps

```r
data.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
par(mfrow=c(2,1))
```

Plot histogram
![](PA1_template_t_files/figure-html/unnamed-chunk-3-1.png)\

Calculate Mean and Median

```r
data.steps.mean <- mean(data.steps, na.rm=TRUE)
data.steps.median <- median(data.steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

Get the average daily pattern

```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
```

Plot the average daily pattern
![](PA1_template_t_files/figure-html/unnamed-chunk-6-1.png)\

Maximum number of steps

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Number of missing rows

```r
missing.rows <- nrow(subset(data, is.na(data$steps)))
```

Replace each missing value with the mean value of its 5-minute interval

```r
new.Value <- as.data.frame(data$steps)
new.Value[is.na(new.Value),] <- tapply(X=data$steps,INDEX=data$interval,FUN=mean,na.rm=TRUE)
new.Data <- cbind(new.Value, data[,2:3])
colnames(new.Data) <- c("Steps", "Date", "Interval")

total.steps <- aggregate(new.Data$Steps, list(new.Data$Date), FUN=sum)
```

Plot histogram of filled data
![](PA1_template_t_files/figure-html/unnamed-chunk-10-1.png)\

Calculate Mean and Median

```r
total.steps.mean <- mean(total.steps$x, na.rm=TRUE)
total.steps.median <- median(total.steps$x, na.rm=TRUE)
```
Filling  the missing values in the original dataset results in greater mean and median estimates.


## Are there differences in activity patterns between weekdays and weekends?
To investigate patterns between weekdays and weekends a new factor column (day) is added

```r
dayofweek <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")

}
new.Data$Date <- as.Date(new.Data$Date)
new.Data$day <- sapply(new.Data$Date, FUN=dayofweek)
```

Plot Activity patterns for both cases


```r
new.avg <- aggregate(Steps ~ Interval + day, data=new.Data, mean)
```
![](PA1_template_t_files/figure-html/unnamed-chunk-14-1.png)\










