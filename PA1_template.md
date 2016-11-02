# Reproducible Research: Peer Assessment 1

## Randy Borregard
##### 11-2-2016


## Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. read.csv())


```r
filename <- unzip("activity.zip")
activity <- read.csv(filename, stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

* Additional packages needed

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.


```r
activity_rm<-activity[which(!is.na(activity$steps)),]
```

1. Calculate the total number of steps taken per day


```r
perday<-tapply(activity_rm$steps, activity_rm$date, sum)
```


```r
print(perday)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(perday,10, main = "Total number of steps taken per day", xlab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(perday)
```

```
## [1] 10766.19
```

```r
median(perday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dailyact<-tapply(activity_rm$steps, activity_rm$interval, mean)
plot(y = dailyact, x = names(dailyact), type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dailyact[dailyact==max(dailyact)]
```

```
##      835 
## 206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
act_new <- activity
act_new[which(is.na(act_new$steps)),1]<-
        dailyact[as.character(act_new[which(is.na(act_new$steps)),3])]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
perday_new<-tapply(act_new$steps, act_new$date, sum)
par(mfrow=c(1,2))
hist(perday,10, main = "Total number of steps taken per day", xlab = "Steps"
     , ylim =c(0, 25))
abline(v = median(perday), col = 4, lwd = 4)
hist(perday_new,10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
abline(v = median(perday_new), col = 4, lwd = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
mean(perday_new)
```

```
## [1] 10766.19
```

```r
median(perday_new)
```

```
## [1] 10766.19
```

```r
mean(perday_new)-mean(perday)
```

```
## [1] 0
```

```r
median(perday_new)-median(perday)
```

```
## [1] 1.188679
```

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
act_new$wd<-weekdays(act_new$date)
act_new$fwd<- as.factor(c("weekend", "weekday"))
act_new[act_new$wd == "Sunday" | act_new$wd == "Saturday" ,5]<- factor("weekend")
act_new[!(act_new$wd == "Sunday" | act_new$wd == "Saturday"),5 ]<- factor("weekday")
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
act_new_we <- subset(act_new, fwd == "weekend") 
act_new_wd <- subset(act_new, fwd == "weekday") 
dailyact_we<-tapply(act_new_we$steps, act_new_we$interval, mean)
dailyact_wd<-tapply(act_new_wd$steps, act_new_wd$interval, mean)
par(mfrow=c(2,1))
plot(y = dailyact_wd, x = names(dailyact_wd), type = "l", xlab = "Interval", 
     main = "Weekdays", ylab = "Number of steps", 
     ylim =c(0, 250))
plot(y = dailyact_we, x = names(dailyact_we), type = "l", xlab = "Interval", 
     main = "Weekends", ylab = "Number of steps", 
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

####Conclusion
We can see from the two graphs that the distribution throughout the day is different.  Weekend activity tends to start later in the day.  There is also a much larger spark during the weekdays, early in the day.



