---
title: "Reproducible Research:Peer Assessment 1"
output: html_document
---

##Preparing the R Enviornment 

```r
library(knitr)
opts_chunk$set(echo=TRUE)
library(ggplot2) #ggplot2 for plotting figures
```

##Loading and Preprocessing the data


```r
unzip("repdata_data_activity.zip")
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

change date datatype to date type

```r
activity$date<-as.Date(activity$date,format='%Y-%m-%d')
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

##What is mean total Number of steps taken per day?

1.Calculate total Number of steps taken per day


```r
total_daily_steps<-aggregate(steps~date,activity,sum,na.rm=TRUE)
head(total_daily_steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2.Make a histogram of the total Number of steps taken each day


```r
ggplot(total_daily_steps,aes(x=date,y=steps,fill=factor(date)))+geom_histogram(stat="identity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

3.Calculate and report the mean and median total number of steps taken per day


```r
mean(total_daily_steps$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
median(total_daily_steps$steps,na.rm=TRUE)
```

```
## [1] 10765
```

##what is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval<-aggregate(steps~interval,data=activity,FUN=mean)
ggplot(steps_interval,aes(x=interval,y=steps))+geom_line(color="orange",size=1)+ylab("Average Number of steps")+xlab("5-minute interval")+labs(title="Average steps per Interval")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval<-steps_interval[which.max(steps_interval$steps),]
max_interval$interval
```

```
## [1] 835
```

##Imputing Missing Values

1.Total number of missing values in the dataset 


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2.Strategy for filling in all of the missing values in the dataset

use the means for the 5-minute intervals as fillers for missing values.

3.new dataset that is equal to the original dataset but with the missing data filled in


```r
activity<-merge(activity,steps_interval,by="interval",suffixes=c("",".y"))
nas<-is.na(activity$steps)
activity$steps[nas]<-activity$steps.y[nas]
activity<-activity[,c(1:3)]
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
total_daily_steps<-aggregate(steps~date,activity,sum)
ggplot(total_daily_steps,aes(x=date,y=steps,fill=factor(date)))+geom_histogram(stat="identity")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

```r
mean(total_daily_steps$steps)
```

```
## [1] 10766.19
```


```r
median(total_daily_steps$steps)
```

```
## [1] 10766.19
```

Mean and Median are Equal

##Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity$dayType <- ifelse(weekdays(activity$date) %in%  c("Saturday", "Sunday"),'weekend','weekday')
```


```r
table(activity$dayType)
```

```
## 
## weekday weekend 
##   12960    4608
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



```r
qplot(x=interval, y=steps,data=activity,geom="smooth", stat='summary', fun.y=mean)  + facet_wrap(~dayType,nrow=2,ncol=1)  + labs(title=' Average steps per days, weekdays and weekend')
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 


