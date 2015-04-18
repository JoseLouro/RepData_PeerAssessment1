# Coursera Peer Assessment 1 for Reproducible Research
### by Jose Manuel Teles Louro da Silva 

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

1. The data file is downloaded if it is not present
2. It is uncompressed and read as a data frame.
3. The data is filtered to remove the missing values (NAs)
4. The data is examined by using summary and str methods on it.

Read in the data.

```r
# download and read the data, convert columns for convenience
    fname = "activity.zip"
    source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fname)) {
        download.file(source_url, destfile=fname, method="curl")
    }
    activity <- unzip(fname, "activity.csv")
    ActivityDataFrame <- read.csv(activity, 
								sep=",", 
								header=T, 
								na.strings="NA", 
								colClasses=c("numeric", "character", "numeric"))
```


We ignore (filter out) the missing values (NAs) in the dataset for the first part.


```r
MyDataFrame <- ActivityDataFrame[!is.na(ActivityDataFrame$steps),]
```

Examine data.

```r
summary(MyDataFrame)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:15264       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```

```r
str(MyDataFrame)
```

```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : chr  "2012-10-02" "2012-10-02" "2012-10-02" "2012-10-02" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?


A Histogram of the total number of steps taken each day,


```r
stepsByDate <- aggregate(steps ~ date, data = MyDataFrame, FUN=sum)
barplot(stepsByDate$steps, names.arg=stepsByDate$date, xlab = "Date", ylab = "Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


Mean total number of steps taken per day,


```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```


Median total number of steps taken per day,


```r
median(stepsByDate$steps) 
```

```
## [1] 10765
```

**For the total number of steps taken per day:**  
- **Mean: 10766.19**
- **Median: 10765**



## What is the average daily activity pattern?

Now lets look at the average daily activity pattern

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsByInterval <- aggregate(steps ~ interval, data=MyDataFrame, FUN=mean)
plot(stepsByInterval, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxStepInterval <- stepsByInterval$interval[which.max(stepsByInterval$steps)]
maxStepInterval
```

```
## [1] 835
```

The **835<sup>th</sup> interval** has the maximum activity on the average.



## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Total number of missing values in the data set is the number of measured steps minus the number of complete rows,


```r
length(ActivityDataFrame$steps) - length(MyDataFrame$steps)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
stepsByDate <- aggregate(steps ~ date, data = ActivityDataFrame, FUN=sum)
FilledDataFrame <- merge(ActivityDataFrame, stepsByDate, by="date", suffixes=c("",".new"))
naSteps <- is.na(FilledDataFrame$steps)
FilledDataFrame$steps[naSteps] <- FilledDataFrame$steps.new[naSteps]
FilledDataFrame <- FilledDataFrame[,1:3]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
stepsByDate <- aggregate(steps ~ date, data=FilledDataFrame, FUN=sum)
barplot(stepsByDate$steps, names.arg=stepsByDate$date, xlab="Date", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Mean for the missing data filled in.


```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```

Median for missing data filled in.


```r
median(stepsByDate$steps)
```

```
## [1] 10765
```
*Do these values differ from the estimates from the first part of the assignment?*

No, they do not.

*What is the impact of imputing missing data on the estimates of the total daily number of steps?*

Little to no impact.


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function will be used to help. 
Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# To force the language to be English to have the weekdays as Saturday and Sunday... 
Sys.setlocale("LC_TIME","English United States")
```

```
## [1] "English_United States.1252"
```

```r
WeekPart <- function(date) {
	if(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
		day <- "Weekend"
	} else {
		day <- "Weekday"
	}

}
FilledDataFrame$weekPart <- as.factor(sapply(FilledDataFrame$date, WeekPart))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(reshape2)
melted <- melt(FilledDataFrame, measure.vars="steps")
meanSteps <- dcast(melted, weekPart+interval~variable, mean)


library(lattice)
xyplot(steps~interval|weekPart,
	data=meanSteps,
	xlab="Interval",
	ylab="Number of steps",
	type="l",
	layout=c(1,2)
)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

We observe that activity on the weekends tends to be more spread out over the day compared to the weekdays. 
This could be due to the fact that activities on weekdays mostly follow a work related routine, whereas weekends tend to be more adhoc.

