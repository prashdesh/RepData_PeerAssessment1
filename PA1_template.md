# Reproducible Research: Peer Assessment 1



## Introduction

This document presents the results of peer assessments 1 of course [Reproducible Research](https://class.coursera.org/repdata-004) on [coursera](https://www.coursera.org). 

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


This document demonstrates results of the Reproducible Research's Peer Assessment 1 in a report using **a single R markdown document** that can be processed by **knitr** and be transformed into an HTML file.  

Through this report you can see that activities on weekdays mostly follow a work related routine, where we find some more intensity activity in little a free time that the employ can made some sport. 

An important consideration is the fact of our data presents as a t-student distribution (see both histograms), it means that the impact of imputing missing values with the mean has a good impact on our predictions without a significant distortion in the distribution of the data.  

## Build the R environment

 **always use echo = TRUE** so that someone else will be able to read the code. 

set echo equal a **TRUE** and results equal a **'hold'** as global options for this document.  

```r
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
```

### Load  libraries. Use install.packages if package is not installed.

```r
install.packages("data.table")
```

```
## Installing package into 'C:/Users/pdhingra/Documents/R/win-library/3.1'
## (as 'lib' is unspecified)
```

```r
library(data.table)
library(ggplot2) #  ggplot2 for plotting figures
```

```
## package 'data.table' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\pdhingra\AppData\Local\Temp\RtmpcROoYc\downloaded_packages
```


## Load and preprocess the data

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  

Show any code that is needed to:  


1. Load the data (i.e. `read.csv()`)
2. Process/transform the data (if necessary) into a format suitable for your analysis

### Load  data



**Note**: It is assumed that the file activity.csv is in the current working directory.  Dataset is available here: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]


```r
activitydata <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

###  preprocess  data

convert the **date** field to `Date` class and **interval** field to `Factor` class.


```r
activitydata$date <- as.Date(activitydata$date, format = "%Y-%m-%d")
activitydata$interval <- as.factor(activitydata$interval)
```

check the data using `str()` method:


```r
str(activitydata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## Question : What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in
the dataset.



```r
steps_per_day <- aggregate(steps ~ date, activitydata, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)
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

1. Make a histogram of the total number of steps taken each day




```r
ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "green", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](./PA1_template_files/figure-html/histo-1.png) 

2. calculate the ***mean*** and ***median*** of the number of steps taken per day.


```r
steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)
```

The mean is **10766.189** and median is **10765**.

## Question: What is the average daily activity pattern?

calculate the aggregation of steps by intervals of 5-minutes and convert the intervals as integers and save them in a data frame called `steps_per_interval`.


```r
steps_per_interval <- aggregate(activitydata$steps, 
                                by = list(interval = activitydata$interval),
                                FUN=mean, na.rm=TRUE)
#convert to integers
##this helps in plotting
steps_per_interval$interval <- 
        as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
colnames(steps_per_interval) <- c("interval", "steps")
```


1. make the plot with the time series of the average number of steps taken (averaged across all days) versus the 5-minute intervals:



```r
ggplot(steps_per_interval, aes(x=interval, y=steps)) +   
        geom_line(color="orange", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](./PA1_template_files/figure-html/plot_time_series_graph-1.png) 


2. find the 5-minute interval with the containing the maximum number of steps:


```r
max_interval <- steps_per_interval[which.max(  
        steps_per_interval$steps),]
```

The **835<sup>th</sup>** interval has maximum **206** steps.


## Imputing missing values:
Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.








### 1. Calculate total number of missing values in the dataset (i.e. the total number of rows with `NA`s):

The total number of missing values in steps can be calculated using `is.na()` method to check whether the value is mising or not and then summing the logical vector to get total number of missing values.


```r
missing_vals <- sum(is.na(activitydata$steps))
```

The total number of ***missing values*** are **2304**.


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

To populate missing values, we choose to replace them with the mean value at the same interval across days. In most of the cases the median is a better centrality measure than mean, but in our case the total median is not much far away from total mean, and probably we can make the mean and median meets.

We create a function `na_fill(data, pervalue)` which the `data` arguement is the `activitydata` data frame and `pervalue` arguement is the `steps_per_interval` data frame.


```r
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}

activitydata_fill <- data.frame(  
        steps = na_fill(activitydata, steps_per_interval),  
        date = activitydata$date,  
        interval = activitydata$interval)
str(activitydata_fill)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

We check that are there any missing values remaining or not


```r
sum(is.na(activitydata_fill$steps))
```

```
## [1] 0
```

Zero output shows that there are ***NO MISSING VALUES***.


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Now let us plot a histogram of the daily total number of steps taken, plotted with a bin interval of 1000 steps, after filling missing values.



```r
fill_steps_per_day <- aggregate(steps ~ date, activitydata_fill, sum)
colnames(fill_steps_per_day) <- c("date","steps")
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
ggplot(fill_steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](./PA1_template_files/figure-html/histo_fill_value-1.png) 



### Calculate and report the **mean** and **median** total number of steps taken per day.




```r
steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)
```

The mean is **10766.189** and median is **10766.189**.

### Do these values differ from the estimates from the first part of the assignment?

Yes, these values do differ slightly.

- **Before filling the data**
    1. Mean  : **10766.189**
    2. Median: **10765**
    
    
- **After filling the data**
    1. Mean  : **10766.189**
    2. Median: **10766.189**

Note that values after filling the data mean and median are equal.

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

As you can see,  that while the mean value remains unchanged, the median value has shifted and matches to the mean.  

Since our data has shown a t-student distribution (see both histograms), it seems that the impact of imputing missing values has increase our peak, but it's not negatively impacting our predictions. 


## Are there differences in activity patterns between weekdays and weekends?

do this comparison with the table with filled-in missing values.  Here are steps 
1. Add a column in table that indicates the day of the week  
2. Subset the table into two parts - weekends (Saturday and Sunday) and weekdays (Monday through Friday).  
3. Tabulate the average steps per interval for each data set.  
4. Plot the two data sets side by side for comparison.  


```r
weekdays_steps <- function(data) {
    weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    weekdays_steps$interval <- 
            as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
    colnames(weekdays_steps) <- c("interval", "steps")
    weekdays_steps
}

data_by_weekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
    
    weekend_steps <- weekdays_steps(weekend_data)
    weekday_steps <- weekdays_steps(weekday_data)
    
    weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
    weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
    
    data_by_weekdays <- rbind(weekend_steps, weekday_steps)
    data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
    data_by_weekdays
}

data_weekdays <- data_by_weekdays(activitydata_fill)
```

Below you can see the panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:

```r
ggplot(data_weekdays, aes(x=interval, y=steps)) + 
        geom_line(color="violet") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps") +
        theme_bw()
```

![](./PA1_template_files/figure-html/plot_weekdays-1.png) 

## conclusion
The 2 graphs show that activity distribution is more uniform over weekend.  In weekday there are bigger peaks.  It may be that people work in office and later do some intense activity such as Gym.
