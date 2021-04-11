---
title: "Reproducible Research Course Project 1"
author: "Taisekwa Chikazhe"
date: "9 April 2021"
output: 
  html_document:
    keep_md: true
---
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Assignment
This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

Questions to be answered:

What is mean total number of steps taken per day?
What is the average daily activity pattern?
Imputing missing values
Are there differences in activity patterns between weekdays and weekends?

Setting Global Options


```r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5,
                      fig.keep = 'all' ,fig.path = 'figures\ ', dev = 'png')
```

Loading necessary packages


```r
library(ggplot2)
library(ggthemes)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
```


Reading in the data


```r
df<-read.csv("C:/Users/chikazhet/activity.csv",header = TRUE, sep = ',', colClasses = c("numeric", "character","integer"))
activity<- df
```

Tidying the data
Change the date into date format using lubridate
Getting the days of all the dates on the dataset
Combining the dataset with the weekday of the dates


```r
activity$date <- strptime(activity$date, format= "%d/%m/%Y")
day <- weekdays(activity$date)
activity <- cbind(activity, day)
```
Viewing the processed data


```r
summary(activity)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```
### What is the mean total number of steps taken per day?


```r
steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 53 x 2
##    date                steps
##    <dttm>              <dbl>
##  1 2012-10-02 00:00:00   126
##  2 2012-10-03 00:00:00 11352
##  3 2012-10-04 00:00:00 12116
##  4 2012-10-05 00:00:00 13294
##  5 2012-10-06 00:00:00 15420
##  6 2012-10-07 00:00:00 11015
##  7 2012-10-09 00:00:00 12811
##  8 2012-10-10 00:00:00  9900
##  9 2012-10-11 00:00:00 10304
## 10 2012-10-12 00:00:00 17382
## # ... with 43 more rows
```
Plotting a histogram using ggplot2

```r
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
```

![](figures unnamed-chunk-7-1.png)<!-- -->

Calculate mean and meadian steps

```r
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
```
Mean and median steps

```r
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

What is the average daily activity pattern?

```r
interval <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

ggplot time series of the five minute interval.

```r
ggplot(interval, aes(x=interval, y=steps)) +
  geom_line(color = "firebrick")
```

![](figures unnamed-chunk-11-1.png)<!-- -->

Maximum steps ,on average, across all the days.

```r
interval[which.max(interval$steps),]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```

### Imputing missing values
summarise all the missing data

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Filling in a missing NA with the average number of steps in the same 5-mim interval

```r
activity_full <- activity
nas <- is.na(activity_full$steps)
avg_interval <- tapply(activity_full$steps, activity_full$interval, mean, na.rm=TRUE, simplify=TRUE)
activity_full$steps[nas] <- avg_interval[as.character(activity_full$interval[nas])]
```

checking there are no missing values

```r
sum(is.na(activity_full$steps))
```

```
## [1] 0
```
Number of steps taken in each 5 minute interval per day

```r
steps_full <- activity_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```
## # A tibble: 61 x 2
##    date                 steps
##    <dttm>               <dbl>
##  1 2012-10-01 00:00:00 10766.
##  2 2012-10-02 00:00:00   126 
##  3 2012-10-03 00:00:00 11352 
##  4 2012-10-04 00:00:00 12116 
##  5 2012-10-05 00:00:00 13294 
##  6 2012-10-06 00:00:00 15420 
##  7 2012-10-07 00:00:00 11015 
##  8 2012-10-08 00:00:00 10766.
##  9 2012-10-09 00:00:00 12811 
## 10 2012-10-10 00:00:00  9900 
## # ... with 51 more rows
```
 
Histogram of steps per day , including missing values

```r
ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![](figures unnamed-chunk-17-1.png)<!-- -->
Mean and median steps with filled in values

```r
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
mean_steps_full
```

```
## [1] 10766.19
```

```r
median_steps_full
```

```
## [1] 10766.19
```
### Difference in activity patterns between week days and weekends

```r
activity_full <- mutate(activity_full, weektype = ifelse(weekdays(activity_full$date) == "Saturday" | weekdays(activity_full$date) == "Sunday", "weekend", "weekday"))
activity_full$weektype <- as.factor(activity_full$weektype)
head(activity_full)
```

```
##       steps       date interval    day weektype
## 1 1.7169811 2012-10-01        0 Monday  weekday
## 2 0.3396226 2012-10-01        5 Monday  weekday
## 3 0.1320755 2012-10-01       10 Monday  weekday
## 4 0.1509434 2012-10-01       15 Monday  weekday
## 5 0.0754717 2012-10-01       20 Monday  weekday
## 6 2.0943396 2012-10-01       25 Monday  weekday
```
Use ggplot to show the difference

```r
interval_full <- activity_full %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

![](figures unnamed-chunk-20-1.png)<!-- -->
   
