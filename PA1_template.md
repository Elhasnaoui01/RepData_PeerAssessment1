---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
library(ggplot2)
library(dplyr)
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

``` r
total_steps_per_day <- data %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))
ggplot(total_steps_per_day, aes(x = total_steps)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Histogram of Total Steps per Day", x = "Total Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean_steps <- mean(total_steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(total_steps_per_day$total_steps, na.rm = TRUE)
mean_steps
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

``` r
avg_steps_interval <- data %>% group_by(interval) %>% summarize(avg_steps = mean(steps, na.rm = TRUE))
ggplot(avg_steps_interval, aes(x = interval, y = avg_steps)) + 
  geom_line(color = "blue") +
  labs(title = "Average Daily Activity Pattern", x = "5-minute Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
max_interval <- avg_steps_interval[which.max(avg_steps_interval$avg_steps), ]
max_interval
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values

``` r
missing_values <- sum(is.na(data$steps))
missing_values
```

```
## [1] 2304
```

``` r
data_imputed <- data
for(i in 1:nrow(data_imputed)) {
  if(is.na(data_imputed$steps[i])) {
    interval_mean <- avg_steps_interval$avg_steps[avg_steps_interval$interval == data_imputed$interval[i]]
    data_imputed$steps[i] <- interval_mean
  }
}
total_steps_per_day_imputed <- data_imputed %>% group_by(date) %>% summarize(total_steps = sum(steps))
ggplot(total_steps_per_day_imputed, aes(x = total_steps)) + 
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Histogram of Total Steps per Day (Imputed Data)", x = "Total Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
mean_steps_imputed <- mean(total_steps_per_day_imputed$total_steps)
median_steps_imputed <- median(total_steps_per_day_imputed$total_steps)
mean_steps_imputed
```

```
## [1] 10766.19
```

``` r
median_steps_imputed
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
data_imputed$date <- as.Date(data_imputed$date)
data_imputed$day_type <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
avg_steps_day_type <- data_imputed %>% group_by(interval, day_type) %>% summarize(avg_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

``` r
ggplot(avg_steps_day_type, aes(x = interval, y = avg_steps, color = day_type)) + 
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Weekday vs Weekend Activity Patterns", x = "5-minute Interval", y = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

