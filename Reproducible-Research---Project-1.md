Reproducible Research - Project 1
================
Sofía Cardona
22/1/2021

# Loading and preprocessing the data

``` r
require(tidyverse)
```

    ## Loading required package: tidyverse

    ## Warning: package 'tidyverse' was built under R version 4.0.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 4.0.3

    ## Warning: package 'tidyr' was built under R version 4.0.3

    ## Warning: package 'readr' was built under R version 4.0.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
require(ggplot2)

setwd("C:/Users/Sofi/Desktop")

data <- read_csv("activity.csv", col_names = T)
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   steps = col_double(),
    ##   date = col_date(format = ""),
    ##   interval = col_double()
    ## )

``` r
str(data)
```

    ## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   steps = col_double(),
    ##   ..   date = col_date(format = ""),
    ##   ..   interval = col_double()
    ##   .. )

## What is mean total number of steps taken per day?

Calculate the total steps per day

``` r
steps_per_day <- data%>% 
    group_by(date)%>%
    summarise(total = sum(steps))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

Make a histogram of the total number of steps taken each day

``` r
ggplot(steps_per_day, aes(total)) + geom_histogram(binwidth = 1000, fill = "skyblue") +
    xlab("Total steps per day") + ylab("Number of steps per day") +
    ggtitle("Histogram of the total steps taken each day") +
    theme_light()
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](Reproducible-Research---Project-1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per
day

``` r
mean(steps_per_day$total, na.rm = T) 
```

    ## [1] 10766.19

``` r
median(steps_per_day$total, na.rm = T) 
```

    ## [1] 10765

## What is the average daily activity pattern?

Calculate the mean of steps taken for each interval per day

``` r
int_steps <- data%>%
    group_by(interval)%>%
    summarise(steps = mean(steps, na.rm = T))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

Make a time series plot of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all days (y-axis)

``` r
ggplot(int_steps, aes(x=interval, y=steps)) +
    geom_line(color = "green", size = 0.8) +
    xlab("Five minute interval") +
    ylab("Number of steps taken") +
    ggtitle("Average daily activity pattern") +
    theme_light()
```

![](Reproducible-Research---Project-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

``` r
int_steps[which.max(int_steps$steps),]
```

    ## # A tibble: 1 x 2
    ##   interval steps
    ##      <dbl> <dbl>
    ## 1      835  206.

The maximum average of steps are 206 and occurs in the 835th interval
(8:35 am)

## Imputing missing values

Calculate and report the total number of missing values in the dataset

``` r
sum(is.na(data))
```

    ## [1] 2304

Are 2304 NA’s in the dataset

Chose a method for imputing all the missing values in the dataset.

In this case I imputed missing values with the interval average of steps
across days

Create a new dataset that is equal to the original dataset but with the
missing data filled in.

``` r
data_imputed <- data %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = T), steps))
```

Calculate the total steps per day

``` r
imputed_steps_per_day <- data_imputed%>% 
    group_by(date)%>%
    summarise(total = sum(steps))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

Make a histogram of the total number of steps taken each day

``` r
ggplot(imputed_steps_per_day, aes(total)) +
    geom_histogram(binwidth = 1000, fill = "magenta") +
    xlab("Total steps per day") + ylab("Number of steps per day") +
    ggtitle("Histogram of the total steps taken each day") +
    theme_light()
```

![](Reproducible-Research---Project-1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per
day.

``` r
mean(imputed_steps_per_day$total, na.rm = T) 
```

    ## [1] 10766.19

``` r
median(imputed_steps_per_day$total, na.rm = T) 
```

    ## [1] 10766.19

Do these values differ from the estimates from the first part of the
assignment?

Before fill NA’s:

  - mean = 10766.19
  - median = 10765

After fill NA’s

  - mean = 10766.19
  - median = 10766.19

What is the impact of imputing missing data on the estimates of the
total daily number of steps?

The mean didnt change, but the median increase and now is equal to the
mean

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable (wday) in the dataset with two levels:
“weekday” & “weekend”

``` r
w_steps <- data_imputed %>%
    mutate(wday = ifelse(weekdays(date) %in% c("sábado", "domingo"), "Weekend", "Weekday" ))

w_steps <- w_steps %>% mutate(wday = as.factor(wday))
```

Group by interval and wday, then calculate mean number of steps

``` r
w_int_steps <- w_steps %>% 
    group_by(interval,wday) %>%
    summarise(mean = mean(steps, na.rm = T))
```

    ## `summarise()` regrouping output by 'interval' (override with `.groups` argument)

Create time series plot with faceting on the new wday factor

``` r
ggplot(w_int_steps, aes(interval, mean, col = wday)) + 
    geom_line(show.legend = F) +
    facet_grid(rows = w_int_steps$wday) +
    xlab("Five minute interval") + ylab("Number of steps") +
    ggtitle("Average of steps taken") +
    theme_light()
```

![](Reproducible-Research---Project-1_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
