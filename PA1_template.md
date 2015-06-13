
---
title: "Reproducible Research: Peer Assessment 1"
author: "Sabine Lengger"
output: 
  html_document:
    keep_md: true
---

# Analysis of a personal step-recording device
========================================================

## 1. Loading the data

I used the unzip function in order to unzip the file. 

```{r}
unzip("activity.zip",  exdir = "C:/Users/slengger/Documents/Coursera/Reproducible_Research/RepData_PeerAssessment1")
```

Then, I loaded the data from the extracted file and made sure that all variables were in the correct format by using the as.Date and as.numeric functions. 

```{r}

data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date)
data$steps <- as.numeric(data$steps)
                                                                                   
```

## 2. What is the mean total number of steps taken per day?

I first split the data according to date, using the split() function and created a list of dataframes called data.day. 


```{r}

data.day <- split(data, data$date)

```

Using sapply and the sum function, I could then calculate the total number of steps taken per day, ignoring the missing values (by setting na.rm = TRUE), and storing them in a vector called steps.day). The values in the vector were named with the days. Then, I made a dataframe with two columns, the day (extracted with the names() function from steps.day, and the total number of steps, Steps) and produced a histogram from this dataframe. 

```{r}

steps.day <- sapply(data.day, function(x) { sum(x$steps, na.rm = TRUE) } )
steps.day.df <- data.frame("Day" = names(steps.day), Steps = steps.day, row.names = NULL)
hist(steps.day.df$Steps, main = "Total number of steps taken per day", xlab = "Number of steps", col = "red")

```

### Mean and median number of steps per day

From the data frame containing the total number of steps I could easily calculate the mean and the median by using the correspondingly named functions. 

```{r}
mean1 <- mean(steps.day.df$Steps)
median1 <-  median(steps.day.df$Steps)
```
The mean was `r mean1` and the median was `r median1`. 

## 3. What is the daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
data.int <- split(data, data$interval)
ave.steps <- sapply(data.int, function(x) { mean(x$steps, na.rm = TRUE) } )
ave.steps.df <- data.frame(Interval = names(ave.steps), Steps = ave.steps, row.names = NULL, stringsAsFactors = FALSE)
plot(ave.steps.df$Interval, ave.steps.df$Steps, type = "n", xlab = "Interval", ylab = "Number of steps")
lines(x = ave.steps.df$Interval, y = ave.steps.df$Steps, type = "l")
```


### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
maximum <- max(ave.steps.df$Steps)
s <- ave.steps.df$Interval[ave.steps.df$steps == maximum]
```

## 4. Imputing missing values


### The total number of missing values

I ran the is.na() function on the dataset, which returned a logical vector that I saved as 'missing'. The sum of this vector gave me the number of NAs. 
```{r}
missing <- is.na(data)
sum(missing)
```

### Filling in the missing values from the dataset

The strategy I decided to use for filling in the missing values is probably a little bit lazy. I think the best way would probably be to use the average of this certain interval on this weekday, but I have not done the module "Getting and cleaning data", so I just used the easiest way and filled in all the missing values with the total average of steps per interval. 

```{r}
data$steps[is.na(data$steps)] <- mean(data$steps, na.rm = TRUE)
missing <- is.na(data)
sum(missing)
```

### Histogram of the processed data

I drew a histogram of the processed data. 

```{r}
data.day <- split(data, data$date)
steps.day <- sapply(data.day, function(x) { sum(x$steps, na.rm = TRUE) } )
steps.day.df <- data.frame("Day" = names(steps.day), Steps = steps.day, row.names = NULL)
hist(steps.day.df$Steps, main = "Total number of steps taken per day", xlab = "Number of steps", col = "red")
```

The data was shifted to what looks more like a normal distribution around the mean. 

```{r echo = FALSE}
mean2 <- mean(steps.day.df$Steps)
median2 <- median(steps.day.df$Steps)
```

The mean (`r mean2`) and the median (`r median2`) are equal now. Table 2 shows a comparison of the mean and median before and after data processing: 

```{r echo = FALSE}
means <- c(mean1, mean2)
medians <- c(median1, median2)
tab <- data.frame(Means = means, Medians = medians)
knitr::kable(tab, digits = 2)
```

## 5. Are there differences in activity patterns between weekdays and weekends?

I first used the weekday() function in order to create a vector that contained the weekdays and called it 'week'. 

```{r}
week <- weekdays(data$date)
```

I created a column in the dataset that is called 'Weekdays' and contains logical values, TRUE if it's a weekend-day, FALSE if it's a weekday. In order to check if that worked well, I calculated the sum. 

```{r}
data$Weekdays <- (week == "Saturday" | week == "Sunday") 
sum.weekdays <- sum(data$Weekdays)
sum.weekdays
```
The sum of all Saturdays and Sundays divided by the total number of values `r sum.weekdays/nrow(data)` should roughly equal 2 divided by 7 (`r 2/7`), which it does. 

I then made this into a numeric, then a factor with two levels (Weekday and Weekend). 
```{r}
data$Weekday_numeric <- as.numeric(data$Weekdays)
data$Weekday_factor <- as.factor(data$Weekday_numeric)
levels(data$Weekday_factor) = c("Weekday", "Weekend")
```

### Plot the average of intervals by weekday / weekend

I tried to plot the average of the intervals by weekday and weekend. 

```{r}
library(ggplot2)
p <- qplot(interval, steps,  data  = data,  stat = "summary", fun.y = "mean", facets  =  Weekday_factor  ~  .,  bin)
p + geom_line()
```

However, I thought this looked really messy, hence I smoothed it using geom = c("point", "smooth"). 

```{r}

q <- qplot(interval, steps,  data  = data,  stat = "summary", fun.y = "mean", geom = c("point", "smooth"), facets  =  Weekday_factor  ~  .,  bin)
q
```

This is a much nicer plot. :)

