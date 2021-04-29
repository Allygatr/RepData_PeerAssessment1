library(dplyr)
library(lubridate)
library(ggplot2)

data = read.csv("activity.csv")

d = ymd(data$date)
data$date = d

## 1) Total number of steps, mean and median

df = filter(data, data$steps != "NA")

total_steps = df %>% group_by(date) %>% summarise( sum_steps = sum(steps))

mean_steps = mean(total_steps$sum_steps)

median_steps = median(total_steps$sum_steps)

hist(total_steps$sum_steps, breaks = 20, xlab = "Total number of steps", main = "Total number of steps taken per day")


## 2) What is the average daily activity 

avg_act = df %>% group_by(interval) %>% summarise( avg_steps = mean(steps))

with(data = avg_act, plot(interval, avg_steps, type = "l"))

## max interval (which.max gives location in list of the max interval)
avg_act[which.max(avg_act$avg_steps),] ## (104 = 206 steps)


## 3) Imputing NAs

# table (True is the number of NAs)
table(is.na(data$steps))

## Finding the mean of each day to impute into the missing values
mean_interval = df %>% group_by(interval) %>% summarise( m = mean(steps))
summary(mean_interval)

## took the mean of the means of the intervals to impute into the missing values

imp = data$steps
imp[is.na(imp)] = mean(mean_interval$m)

imp_data = data
imp_data$steps = imp

imp_total_steps = imp_data %>% group_by(date) %>% summarise( imp_sum_steps = sum(steps))

imp_mean_steps = mean(imp_total_steps$imp_sum_steps)

imp_median_steps = median(imp_total_steps$imp_sum_steps)

hist(imp_total_steps$imp_sum_steps, breaks = 20, xlab = "Total number of steps", main = "Total number of steps taken per day of Imputed Data")

## There is an increase in the 10,000 - 11,000 interval. But there has been no change in the mean and median,
## the median and mean for the imputed data set are the same



## 4) differences in activity patterns between weekdays and weekends

week_df = df

week_df$days = weekdays(week_df$date)

weekend = c("Saturday","Sunday")

week_df$week = ifelse(week_df$days %in% weekend,"weekend","weekday")

week_steps = week_df %>% group_by(interval,week) %>% summarise( w_mean_steps = sum(steps))

ggplot(data = week_steps, aes(interval,w_mean_steps, colour = week)) + geom_line() + 
  facet_grid(week~.) + labs(x = "Interval (per 5 mins)", y = "Avg. Number of steps", title = "Comparision of Activity over Weekdays and Weekends")

## 
