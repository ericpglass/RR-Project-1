---
title: "Reproducible Research - Project 1"
author: "Eric Glass"
date: "Saturday, January 17, 2015"
output: html_document
---

code for reading in the dataset and/or processing the data:

```{r}
library(plyr)
url<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "activity.zip")
unzip("activity.zip", exdir = getwd())
data<-read.csv("activity.csv")
```

histogram of the total number of steps taken each day:

```{r}
S=with(data,tapply(steps,date,sum,na.rm=T))
hist(S,main="",xlab="total number of steps",ylab="")
```

mean and median number of steps taken each day:

```{r}
mean(S)
median(S)
```

time series plot of the average number of steps taken (averaged across all days) versus the 5-minute intervals:

```{r}
T=with(data,tapply(steps,interval,mean,na.rm=T))
plot(names(T),T,type="l",xlab="time interval",ylab ="number of steps")  
```

5-minute interval that, on average, contains the maximum number of steps:

```{r}
which(T==max(T))
T[which(T==max(T))]
```

describe and show with code a strategy for imputing missing data:
first, identify n/a in summary table. then if n/a, impute missing data by replacement w/ actuals.
```{r}
summary(data)
data2 = data
for (k in 1:17568)
{if (is.na(data[k,"steps"])) data2[k,"steps"]= T[[as.character(data[k,"interval"])]]
}
```

histogram of the total number of steps taken each day after missing values were imputed:

```{r}
S2=with(data2,tapply(steps,date,sum,na.rm=T))
hist(S,main="",xlab="total number of steps",ylab="")
```

mean and median number of steps taken each day:

```{r}
mean(S2)
median(S2)
```
panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:

```{r}
data2$day=weekdays(as.Date(data2$date))
data2wday =subset(data2, ! day %in% c("Saturday","Sunday"))
data2wkend =subset(data2,  day %in% c("Saturday","Sunday"))

par(mfrow=c(2,1))
Tday=with(data2wday,tapply(steps,interval,mean))
plot(names(Tday),Tday,type="l",xlab="Weekday Activity",ylab ="number of steps")
Tend=with(data2wkend,tapply(steps,interval,mean))
plot(names(Tend),Tend,type="l",xlab="Weekend Activity",ylab ="number of steps")
```


