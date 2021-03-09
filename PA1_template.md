---
title: "Assignment Reproducible Research 1"
---

First of all we're loading the data:

```{r "loading data", echo=TRUE, cache=TRUE}
data<- read.csv(file ="activity.csv",sep=",",colClasses =c( "numeric","Date","numeric"))
data$day<-weekdays(data$date)
```
The data can be found under `data`.

# Mean total number of steps
Now we calculate the mean total number of steps taken per day:

```{r "mean steps" ,echo=TRUE}
library(ggplot2)
data1<-aggregate(steps~date,data,sum,na.rm=TRUE)

mean0<-mean(data1$steps,na.rm = TRUE)
median0<-median(data1$steps,na.rm = TRUE)
qplot(steps,data=data1,binwidth=1000)

```

We found out that the mean is `r mean(data1$steps,na.rm = TRUE)` and the median `r median(data1$steps,na.rm = TRUE)`per day.

# Average daily Pattern

``` {r echo=TRUE}
intervallAv<- aggregate(steps~interval,data,mean,na.rm=TRUE)
ggplot(intervallAv,aes(x=interval,y=steps))+geom_line(color="blue")+labs(x="Intervall",y="Average Steps ")
```

# Inputting missing values

After counting the missing values we fill them up.

```{r "missing value",echo=TRUE}
sum(is.na(data$steps))
```

## Method for Replacing missing values
Every missing value will be replace by the mean of the week.

``` {r "replace NA", echo=TRUE}
weekAve<-aggregate(steps~interval+date,data,mean,na.rm=TRUE)
replaceNA<-merge(data,weekAve,by =c("interval","date"))

replaceNA<- data.frame(replaceNA[,1:4])
names(replaceNA)<-c("interval","day","steps","date")
replaceNA<-replaceNA[order(replaceNA$date,replaceNA$interval),]
```

# Plotting the Histogram

```{r "histogram",echo=TRUE,cache=TRUE}
histogram<-aggregate(steps~date,data,sum,na.rm=TRUE)
qplot(steps,data=histogram,binwidth=1000)
```

# Computing Mean and Median

```{r "mean 2.0",echo=TRUE}

mean(histogram$steps)
median(histogram$steps)
```


# Differences in Weekdays & - ends?

```{r "weekdays", echo=TRUE,cache=TRUE}

replaceNA$daytype<-  ifelse(replaceNA$date %in% c("Samstag", "Sonntag"),"Weekend", "Weekday")
weekagg<-aggregate(steps ~ interval + daytype, replaceNA, mean)
ggplot(weekagg,aes(x=interval,y=steps))+
  geom_line(color="red",size=1)+
  facet_wrap(~daytype,nrow = 2,ncol=1)+
  labs(x="Intervall",y="Steps",title = "Differences between Weekdays and Weekend")
```