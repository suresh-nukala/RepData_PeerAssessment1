## install plotting packages

install.packages("gplots")
install.packages("ggplot2")
install.packages("caret")
library(gplots)
library(ggplot2)
library(caret)

## Reading data file
data = read.csv("activity.csv", header = TRUE)
missing = is.na(data) 
clean.data = subset(data, missing == 'FALSE', select = steps:interval)

# summary of clean data by day
sum.by.date = aggregate(clean.data$steps, by = list(clean.data$date), FUN = sum)
median(sum.by.date$x)
mean(sum.by.date$x)

# summary of clean data by 5 min interval
meanInterval = aggregate(clean.data$steps, by = list(clean.data$interval), FUN = mean)

qplot(meanInterval$Group.1, meanInterval$x, geom="line", xlab="interval", ylab="avg steps")

test = data
for (i in 1:nrow(test)){
  t1<- test[i,]
  if(is.na(t1$steps) == TRUE){
    t2 = subset(meanInterval, meanInterval$Group.1 == t1$interval)
    test$steps[i]=t2$x
  }
}
View(test)

## Imputing missing data using 5min interval mean data
SumDate_Imputed <- aggregate(test$steps, by = list(test$date), FUN = sum)
hist(SumDate_Imputed$x, col="gray")


## Identifying weekday & weekend
test$date <- as.Date(test$date, format = "%Y-%m-%d")
test$weekday <- weekdays(test$date)


for (i in 1:nrow(test)){
    if(test$weekday[i] == "Sunday" || test$weekday[i] == "Saturday" ){
    test$weekend[i] = 1
  }
  else{
    test$weekend[i] = 100
  }
}

join <- aggregate(test$steps,by = list(test$interval, test$weekend), FUN = mean)
colnames(join) = c("interval","Weekday","Avgsteps")
xyplot(Avgsteps ~ interval| Weekday, data=join, layout =c(1,2),type='l')
