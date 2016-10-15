rm(list=ls());
set.seed(1);
library(ggplot2);
library(plyr);
library(dplyr);
library(lubridate);
library(data.table);
library(randomForest);
library(stringr);
library(caret);

train<- read.csv(file='train.csv');
test<- read.csv(file='test.csv');

RMSLE <- function(x) sqrt(mean(x^2));

extractFeature<-function(x,y) x[,y];

features <- c("season",
               "seasontemp",
               "seasonatemp",
               "seasonhumidity",
               "seasonwindspeed",
               "weather",
               "weathertemp",
               "weatheratemp",
               "weatherhumidity",
               "weatherwindspeed",
               "temp",
               "logtemp",
               "sqrttemp",
               "divsqrttemp",
               "tempatemp",
               "temphumidity",
               "tempwindspeed",
               "atemp",
               "logatemp",
               "sqrtatemp",
               "divsqrtatemp",
               "atemphumidity",
               "atempwindspeed",
               "humidity",
               "loghumidity",
               "sqrthumidity",
               "divsqrthumidity",
               "humiditywindspeed",
               "windspeed",
               "logwindspeed",
               "sqrtwindspeed",
               "divsqrtwindspeed",
               "holiday",
               "holidaytemp",
               "holidayatemp",
               "holidayhumidity",
               "holidaywindspeed",
               "workingday",
               "workingdaytemp",
               "workingdayatemp",
               "workingdayhumidity",
               "workingdaywindspeed",
               "day",
               "daytemp",
               "dayatemp",
               "dayhumidity",
               "daywindspeed",
               "hour",
               "hourtemp",
               "houratemp",
               "hourhumidity",
               "hourwindspeed");

# train$hour    <- as.POSIXlt(as.character(word(train$datetime,2)), format = "%H:%M")
# train$hour    <- as.numeric(format(train$hour,"%H"))
# 
# train$day    <- as.POSIXlt(as.character(word(train$datetime,1)), format = "%m/%d/%Y") # works
# day1 <- train$day
# train$day <- (day1$day + 1) # SUN==1, MON==2,...., SAT==7
# 
# test$hour    <- as.POSIXlt(as.character(word(test$datetime,2)), format = "%H:%M")
# test$hour    <- as.numeric(format(test$hour,"%H"))
# 
# test$day    <- as.POSIXlt(as.character(word(test$datetime,1)), format = "%Y-%m-%d") 
# day2 <- test$day
# test$day <- (day2$day + 1)

train$hour <- as.POSIXlt(as.character(word(train$datetime,2)), format = "%H:%M")
train$hour <- as.numeric(format(train$hour,"%H"))

train$day <- as.POSIXlt(as.character(word(train$datetime,1)), format = "%Y-%m-%d")$wday

test$hour <- as.POSIXlt(as.character(word(test$datetime,2)), format = "%H:%M")
test$hour <- as.numeric(format(test$hour,"%H"))

test$day <- as.POSIXlt(as.character(word(test$datetime,1)), format = "%Y-%m-%d")$wday


train$seasontemp<-train$season*train$temp;
train$seasonatemp<-train$season*train$atemp;
train$seasonhumidity<-train$season*train$humidity;
train$seasonwindspeed<-train$season*train$windspeed;

train$weathertemp<-train$weather*train$temp;
train$weatheratemp<-train$weather*train$atemp;
train$weatherhumidity<-train$weather*train$humidity;
train$weatherwindspeed<-train$weather*train$windspeed;

train$holidaytemp<-train$holiday*train$temp;
train$holidayatemp<-train$holiday*train$atemp;
train$holidayhumidity<-train$holiday*train$humidity;
train$holidaywindspeed<-train$holiday*train$windspeed;

train$workingdaytemp<-train$workingday*train$temp;
train$workingdayatemp<-train$workingday*train$atemp;
train$workingdayhumidity<-train$workingday*train$humidity;
train$workingdaywindspeed<-train$workingday*train$windspeed;

train$daytemp<-train$day*train$temp;
train$dayatemp<-train$day*train$atemp;
train$dayhumidity<-train$day*train$humidity;
train$daywindspeed<-train$day*train$windspeed;

train$hourtemp<-train$hour*train$temp;
train$houratemp<-train$hour*train$atemp;
train$hourhumidity<-train$hour*train$humidity;
train$hourwindspeed<-train$hour*train$windspeed;

train$logtemp<-log(train$temp);
train$sqrttemp<-sqrt(train$temp);
train$divsqrttemp<-(train$temp)^(-1/2);
train$tempatemp<-train$temp*train$atemp;
train$temphumidity<-train$temp*train$humidity;
train$tempwindspeed<-train$temp*train$windspeed;

train$logatemp<-log(train$atemp);
train$sqrtatemp<-sqrt(train$atemp);
train$divsqrtatemp<-(train$atemp)^(-1/2);
train$atemphumidity<-train$atemp*train$humidity;
train$atempwindspeed<-train$atemp*train$windspeed;

train$loghumidity<-log(train$humidity+0.000001);
train$sqrthumidity<-sqrt(train$humidity);
train$divsqrthumidity<-(train$humidity+0.000001)^(-1/2);
train$humiditywindspeed<-train$humidity*train$windspeed;

train$logwindspeed<-log(train$windspeed+0.000001);
train$sqrtwindspeed<-sqrt(train$windspeed);
train$divsqrtwindspeed<-(train$windspeed+0.000001)^(-1/2);

test$seasontemp<-test$season*test$temp;
test$seasonatemp<-test$season*test$atemp;
test$seasonhumidity<-test$season*test$humidity;
test$seasonwindspeed<-test$season*test$windspeed;

test$weathertemp<-test$weather*test$temp;
test$weatheratemp<-test$weather*test$atemp;
test$weatherhumidity<-test$weather*test$humidity;
test$weatherwindspeed<-test$weather*test$windspeed;

test$holidaytemp<-test$holiday*test$temp;
test$holidayatemp<-test$holiday*test$atemp;
test$holidayhumidity<-test$holiday*test$humidity;
test$holidaywindspeed<-test$holiday*test$windspeed;

test$workingdaytemp<-test$workingday*test$temp;
test$workingdayatemp<-test$workingday*test$atemp;
test$workingdayhumidity<-test$workingday*test$humidity;
test$workingdaywindspeed<-test$workingday*test$windspeed;

test$daytemp<-test$day*test$temp;
test$dayatemp<-test$day*test$atemp;
test$dayhumidity<-test$day*test$humidity;
test$daywindspeed<-test$day*test$windspeed;

test$hourtemp<-test$hour*test$temp;
test$houratemp<-test$hour*test$atemp;
test$hourhumidity<-test$hour*test$humidity;
test$hourwindspeed<-test$hour*test$windspeed;

test$logtemp<-log(test$temp);
test$sqrttemp<-sqrt(test$temp);
test$divsqrttemp<-(test$temp)^(-1/2);
test$tempatemp<-test$temp*test$atemp;
test$temphumidity<-test$temp*test$humidity;
test$tempwindspeed<-test$temp*test$windspeed;

test$logatemp<-log(test$atemp);
test$sqrtatemp<-sqrt(test$atemp);
test$divsqrtatemp<-(test$atemp)^(-1/2);
test$atemphumidity<-test$atemp*test$humidity;
test$atempwindspeed<-test$atemp*test$windspeed;

test$loghumidity<-log(test$humidity+0.000001);
test$sqrthumidity<-sqrt(test$humidity);
test$divsqrthumidity<-(test$humidity+0.000001)^(-1/2);
test$humiditywindspeed<-test$humidity*test$windspeed;

test$logwindspeed<-log(test$windspeed+0.000001);
test$sqrtwindspeed<-sqrt(test$windspeed);
test$divsqrtwindspeed<-(test$windspeed+0.000001)^(-1/2);


submission <- data.frame(datetime=test$datetime, count=NA);
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# for (i_year in unique(year(ymd_hms(test$datetime)))) {
#   for (i_month in unique(month(ymd_hms(test$datetime)))) {
    i_year<-2012;
    i_month<-12;
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testFea<-test[testLocs,];
    trainFea<-subset(train,ymd_hms(datetime) <= min(ymd_hms(testFea$datetime)));
    print(c(i_year,i_month));

    rf <- randomForest(trainFea[,features], trainFea[,"casual"], ntree=100, importance=TRUE)
    imp <- importance(rf, type=1)
    featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
    sortedImportance <- arrange(featureImportance, desc(Importance))
    
#     rfe.1 <- rfe(trainFea[,features], trainFea[,"casual"], sizes=c(1:52), rfeControl=control);
#     rfe.2 <- rfe(trainFea[,features], trainFea[,"registered"], sizes=c(1:20), rfeControl=control);
    
   
#     submission[testLocs, "count"] <- predict(rfe.1,testFea)+predict(rfe.2,testFea);
#   }
# }
