rm(list=ls());
set.seed(1);
library(ggplot2);
library(dplyr);
library(lubridate);
library(data.table);
library(randomForest);
library(stringr);
library(caret);

train<- read.csv(file='train.csv')

RMSLE <- function(x) sqrt(mean(x^2));

features1 <- c("season",
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
              "arcsintemp",
              "divsqrttemp",
              "tempatemp",
              "temphumidity",
              "tempwindspeed",
              "atemp",
              "logatemp",
              "sqrtatemp",
              "arcsinatemp",
              "divsqrtatemp",
              "atemphumidity",
              "atempwindspeed",
              "humidity",
              "loghumidity",
              "sqrthumidity",
              "arcsinhumidity",
              "divsqrthumidity",
              "humiditywindspeed",
              "windspeed",
              "logwindspeed",
              "sqrtwindspeed",
              "arcsinwindspeed",
              "divsqrtwindspeed"
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
              "hourwindspeed"
              "casual");
features2 <- c("season",
              "weather",
              "temp",
              "atemp",
              "humidity",
              "windspeed",
              "holiday",
              "workingday",
              "day",
              "hour",
              "registered");

train$hour <- as.POSIXlt(as.character(word(train$datetime,2)), format = "%H:%M")
train$hour <- as.numeric(format(train$hour,"%H"))

train$day <- as.POSIXlt(as.character(word(train$datetime,1)), format = "%Y-%m-%d")$wday

results<-matrix(data=99999,nrow=12*2-1,ncol=1);
j<-1;
for (i_year in unique(year(ymd_hms(train$datetime)))) {
  for (i_month in unique(month(ymd_hms(train$datetime)))) {
#   i_year<-2011;
#   i_month<-2
    if (!(i_year==2011 & i_month==1)){
      testmp<-subset(train,year(ymd_hms(datetime))==i_year & month(ymd_hms(datetime))==i_month);
      trainFea<-subset(train,ymd_hms(datetime) <= min(ymd_hms(testmp$datetime)));
      testFea<-subset(testmp,day(ymd_hms(datetime))<=10);
      resultscas<-matrix(data=99999,nrow=5*4,ncol=1);
      resultsreg<-matrix(data=99999,nrow=5*4,ncol=1);
      k<-1;
      for (i in seq(100,500,100)){
        for (ll in 2:5){
#         i<-100;
          print(c(i_year,i_month,i,ll));
              
          rf.1 <- randomForest(casual ~ ., data=trainFea[,features1],mtry=ll,ntree=i);
          
          x11<-RMSLE( log(predict(rf.1)+1)-log(trainFea$casual+1));
          x21<-RMSLE( log(predict(rf.1,testFea)+1)- log(testFea$casual+1));
          resultscas[k,]<-x21
          
          rf.2 <- randomForest(registered ~ ., data=trainFea[,features2],mtry=ll,ntree=i);
          
          x12<-RMSLE( log(predict(rf.2)+1)-log(trainFea$registered+1));
          x22<-RMSLE( log(predict(rf.2,testFea)+1)- log(testFea$registered+1));
          resultsreg[k,]<-x22
          k<-k+1;
        }
      }
      results[j,]<-min(resultscas)+min(resultsreg);
      j<-j+1;
    }
  }
}
best<-mean(results);
