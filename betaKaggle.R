rm(list=ls());
set.seed(1);
library(ggplot2);
library(dplyr);
library(lubridate);
library(data.table);
library(betareg);

train<- read.csv(file='train.csv')
# N<-dim(train);
# N<-N[1];
# nTrain<-ceiling(N[1]*0.8);

RMSLE <- function(x) sqrt(mean(x^2));

features <- c("season",
              "weather",
              "temp",
              "atemp",
              "humidity",
              "windspeed");
randomeff<-c("holiday",
              "workingday",
              "day",
              "hour");

maxC<-max(train$count)+1;
train$countC<-train$count/maxC;
maxCcas<-max(train$casual)+2;
train$casualtmp<-train$casual+1;
train$countCcas<-train$casualtmp/maxC;
maxCreg<-max(train$registered)+2;
train$registeredtmp<-train$registered+1;
train$countCreg<-train$registeredtmp/maxC;
train$hour <- hour(ymd_hms(train$datetime));
train$day <- as.POSIXlt(ymd_hms(train$datetime))$wday;
# trainFea <- train[1:nTrain,];
# 
# testFea <- train[c(nTrain+1:N),];
# testFea<-testFea[!rowSums(is.na(testFea)),];

truetable<-matrix(c(
                rep(c(rep(0,2^5),rep(1,2^5)),2^0),
                rep(c(rep(0,2^4),rep(2,2^4)),2^1),
                rep(c(rep(0,2^3),rep(3,2^3)),2^2),
                rep(c(rep(0,2^2),rep(4,2^2)),2^3),
                rep(c(rep(0,2^1),rep(5,2^1)),2^4),
                rep(c(rep(0,2^0),rep(6,2^0)),2^5))
              ,nrow=2^6,ncol=6,byrow=FALSE);
results<-matrix(data=99999,nrow=12*2-1,ncol=1);

# strC="countC~";
strCcas="countCcas~";
strCreg="countCreg~";
strRE="|holiday+hour+workingday+day"
sumRMSLE<-0;

j<-0;
for (i_year in unique(year(ymd_hms(train$datetime)))) {
  for (i_month in unique(month(ymd_hms(train$datetime)))) {
#     i_year<-2011;
#     i_month<-2;
    if (!(i_year==2011 & i_month==1)){
      testmp<-subset(train,year(ymd_hms(datetime))==i_year & month(ymd_hms(datetime))==i_month);
      trainFea<-subset(train,ymd_hms(datetime) <= min(ymd_hms(testmp$datetime)));
      ifoutFea<-matrix(data=1,nrow=1,ncol=6);
      for (k in 1:6){
        if (length(unique(trainFea[,features[k]]))<2) ifoutFea[k]<-0; 
      }
      testFea<-subset(testmp,day(ymd_hms(datetime))<=10);
      resultscas<-matrix(data=99999,nrow=2^6,ncol=8);
      resultsreg<-matrix(data=99999,nrow=2^6,ncol=8);
      for (i in 2:2^6){
#         i<-2;
        print(c(i_year,i_month,i));
        candidate<-ifoutFea*truetable[i,];
        if (sum(candidate)>0){
          strDe<-paste(features[candidate],collapse="+");
#           str<-paste(strC,strDe,strRE);
          strcas<-paste(strCcas,strDe,strRE);
          strreg<-paste(strCreg,strDe,strRE);
          
          ols.1<-betareg(strcas, data = trainFea);
          ols.2<-betareg(strreg, data = trainFea);
          
          x11<-RMSLE( log(predict(ols.1)*maxCcas-1+1)-log(trainFea$casual+1));
          x21<-RMSLE( log(predict(ols.1,testFea)*maxCcas-1+1)- log(testFea$casual+1));
          resultscas[i,1:6]<-candidate;
          resultscas[i,7]<-x11;
          resultscas[i,8]<-x21;
          x12<-RMSLE( log(predict(ols.2)*maxCreg-1+1)-log(trainFea$registered+1));
          x22<-RMSLE( log(predict(ols.2,testFea)*maxCreg-1+1)- log(testFea$registered+1));
          resultscas[i,1:6]<-candidate;
          resultsreg[i,7]<-x12;
          resultsreg[i,8]<-x22;

        }
      }
      j<-j+1;
      results[j,]<-min(resultscas[,8])+min(resultsreg[,8]);
    }
  }
}
best<-mean(results);
