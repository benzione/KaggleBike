##Question 2
rm(list=ls());
library(dplyr)
library(nycflights13)
library(lubridate)

flights;
x<-filter(flights,month==11);
n<-nrow(x);
dateflight<-ymd(paste(x[1,1],'-',x[1,2],'-',x[1,3]));
for (i in 2:n){
  dateflight<-c(dateflight,ymd(paste(x[i,1],'-',x[i,2],'-',x[i,3])))
};
depDelay<-x[,'dep_delay'];
df<-data.frame(dateflight,depDelay);
b<-filter(df, weekdays(dateflight) == "Sunday");
c<-mean(b[,2], na.rm=TRUE);

##Question 3
rm(list=ls());
library(dplyr);
library(nycflights13);
library(lubridate);
flights;
airlines;
daysarr<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday");
x<-flights %>% left_join(airlines,by="carrier");
y<-filter(x,name=="JetBlue Airways");

pdf("HistAll.pdf",width=7,height=5);
hist(y$dep_delay);
dev.off();

pdf("BoxplotAll.pdf",width=7,height=5);
boxplot(y$dep_delay);
dev.off();

hist(y$dep_delay);
boxplot(y$dep_delay);
n<-nrow(y);
dateflight<-ymd(paste(x[1,1],'-',x[1,2],'-',x[1,3]));
for (i in 2:n){
  dateflight<-c(dateflight,ymd(paste(x[i,1],'-',x[i,2],'-',x[i,3])))
};
depDelay<-y[,'dep_delay'];
df<-data.frame(dateflight,depDelay);
for (i in 1:7){
  b<-filter(df, weekdays(dateflight) == daysarr[i]);
  
  str<-paste("Hist",daysarr[i],".pdf",sep="")
  pdf(str,width=7,height=5);
  hist(b$dep_delay);
  dev.off();

  str<-paste("Boxplot",daysarr[i],".pdf",sep="")
  pdf(str,width=7,height=5);
  boxplot(b$dep_delay);
  dev.off();
}

##Question 4
rm(list=ls());
library(dplyr);
library(nycflights13);
library(lubridate);
gender <- c(rep('Male', 22), rep('Female', 23));
drink <- c(rep('Coke', 12), rep('Coffee', 10), rep('Coke', 3), rep('Coffee', 20));
drinks<-data.frame(gender, drink);
write.csv(x=drinks,file="drink.csv",row.names = FALSE);
rm(list=ls());
drinks<-read.csv(file="drink.csv");
table(drinks)

##Michael
rm(list=ls());
library(dplyr);
library(nycflights13);
library(lubridate);
flights;
airlines;
joinedData <- left_join(airlines,flights, by="carrier")
joinedData <- filter(joinedData, carrier=="B6" )
pdf("HistogramOfDelay.pdf",width=7,height=5)
hist(joinedData$dep_delay)
dev.off()
pdf("BoxPlotOfDelay.pdf",width=7,height=5)
boxplot(joinedData$dep_delay)
dev.off()
daysArray <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
joinedData <- mutate(joinedData, DayName = factor(daysArray[day]))
pdf("HistogramByDay.pdf",width=10,height=7)
par(mfrow=c(4,3))
by(joinedData,joinedData$DayName,function(i){
  hist(i$dep_delay, xlab=unique(i$DayName), main=unique(i$DayName))
})
dev.off()
pdf("BoxPlotByDay.pdf",width=10,height=7)
par(mfrow=c(4,3))
by(joinedData,joinedData$DayName,function(i){
  boxplot(i$dep_delay, xlab=unique(i$DayName), main=unique(i$DayName))
})
dev.off()

rm(list=ls());
library(dplyr);
library(nycflights13);
library(lubridate);
a<-c('a','b','c');
b<-c(1:5)
c<-factor(a[b])
