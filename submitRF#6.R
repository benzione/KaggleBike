library(lubridate)
library(randomForest)
library(stringr)
library(dplyr)
library(randomForest)
library(ggplot2)


# My own utility functions:
l2 <- function(x) x^2 %>% sum %>% sqrt 
l1 <- function(x) abs(x) %>% sum  
MSE <- function(x) x^2 %>% mean 
RMSE <- function(x) x^2 %>% mean %>% sqrt


train <- read.csv("C:\\Users\\bordezki\\Google Drive\\Data minning course\\train.csv")
test <- read.csv("C:\\Users\\bordezki\\Google Drive\\Data minning course\\test.csv")

train$restday <- (train$holiday | !(train$workingday)) %>% as.integer
test$restday <- (test$holiday | !(test$workingday)) %>% as.integer

train$howHot1 <- train$temp * train$humidity
test$howHot1 <- test$temp * test$humidity

train$howHot2 <- train$atemp * train$humidity
test$howHot2 <- test$atemp * test$humidity

train$WandW <- train$weather * train$windspeed
test$WandW <- test$weather * test$windspeed

train$hour    <- as.POSIXlt(as.character(word(train$datetime,2)), format = "%H:%M")
train$hour    <- as.numeric(format(train$hour,"%H"))

train$weekday    <- as.POSIXlt(as.character(word(train$datetime,1)), format = "%m/%d/%Y") # works
day1 <- train$weekday
train$weekday <- (day1$wday + 1) # SUN==1, MON==2,...., SAT==7

test$hour    <- as.POSIXlt(as.character(word(test$datetime,2)), format = "%H:%M")
test$hour    <- as.numeric(format(test$hour,"%H"))

test$weekday    <- as.POSIXlt(as.character(word(test$datetime,1)), format = "%Y-%m-%d") 
day2 <- test$weekday
test$weekday <- (day2$wday + 1) # SUN==1, MON==2,...., SAT==7


extractFeatures <- function(data) {
  features <- c("season",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "restday",
                "hour",
                "weekday",
                "howHot1",
                "howHot2",
                "WandW")
  
  return(data[,features])
}

submission <- data.frame(datetime=test$datetime, count=NA)
submission.row <- 1

# We only use past data to make predictions on the test set, 
# so we train a new model for each test set cutoff point

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testData <- test[testLocs,]
    testData$prediction <- NA
    
    trainLocs  <- mdy_hm(train$datetime) <= min(ymd_hms(testData$datetime))
    trainData <- train[trainLocs,]
    
    for(h in 0:23){
      dataByHour <- filter(trainData, hour == h)
      cat("Hour: ", h, "\n")
      rf.casual <- randomForest(extractFeatures(dataByHour), dataByHour[,"casual"], ntree=100)
      rf.registered <- randomForest(extractFeatures(dataByHour), dataByHour[,"registered"], ntree=100)
     
      for (j in 1:nrow(testData)){
        if ((testData[j,"hour"] == h)) { 
          casual.prediction <- predict(rf.casual, extractFeatures(testData[j,]))
          registered.prediction <- predict(rf.registered, extractFeatures(testData[j,]))
          testData[j,"prediction"] <- casual.prediction + registered.prediction      
        }#if
      }# For
    }# For 
                                     
    for (l in 1:nrow(testData)){
      submission$count[submission.row] <- testData$prediction[l]
      submission.row <- submission.row+1
    }                                                                    
  } #For - a month
} #For - a year


write.csv(submission, file = "C:/Users/bordezki/Google Drive/Data minning course/submitRF#6.csv", row.names=FALSE)


# Train a model across all the training data and plot the variable importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
sortedImportance <- arrange(featureImportance, desc(Importance))
View(sortedImportance)









