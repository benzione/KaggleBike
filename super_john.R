source('make_samples.R');
library(magrittr) # for piping
library(dplyr) # for handeling data frames

# My own utility functions:
l2 <- function(x) x^2 %>% sum %>% sqrt;
l1 <- function(x) abs(x) %>% sum;
MSE <- function(x) x^2 %>% mean;
missclassification <- function(tab) sum(tab[c(2,3)])/sum(tab);

set.seed(2015);

View(prostate);
# now verify that your data looks as you would expect....

ols.1<-(lm(lcavol~. ,data = prostate.train));
# Train error:
MSE( predict(ols.1)- prostate.train$lcavol);
# Test error:
MSE( predict(ols.1, newdata = prostate.test)- prostate.test$lcavol);

