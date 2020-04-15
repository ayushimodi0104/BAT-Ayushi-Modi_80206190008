rm(list = ls())

setwd('C:/Users/ayumodi/Desktop')

df <- read.csv('batdataset.csv',sep = ',')
df

colnames(df)[1] <- 'Name'

#Some stats
summary(df) 

#Checking the datatypes of each column
sapply(df, class)

#Computing the mean of each column
size <- dim(df)[1]

custom.mean = function(column) {
  sum <- 0
  for (i in column){
    sum  <- sum + i
  }
  sum/size
}

att.mean <- custom.mean(df$Attendence) # = 76.43333
etm.mean <- custom.mean(df$EndTermMarks) # = 85.96667

#Computing the standard deviation
#This function computes sample standard deviation and uses size-1 in the denominator
#Use size instead of size-1 to compute population standard deviation
custom.std.dev = function(column) {
  sum <- 0
  mu <- custom.mean(column)
  for (i in column){
    sum <- sum + (i - mu)^2
  }
  (sum/(size-1))^0.5  
}

att.stddev <- custom.std.dev(df$Attendence)
etm.sttdev <- custom.std.dev(df$EndTermMarks)

z.score.converter = function(column) {
  column <- (column - custom.mean(column))/custom.std.dev(column)
  column
}
df$Attendence <- z.score.converter(df$Attendence)
df$EndTermMarks <- z.score.converter(df$EndTermMarks)

one.hot.encoding = function(column){
  coded <- c()
  for (i in column){
    if (i == 'F') {
      coded <- c(coded, 0)
    } 
    else {
      coded <- c(coded, 1)
    }
  }
  coded
}
df$Gender <- one.hot.encoding(df$Gender)

#Train Test split using Stratified Sampling
library(caret)
idx <- createDataPartition(df$Gender, p = 0.7, list  = FALSE)
train <- df[idx, ]
test <- df[-idx, ]


y <- train$EndTermMarks
x <- train$Attendence

ytest<- test$EndTermMarks
xtest<- test$Attendence

size <- dim(x)[1]

#Gradient Descent
x <- cbind(1, matrix(x))
param <- matrix(rnorm(2), nrow = 2)

lr <- 0.01

for (i in 1:100){
  err <- (x %*% param - y)
  
  #Finding first derivatives of the parameters
  der <- t(x) %*% err / size 
  
  #Updating the parameters
  param <- param - lr * der 
}

param
#First term is intercept - 0.06160208
#Second term is slope - 0.21881591

xtest <- cbind(1, matrix(xtest))

#Predictions
ypred <- xtest %*% param  

#Root Mean Square Error
(mean((ypred - ytest)^2))^0.5

#Compare with parameters obtain through inbuilt linear regression function
lm.model <- lm(y ~ x)
summary(lm.model)

