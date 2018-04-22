install.packages("caret")
library(caret)
library(ISLR)
library(boot)
attach(Auto)


set.seed(1)
#split the data into training and test data
#randomly select 196 of the 392 values to use as a training dataset
train <- sample(392, 196) 
#fit a linear model using only training dataset (here it is a random selection of half the data)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
#calculate the average SSE (i.e., MSE) using the test dataset
mean((mpg - predict(lm.fit, Auto))[-train]^2)
#use higher powers of the predictors with poly()
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


#repeat this process with a different training set
set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
#we find in both cases that a model that predicts mpg using a quadratic performs best

# ---------- Leave One Out Cross-Validation (LOOCV) --------
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
#the returned delta value is the LOOCV estimate for the test MSE which in this case is the average of the 392 test error estimates
cv.err$delta

# ---------- k-Fold Cross-Validation --------------
set.seed(17)
cv.error.10 <- rep(0,10)
for(i in 1:10) {
      glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
      #randomly divide the data into 10 equal groups
      cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10 #the estimate for the test MSE or average of the 10 test error estimates







