install.packages("ISLR")
library(ISLR)

data("Smarket") #use the Smarket dataset in the ISLR package
options(digits = 3)  #set the number of digits after the decimal to 3
names(Smarket)
summary(Smarket)
str(Smarket)
pairs(Smarket)
cor(Smarket[,-9])  #look at the pairwise correlation of the predictors

# --------- Logistic Regression to classify the response variable, Direction
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)

#the type="response" option provides output probabilities, P(Y=1|X) rather than the log-odds (probabilities on logit scale)
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
glm.probs2 <- predict(glm.fits) #the log-odds
glm.probs2[1:10]
contrasts(Smarket$Direction) #dummy variable 1 is assigned to Up

glm.pred <- rep("Down", 1250) #a vector of 1250 Down values
glm.pred[glm.probs > 0.5] <- "Up"  #change those > .5 to Up
table(glm.pred, Smarket$Direction) #create the confusion matrix for prediction
mean(glm.pred == Smarket$Direction) #percent predicted correctly
#However, we trained this model on the entire dataset which is optimistic
#it is called training error rate because we only used training data, no test data
#training error rate = 1 - correct rate which is 47.8% in this case
#our correct rate was 52.2%

#let's create a training set containing years 2001-2004
train <- (Smarket$Year < 2005)  #a logical vector
Smarket.2005 <- Smarket[!train,]  #this will become our test dataset
Direction.2005 <- Smarket$Direction[!train]

#fit a model with only the training data
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response") #test with 2005 test data
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"  #a character vector of Up or Down based on predictions against the test data
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #the % of predictions we got correct is worse than before
mean(glm.pred != Direction.2005) #test error rate (1 - correct rate)

#fit a glm with only 2 predictors and remove those that are not significant
glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #accuracy rate
mean(glm.pred != Direction.2005) #test error rate

#Finally, predict a probability with specific values of predictors
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), 
                                       Lag2 = c(1.1, -0.8)), type = "response")

# ------------------- K-Nearest Neighbors KNN -----------
library(class)
attach(Smarket)
train.X <- cbind(Lag1, Lag2)[train,] #training data from 2001-2004
test.X <- cbind(Lag1, Lag2)[!train,] #test data
train.Direction <- Direction[train] #classification values for Direction from 2001-2004

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
