
# Penalized Regression (Lab 3); Kyle Davis
# ALT+O to collapse all

set.seed(12345) # for replication

# 1: DGP and Test/Training Sets ----------------------------------------------------------------------

# Packages:
library(caret)
library(MASS) #mvrnorm()
library(texreg) #for tables
library(Rlab) #for rbern()
library(ggplot2)

N     <- 1000
P     <- 100
Sigma <- rWishart(n=1, df=P, Sigma=diag(P))[,,1] # Wishart distribution for positive definite matricies

## Note: the [,,1] is required because the Wishart is a distribution
## over matrices, so each simulated 'draw' is a matrix in 2 dimensions
## so each draw exists in a third dimension of the resulting array.

X  <- mvrnorm(N, runif(P, -10, 10), Sigma)
p  <- rbern(P, 0.1)
b  <- p * rnorm(P, 5, 5) + (1-p) * rnorm(P, 0, 0.1)
e  <- rnorm(N, 0, sd=sqrt(10))
y = X%*%b + e
m1 <- lm(y~X)

my_data = data.frame(X,y)


### Test Set; Training Set
trainidx <- createDataPartition(y, times = 1, p=.80, list=FALSE) # Get 800 random index from y
trainy <- y[trainidx] # Give traniing set index numbers y's values
testy <- y[-trainidx] # Get test set remaining index, give it y's values

trainx <- X[trainidx,]
testx <- X[-trainidx,]

qplot(train, bins=50)+
  theme_bw()

qplot(test, bins=50)+
  theme_bw()

train_data = data.frame(trainx, trainy)
test_data = data.frame(testx, testy)


# 2: Calculate Errors ----------------------------------------------------------------------


#   m1$coef[-1] #Takes Intercept out compared to m1$coef
##  We can plot the true betas against the lm coefficients
qplot(b, coef(m1)[-1])+
  theme_bw()

#   Calculate error
##  error <- actual - predicted


#Training Model:
trainmod <- lm(train_data$trainy ~ trainx, data = train_data)
# summary(trainmod) # Check that this makes sense

error <- train_data$trainy - predict(trainmod)
# head(error)

#  Ideally we want normality here, mean zero
qplot(error, bins=50)+
  theme_bw()


#Root Mean Squred Error
sqrt(mean(error^2))

#Mean Aboslute Error
mean(abs(error))




# 3: LASSO ----------------------------------------------------------------


###
### The LASSO
###

train_data = data.frame(trainx, trainy)

# Using the caret package
mod_lasso = train(trainy~., method="glmnet", # formula and method
                  tuneGrid=expand.grid(alpha=0,
                                       lambda=seq(0,100,1)), #allow lambda to step down to zero
                  data=train_data,
                  preProcess=c("center"), #NEED to center for this
                  trControl=trainControl(method="cv",number=2, search="grid"))

# mod_lasso  #final values were around lambda = 14 using RMSE
qplot( mod_lasso$results$lambda, mod_lasso$results$RMSE)+
  theme_bw()

# narrowing:
mod_lasso = train(trainy~., method="glmnet",
                  tuneGrid=expand.grid(alpha=0,
                                       lambda=seq(0,40,0.01)),
                  data=train_data,
                  preProcess=c("center"),
                  trControl=trainControl(method="cv",number=2, search="grid"))

#mod_lasso  #narrowing in, we found a new best lambda of 13.79!

## We can generate predictions...
yhat = predict(mod_lasso)
qplot(trainy, yhat)+
  theme_bw()

## And we can see the best values for the best lambda
lasso_beta = coef(mod_lasso$finalModel, mod_lasso$bestTune$lambda)
qplot(b, lasso_beta[-1])
## Note---the [-1] is because the model contains an intercept.



# 4: Ridge ----------------------------------------------------------------


###
### Ridge regression
###



###  Testing:

library(dplyr)
library(MASS)
trainmod <- lm(train_data$trainy ~ trainx, data = train_data)
#select(lm.ridge(trainmod , lambda = seq(0,1,.001)))
ridge.model <- lm.ridge(trainmod , lambda = seq(0,1,.001))

qplot(ridge.model$lambda, ridge.model$GCV)

#zoom in on this
qplot(ridge.model$lambda, ridge.model$GCV)+
  coord_cartesian(xlim = c(0, .3), ylim = c(0.01509, .0155))+
  xlab( expression(paste("Ridge Model " , lambda)))+
  ylab( "Ridge Model GVC ")+
  theme_bw()+
  theme(axis.text=element_text(size=12))

###  End Testing



mod_ridge = train(trainy~., method="glmnet",
                  tuneGrid=expand.grid(alpha=1,
                                       lambda=seq(0,10,0.01)),
                  data=train_data,
                  preProcess=c("center"),
                  trControl=trainControl(method="cv",number=2, search="grid"))

#Best lambda (RMSE) is 1.01

mod_ridge = train(trainy~., method="glmnet",
                  tuneGrid=expand.grid(alpha=1,
                                       lambda=seq(0,3,0.01)),
                  data=train_data,
                  preProcess=c("center"),
                  trControl=trainControl(method="cv",number=2, search="grid"))

#New best lambda of .94

yhat = predict(mod_ridge)
qplot(trainy, yhat)

ridge_beta = coef(mod_ridge$finalModel, mod_ridge$bestTune$lambda)
qplot(b, ridge_beta[-1])



# 5: Elastic Net  ---------------------------------------------------------

###
### Elastic Nets
###

## Now, for the Elastic Net
mod_enet = train(trainy~., method="glmnet",
                 tuneGrid=expand.grid(alpha=seq(0,1,0.1), # can set both alphas, can "grid" as we do alpha and combo's
                                      lambda=seq(0,100,1)),
                 data=train_data,
                 preProcess=c("center"),
                 trControl=trainControl(method="cv",number=2, search="grid"))


# alpha used was .9 and lambda ended up being 1.


mod_enet = train(trainy~., method="glmnet",
                 tuneGrid=expand.grid(alpha=seq(0.6,1,0.5),
                                      lambda=seq(0,10,0.5)),
                 data=train_data,
                 preProcess=c("center"),
                 trControl=trainControl(method="cv",number=2, search="grid"))

#alpha ended up at .6 and lambda at 1.


yhat = predict(mod_enet)
qplot(trainy, yhat)

enet_beta = coef(mod_ridge$finalModel, mod_ridge$bestTune$lambda)
qplot(b, ridge_beta[-1])


#Let's plot each of these:

qplot(mod_lasso$results$lambda, mod_lasso$results$RMSE)+
  theme_bw()

qplot(mod_ridge$results$lambda, mod_ridge$results$RMSE)+
  theme_bw()

qplot(mod_enet$results$lambda, mod_enet$results$RMSE)+
  theme_bw()

