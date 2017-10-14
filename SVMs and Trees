
# Lab 5; Quant III, Kyle Davis
# Collapse All: ALT+O
# Instert Section: CTRL+SHIFT+R
getwd()
library(doParallel)
registerDoParallel(cl = makeCluster(detectCores()))

# 1: Generating DGP --------------------------------------------------------------


library(caret)
library(MASS)
library(Rlab)
library(boot)
library(ggplot2)
library(ggridges)
library(dplyr)


set.seed(12345)

N <- 2000
P <- 50

## Ballanced
mu    <- runif(P, -1,1)
Sigma <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma <- ifelse(row(Sigma) != col(Sigma), 0, Sigma)
X     <- mvrnorm(N, mu=mu, Sigma = Sigma)
p     <- rbern(P, 1)
beta  <- p*rnorm(P,1,0.9) + (1-p)*rnorm(P,0,0.3)
eta   <- X%*%beta
pi    <- inv.logit(eta)
Y     <- rbern(N, pi)
## sum(Y)/length(Y)
Y     <- as.factor(Y)
data.1.Bal <- data.frame(X, Y)


## High >> .5
mu2    <- runif(P, 0.5, 1.5)
Sigma2 <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma2 <- ifelse(row(Sigma2) != col(Sigma2), 0, Sigma2)
X2     <- mvrnorm(N, mu=mu2, Sigma = Sigma2)
p2     <- rbern(P, 1)
beta2  <- p2*rnorm(P,1,0.1) + (1-p2)*rnorm(P,0,1)
eta2   <- X2%*%beta2
pi2    <- inv.logit(eta2)
Y2     <- rbern(N, pi2)
## sum(Y2)/length(Y2)
Y2     <- as.factor(Y2)
data.2.High <- data.frame(X2, Y2)


## Low << .5
mu3    <- runif(P, -2, 0)
Sigma3 <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma3 <- ifelse(row(Sigma3) != col(Sigma3), 0, Sigma3)
X3     <- mvrnorm(N, mu=mu3, Sigma = Sigma3)
p3     <- rbern(P, 1)
beta3  <- p3*rnorm(P,1,1.7) + (1-p3)*rnorm(P,0,0.01)
eta3   <- X3%*%beta3
pi3    <- inv.logit(eta3)
Y3     <- rbern(N, pi3)
## sum(Y3)/length(Y3)
Y3     <- as.factor(Y3)
data.3.Low <- data.frame(X3, Y3)



### ::Test:: Function DGPs ----------------------------------------------------


gen_binary_data = function(seed=12345, mean_beta, N=2000, P=20)
{
     set.seed(seed)

     ###
     ### Generating Data
     ###


     ## We can use the Wishart distribution to simulate positive-
     ## definite matrices. This isn't the only way, but for this
     ## example it's convenient.
     Sigma = rWishart(n=1, df=P, Sigma=diag(P))[,,1]
     ## Note: the [,,1] is required because the Wishart is a distribution
     ## over matrices, so each simulated 'draw' is a matrix in 2 dimensions
     ## so each draw exists in a third dimension of the resulting array.

     ## And we can then simulate our predictors
     X <- mvrnorm(N, runif(P, 2.0, 3.0), Sigma)

     ## Generate the betas
     beta = rnorm(P, mean_beta , .5)

     ## And generate some target probabilities
     p = 1/(1+exp(-X%*%beta))

     ## And now y
     y = rbinom(N, 1, p)

     ## We will convert this to a factor so that the glmnet/caret
     ## functions know not to treat it as a continuous variable.
     y = factor(y)
     table(y)

     ## Make a data frame
     my_data = data.frame(X,y)
     my_data
}




## Make the data
my_data = gen_binary_data(mean_beta=-0.25, N=2000, P=200)


## See if it's imbalanced
table(my_data$y)


# 2: Test Set/Training Set --------------------------------------------------------------

## Test Set's Training Sets:

## Test/Training Sets for Even Data
test.data.1.Bal  <- data.1.Bal[1:500,]
train.data.1.Bal <- data.1.Bal[501:2000,]

## Test/Training Sets for High Success Data
test.data.2.High  <- data.2.High[1:500,]
train.data.2.High <- data.2.High[501:2000,]

## Test/Training Sets for Low Success Data
test.data.3.Low  <-  data.3.Low[1:500,]
train.data.3.Low <- data.3.Low[501:2000,]


# 3: Elastic Nets and Predictions --------------------------------------------------------------

## Elastic Net:
enet.Bal <- train(Y~., method="glmnet",
                     tuneGrid=expand.grid(alpha=seq(0, 1, .1),
                                          lambda=seq(0, 5, .05)),
                     data=train.data.1.Bal,
                     preProcess=c("center"),
                     trControl=trainControl(method="cv",number=2, search="grid"))

ggplot(enet.Bal, aes(x=enet.Bal$results$lambda,
                     y=enet.Bal$results$Accuracy))+
     geom_point( aes(colour = factor(enet.Bal$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For Ballanced Data")+
     xlab( expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))+
     theme(legend.position = c(.85, .65))

yhat = predict(enet.Bal)
qplot(train.data.1.Bal$Y, yhat, alpha=I(0.25))+
     geom_jitter()+
     xlab( expression(paste("Training ", hat(y))))+
     ylab( expression(paste("Elastic Net ", y)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))

enet_beta = coef(enet.Bal$finalModel, enet.Bal$bestTune$lambda)
qplot(beta, enet_beta[-1])+
     xlab( expression(paste("True " , beta)))+
     ylab( expression(paste("Elastic Net " , hat(beta))))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))

enet.Bal$bestTune$lambda # 0
enet.Bal$bestTune$alpha  # 0


## High
enet.High <- train(Y2~., method="glmnet",
                     tuneGrid=expand.grid(alpha=seq(0, 1, .1),
                                          lambda=seq(0,.4,.005)),
                     data=train.data.2.High,
                     preProcess=c("center"),
                     trControl=trainControl(method="cv",number=2, search="grid"))

ggplot(enet.High, aes(x=enet.High$results$lambda,
                     y=enet.High$results$Accuracy))+
     geom_point( aes(colour = factor(enet.High$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For High Success DGP")+
     xlab( expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))+
     theme(legend.position = c(.85, .65))

yhat = predict(enet.High)
qplot(train.data.2.High$Y2, yhat, alpha=I(0.25))+
     geom_jitter()+
     xlab( expression(paste("Training ", hat(y))))+
     ylab( expression(paste("Elastic Net ", y)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))

enet_beta = coef(enet.High$finalModel, enet.High$bestTune$lambda)
qplot(beta2, enet_beta[-1])+
     xlab( expression(paste("True " , beta)))+
     ylab( expression(paste("Elastic Net " , hat(beta))))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))


enet.High$bestTune$lambda # 0
enet.High$bestTune$alpha  # 0.2


## Low
enet.Low <- train(Y3~., method="glmnet",
                    tuneGrid=expand.grid(alpha=seq(0,  1, .1),
                                         lambda=seq(0, .7, .005)),
                    data=train.data.3.Low,
                    preProcess=c("center"),
                    trControl=trainControl(method="cv",number=2, search="grid"))

ggplot(enet.Low, aes(x=enet.Low$results$lambda,
                      y=enet.Low$results$Accuracy))+
     geom_point( aes(colour = factor(enet.Low$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For Low Success DGP")+
     xlab( expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))+
     theme(legend.position = c(.85, .65))


yhat = predict(enet.Low)
qplot(train.data.3.Low$Y3, yhat, alpha=I(0.25))+
     geom_jitter()+
     xlab( expression(paste("Training ", hat(y))))+
     ylab( expression(paste("Elastic Net ", y)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))

enet_beta = coef(enet.Low$finalModel, enet.Low$bestTune$lambda)
qplot(beta3, enet_beta[-1])+
     xlab( expression(paste("True " , beta)))+
     ylab( expression(paste("Elastic Net " , hat(beta))))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))

enet.Low$bestTune$lambda # 0
enet.Low$bestTune$alpha  # 0.1

## enet.Bal$results
enet.Bal$results[as.numeric(rownames(enet.Bal$bestTune)),]

## enet.High$results
enet.High$results[as.numeric(rownames(enet.High$bestTune)),]

## enet.Low$results
enet.Low$results[as.numeric(rownames(enet.Low$bestTune)),]

# 4: Up-sampling Down-sampling Robustness Checking --------------------------------------------------------------

### Up Sampling
# High Data
train.up.high <- upSample(x= train.data.2.High[, -ncol(train.data.2.High)],
                          y= train.data.2.High$Y2)
## table(train.up.high$Class) #Checking
# Low Data
train.up.low  <- upSample(x= train.data.3.Low[, -ncol(train.data.3.Low)],
                          y= train.data.3.Low$Y3)
## table(train.up.low$Class)


### Down Sampling
# High Data
train.down.High <- downSample(x= train.data.2.High[, -ncol(train.data.2.High)],
                              y= train.data.2.High$Y2)
## table(train.down.High$Class)

# Low Data
train.down.Low <- downSample(x= train.data.3.Low[, -ncol(train.data.2.High)],
                             y= train.data.3.Low$Y3)
## table(train.down.Low$Class)



### Rerun elastic net model on upscaled and downscaled data:

enet.up.high <- train(Class~., method="glmnet",
                         tuneGrid=expand.grid(alpha=seq(0, 1, 0.1),
                                              lambda=seq(0, 200, 1)),
                         data=train.up.high,
                         preProcess=c("center"),
                         trControl=trainControl(method="cv",number=2, search="grid"))

plot(enet.up.high)


enet.up.low <- train(Class~., method="glmnet",
                       tuneGrid=expand.grid(alpha=seq(0, 1, 0.1),
                                            lambda=seq(0, 200, 1)),
                       data=train.up.low,
                       preProcess=c("center"),
                       trControl=trainControl(method="cv",number=2, search="grid"))

plot(enet.up.low)

## Trained Down Data:

enet.down.high <- train(Class~., method="glmnet",
                      tuneGrid=expand.grid(alpha=seq(0, 1, 0.1),
                                           lambda=seq(0, 200, 1)),
                      data=train.down.High,
                      preProcess=c("center"),
                      trControl=trainControl(method="cv",number=2, search="grid"))

plot(enet.down.high)


enet.down.low <- train(Class~., method="glmnet",
                     tuneGrid=expand.grid(alpha=seq(0, 1, 0.1),
                                          lambda=seq(0, 200, 1)),
                     data=train.down.Low,
                     preProcess=c("center"),
                     trControl=trainControl(method="cv",number=2, search="grid"))

plot(enet.down.low)


#Results for each:
enet.up.high$results[as.numeric(rownames(enet.up.high$bestTune)),]

enet.up.low$results[as.numeric(rownames(enet.up.low$bestTune)),]

enet.down.high$results[as.numeric(rownames(enet.down.high$bestTune)),]

enet.down.low$results[as.numeric(rownames(enet.down.low$bestTune)),]


#Testing predictions:
## up-sampling
yhat.up.high <- predict(enet.up.high, newdata = test.data.2.High)
table(test.data.2.High$Y2, yhat.up.high)  # Check Results
accuracy  <- 492/500 ## Probability correct / All observation
accuracy

yhat.up.low <- predict(enet.up.low, newdata = test.data.3.Low)
table(test.data.3.Low$Y3, yhat.up.low)
accuracy  <- 492/500
accuracy

## down-sampling
yhat.down.high <- predict(enet.down.high, newdata = test.data.2.High)
table(test.data.2.High$Y2, yhat.down.high)
accuracy <- 490/500
accuracy

yhat.down.low <- predict(enet.down.low, newdata = test.data.3.Low)
table(test.data.3.Low$Y3, yhat.down.low)
accuracy <- 490/500
accuracy


# 5: SVMs and Regression Trees --------------------------------------------------------------

# Let's use a support vector machine (SVM) on ballanced data:

# More simple, linear model, with squared error loss reduction to classifiy (separate) data:

linear.svm.bal  = train(Y~.,
                       data=train.data.1.Bal,
                       method="svmLinear")

linear.svm.bal$results$Accuracy  # 96% accuracy

# A polynomial kernal: (may take awhile to run)
poly.svm.bal    = train(Y~.,
                     data=train.data.1.Bal,
                     method="svmPoly")

poly.svm.bal$results[as.numeric(rownames(poly.svm.bal$bestTune)),]
# 92% accuracy, first degree.

# This kernal uses bootstrapped samples for penalties, this exibits a high penalty
# for misclassification:
radial.svm.bal  = train(Y~.,
                     data=train.data.1.Bal,
                     method="svmRadialCost")

radial.svm.bal$results[as.numeric(rownames(radial.svm.bal$bestTune)),]
#about 54% accuracy.

## let's run this on the high and low data, but first here's another option using the DMwR package:
## In this case I found this incredibly computationally difficult.

## We need the DMwR library installed:
library(DMwR)

## We can create a training set:
smote_train = SMOTE(Y~., data= data.1.Bal)

## We can play around with the training controls, in this case
## using 3-fold cross validation
ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

## And we can fit a model, like an SVM
ctrl.poly.svm = train(Y~.,
                    data=smote_train,
                    method="svmPoly",
                    trainControl= ctrl)


### High and Low data linear SVMs:

# High
linear.svm.high  = train(Y2~.,
                        data=train.data.2.High,
                        method="svmLinear")

linear.svm.high$results$Accuracy  # 97% accuracy


# Low
linear.svm.low  = train(Y3~.,
                         data=train.data.3.Low,
                         method="svmLinear")

linear.svm.low$results$Accuracy  # 97% accuracy




### 5a: Regression Trees --------------------------------------------

# The theory behind regression trees is to run multiple regressions, specifying our results at each branch, and applying various
# methods (pruning, bagging, random forrests) to make these more robust and explainatory. Here we will be using trees for
# classification, but regression is ran very similarly.

# Using "rpart", may need to reload R to get things to work.
library(rpart)

# grow tree
tree.bal <- rpart(Y~.,
             method="class", data=train.data.1.Bal)

printcp(tree.bal) # display the results
plotcp(tree.bal) # visualize cross-validation results
summary(tree.bal) # detailed summary of splits

#This is all well and good,

# plot tree
plot(tree.bal, uniform=TRUE,
     main="Classification Tree for Ballanced Data")
text(tree.bal, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(tree.bal,
     title = "Classification Tree for Ballanced Data")

# then we have a lot of options off of this, including pruning the tree back, or making -multiple- trees into a forrest and
# selecting the best one! (intutively a form of advanced bootstrapping!)

# prune the tree, by the error that was previously generated above
ptree.bal <- prune(tree.bal, cp=   tree.bal$cptable[which.min(tree.bal$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(ptree.bal, uniform=TRUE,
     main="Pruned Classification Tree for Ballanced Data")
text(ptree.bal, use.n=TRUE, all=TRUE, cex=.8)
post(ptree.bal,
     title = "Pruned Classification Tree for Ballanced Data")

# Great! But how does this work on our low predictive model, for example?

## Low Data:
# grow tree
tree.low <- rpart(Y3~.,
                  method="class", data=train.data.3.Low)

printcp(tree.low) # display the results
plotcp(tree.low) # visualize cross-validation results
summary(tree.low) # detailed summary of splits

# plot tree for the low data:
plot(tree.low, uniform=TRUE,
     main="Classification Tree for Ballanced Data")
text(tree.low, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(tree.low,
     title = "Classification Tree for Low Data")

# prune the tree, by the error that was previously generated above
ptree.low <- prune(tree.low, cp=   tree.low$cptable[which.min(tree.low$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(ptree.low, uniform=TRUE,
     main="Pruned Classification Tree for Low Data")
text(ptree.low, use.n=TRUE, all=TRUE, cex=.8)
post(ptree.low,
     title = "Pruned Classification Tree for Low Data")



