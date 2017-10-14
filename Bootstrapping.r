# LAB 2; bootstrapping practice. Kyle Davis
#####




#####
# Question 1
# synthetic data:

N <- 1000; S <- 1e5
x <- rnorm(N, mean=10, sd=1)
mean_x <- rep(NA, S) #Empty Matrix

mean_x <- sample(x, 1e7, replace=TRUE, prob = NULL)

mean(mean_x) ; 1/mean(mean_x)
# Mean 10
# 1/x = .1

# optional:
# Look into Taylor Series:
install.packages("pracma")
library(pracma) #runs Local polynomial approximation through Taylor series.
f <- function(x) 1/x
taylor.series <- taylor(f, mean(mean_x), n = 4) #taylor(function, series expansion, Taylor Series Order (1:4))
mean(taylor.series)







#####
# Question 2

# Cosine Simularity Function:
cos.sim <- function(x,y){
  if(!is.numeric(x)){return("Numbers Needed in matrix (x)")}
  if(!is.numeric(y)){return("Numbers Needed in matrix (y)")}
  
  cosine.similarity <- sum( x%*%y )/(sqrt( sum(x^2) ) %*% sqrt( sum(y^2) ))
  distance <- cosine.similarity -1
  return.list <- list("cosine similarity" = cosine.similarity, "distance" = distance)
  
  return(return.list)
}

x <- c(8,1,5)
y <- c(8,1,5)

cos.sim(x,y)

#Testing:
x <- c("Flag", 2, 3)
y <- c(4, -5, "Baseball")
cos.sim(x,y) #Warns Users if value is string=T
#End Testing

#                              Some (very rough) Notes:
# 1 - similarity = distance. So if vectors point in same direction it will be =1 
# The normalizing function makes the similar vectors 1, and are not effected by any magnitude. (similar to Pearson)
# their cosine "similarity" will always be 1 for x>0, and -1 for x<0. so x=0 if we subtract one to get our distance.
# Some of this depends on if the values in the vectors are positive or negative. 

library(Rlab)

N <-1000
x <- rbern(N, .5)
y <- rbern(N, .5)
cos.sim(x,y) 




#Empty matrix for simulation:
simulation1 <- matrix(nrow = 1000, ncol = 1)
simulation2 <- matrix(nrow = 1000, ncol = 1)

for (i in 1:1000)
{
  N <-1000
  x <- rbern(N, .5)
  y <- rbern(N, .5)

  x2 <- rbern(N, .8)
  y2 <- rbern(N, .8)
    
  simulation1[i] <- cos.sim(x,y)$'cosine similarity'
  simulation2[i] <- cos.sim(x2,y2)$'cosine similarity'
  
}

quantile(simulation1,  probs = c(.1, .5, .9))
quantile(simulation2,  probs = c(.1, .5, .9))

library(ggplot2)
p1 <- qplot(simulation1)+
  geom_vline(xintercept = .4742163, col="Red")+
  geom_vline(xintercept = .5254967, col="Red")+
  ggtitle("1000 Bootstrapped Simulated Cosine Similarities")+
  xlab("Simulated Cosine Similarities")+
  theme_bw()

p2 <- qplot(simulation2)+
  geom_vline(xintercept = .7846132, col="Red")+
  geom_vline(xintercept = .8141496, col="Red")+
  ggtitle("1000 Bootstrapped Simulated Cosine Similarities")+
  xlab("Simulated Cosine Similarities")+
  theme_bw()

p1

p2




#####
#Question 3

exp_x <- rep(NA, N) #Empty

for(i in 1:1000)
{
  N <- 1000
  x <- rnorm(N, mean=2, sd=1)
  exp_x[i] <- exp(mean(x))
}

sqrt(mean(exp_x))
sqrt(mean(exp_x)/2)


library(ggplot2)
qplot(exp_x)+
  ggtitle("exp_x Histogram")+
  theme_bw()



#####
#Question 4


library(MASS) #for mvrnorm
library(texreg) #for tables

  N <- 1000
  mu <- c(5,5)
  Sigma <- matrix( c(2, 0, 0, 3), nrow = 2, ncol= 2)
  mvr <- mvrnorm(N, mu, Sigma)
  b <- c(2, -1, 3)
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
  e <- rnorm(N, 0, sd=sqrt(4)) #errors as listed in DGP
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
  my_data <- data.frame(ones, mvr[,1], mvr[,2], y)
  m1 <- lm(y ~ X[,2] + X[,3])
  
#summary(m1)$coef 


#For Bootstrapping:
bs.sample <- function(dat)
{
  idx <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
  dat[idx,]
}

bs.routine <- function(dat)
{
  sample.dta <- bs.sample(dat)
  coef(lm(y ~ X[,2] + X[,3], data = my_data))
}

bs_est <- replicate(1000, bs.routine(dat = my_data)) #replicate our estimates
bs_mean <- rowMeans(bs_est) #report our bootstrap estimate means
bs_ci <- t(apply(bs_est, 1, quantile, probs = c(0.05, 0.95))) #make ci's
bs_results <- cbind(bs_mean, bs_ci) #create reference matrix for texreg
colnames(bs_results) <- c("Est", "Low", "High") #rename

#create table output, overriding confidence intervals with bootstrap low-high bounds
texreg(l= list(m1), stars = numeric(0),
       custom.coef.names = c("Intercept", "x_1", "x_2"),
       override.ci.low = c(bs_results[, 2]),
       override.ci.up = c(bs_results[, 3]),
       caption.above = T, float.pos = "h!",
       custom.note = "CI's Overridden by Bootstrap; [.05, .95]")




#Changing out Errors: Overriding model 1

N     <- 1000
mu    <- c(5,5)
Sigma <- matrix( c(2, 0, 0, 3), nrow = 2, ncol= 2)
mvr   <- mvrnorm(N, mu, Sigma)
b     <- c(2, -1, 3)
e     <- rnorm(N, 0, sd=sqrt(abs(x[,2]))) #error now changed
ones  <- matrix(1, nrow=1000, ncol=1)
X     <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
y     <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
my_data <- data.frame(ones, mvr[,1], mvr[,2], y)
m1    <- lm(y ~ X[,2] + X[,3])

#For Bootstrapping:
bs.sample <- function(dat)
{
  idx <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
  dat[idx,]
}

bs.routine <- function(dat)
{
  sample.dta <- bs.sample(dat)
  coef(lm(y ~ X[,2] + X[,3], data = my_data))
}

bs_est <- replicate(1000, bs.routine(dat = my_data))
bs_mean <- rowMeans(bs_est)
bs_ci <- t(apply(bs_est, 1, quantile, probs = c(0.05, 0.95)))
bs_results <- cbind(bs_mean, bs_ci)
colnames(bs_results) <- c("Est", "Low", "High")

texreg(l= list(m1), stars = numeric(0),
       custom.model.names = c("Changed Error Model 1"),
       custom.coef.names = c("Intercept", "x_1", "x_2"),
       override.ci.low = c(bs_results[, 2]),
       override.ci.up = c(bs_results[, 3]),
       caption.above = T, float.pos = "h!",
       custom.note = "CI's Overridden by Bootstrap; [.05, .95]")



#####
#Question 5


#Generate DGP:
library(MASS) #for mvrnorm

N     <- 1000
mu    <- c(5,5)
Sigma <- matrix( c(2, 0, 0, 3), nrow = 2, ncol= 2)
mvr   <- mvrnorm(N, mu, Sigma)
b     <- c(2, -1, 3)
ones  <- matrix(1, nrow=1000, ncol=1)
X     <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
e     <- rnorm(N, 0, sd=sqrt(4))
y     <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
my_data <- data.frame(ones, mvr[,1], mvr[,2], y)
m1    <- lm(y ~ X[,2] + X[,3], data=my_data)
# summary(m1) 


#Test set training set
n = nrow(my_data)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = my_data[trainIndex ,] #80% sample
test = my_data[-trainIndex ,] #Remaining 20% by adding minus (-) sign

colnames(train) <- c("Intercept", "MVRx1", "MVRx2", "y") #rename
colnames(test) <- c("Intercept", "MVRx1", "MVRx2", "y") #rename


#creating data frames for MSE:
training.data <- data.frame(train$MVRx1, train$MVRx2)
testing.data <- data.frame(test$MVRx1, test$MVRx2)


#linear models for each:
mtrain <- lm(train$y ~ train$MVRx1 + train$MVRx2, data = train)
# summary(mtrain) 
# predict(mtrain) #predicted values for mtrain
mtest <- lm(test$y ~ test$MVRx1 + test$MVRx2, data = test)
# summary(mtest)



#MSE between full data and our training data
mean((my_data - predict(mtrain))^2)
# 92.37

mean((training.data - predict(mtest))^2)
# 85.41

mTesty <- lm(test$y ~ 2(test$MVRx1) + (test$MVRx2)^3, data = test)
# summary(mTesty)

mean((training.data - predict(mTesty))^2)
# 85.41




# function to split into arbitrary number of sets
#    (Stack Overflow helped a ton here to make this function work:)


test_split <- function(df, cuts, prob, ...)
{
  idx <- sample(seq(1, cuts), size = nrow(df), replace = FALSE, prob = prob, ...)
  z = list()
  for (i in 1:cuts)
  {
    z[[i]] <- df[idx == i,]
  }
  z
}
z <- test_split(my_data, 2, c(0.8, .2))


train <- z[1] #80% of data
test <- z[2]  #20% of data
head(train)

#matrix.train<- as.matrix(train)






#mean_x <- rep(x, S)
#?rep


N <- 1000; S <- 1e5
x <- rnorm(N, mean=0, sd=2)
mean_x <- rep(NA, S)

for(s in 1:S)
{
  sample_of_x <- sample(N, size=1000, replace=TRUE)
  mean_x[s] <- mean(x[sample_of_x])
}

mean(mean_x) ; sd(mean_x)

#sd is about the sqrt of 4 over 1000
library(ggplot2)
qplot(mean_x)



# Further (rough) notes:
# std deviation = std. error in simulation, but there's some theoretical differences here.
# Laws of large numbers! But we want our resampling to look about the same as our original N. 
# So our bootstraps should converge towards the truth DGP, if data is clustered or grouped corrilated with one another may be off
# bootstrapping can be modified for these data, doing different hierarchical clustering models for different types of this.
#
# Our estimater needs to be smooth (non-smooth estimators don't work in bootstrapping typically) quantile, or percentile analysis.
# Smooth, meaning we can't really take the first derivative of, jagged. 
# 
# two vectors we can do cosign simulators and get some uncertainty estimate. 
#

# This is non-paremetric bootstrapping

# Jackknifing is leaving one observation through taking out an observation, taking means and storing it at each step. 
# Doesn't work too well in small samples. doesn't really give you much more than a regular bootstrap. 

# more concerned for if model fits well than theoretical, need good model to begin with, if it's crap we don't care


# cross-validation is taking some data aside and having a test-set training-set. Our model see's the training set, then we validate
# using a test set that we originally witheld. WE can either "leave one out" take off an observation and predict that value
# And we can leave one out multiple times for each dataset across all observation. Or we could chunk the data and predcit chunks
#

