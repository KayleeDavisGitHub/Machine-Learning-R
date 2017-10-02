#multiplot
#####
###    This function will come in later:


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}








#Begin Lab
#####
#####  LAB 1 (accidently saved as Lab2)
#      Submit on Carmen


#Q1
#####
#              Question 1

set.seed(12345)
N <- 1000
#Two random x variables, with same mean and same error. Both normal
x_1 <- rnorm(N, mean=5, sd=sqrt(16))      
x_2 <- rnorm(N, mean=5, sd=sqrt(16))
#beta values to multiply with the Intercept, X_1, and X_2 matrix. 
b <- c(2, -1, 3)                          
#Perfect-world errors: e~Normal(0, sig^2). set pretty low, Number=1000
e <- rnorm(N, 0, sd=sqrt(4))          
#This vector of ones is to generate our intercept along with the "2" in our beta
ones <- matrix(1, nrow=1000, ncol=1)      
X <- matrix(c(ones, x_1, x_2), nrow=N)
head(X)  #Checked that the Matrix comes out right, and is intuitive
#y = intercept + b1*X_1 + b2*X_2 + some error
y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e  
my_data <- data.frame(x_1, x_2, y) 
#After having y, we can run our formula, OLS using lm()
m1 <- lm(y ~ x_1 + x_2, data=my_data)                    
summary(m1)$coef

#Do these results make sense?


#Q2

#####
##           Question 2

#Empty matrix
simulation1 <- matrix(nrow = 1000, ncol = 6)
head(simulation1) #To make an empty matrix of NA's to fill in simulated values later.

for (i in 1:1000)
{
  N <- 1000                           #Model from earlier
  x_1 <- rnorm(N, mean=5, sd=sqrt(16))
  x_2 <- rnorm(N, mean=5, sd=sqrt(16))
  b <- c(2, -1, 3)
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, x_1, x_2), nrow=N)
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
  m1 <- lm(y ~ x_1 + x_2)
  
  sm1 <- summary(m1)      #summary table is used to easily locate each number to store
  
  simulation1[i,1] <- sm1$coef[1,1]      #storing models first coef: Intercept
  simulation1[i,2] <- sm1$coef[2,1]      #x_1 coef estimate
  simulation1[i,3] <- sm1$coef[3,1]      #x_2 coef estimate
  simulation1[i,4] <- sm1$coef[1,2]     #standard error for Intercept
  simulation1[i,5] <- sm1$coef[2,2]     #se x_1
  simulation1[i,6] <- sm1$coef[3,2]     #se x_2
}

head(simulation1)  #To check our now-full matrix to see if values make sense




#Removing x_2 now
#New empty matrix
simulation2 <- matrix(nrow = 1000, ncol = 4)
head(simulation2)  #Empty

for (i in 1:1000)
{
  N <- 1000
  x_1 <- rnorm(N, mean=5, sd=sqrt(16)) 
  x_2 <- rnorm(N, mean=5, sd=sqrt(16))
  b <- c(2, -1, 3)                      
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, x_1, x_2), nrow=N)  
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e      
  m1 <- lm(y ~ x_1)                    #no X_2 regressed, but included in DGP
  
  sm1 <- summary(m1) 
  
  simulation2[i,1] <- sm1$coef[1,1]     #Storing models first coef, Intercept
  simulation2[i,2] <- sm1$coef[2,1]     #x_1 coef estimate
  simulation2[i,3] <- sm1$coef[1,2]      #standard error for Intercept
  simulation2[i,4] <- sm1$coef[2,2]      #se for x_1
}

 head(simulation1)
 head(simulation2)

#Full Model Means
 mean(simulation1[,1]) #mean estimate for b_0 (= 2)
 mean(simulation1[,2]) #mean estimate for b_1 X_1 estimate (= -1)
 mean(simulation1[,3]) #mean estimate for b_2 x_2 estimate (= 3)
#These all make sense due to our beta.
 
#Limited Model Means
 mean(simulation2[,1]) #mean b_0 (=17)
 mean(simulation2[,2]) #mean b_1 of x_1 (=-1)
#Our Intercept is off in simulation 2 because we didn't estimate our beta and x_2 

 
 
 library(ggplot2)
 #Creating each graph as an object (p) and the location I wish
 #  the multiplot function to place them at.
 
 #Intercept Simulation 1
 p1 <- qplot(simulation1[,1])+
   theme_bw()+
   ggtitle(expression(paste(beta[0], " Values (Sim. 1)")))+
   xlab(expression(paste("Simulated ", beta[0])))
 
 #X_1 Simulation 1
 p3 <- qplot(simulation1[,2])+
   theme_bw()+
   ggtitle(expression(paste( x[1], " Values (Sim. 1)")))+
   xlab(expression(paste("Simulated ", beta[1])))
 
 #X_2 Simulation 1
 p5 <- qplot(simulation1[,3])+
   theme_bw()+
   ggtitle(expression(paste( x[2], " Values (Sim. 1)")))+
   xlab(expression(paste("Simulated ", beta[2])))
 
 
 #Intercept Simulation 2
 p2 <- qplot(simulation2[,1])+
   theme_bw()+
   ggtitle(expression(paste(beta[0], " Values (Sim. 2)")))+
   xlab(expression(paste("Simulated ", beta[0])))
 
 #X_1 Simulation 2
 p4 <- qplot(simulation2[,2])+
   theme_bw()+
   ggtitle(expression(paste( x[1], " Values (Sim. 2)")))+
   xlab(expression(paste("Simulated ", beta[1])))
 
 
#custom multiplot function, I rearranged some of the graph objects to arrange them better
multiplot(p1, p2, p3, p4, p5, cols=3)

#We see not much changes between Simulation 1 and 2 in terms of the X_1 beta, this makes sense because X_2's 
#beta coeff is indifferent to X_1's

mean(simulation1[,5]) #Simulation 1's X_1 se's (0.0158)
mean(simulation2[,4]) #simulation 2's X_1 se's (0.0962)


mean(simulation1[,4])  #Simulation 1's Intercept se (.128)
mean(simulation2[,3])  #Simulation 2's Intercept se (.615)
#Errors are larger when we take x_2 out of our analysis (less variation understood)


#How do these two simulations/models compare and contrast? 








#Q3

#####
##              Question 3
library(MASS)   #For the mvrnorm() function


N <- 1000
#Set mean of variables
mu <- c(5,5)
x_1 <- rnorm(N, mean=5, sd=sqrt(16))
x_2 <- rnorm(N, mean=5, sd=sqrt(16))
Sigma <- matrix( c(4, 2, 2, 4 ), nrow = 2, ncol= 2)
#Sigma   # Note the cov is 2 b/c correlation is 0.5 in assignment
#          Correlation = Covariance/(sd*sd), so Cov = Corr(sd*sd). In this case,
#          cov = 0.5(2*2) = 0.5*4 = 2

# Using the means, and their variance-covariance matricies we can get our values:
mvr <- mvrnorm(N, mu, Sigma)
#head(mvr)
b <- c(2, -1, 3)
e <- rnorm(N, 0, sd=sqrt(4))
ones <- matrix(1, nrow=1000, ncol=1)
#Plug all of this into our matrix "X"
X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
#head(X)
y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
m1 <- lm(y ~ X[,2] + X[,3])
summary(m1)



#Simulating Regression 1,000 times
simulation3 <- matrix(nrow = 1000, ncol = 6)
head(simulation3)

for (i in 1:1000)
{
  N <- 1000
  mu <- c(5,5)
  x_1 <- rnorm(N, mean=5, sd=sqrt(16))
  x_2 <- rnorm(N, mean=5, sd=sqrt(16))
  Sigma <- matrix( c(4, 2, 2, 4), nrow = 2, ncol= 2)
  mvr <- mvrnorm(N, mu, Sigma)
  b <- c(2, -1, 3)
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
  m1 <- lm(y ~ X[,2] + X[,3])
  summary(m1)
  
  sm1 <- summary(m1)

  #Estimates
  simulation3[i,1] <- sm1$coef[1,1] #recording Intercept Estimate
  simulation3[i,2] <- sm1$coef[2,1] #x_1 Estimate
  simulation3[i,3] <- sm1$coef[3,1] #x_2 Estimate
  #Errors
  simulation3[i,4] <- sm1$coef[1,2] #Intercept Std. Error
  simulation3[i,5] <- sm1$coef[2,2] #se x_1
  simulation3[i,6] <- sm1$coef[3,2] #se x_2
}

#Check Simulation was ran correctly
head(simulation3)
  





#Restricted Model Without x_2
simulation4 <- matrix(nrow = 1000, ncol = 4)
head(simulation4)


for (i in 1:1000)
{
  N <- 1000
  mu <- c(5,5)
  x_1 <- rnorm(N, mean=5, sd=sqrt(16))
  x_2 <- rnorm(N, mean=5, sd=sqrt(16))
  Sigma <- matrix( c(4, 2, 2, 4), nrow = 2, ncol= 2)
  mvr <- mvrnorm(N, mu, Sigma)
  mvr
  b <- c(2, -1,3) 
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e 
  m1 <- lm(y ~ X[,2])
  
  sm1 <- summary(m1)
  
  
  #Estimates
  simulation4[i,1] <- sm1$coef[1,1] #recording Intercept Estimate
  simulation4[i,2] <- sm1$coef[2,1] #x_1 Estimate

  #Errors
  simulation4[i,3] <- sm1$coef[1,2] #Intercept Std. Error
  simulation4[i,4] <- sm1$coef[2,2] #se x_1

}


head(simulation3) #Full
head(simulation4) #Limited



library(ggplot2)
#Creating each graph as an object (p) and the location I wish
#  the multiplot function to place them at.

#Intercept Simulation 3
p1 <- qplot(simulation3[,1])+
  theme_bw()+
  ggtitle(expression(paste(beta[0], " Values (Sim. 3)")))+
  xlab(expression(paste("Simulated ", beta[0])))

#X_1 Simulation 3
p3 <- qplot(simulation3[,2])+
  theme_bw()+
  ggtitle(expression(paste( x[1], " Values (Sim. 3)")))+
  xlab(expression(paste("Simulated ", beta[1])))

#X_2 Simulation 3
p5 <- qplot(simulation3[,3])+
  theme_bw()+
  ggtitle(expression(paste( x[2], " Values (Sim. 3)")))+
  xlab(expression(paste("Simulated ", beta[2])))


#Intercept Simulation 4
p2 <- qplot(simulation4[,1])+
  theme_bw()+
  ggtitle(expression(paste(beta[0], " Values (Sim. 4)")))+
  xlab(expression(paste("Simulated ", beta[0])))

#X_1 Simulation 2
p4 <- qplot(simulation4[,2])+
  theme_bw()+
  ggtitle(expression(paste( x[1], " Values (Sim. 4)")))+
  xlab(expression(paste("Simulated ", beta[1])))


#custom multiplot function, I rearranged some of the graph objects to arrange them better
multiplot(p1, p2, p3, p4, p5, cols=3)



mean(simulation3[,5]) #Simulation 3's X_1 se's 
mean(simulation4[,4]) #simulation 4's X_1 se's 

mean(simulation3[,4])  #Simulation 3's Intercept se
mean(simulation4[,3])  #Simulation 4's Intercept se








#Q4

#####
##                                Question 4

#Inverse Logit Function:

inv.logit = function(x){
  if(!is.numeric(x)){return("Error 404: Numbers Not Found")}
  exp(x)/(1+exp(x))
}

invlogit(.95) #built into R to check
inv.logit(.95)
inv.logit("Normative")


#                              Building Model
N <- 1000
#Set mean of variable(s)
mu <- c(5,5)
Sigma <- matrix( c(2, .5, .5, 3 ), nrow = 2, ncol= 2) 
Sigma
#Using the means, and their variance-covariance matricies we can get our values:
mvr <- mvrnorm(N, mu, Sigma)
#head(mvr)
b <- c(.2, -1, 1.2)
e <- rnorm(N, 0, sd=sqrt(4))
ones <- matrix(1, nrow=1000, ncol=1)
#Plug all of this into our matrix "X"
X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
head(X)
y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e

median(y)
y <- as.numeric(y >= 1.545866)
y



#Inverse Logits
#Intercept
inv.logit.a <- inv.logit(b[1]*X[,1]) #All X variables multiplied by their respective beta values, inverse logit-ed
b0 <- matrix(inv.logit.a, nrow=N, ncol= 1)
head(b0)

#X_1
inv.logit.x2 <- inv.logit(b[2]*X[,2]) 
b1 <- matrix(inv.logit.x2, nrow=N, ncol= 1)
head(b1)

#X_2
inv.logit.x3 <- inv.logit(b[3]*X[,3]) 
b2 <- matrix(inv.logit.x3, nrow=N, ncol= 1)
head(b2)

pi <- matrix(c(b0, b1, b2), nrow=N, ncol=3)
head(pi)


install.packages("Rlab")
library(Rlab)
#y values must be from 0 <= y <= 1
y <- rbern(N, pi)
y

?rbern

m1 <- lm(y ~ X[,2]+ X[,3])
summary(m1)

m1 <- glm(y ~ X[,2]+ X[,3], family = "binomial")
summary(m1)








library(Rlab)
simulation5 <- matrix(nrow = 1000, ncol = 6)
head(simulation5)

for (i in 1:1000)
{
  N <- 1000
  mu <- c(5,5)
  Sigma <- matrix( c(2, .5, .5, 3 ), nrow = 2, ncol= 2) 
  mvr <- mvrnorm(N, mu, Sigma)
  b <- c(.2, -1, 1.2)
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
  y <- as.numeric(y >= median(y))
  
  #Inverse Logits
  #Intercept
  inv.logit.a <- inv.logit(b[1]*X[,1]) #All X variables multiplied by their respective beta values, inverse logit-ed
  b0 <- matrix(inv.logit.a, nrow=N, ncol= 1)
  
  #X_1
  inv.logit.x2 <- inv.logit(b[2]*X[,2]) 
  b1 <- matrix(inv.logit.x2, nrow=N, ncol= 1)
  
  #X_2
  inv.logit.x3 <- inv.logit(b[3]*X[,3]) 
  b2 <- matrix(inv.logit.x3, nrow=N, ncol= 1)
  
  pi <- matrix(c(b0, b1, b2), nrow=N, ncol=3)
  y <- rbern(N, pi)
  
  m5 <- lm(y ~ X[,2]+ X[,3])
  sm5 <- summary(m5)

  simulation5[i,1] <- sm5$coef[1,1] #Intercept estimate
  simulation5[i,2] <- sm5$coef[2,1] #x_1 coef estimate
  simulation5[i,3] <- sm5$coef[3,1] #x_2 coef estimate
  
  simulation5[i,4] <- sm5$coef[1,2] #standard error for Intercept
  simulation5[i,5] <- sm5$coef[2,2] #se x_1
  simulation5[i,6] <- sm5$coef[3,2] #se x_2
}

head(simulation5)






library(ggplot2)
#Creating each graph as an object (p) and the location I wish
#  the multiplot function to place them at.

#Intercept Simulation 5
p1 <- qplot(simulation5[,1])+
  theme_bw()+
  ggtitle(expression(paste(beta[0], " Values (Sim. 5)")))+
  xlab(expression(paste("Simulated ", beta[0])))

#X_1 Simulation 5
p2 <- qplot(simulation5[,2])+
  theme_bw()+
  ggtitle(expression(paste( x[1], " Values (Sim. 5)")))+
  xlab(expression(paste("Simulated ", beta[1])))

#X_2 Simulation 5
p3 <- qplot(simulation5[,3])+
  theme_bw()+
  ggtitle(expression(paste( x[2], " Values (Sim. 5)")))+
  xlab(expression(paste("Simulated ", beta[2])))



#custom multiplot function, I rearranged some of the graph objects to arrange them better
multiplot(p1, p2, p3, cols=3)



predictm5 <- qplot(predict(m5))+
  theme_bw()+
  ggtitle("Pr. Values (Sim. 5)")

predictm5






#Q5
#####
#           Question 5



simulation7 <- matrix(nrow = 1000, ncol = 6)
#head(simulation7)
library(Rlab)

for (i in 1:1000)
{
  N <- 1000
  mu <- c(5,5)
  Sigma <- matrix( c(2, .5, .5, 3 ), nrow = 2, ncol= 2) 
  mvr <- mvrnorm(N, mu, Sigma)
  b <- c(.2, -1, 1.2)
  e <- rnorm(N, 0, sd=sqrt(4))
  ones <- matrix(1, nrow=1000, ncol=1)
  X <- matrix(c(ones, mvr[,1], mvr[,2]), nrow=N)
  y <- b[1]*X[,1]+ b[2]*X[,2]+ b[3]*X[,3] + e
  y <- as.numeric(y >= median(y))
  
  #Inverse Logits
  #Intercept
  inv.logit.a <- inv.logit(b[1]*X[,1]) #All X variables multiplied by their respective beta values, inverse logit-ed
  b0 <- matrix(inv.logit.a, nrow=N, ncol= 1)
  
  #X_1
  inv.logit.x2 <- inv.logit(b[2]*X[,2]) 
  b1 <- matrix(inv.logit.x2, nrow=N, ncol= 1)
  
  #X_2
  inv.logit.x3 <- inv.logit(b[3]*X[,3]) 
  b2 <- matrix(inv.logit.x3, nrow=N, ncol= 1)
  
  pi <- matrix(c(b0, b1, b2), nrow=N, ncol=3)
  y <- rbern(N, pi)
  
  
  #Using the proper function:
  m7 <- glm(y ~ X[,2]+ X[,3], family = binomial(link=logit))
  sm7 <- summary(m7)
  
  simulation7[i,1] <- sm7$coef[1,1] #Intercept estimate
  simulation7[i,2] <- sm7$coef[2,1] #x_1 coef estimate
  simulation7[i,3] <- sm7$coef[3,1] #x_2 coef estimate
  
  simulation7[i,4] <- sm7$coef[1,2] #standard error for Intercept
  simulation7[i,5] <- sm7$coef[2,2] #se x_1
  simulation7[i,6] <- sm7$coef[3,2] #se x_2
}

#head(simulation7)




full <- qplot(predict(m7))+
  theme_bw()+
  ggtitle("Pr. Values (Sim. 7)")



#Plotting

library(ggplot2)
#Creating each graph as an object (p) and the location I wish
#  the multiplot function to place them at.

#Intercept Simulation 7
p1 <- qplot(simulation7[,1])+
  theme_bw()+
  ggtitle(expression(paste(beta[0], " Values (Sim. 7)")))+
  xlab(expression(paste("Simulated ", beta[0])))

#X_1 Simulation 7
p3 <- qplot(simulation7[,2])+
  theme_bw()+
  ggtitle(expression(paste( x[1], " Values (Sim. 7)")))+
  xlab(expression(paste("Simulated ", beta[1])))

#X_2 Simulation 7
p5 <- qplot(simulation7[,3])+
  theme_bw()+
  ggtitle(expression(paste( x[2], " Values (Sim. 7)")))+
  xlab(expression(paste("Simulated ", beta[2])))


#Intercept Simulation 8
p2 <- qplot(simulation8[,1])+
  theme_bw()+
  ggtitle(expression(paste(beta[0], " Values (Sim. 8)")))+
  xlab(expression(paste("Simulated ", beta[0])))

#X_1 Simulation 8
p4 <- qplot(simulation8[,2])+
  theme_bw()+
  ggtitle(expression(paste( x[1], " Values (Sim. 8)")))+
  xlab(expression(paste("Simulated ", beta[1])))


#custom multiplot function, I rearranged some of the graph objects to arrange them better
multiplot(p1, p2, p3, p4, p5, cols=3)



library(ggplot2)
pred7 <-predict(m7)
probs <- exp(pred7)/(1+exp(pred7))


binary.probs <- as.numeric(probs > 0.5 )
qplot(bin.probs)+
  theme_bw()+
  ylab("Number of Predictions")+
  xlab("Ratio of Correct Prediction, Model 7")+
  ggtitle("Predictions of Model 7")






#gets messy at end, might need to revist a lot of this/






