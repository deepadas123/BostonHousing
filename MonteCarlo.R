library(stats);
library(nnet);
library(MASS);
# import data
data(Boston);
n<-dim(Boston)[1];


# randomly divide dataset into 2 parts
#set.seed(2005);
m<-100; #100 rows of test data
ind<-sample(1:n,m);
test.Boston<-Boston[ind,];
train.Boston<-Boston[-ind,];
#linear regression
boston.lm<-lm(medv~crim+zn+chas+nox+rm+dis+rad+tax+ptratio+black+lstat, data=Boston)
plot(boston.lm$fitted,boston.lm$residuals, xlab = 'fitted', ylab = 'residuals', main = 'Residuals by Fitted from GLM', col = 'blue')



#################################################################################
# Simulation Study for Linear Regression sample code for 1.2
#################################################################################
library(stats)
###  1.a)
# Normally distributed numbers, a 200 of them.
n<-200
sig<-0.5
z1 <- rnorm(n)
x1 <- 2 + 3*z1  		# Makes a vector x out of the vector z
summary(x1)
z2<-rnorm(n)
x2<-0.5*z2-1
x3 <- x1*x1

# An OLS model
u <- rnorm(n,sd=sig)
y <- 7 + 9*x1 +0.3*x2 + u  #true model
summary(y)

#you may want to generate data in another file and save above data as data1.Rdata
#so that you can use the same data set in future
# then load the previous saved data

#  load("E:\\WORK\\Course\\Spring 05\\QA727\\data1.RData")

# A regression
model <- lm(y ~ x1+x2+x3)
summary(model)

#stepwise
model.step<-step(model)#based on AIC

# manually, try other criteria, e.g. R2, MSE etc
model1 <- lm(y ~ x1+x2+x3)
summary(model1)
model2 <- lm(y ~ x1+x2)
summary(model2)
model3 <- lm(y ~ x1+x3)
summary(model3)
#and so on, you may want to try all possible combinations of x's


############# sample code for 1.3 #############################
list.MSE <- NULL
list.beta0 <- NULL
list.beta1 <- NULL
list.beta2 <- NULL
x1 <- rnorm(n = 200, mean = 2, sd = 1)
x2 <- rnorm(n = 200, mean = -1, sd = 0.5)  
for(j in 1:100) 
  #for 100 times	
{		
  u <- rnorm(n = 200, mean = 0, sd = 1)		
  y <- 3 + 0.5 * x1 + 2 * x2 + u #simulate the response variable		
  lm.out <- lm(y ~ x1+x2) #fit a linear regression model		
  summary <- summary(lm.out) 		
  list.MSE[j] <- (summary$sigma)^2           # get the model MSE for each m		
  list.beta0[j] <- summary$coefficients[1]   # get the estimate of the intercept for each m	
  list.beta1[j] <- summary$coefficients[2]   # get the estimate of the coefficient of x1 for each m	
  list.beta2[j] <- summary$coefficients[3]   # get the estimate of the coefficient of x2 for each m	
}

MSE <- mean(list.MSE)		
# get the average MSE of the 200 model MSEs
beta0 <- mean(list.beta0)	
# get the average estiamte value of the intercept 
beta1 <- mean(list.beta1)	
# get the average estiamte value of the coefficient of x1
beta2 <- mean(list.beta2)	
# get the average estiamte value of the coefficient of x2
mean.beta <- c(beta0, beta1, beta2)	
# make the above three estimates into one vector
bias.beta <- c(beta0 - 3, beta1 - 0.5, beta2 - 2)	
# get the bias of the coefficients estimate
var.beta0 <- var(list.beta0)	# get variance of the intercept estimates
var.beta1 <- var(list.beta1)	# get variance of x1 coefficient estimates
var.beta2 <- var(list.beta2)	# get variance of x2 coefficient estimates
var.beta <- c(var.beta0, var.beta1, var.beta2)	# make the above three variances into one vector
mse.beta0 <- bias.beta[1]^2 + var.beta0
mse.beta1 <- bias.beta[2]^2 + var.beta1
mse.beta2 <- bias.beta[3]^2 + var.beta2
mse.beta <- c(mse.beta0, mse.beta1, mse.beta2)	
# get the mse of the coefficient estimates
# print the results
true.beta <- c(3,0.5,2)
true.beta
mean.beta
bias.beta
var.beta
mse.beta
MSE