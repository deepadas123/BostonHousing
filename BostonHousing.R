library(readr)
library(ggplot2)
library(corrplot)
library(mlbench)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
library(MASS)
library(GGally)
library(glmnet)
library(leaps)

set.seed(12927356) # required to reproduce the results
index <- sample(nrow(Boston), nrow(Boston) * 0.75)
Boston.train <- Boston[index, ]
Boston.test <- Boston[-index, ]

head(Boston.train)

str(Boston.train)
sum(is.na(Boston.train))
sum(duplicated(Boston.train))

missmap(Boston.train,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

cormat=cor(Boston.train)
corrplot(cormat, type = "upper", method="number")
#corrplot(cor(select(Boston.train, -chas)))
corrplot.mixed(cormat)

summary(Boston.train)
par(mfrow = c(3,5))
mapply(boxplot, Boston.train[-4], main=paste("Boxplot of",colnames(Boston.train[-4])), xlab=colnames(Boston.train[-4]))

boxplot(Boston.train$medv)
#Linear regression model1
model1 <- lm(medv~.,data = Boston.train)
AIC(model1)
BIC(model1)
data.frame(coef = round(model1$coefficients,2))
sm <- summary(model1)
mean(sm$residuals^2)
#Residual Analysis
par(mfrow=c(2,2))
plot(model1)

pred.lm <- predict(model1, newdata = Boston.test)

# Root-mean squared error
rmse.lm <- sqrt(sum((pred.lm - Boston.test$medv)^2)/
                  length(Boston.test$medv))
mean((Boston.test$medv - pred.lm)^2)
c(RMSE = rmse.lm, R2 = summary(model1)$r.squared)
#Linear regression model2
model2 <- lm(medv~. -indus -age,data = Boston.train)
data.frame(coef = round(model2$coefficients,2))
summary(model2)

#Best Subset Selection
model.subset<- regsubsets(medv~.,data=Boston.train, nbest=1, nvmax = 13)
subset_fit=summary(model.subset)
subset_fit

plot(model.subset, scale = "bic")
#Stepwise selection
null.model<-lm(medv~1, data=Boston)
full.model<-lm(medv~., data=Boston.train)
result<-step(null.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="forward")

result$anova

result<-step(full.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="backward")
result$anova

result<-step(null.model, scope=list(lower=null.model, upper=full.model), k = 2, direction="both")
result$anova

#LASSO
set.seed(12927356) # required to reproduce the results
index <- sample(nrow(Boston), nrow(Boston) * 0.75)
Boston$chas<-as.numeric(Boston$chas)
#Standardize covariates before fitting LASSO
Boston.X.std<- scale(select(Boston,-medv))
X.train<- as.matrix(Boston.X.std)[index,]
X.test<-  as.matrix(Boston.X.std)[-index,]
Y.train<- Boston[index, "medv"]
Y.test<- Boston[-index, "medv"]

lasso.fit<- glmnet(x=X.train, y=Y.train, family = "gaussian", alpha = 1)
plot(lasso.fit, xvar = "lambda", label=TRUE)