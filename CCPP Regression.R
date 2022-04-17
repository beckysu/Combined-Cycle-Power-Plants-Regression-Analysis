## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----extract data------------------------------------------------------------------------------------------------------------------
library(readxl)
library(car)
data.ccpp<-read_excel("/Users/beckysu/Desktop/ccpp.xlsx")
data(data.ccpp)
attach(data.ccpp)

a<-data.ccpp$AT
b<-data.ccpp$V
c<-data.ccpp$AP
d<-data.ccpp$RH
Y<-data.ccpp$PE


## ----plot--------------------------------------------------------------------------------------------------------------------------
pairs(~Y+a+b+c+d)

full.lm<-lm(Y~a+b+c+d)
avPlots(full.lm, id=FALSE)


## ----Residual----------------------------------------------------------------------------------------------------------------------
Residualtest<-full.lm$residuals
Fitted<-full.lm$fitted.values
plot(Residualtest~Fitted, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Residuals vs Fits Plot')
abline(h = 0, lty = 2)


## ----Q-Q---------------------------------------------------------------------------------------------------------------------------
e<-Residualtest
qqnorm(e)
qqline(e)


## ----Outliers fu-------------------------------------------------------------------------------------------------------------------
ccpp.lm <- lm(Y~a+b+c+d)
s.resid <- rstudent(ccpp.lm)
as=abs(s.resid)
which(as==max(as))

#test for outliers
outlierTest(ccpp.lm)


## ----influential point fu----------------------------------------------------------------------------------------------------------
influenceIndexPlot(full.lm)


## ----------------------------------------------------------------------------------------------------------------------------------
library(leaps)
#Full model
mod.0 <- lm(Y~1)
mod.full= ~a + d + b + c

mod.1 <- update(mod.0, mod.full)
mod.backward <- step(mod.1, scope = c(lower = ~ 1, direction = 'backward'))


## ----powerTransform----------------------------------------------------------------------------------------------------------------
Trans.ccpp<-powerTransform(cbind(a, b, c, d)~1)
summary(Trans.ccpp)


## ----BoxCox------------------------------------------------------------------------------------------------------------------------
ccpp.Trans <- with(data.ccpp, data.frame(Y, a, log(b), c, d))
ccpp.lm<-lm(Y~., data = ccpp.Trans)
boxCox(ccpp.lm)


## ----------------------------------------------------------------------------------------------------------------------------------
pairs(~I(Y^(-2))+a+log(b)+c+d)

full.fit<-lm(Y^(-2) ~ a + log(b) + c+ d)
avPlots(full.fit, id=FALSE)

## ----------------------------------------------------------------------------------------------------------------------------------
library(leaps)
#Full model
mod.full= ~a +log(b) + c + d + a*b

# Base model
mod.0 <- lm(Y^(-2)~1)
mod.1 <- update(mod.0, mod.full)
mod.backward <- step(mod.1, scope = c(lower = ~ 1, direction = 'backward'))


## ----new residuals-----------------------------------------------------------------------------------------------------------------
Residual<-full.fit$residuals
Fitted<-full.fit$fitted.values
plot(Residual~Fitted, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Residuals vs Fits Plot')
abline(h = 0, lty = 2)


## ----new Q-Q-----------------------------------------------------------------------------------------------------------------------
e<-Residual
qqnorm(e)
qqline(e)


## ----Outliers----------------------------------------------------------------------------------------------------------------------
ccpp.lm <- lm(Y^(-2)~a+log(b)+c+d+a*b)
s.resid <- rstudent(ccpp.lm)
as=abs(s.resid)
which(as==max(as))

#test for outliers
outlierTest(ccpp.lm)


## ----influential point-------------------------------------------------------------------------------------------------------------
full.fit<-lm(Y^(-2) ~ a + log(b) + c + d+a*b)
influenceIndexPlot(full.fit)


## ----remove influential points-----------------------------------------------------------------------------------------------------
ccpp.lm<-lm((Y^(-2) ~ a + log(b) + c + d+a*b), subset=-c(3118,7665,3384,3896,7399,8718,8363,8188,1439,4219))


## ----------------------------------------------------------------------------------------------------------------------------------
Residual<-ccpp.lm$residuals
e<-Residual
qqnorm(e)
qqline(e)


## ----global F-test-----------------------------------------------------------------------------------------------------------------
#PROBLEM 3B:
ccpp.full.lm<-lm(Y^(-2)~a+log(b)+c+d+a*b)
summary(ccpp.full.lm)


## ----partial F-test----------------------------------------------------------------------------------------------------------------
#PROBLEM 3D:
ccpp.red.lm<-lm(Y^(-2) ~ a + log(b) + d +a*b)
summary(ccpp.red.lm)

