# Week 5 Quiz
## Question 1
## Load the weaving data with the commands:
  
data(warpbreaks)

## Fit an ANOVA model where the outcome is the number of breaks. Fit an ANOVA model
## including the wool and tension variables as covariates. What are the dgrees of
## freedom for the tension variable and what is the F-statistic for tension after
## accounting for the variation due to wool?

warpANOVA <- aov(warpbreaks$breaks ~ warpbreaks$wool + warpbreaks$tension)
summary(warpANOVA)

## The degrees of freedom for tension is 2 and the F-statistic is 7.537.
  
## Question 2
## Suppose that the probability an event is true is 0.2. What are the log odds of that event?

log( (0.2 / (1 - 0.2)) )

## -1.3863
  
## Question 3
## Load the [horseshoe crab](http://en.wikipedia.org/wiki/Horseshoe_crab) data using the commands:
  
library(glm2)
data(crabs)

## Fit a Poisson regression model with the number of Satellites as the outcome and
## the width of the female as the covariate. What is the multiplicative change in
## the expected number of crabs for each additional centimeter of width?

plot(crabs$Width, crabs$Satellites,
     pch=19, col="darkgrey")
crabsGLM <- glm(crabs$Satellites ~ crabs$Width, family="poisson")
abline(crabs$Width, crabsGLM$fitted.values, col="blue", lwd=3)
abline(0, 0.16405, col="blue", lwd=3)
lines(crabs$Width, crabsGLM$fitted.values, col="blue", lwd=3)

summary(crabsGLM)

exp(crabsGLM$coefficients[[2]])

## 0.1640
## 1.1782

## Question 4
## What is the expected number of Satellites for a female of width 22cm?

exp(crabsGLM$coefficients[[1]]) * exp(crabsGLM$coefficients[[2]] * 22)

## 3.6080
## 1.3556

## Question 5
## Load the school absenteeism data set and fit a linear model relating the log of
## the number of days absent to the other variables with the commands:
  
library(MASS)
data(quine)

lm1 <- lm(log(Days+2.5) ~ ., data=quine)

## Use the `step()` function in R to perform model selection using default
## parameters. What variables remain in the model after model selection?

aic <- step(lm1)
aic$model

## Eth, Age
