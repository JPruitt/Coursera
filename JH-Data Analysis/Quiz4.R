# Week 4 Quiz

## Question 1
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and the box office gross is the only covariate. What is the
## regression coefficient for the slope and it's interpretation?

setwd("C:/Users/joseph.pruitt/Desktop/Coursera Data Analysis/Week 4")
download.file('https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt', 'movies.txt')
movies <- read.delim('movies.txt')

## What does the dataframe look like?
names(movies)
head(movies)

## Linear Regression Model
plot(movies$box.office, movies$score)
moviesLM <- lm(movies$score ~ movies$box.office)
lines(movies$box.office, moviesLM$fitted, col="red", lwd=3)
moviesLM$coefficients


## The regression coefficient is 0.09676. The interpretation is that an increase
## of one million dollars in box office gross is associated with a 0.09676 increase
## in Rotten Tomatoes Score.

## Question 2
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and the box office gross is the only covariate. What is the 90%
## confidence interval for the intercept term and what can you deduce from the 90%
## confidence interval?

confint(moviesLM,level=0.9)

## The 90% confidence interval for the intercept is (47.52, 52.63). If we
## repeated this study 100 times, we would expect our calculated interval to cover
## the true value on average 90% of the time.**

## Question 3
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt 
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and box office gross and running time are the covariates. What is
## the value for the regression coefficient for running time? How is it interpreted?

plot(movies$running.time, movies$score)
moviesLM2 <- lm(movies$score ~ movies$box.office + movies$running.time)
lines(movies$running.time, moviesLM2$fitted, col="red", lwd=3)

# well clearly that isn't right

moviesLM2$coefficients

## The coefficient is 0.12752. That means that if two movies have the same box
## office gross, an increase of one minute in running time is associated with an
## average increase of 0.12752 in score.
  
## Question 4
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt 
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and box office gross and running time are the covariates. Is
## running time a confounder for the relationship between Rotten Tomatoes score and
## box office gross? Why or why not?

q4LM <- lm(movies$score ~ movies$box.office + movies$running.time)

par(mfrow=c(3,1))
plot(movies$box.office, movies$score)
lines(movies$box.office, lm(movies$score ~ movies$box.office)$fitted, col="red", lwd=3)
plot(movies$running.time, movies$score)
lines(movies$running.time, lm(movies$score ~ movies$running.time)$fitted, col="red", lwd=3)
plot(movies$running.time, movies$box.office)
lines(movies$running.time, lm(movies$box.office ~ movies$running.time)$fitted, col="red", lwd=3)

q4LM$coefficients
summary(q4LM)

## Yes running time is a confounder. It is correlated both with the Rotten
## Tomatoes score and the box office gross.
  
## Question 5
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt 
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and box office gross and running time are the covariates. Make a
## plot of the movie running times versus movie score. Do you see any outliers? If
## you do, remove those data points and refit the same regression (Rotten Tomatoes
## score is the outcome and box office gross and running time are the covariates).
## What do you observe?

par(mfrow=c(1,1))
plot(movies$running.time, movies$score)

movies.shorter <- movies[movies$running.time < 200,]

moviesLM2b <- lm(movies.shorter$score ~ movies.shorter$box.office + movies.shorter$running.time)

plot(movies.shorter$running.time, movies.shorter$score)
lines(movies.shorter$running.time, moviesLM2b$fitted, col="red", lwd=3)

summary(moviesLM2b)
# look for P-values

## Yes there are two outliers. After removing them and refitting the regression
## line, the running time coefficient has a larger magnitude and is more
## statistically significant.
  
## Question 6
## Load the movies data available from the course website here: 
## https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt 
## Fit a linear regression model by least squares where the Rotten Tomatoes score
## is the outcome and running time and box office gross are covariates. What is the
## P-value for running time and how is it interpreted?

q6LM <- lm(movies$score ~ movies$running.time + movies$box.office)
summary(q6LM)

## The P-value is 0.0187. It is the probability of observing a t-statistic as big
## as, or larger than, the one we saw, if there was no relationship between Rotten
## Tomatoes score and running time for a fixed box office gross.
  
  
## Question 7
## Fit a linear model by least squares where Rotten Tomatoes score is the outcome
## and the covariates are movie rating, running time, and an interaction between
## running time and rating are the covariates. What is the coefficient for the
## interaction between running time and the indicator/dummy variable for PG rating?

moviesLMx <- lm(movies$score ~ movies$rating + movies$running.time + movies$running.time*movies$rating)
summary(moviesLMx)

## -0.6901.

## Question 8
## Fit a linear model by least squares where Rotten Tomatoes score is the outcome
## and the covariates are movie rating, running time, and an interaction between
## running time and rating are the covariates. What is the estimated average change
## in score for a PG movie for a one minute increase in running time?

q8LM <- lm(movies$score ~ movies$rating + movies$running.time + movies$running.time*movies$rating)

summary(q8LM)
coeffQ8 <- 1.1852 + (-0.6901156)
coeffQ8  ## 0.4951
  
## Question 9
## Load the data on number of breaks during weaving into R with the command:

data(warpbreaks)

## Fit a linear model where the outcome is the number of breaks and the covariate
## is tension. What is a 95% confidence interval for the average difference in
## number of breaks between medium and high tension?

warpLM <- lm (warpbreaks$breaks ~ warpbreaks$tension)
confint(warpLM, level=0.95)
summary(warpLM)

## Question 10

Based on this graph is there a significant association between organic food
sales and autism rates? Would you believe this association could be used to
reduce autism rates? Why or why not?

## There is a statistically significant association. We may be skeptical this
## association could be used to reduce autism rates, since there are many possible
## explanations for the association that do not involve a direct relationship
## between organic foods and autism.
