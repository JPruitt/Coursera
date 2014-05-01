#Week 2 Quiz
#Question #1

cube <- function(x, n) {
  x^3
}

cube(3)

#Question #2
pow <- function(x = 4, n = 3) {
  x^n
}

pow()

#Quetsion #3
x <- 1:10
if(x > 5) {
  x <- 0
}

#The "if" statement can only take one value, not a vector.

#Question #4
library(datasets)
data(iris)

?iris

str(iris) #identifies data frame properties

iris_V <- iris$Species == "virginica"  #creates logical vector that assigns TRUE values to rows with species virginica
sub=iris[iris_V, ]  #creates a data frame (subset) that contains only data for species virginica
sapply(sub, mean) #applies the mean function to the columns in the sub data frame

#Question #5
rowMeans(iris[, 1:4]) #no
apply(iris, 1, mean) #no
apply(iris[, 1:4], 1, mean) #no
apply(iris[, 1:4], 2, mean) #yes
colMeans(iris) #no
apply(iris, 2, mean) #no

#Question #6
library(datasets)
data(mtcars)

?mtcars

sapply(split(mtcars$mpg, mtcars$cyl), mean) #yes
apply(mtcars, 2, mean)  #no
mean(mtcars$mpg, mtcars$cyl)  #no
split(mtcars, mtcars$cyl)  #no

#Question #7
head(mtcars)
hplist <- sapply(split(mtcars$hp, mtcars$cyl), mean) #creates a list with average hp by cylinder
hpDiff <- hplist["8"] - hplist["4"] #calculates the hp difference between 8 cylinder cars and 4 cylinder cars
hpDiff

#Question #8
## no code

#Question #9
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

#Question #10
debug(ls)
ls()
