##  1 How many of each cause of homicide?

## The goal of this problem is to count the number of different types of homicides that 
## are in this dataset. In each record there is a field with the word "Cause" in it 
## indicating the cause of death (e.g. \Cause: shooting"). The basic goal is to extract 
## this field and count the number of instances of each cause.

## Write a function named count that takes one argument, a character string indicating the
## cause of death. The function should then return an integer representing the number of
## homicides from that cause in the dataset. If no cause of death is specified, then the function
## should return an error message via the stop function.

## Your function should read the homicides dataset in the manner indicated above.
## The options for cause of death are \asphyxiation", \blunt force", \other", \shooting",
## \stabbing", \unknown". No other causes are allowed. If a cause of death is specified
## that is not one of these, then the function should throw an error with the stop function.
## Note that some homicides in the dataset do not have a cause of death listed and those
## records should be ignored.
## Your function should deal with some irregularities in the dataset like capitalization.
## For example \Shooting" and \shooting" should be counted as the same cause of death.

count <- function(cause = NULL) {
  ## establish causes that will be counted
  allowedCauses = c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  
  ## chack that couse is valid
  if(is.null(cause) || !cause %in% allowedCauses) {
    stop("invalid cause")
  }
  
  ## read in text file
  setwd("C:/Users/joseph.pruitt/Desktop/Coursea Computing for Data Analysis/Homework/Week 4")
  homicides <- readLines("homicides.txt")
  
  ## look for matches in the causes field
  r <- regexec("<dd>[Cc]ause: (.*?)</dd>", homicides)
  m <- regmatches(homicides, r)
  
  ## extract cause
  actualCauses <- sapply(m, function(x) x[2])
  
  ## count matches by cause
  matchingCauses <- actualCauses[tolower(actualCauses) == cause]
  matchingCauses <- matchingCauses[!is.na(matchingCauses)]
  
  ## Return integer containing count of homicides for that cause
  length(matchingCauses)
  
}
