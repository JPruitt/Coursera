library("ggplot2", lib.loc="C:/Program Files/R/R-2.15.2/library")
library("lattice", lib.loc="C:/Program Files/R/R-2.15.2/library")
library("knitr", lib.loc="C:/Program Files/R/R-2.15.2/library")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hospital <- read.csv("hospital-data.csv", colClasses = "character")

## Assignment #1

head(outcome)  ## top six rows with headers for all columns
ncol(outcome)  ## number of columns
nrow(outcome)  ## number of rows
names(outcome)  ## names of columns (vector)

## Histogram (30 day death rates of heart attack)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

## 1. Add a label to the x-axis that says "30-day Death Rate".
## 2. Add a title for the histogram that says "Heart Attack 30-day Death Rate".
hist(outcome[, 11],xlab = "30-day Death Rate", main = "Heart Attack 30-day Death Rate")

## Assignment #2

HA <- as.numeric(outcome[, 11])
HF <- as.numeric(outcome[, 17])
PN <- as.numeric(outcome[, 23])

par(mfrow = c(3,1))

hist(HA,xlab = "30-day Death Rate", main = "Heart Attack")
hist(HF,xlab = "30-day Death Rate", main = "Heart Failure")
hist(PN,xlab = "30-day Death Rate", main = "Pneumonia")

## Calculate the range of values for each histogram
Range <- c(HA, HF, PN)

## standardize numerical ranges
hist(HA,xlab = "30-day Death Rate", main = "Heart Attack", xlim = range(Range, na.rm = TRUE))
hist(HF,xlab = "30-day Death Rate", main = "Heart Failure", xlim = range(Range, na.rm = TRUE))
hist(PN,xlab = "30-day Death Rate", main = "Pneumonia", xlim = range(Range, na.rm = TRUE))

## Plot histograms side by side
par(mfrow = c(1, 3))
hist(HA,xlab = "30-day Death Rate", main = "Heart Attack", xlim = range(Range, na.rm = TRUE))
hist(HF,xlab = "30-day Death Rate", main = "Heart Failure", xlim = range(Range, na.rm = TRUE))
hist(PN,xlab = "30-day Death Rate", main = "Pneumonia", xlim = range(Range, na.rm = TRUE))

## Include median values for each histogram
hist(HA,breaks = 15, xlab = "30-day Death Rate", main = "Heart Attack", xlim = range(Range, na.rm = TRUE))
abline(v = median(HA, na.rm = TRUE), col = "blue")
hist(HF,breaks = 15, xlab = "30-day Death Rate", main = "Heart Failure", xlim = range(Range, na.rm = TRUE))
abline(v = median(HF, na.rm = TRUE), col = "blue")
hist(PN,breaks = 15, xlab = "30-day Death Rate", main = "Pneumonia", xlim = range(Range, na.rm = TRUE))
abline(v = median(PN, na.rm = TRUE), col = "blue")

## Include mean values in title
par(mfrow = c(3, 1))
AvgHA <- mean(HA, na.rm = TRUE)
hist(HA,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Heart Attack",mtext(bquote(bar(x)==. (AvgHA)))), 
     xlim = range(Range, na.rm = TRUE))
AvgHF <- mean(HF, na.rm = TRUE)
hist(HF,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Heart Failure",mtext(bquote(bar(x)==. (AvgHF)))), 
     xlim = range(Range, na.rm = TRUE))
AvgPN <- mean(PN, na.rm = TRUE)
hist(PN,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Pneumonia",mtext(bquote(bar(x)==. (AvgPN)))), 
     xlim = range(Range, na.rm = TRUE))

## Include density estimate on top of histogram

par(mfrow = c(3, 1))
AvgHA <- mean(HA, na.rm = TRUE)
hist(HA,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Heart Attack",mtext(bquote(bar(x)==. (AvgHA)))), 
     xlim = range(Range, na.rm = TRUE), prob = TRUE)
lines(density(HA, na.rm = TRUE, from = min(Range, na.rm = TRUE), to = max(Range, na.rm = TRUE)))
abline(v = median(HA, na.rm = TRUE), col = "blue")

AvgHF <- mean(HF, na.rm = TRUE)
hist(HF,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Heart Failure",mtext(bquote(bar(x)==. (AvgHF)))), 
     xlim = range(Range, na.rm = TRUE), prob = TRUE)
lines(density(HF, na.rm = TRUE, from = min(Range, na.rm = TRUE), to = max(Range, na.rm = TRUE)))
abline(v = median(HF, na.rm = TRUE), col = "blue")

AvgPN <- mean(PN, na.rm = TRUE)
hist(PN,breaks = 15, xlab = "30-day Death Rate", 
     main = paste0("Pneumonia",mtext(bquote(bar(x)==. (AvgPN)))), 
     xlim = range(Range, na.rm = TRUE), prob = TRUE)   
lines(density(PN, na.rm = TRUE, from = min(Range, na.rm = TRUE), to = max(Range, na.rm = TRUE)))
abline(v = median(PN, na.rm = TRUE), col = "blue")     

##Alternate Method:  could clean up the rest to code more efficiently
par(mfrow = c(1, 1))
AvgHA <- mean(HA, na.rm = TRUE)
hist(HA,breaks = 15, xlab = "30-day Death Rate", 
     main = substitute("Heart Attack; " * bar(x)==z, list(z = AvgHA)), 
     xlim = range(Range, na.rm = TRUE), prob = TRUE)
lines(density(HA, na.rm = TRUE, from = min(Range, na.rm = TRUE), to = max(Range, na.rm = TRUE)))
abline(v = median(HA, na.rm = TRUE), col = "blue")

## Assignment #3

## Count the number of observations (hospitals) by state.
Tbl <- table(outcome$State)

## Subset the table to contain only states with 20 or more observations (hospitals)
StateGT20 <- subset(Tbl, Tbl>=20)

## Creates a character vector of State names that all contain 20 or more observations (hospitals)
StateNm <- names(StateGT20)

## Subsets data frame using character vector as an inclusive (%in%) filter
outcome2 <- subset(outcome, outcome$State %in% StateNm)

## Create a boxplot of death rates by state.
death <- outcome2[, 11]
state <- outcome2$State
boxplot(death ~ state)

## Set the y-axis label to read "30-day Death Rate", 
## the title to read "Heart Attack 30-day Death Rate by State",
## the x- and y-axis labels to be perpendicular to the axes, so that all state names are legible,
boxplot(death ~ state, 
        ylab = "30-day Death Rate", 
        main = "Heart Attack 30-day Death Rate by State",
        par(las = 2))

## Sort the states by their median 30-day death rate and plot the boxplots in order of 
## their median rateshink x-axis labels to eliminate overlapping,
## and include the number of hospitals in that state in the axis label in parentheses.
bymedian <- with(outcome2,reorder(as.factor(state),death,median, na.rm = TRUE))
bymedian_t <- table(bymedian)
boxplot(death ~ bymedian, data = outcome2,
        ylab = "30-day Death Rate", 
        main = "Heart Attack 30-day Death Rate by State",
        names=paste(names(bymedian_t), " (", as.vector(bymedian_t),")", sep=""),
        par(las = 2, cex.axis = 0.5))

## Assignment #4
outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")

death <- as.numeric(outcome.hospital[, 11])  ## Heart Attack Outcomes
npatient <- as.numeric(outcome.hospital[, 15], rm.na = "TRUE")
owner <- factor(outcome.hospital$Hospital.Ownership)

par(mfrow = c(3, 3))

xyplot(death ~ npatient | owner, 
       ylab="30-day Death Rate", 
       xlab="Number of Patients Seen",
       main="Heart Attack 30-day Death Rate by Ownership",
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, col="black")
       } )

## Assignment #5:  Finding the best hospital in a state.  Test calls.
source("best.R")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")
       
## Assignment #6:  Ranking hospitals by outcome in a state.
source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

## Assignment #7:  Ranking hospitals in all states.
source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
