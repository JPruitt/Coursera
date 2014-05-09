## Load data set
load(url("http://bit.ly/dasi_gss_data"))

## Subset data to the variables of interest
selectCol<-c(2,3,4,5,6,8,12,15,18,17,27,28)
## includes variables: year, age, race, sex, hispanic, educ, degree, wrksta, marital, spwrksta,
##      coninc, & region.
myData<-gss[ ,selectCol]

## Check subset
names(myData)
dim(myData)

## further subset data to only single men and women (exclude "Married", "Separated", and "NA") 
## (coninc is household income, inclusion of other group may skew results).
myData<-myData[myData$marital=="Widowed" | myData$marital=="Divorced" | myData$marital=="Never Married", ]
## Remove "NA" values.
completeData<-complete.cases(myData[, "marital"])
myData<-myData[completeData, ]

## Remove "NA" values for coninc (income) variable.
completeData<-complete.cases(myData[, "coninc"])
myData<-myData[completeData, ]

## Subset to include only those respondents employed full-time.
myData<-myData[myData$wrkstat=="Working Fulltime", ]

## Generate some descriptive statistics
summary(myData$coninc)
## Min. 1st Qu.  Median  Mean   3rd Qu.    Max. 
## 383   11500   23560   31880   42080    180400 

## Generate a vector containing only those records with reported incomes.
reportIncome<-na.omit(myData$coninc)
summary(reportIncome)
##    Min. 1st Qu.  Median    Mean 3rd Qu.  Max.   
##    383   18440   35600   44500   59540  180400  

hist(reportIncome, col="blue", main="Income distibution from 1972 through 2012", 
     xlab="Income in constant 1986 dollars", ylab="Frequency")

## Look at median income by year (in constant 1986 dollars).
selectCol2<-c(1,11)
meanInc<-myData[ , complete.cases(selectCol2)]

boxplot(meanInc$coninc ~ meanInc$year, col="gray", xlab="Year", 
        ylab="Income in constant 1986 dollars", main="Median Income by Year")
## median income seems to have remained relatively stable from 1972 to 2012.
incLM<-lm(meanInc$coninc ~ meanInc$year)
plot(incLM)
## the relationship between income and year does not seem to be linear (qq plot)

## Look at median income by year and gender
boxplot(meanInc$coninc ~ meanInc$year + meanInc$sex)
## Too Busy, lets look at just the first and last years

meanInc.72<-meanInc[meanInc$year==1972, ]
meanInc.12<-meanInc[meanInc$year==2012, ]

par(mfrow = c(1,2))
boxplot(meanInc.72$coninc ~ meanInc.72$sex)
boxplot(meanInc.12$coninc ~ meanInc.12$sex)

## 1972 mean values by gender
inc1972<-meanInc[meanInc$year=="1972",]
male1972<-inc1972[inc1972$sex=="Male",]
female1972<-inc1972[inc1972$sex=="Female",]
summary(male1972$coninc)
summary(female1972$coninc)

## 2012 mean values by gender
inc2012<-meanInc[meanInc$year=="2012",]
male2012<-inc2012[inc2012$sex=="Male",]
female2012<-inc2012[inc2012$sex=="Female",]
summary(male2012$coninc)
summary(female2012$coninc)

## What is the difference
mean(male1972$coninc, na.rm=TRUE)-mean(female1972$coninc, na.rm=TRUE)
median(male1972$coninc, na.rm=TRUE)-median(female1972$coninc, na.rm=TRUE)
mean(male2012$coninc, na.rm=TRUE)-mean(female2012$coninc, na.rm=TRUE)
median(male2012$coninc, na.rm=TRUE)-median(female2012$coninc, na.rm=TRUE)

## Save the plots to a file
pdf("myPlots.pdf")

par(mfrow = c(2,2))

hist(reportIncome, col="blue", main="Income distibution from 1972 through 2012", 
     xlab="Income in constant 1986 dollars", ylab="Frequency")

boxplot(meanInc$coninc ~ meanInc$year, col="gray", xlab="Year", 
        ylab="Income in constant 1986 dollars", main="Median Income by Year")

boxplot(meanInc.72$coninc ~ meanInc.72$sex, xlab="Income in constant dollars", 
        main="1972 Median Income Data", ylab="Gender", col=(c("gold","green")), horizontal=T)
boxplot(meanInc.12$coninc ~ meanInc.12$sex, xlab="Income in constant dollars", 
        main="2012 Median Income Data", ylab="Gender", col=(c("gold","green")), horizontal=T)

dev.off()

par(mfrow = c(1,2))
maleData<-myData[myData$sex=="Male", ]
hist(maleData$coninc, col="blue", main="Income distibution from 1972 through 2012", 
     xlab="Income in constant 1986 dollars", ylab="Frequency")
femaleData<-myData[myData$sex=="Female", ]
hist(femaleData$coninc, col="red", main="Income distibution from 1972 through 2012", 
     xlab="Income in constant 1986 dollars", ylab="Frequency")

par(mfrow = c(1,2))
boxplot(maleData$coninc ~ maleData$year, col="blue", xlab="Year", 
        ylab="Income in constant 1986 dollars", main="Male Median Income by Year")
boxplot(femaleData$coninc ~ femaleData$year, col="red", xlab="Year", 
        ylab="Income in constant 1986 dollars", main="Female Median Income by Year")

par(mfrow = c(1,2))
meanInc.72<-myData[myData$year==1972, ]
meanInc.12<-myData[myData$year==2012, ]
boxplot(meanInc.72$coninc ~ meanInc.72$sex, xlab="Income in constant dollars", 
        main="1972 Median Income Data", ylab="Gender", col=(c("blue","red")), horizontal=T)
boxplot(meanInc.12$coninc ~ meanInc.12$sex, xlab="Income in constant dollars", 
        main="2012 Median Income Data", ylab="Gender", col=(c("blue","red")), horizontal=T)

by(myData$coninc, myData$sex, length)

paste(round(mean(maleData$coninc, na.rm = T), 2), " - ", round(mean(femaleData$coninc, na.rm = T), 2), " = ", round(mean(maleData$coninc, na.rm = T)-mean(femaleData$coninc, na.rm = T), 2))

paste(" = ", round(sqrt(((sd(maleData$coninc, na.rm = T)^2)/length(maleData$coninc)) + ((sd(femaleData$coninc, na.rm = T)^2)/length(femaleData$coninc)))))

incDiff<-mean(maleData$coninc, na.rm = T)-mean(femaleData$coninc, na.rm = T)
incSE<-sqrt(((var(maleData$coninc, na.rm = T))/length(maleData$coninc)) + ((var(femaleData$coninc, na.rm = T))/length(femaleData$coninc)))
incZ<-(incDiff-0)/incSE

## Get looping delimiter
years<-c(1972, 1973, 1974, 1975, 1976, 1977, 1978, 1980, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1993, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012)

## Execute loop to test hypotheses by year 
for (i in 1:length(years)){
  ## subset myData
  yearData<-myData[myData$year == years[i], ]
  yearData
  ## execute tests
  suppressWarnings(inference(y = yearData$coninc, x = yearData$sex, est = "mean", type = "ht", null = 0, alternative ="twosided", method = "theoretical", order = c("Male","Female")))
}

