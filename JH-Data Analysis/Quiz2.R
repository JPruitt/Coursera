# open a connection to http://simplystatistics.tumblr.com/ & assign to vector `simplyStats`
simplyStats <- readLines(url('http://simplystatistics.tumblr.com/'), 150)

# apply `nchar()`
simplyStatsChars <- nchar(simplyStats)
# how many characters long is line 2?
simplyStatsChars <- nchar(simplyStats)[2]
# how many characters long is line 45?
simplyStatsChars <- nchar(simplyStats)[45]
# how many characters long is line 122?
simplyStatsChars <- nchar(simplyStats)[122]

# Download 2006 microdata survey re: housing for Idaho using download.file()
setwd("C:/Users/joseph.pruitt/Desktop/Coursera Data Analysis/Week 2")

download.file('https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv',
              "ss06hid.csv")

# Download the code book:
download.file('https://spark-public.s3.amazonaws.com/dataanalysis/PUMSDataDict06.pdf',
              "PUMSDataDict06.pdf")

# load the data into R
idahoData <- read.csv("ss06hid.csv", header=TRUE)

# [MINE] are we sure it's just Idaho data?
unique(idahoData$ST)

# How many housing units [are] worth more than $1,000,000?
nrow(idahoData[idahoData$TYPE==1 & !is.na(idahoData$VAL) & idahoData$VAL==24,])

str(idahoData)

unique(idahoData$FES)

# How many households have 3 bedrooms and 4 total rooms?
nrow(idahoData[!is.na(idahoData$BDS) & idahoData$BDS==3 &
                 !is.na(idahoData$BDS) & idahoData$RMS==4,])
# How many households have 2 bedrooms and 5 total rooms?
nrow(idahoData[!is.na(idahoData$BDS) & idahoData$BDS==2 &
                 !is.na(idahoData$BDS) & idahoData$RMS==5,])
# How many households have 2 bedrooms and 7 total rooms?
nrow(idahoData[!is.na(idahoData$BDS) & idahoData$BDS==2 &
                 !is.na(idahoData$BDS) & idahoData$RMS==7,])

# [mine] [question] More elegant way to filter out non-NA values?

## What are the first 3 values that result?

# [mine] neat trick that I wound up not needing
q6cols <- c("ACR", "AGS")
which(names(idahoData) %in% q6cols)
agricultureLogical <- idahoData$ACR==3 & idahoData$AGS==6

# and:
which(agricultureLogical) 

# Do what they ask...
indexes <- which(agricultureLogical)
subsetIdahoData <- idahoData[indexes,]
subsetDataFrame = idahoData[indexes,]
# And then:
nrow(subsetDataFrame[is.na(subsetIdahoData$MRGX),])

strsplit(names(idahoData), "wgtp")[123]

quantile(idahoData$YBL, na.rm=TRUE)

download.file('https://spark-public.s3.amazonaws.com/dataanalysis/ss06pid.csv',
              'ss06pid.csv')

rm(idahoData)
housingData <- read.csv("ss06hid.csv", header=TRUE)
populationData <- read.csv("ss06pid.csv", header=TRUE)

dim(merge(housingData, populationData, by="SERIALNO", all=TRUE))
