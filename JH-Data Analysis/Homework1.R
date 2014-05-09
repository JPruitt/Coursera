#Homework #1 Effort (20130925)

#Working directory has been set through R-settings menu
#read data frame into R
hw1<-read.csv("C:/Users/joseph.pruitt/Desktop/Coursea Computing for Data Analysis/Homework/hw1_data.csv")
attach(hw1)

#Question #1
#What are the column names of the data set?
str(hw1)

#Question #2
#Extract the first two rows of the data frame and print them to the console.  What does the output look like?
first.two<-hw1[1:2, ]
first.two

#Question #3
#How many observations (i.e. rows) are in this data frame?
nrow(hw1)

#Question #4
last.two<-hw1[152:153, ]
last.two

#Question #5
#What is the value of Ozone in the 47th row?
oz47<-c(hw1[47, ])
oz47
oz471<-oz47[1]
oz471

#Question #6
#How many missing values are in the ozone column of this data frame?
sub.Oz<-c(hw1$Ozone)  #creates a vector containing only Ozone values
sub.Oz
bad.Oz<-is.na(sub.Oz)  #assigns TRUE for values, and FALSE for missing values
bad.Oz
bad.count<-c(sub.Oz[bad.Oz])  #creates a vector containing all of the missing values
str(bad.count)  #outputs properties of bad.count vector

#Question #7
#What is the mean of the Ozone column in this data set?  Exclude missing values (coded as NA) from this calculation
good.Oz<-sub.Oz[!bad.Oz]
mean(good.Oz)

#Question #8
#Extract the subset of rows of the data frame where Ozone is values are above 31 and Temp values are above 90.  What is the mean of Solar.R in this subset?
good<-complete.cases(hw1) #creates a logical data frame where missing values are assigned a FALSE value
sub1<-hw1[good,]  #creates a data set (subset) with no missing values
sub1
sub2<-Ozone>31  #creates a data set containing TRUE values where Ozone is greater than 31
sub2
sub3<-Temp>90  #creates a data set containing TRUE values where Temp is greater than 90
sub3
sub4<-subset(hw1, sub2 & sub3) #creates a data set (subset) containing all rows where both Ozone is greater than 31 and Temp is greater than 90
sub4
MS<-mean(sub4$Solar.R)  #Calculates the mean of the Solar.R column from subset4 (sub4)
MS

#Question #9
#What is the mean of "Temp" when "Month" is equal to 6?
sub5<-subset(hw1,Month==6)  #creates a data set (subset) containing all rows where Month is equal to 6
sub5
MT<-mean(sub5$Temp)  #Calculates the mean of the Temp column from subset5 (sub5)
MT

#Question #10
#What is the maximum ozone value in the month of May (i.e. Month = 5)?
sub6<-subset(hw1,Month==5)  #creates a data set (subset) containing all rows where Month is equal to 5
OZ<-c(sub6$Ozone)  #Creates a vaector containing Ozone values from May (sub6)
OZ
good<-complete.cases(OZ)  #creates a logical vector where missing values are assigned a FALSE value 
OZ1<-OZ[good]  #Creates a vector (subset) of Ozone values with missing values removed
OZ1
MO<-max(OZ1)  #Finds the maximum Ozone value in OZ1
MO
