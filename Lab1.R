load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/cdc.Rdata"))

names(cdc)

head(cdc)
tail(cdc)

dim(cdc)

# View the head or tail of both the height and the genhlth variables:
head(cdc$genhlth)
tail(cdc$height)
# Assign your sum here:
sum = 84941+19686
# Assign your multiplication here:
mult = 73*51

mean(cdc$weight)
var(cdc$weight)
median(cdc$weight)
summary(cdc$weight)

# Create the frequency table here:
table(cdc$genhlth)
# Create the relative frequency table here:
table(cdc$genhlth)/nrow(cdc)

# Draw the barplot:
barplot(table(cdc$smoke100))

summary(cdc$gender)

gender_smokers = table(cdc$gender, cdc$smoke100)
gender_smokers
# Plot the mosaicplot:
mosaicplot(gender_smokers)

# Create the subsets:
height_1337 = cdc$height[1337]
weight_111 = cdc$weight[111]
# Print the results:
height_1337
weight_111

# Create the subsets:
first8 = cdc[1:8, 3:5]
wt_gen_10_20 = cdc[10:20, 6:9]
# Print the subsets:
first8
wt_gen_10_20

# Create the subsets:
resp205 = cdc[205, ]
ht_wt = cdc[, 5:6]
# Print the subsets:
resp205
head(ht_wt)

# Create the subsets:
resp1000_smk = cdc$smoke100[1000]
first30_ht = cdc$height[1:30]
# Print the subsets:
resp1000_smk
first30_ht

# Create the subsets:
very_good = subset(cdc, cdc$genhlth == "very good")
age_gt50 = subset(cdc, cdc$age > 50)
# Print the subsets:
head(very_good)
head(age_gt50)

# Create the subset:
under23_and_smoke = subset(cdc, cdc$age<23 & cdc$smoke100==1)
# Print the top six rows of the subset:
head(under23_and_smoke)

dim(under23_and_smoke)

# Draw the boxplot of the respondents heights:
boxplot(cdc$height)
# Print the summary:
summary(cdc$height)

# Draw the boxplot of the weights versus smoking:
boxplot(cdc$weight ~ cdc$smoke100)

bmi<-((cdc$weight)/(cdc$height)^2)*703
boxplot(bmi ~ cdc$genhlth)

hist(bmi)
hist(bmi, breaks = 50)
hist(bmi, breaks = 100)

plot(cdc$weight ~ cdc$wtdesire)

source("http://www.openintro.org/stat/data/present.R")

present

dim(present)
names(present)
present$boys
present$girls

plot(x=present$year, y=present$girls)
plot(x=present$year, y=present$girls, type="l")

plot(present$year, present$boys + present$girls, type="l")

total<-c(present$girls+present$boys)
max(total)

present$boys[21]+present$girls[21]
present$boys[22]+present$girls[22]
present$boys[18]+present$girls[18]
present$boys[52]+present$girls[52]

plot(present$year, present$boys/(present$boys+present$girls), type="l")

plot(present$year, present$boys/present$girls, type="l")

present$diffAbs<-abs(present$boys-present$girls)
max(present$diffAbs)
present[present$diffAbs==max(present$diffAbs),]
