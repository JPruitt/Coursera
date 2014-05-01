## Peer 2
## Exploratory Analysis

load("samsungData.rda")

## list variables
names(samsungData)

## list of research subjects
unique(samsungData$subject)
## 1  3  5  6  7  8 11 14 15 16 17 19 21 22 23 25 26 27 28 29 30

## Data cleaning steps
## set activity as a factor
samsungData$activity <- as.factor(samsungData$activity)

## clean the duplicate column names
for (n in 303:316) {
  colnames(samsungData)[n] <- paste("X-", colnames(samsungData)[n], sep="")
}
for (n in 317:330) {
  colnames(samsungData)[n] <- paste("Y-", colnames(samsungData)[n], sep="")
}
for (n in 331:344) {
  colnames(samsungData)[n] <- paste("Z-", colnames(samsungData)[n], sep="")
}
for (n in 382:395) {
  colnames(samsungData)[n] <- paste("X-", colnames(samsungData)[n], sep="")
}
for (n in 396:409) {
  colnames(samsungData)[n] <- paste("Y-", colnames(samsungData)[n], sep="")
}
for (n in 410:423) {
  colnames(samsungData)[n] <- paste("Z-", colnames(samsungData)[n], sep="")
}
for (n in 461:474) {
  colnames(samsungData)[n] <- paste("X-", colnames(samsungData)[n], sep="")
}
for (n in 475:488) {
  colnames(samsungData)[n] <- paste("Y-", colnames(samsungData)[n], sep="")
}
for (n in 489:502) {
  colnames(samsungData)[n] <- paste("Z-", colnames(samsungData)[n], sep="")
}

## coerce out `-` and `(` and `)`)
samsungData <- data.frame(samsungData)

### Split the Data

set.seed(20130228)

trainConstants = c(1, 3, 5, 6)
testConstants = c(27, 28, 29, 30)
remainderSubjects = setdiff(setdiff(unique(samsungData$subject), trainConstants), testConstants)
trainConstants <- c(trainConstants, setdiff(remainderSubjects,
                                            sample(remainderSubjects, length(remainderSubjects)/2)))
testConstants <- c(testConstants, setdiff(remainderSubjects, trainConstants))


trainData = samsungData[samsungData$subject %in% trainConstants,]
testData = samsungData[samsungData$subject %in% testConstants,]

### Singular value decomposition?
train.svd <- trainData[,-c(562:564)]
for (c in names(train.svd)) {
train.svd <- train.svd[!is.na(train.svd[[c]]),]
}

svd1 = svd(scale(train.svd))

pca.of.train <- princomp(train.svd)
pca.of.train$loadings

par(mfrow=c(1,2))
plot(svd1$d, xlab="Column", ylab="Singular value", pch=19)
svd1Pct <- svd1$d^2/sum(svd1$d^2)
plot(svd1Pct, xlab="Column", ylab="Percent of variance explained", pch=19)

par(mfrow=c(1,1))
plot(svd1$v[,2],pch=19)

# 5 values >= 200
svd1$d[svd1$d >= 200]
# 2 values >= 5%
svd1Pct[svd1Pct >= 0.05]

# 5 max contributors
maxContrib <- which.max(svd1$v[,2])

second <- svd1$v[,2]
second <- second[! second %in% c(second[maxContrib])]
maxContrib2 <- which.max(second)

third <- second[! second %in% c(second[maxContrib2])]
maxContrib3 <- which.max(third)

fourth <- third[! third %in% c(third[maxContrib3])]
maxContrib4 <- which.max(fourth)

fifth <- fourth[! fourth %in% c(fourth[maxContrib4])]
maxContrib5 <- which.max(fifth)

maxContribs <- c(maxContrib, maxContrib2, maxContrib3, maxContrib4, maxContrib5)
names(train.svd)[maxContribs]
maxContribs.names <- sapply(maxContribs, function(x) names(trainData)[x])

### AN ALTERNATIVE APPROACH:
all.maxContribs <- apply(svd1$v, 2, which.max)
length(all.maxContribs) # 561
length(unique(all.maxContribs)) # 365
all.maxContribs.df <- as.data.frame(table(all.maxContribs))
all.maxContribs.df[all.maxContribs.df$Freq > 3,]
# 7 columns:
maxContribs2 <- as.numeric(as.vector(
all.maxContribs.df[all.maxContribs.df$Freq > 3,]$all.maxContribs))
maxContribs2.names <- sapply(maxContribs2, function(x) names(trainData)[x])

### NO INTERSECTION
intersect(maxContribs2.names, maxContribs.names)
maxContribs2.names %in% maxContribs.names
intersect(maxContribs.names, maxContribs2.names)
maxContribs.names %in% maxContribs2.names

## tree
library(tree)
par(mfrow=c(2,1))
par(mfrow=c(1,1))
trainTree <- tree(as.factor(trainData[["activity"]]) ~
    ## trainData[[maxContribs.names[1]]] +
    ## trainData[["fBodyAcc-meanFreq()-Z"]] +
    trainData[[296]] +
    ## trainData[[maxContribs.names[2]]] +
    ## trainData[["tGravityAcc-arCoeff()-Z,3"]] +
    trainData[[76]] +
    ## trainData[[maxContribs.names[3]]] +
    ## trainData[["tGravityAcc-arCoeff()-Z,1"]] +
    trainData[[74]] +
    ## trainData[[maxContribs.names[4]]] +
    ## trainData[["tGravityAccMag-iqr()"]] +
    trainData[[221]] +
    ## trainData[[maxContribs.names[5]]],
    ## trainData[["tBodyAccMag-iqr()"]],
    trainData[[208]],
    data=trainData)
trainTree <- tree(activity ~ ., data=trainData)

plot(trainTree)
text(trainTree)
summary(trainTree)

## compare SVD results vs. tree variables
# --- no intersection ---
tree.actualVariables <- c("fBodyAccJerk.std...X", "tGravityAcc.min...X",
                          "tGravityAcc.max...Y", "tBodyAccMag.std..",
                          "tGravityAcc.arCoeff...Y.2", "fBodyAccJerk.maxInds.X",
                          "tBodyGyro.arCoeff...Y.1" )
intersect(tree.actualVariables, maxContribs.names) # NONE
intersect(tree.actualVariables, maxContribs2.names)
# "fBodyAccJerk.maxInds.X"

trainTree2 <- tree(activity ~ fBodyAccJerk.maxInds.X +
                     fBodyAccJerk.std...X + 
                     tGravityAcc.min...X +
                     tGravityAcc.max...Y +
                     tBodyAccMag.std.. +
                     tGravityAcc.arCoeff...Y.2 +
                     tBodyGyro.arCoeff...Y.1,
                   data=trainData)
plot(trainTree2)
text(trainTree2)
summary(trainTree2)

par(mfrow=c(1,1))
plot(trainTree)
text(trainTree)
summary(trainTree)

## linear model
lm1 <- lm(as.numeric(activity) ~ fBodyAccJerk.maxInds.X +
            fBodyAccJerk.std...X +
            tGravityAcc.min...X +
            tGravityAcc.max...Y +
            tBodyAccMag.std.. +
            tGravityAcc.arCoeff...Y.2 +
            tBodyGyro.arCoeff...Y.1,
          data=trainData)
summary(lm1)
confint(lm1)
anova(lm1)

### from week 6 quiz
missClass = function(values, predictions) {
  abs(sum(values - round(predictions))) / length(values)
}

missClass(as.numeric(trainData$activity), predict(lm1, type="response"))
# 0.03789474 ... so about 3-4% (beats 10%)


# do it on the test data: drop the tail of prediction
missClass(as.numeric(testData$activity),
          predict(lm1, newData=testData, type="response")[1:length(testData$activity)])
# 0.05320946 -- about 5% (we'll take it)

plot(lm1$residuals~lm1$fitted, col=as.factor(trainData$activity), 
  pch=19, cex=0.75,
  xlab="Residuals",
  ylab="Fitted Values",
  main="Test Set Residuals")
legend("topright",
  legend=levels(trainData$activity),
  col=1:6,
  cex = 0.75,
  pch=19)
abline(0,0, lty = 2, col="darkgray")
lines(lowess(lm1$residuals~lm1$fitted), col="red")

qqnorm(lm1$residuals, col=as.factor(trainData$activity), 
       pch=19, cex=0.75,
       xlab="Theoretical Quantiles",
       ylab="Residuals",
       main="Figure 4. QQ-Plot")
qqline(lm1$residuals)


plot(lm1)

lm2 <- lm(as.numeric(activity) ~ fBodyAccJerk.maxInds.X +
            fBodyAccJerk.std...X +
            tGravityAcc.min...X +
            tGravityAcc.max...Y +
            tBodyAccMag.std.. +
            tBodyGyro.arCoeff...Y.1,
          data=trainData)
summary(lm2)
confint(lm2)
anova(lm2)

plot(lm2$residuals~lm2$fitted, col=as.factor(trainData$activity), 
     pch=19)
legend("topright",
       legend=levels(trainData$activity),
       col=1:6,
       pch=19)
abline(0,0, col="red")

missClass(as.numeric(trainData$activity), predict(lm2, type="response"))

missClass(as.numeric(testData$activity),
          predict(lm2, newData=testData, type="response")[1:length(testData$activity)])

rlm2 <- rlm(as.numeric(activity) ~ fBodyAccJerk.maxInds.X +
            fBodyAccJerk.std...X +
            tGravityAcc.min...X +
            tGravityAcc.max...Y +
            tBodyAccMag.std.. +
            tGravityAcc.arCoeff...Y.2 +
            tBodyGyro.arCoeff...Y.1,
          data=trainData)
summary(rlm2)
confint(rlm2)
anova(rlm2)

ggplot(rlm2)
