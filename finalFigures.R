## Final Figures

### PDF open:
pdf(file="Peer2-Figure.pdf", height=11, width=8.5)

par(mfrow=c(2,2))

trainData.for.tree <- trainData
clean.ugly.col.names <- c("fBodyAccJerk.maxInds.X", "fBodyAccJerk.std...X",
                          "tGravityAcc.min...X", "tGravityAcc.max...Y",
                          "tBodyAccMag.std..", "tGravityAcc.arCoeff...Y.2",
                          "tBodyGyro.arCoeff...Y.1")
alpha.order <- c("A", "B", "C", "D", "E", "F", "G")
for (n in 1:length(clean.ugly.col.names)) {
  colnames(trainData.for.tree)[which(names(trainData.for.tree) == clean.ugly.col.names[n])] <- alpha.order[n]
}

trainTree <- tree(activity ~ A + B + C + D + E + F + G, data=trainData.for.tree)
plot(trainTree, lwd=1)
title(main="Figure 1: Classification Tree Training Data")
text(trainTree, col="blue", bg="white", cex=0.75, adj=c(0.5, 0.25)) #adj=c(1.1,-0.1)
legend("bottomright", legend=c("A = fBodyAccJerk-maxInds-X",
                            "B = fBodyAccJerk-std())-X",
                            "C = tGravityAcc-min()-X",
                            "D = tGravityAcc-max()-Y",
                            "E = tBodyAccMag-std()",
                            "F = tGravityAcc-arCoeff()-Y,2",
                            "G = tBodyGyro-arCoeff()-Y,1"),
       cex=0.5)

plot(jitter(trainData$fBodyAccJerk.maxInds.X), jitter(trainData$fBodyAccJerk.std...X),
     col=as.factor(trainData$activity), pch=19, cex=0.75,
     xlab="Maximum Linear Acceleration-X",
     ylab="Standard Deviation, Linear Acceleration-X",
     main="Figure 2: Activity Type v. Linear Acceleration")
legend("topright",
       legend=levels(trainData$activity),
       col=1:6,
       cex=0.75,
       pch=19)

plot(lm1$residuals~lm1$fitted, col=as.factor(trainData$activity), 
     pch=19, cex=0.75,
     xlab="Residuals",
     ylab="Fitted Values",
     main="Figure 3: Residuals v. Fitted Values (Training Data)")
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

# close PDF
dev.off()
