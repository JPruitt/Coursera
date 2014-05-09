library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction) {
  sum(((prediction > 0.5)*1) != values) / length(values)
}

glm1 <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
            family="binomial", data=trainSA)

missClass(trainSA$chd, predict(glm1, type="response"))

missClass(testSA$chd, predict(glm1, newdata=testSA, type="response"))

library(pgmm)
data(olive)
olive = olive[,-1]


newData = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, 
                     Linolenic = 32, Arachidic=60,Eicosenoic=6)

library(tree)
tree1 <- tree(Area ~ Palmitic + Palmitoleic + Stearic + Oleic + Linoleic + Linolenic + Arachidic + Eicosenoic, data=olive)
summary(tree1)

plot(tree1)
text(tree1)

predict(tree1, newData)
