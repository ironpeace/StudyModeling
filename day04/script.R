data(Titanic)
Titanic.tables <- as.data.frame(Titanic)
Titanic.data <- data.frame(
  Class = rep(Titanic.tables$Class, Titanic.tables$Freq),
  Sex = rep(Titanic.tables$Sex, Titanic.tables$Freq),
  Age = rep(Titanic.tables$Age, Titanic.tables$Freq),
  Survived = rep(Titanic.tables$Survived, Titanic.tables$Freq)
)


table(Titanic.data$Class, Titanic.data$Survived)
table(Titanic.data$Sex, Titanic.data$Survived)
table(Titanic.data$Age, Titanic.data$Survived)

library(rpart)
Titanic.tree <- rpart(Survived~., data=Titanic.data)

# install "partykit" package
# install "rpart.plot" package

library(partykit)
plot(as.party(Titanic.tree))

library(rpart.plot)
prp(Titanic.tree)
prp(Titanic.tree, type=3, extra=7, clip.right.labs=F, fallen.leaves=TRUE)

#treeの右から２番目が46%とまだ大勢決まっていないのに何故ここで止まっているのか
node1 <- Titanic.data[Titanic.data$Sex=="Female" & Titanic.data$Class=="3rd",]
table(node1$Age, node1$Survived)

# No Yes
# Child 17  14
# Adult 89  76

# 14/ 31 -> 45%
# 76 / (89 + 76) -> 46%
# これ以上分岐しても、46%からあまり改善されないので、ここで止めてしまっている。


PROB <- predict(Titanic.tree, newdata=Titanic.data, type="prob")
PROB[1:10,]
PD <- PROB[,1]

source("calcAR.R")

calcAR(X=PD, y=Titanic.data$Survived, TARGET="No", plotCAP=TRUE, plotpr=TRUE)


Titanic.tree2 <- rpart(Survived~., data=Titanic.data, cp=-1)
plot(as.party(Titanic.tree2))
prp(Titanic.tree2)
prp(Titanic.tree2, type=3, extra=7, clip.right.labs=F, fallen.leaves=TRUE)

PROB2 <- predict(Titanic.tree2, newdata=Titanic.data, type="prob")
PD2 <- PROB2[,1]
calcAR(X=PD2, y=Titanic.data$Survived, TARGET="No", plotCAP=TRUE, plotpr=TRUE)
