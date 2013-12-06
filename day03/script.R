data(Titanic)

Titanic.tables <- as.data.frame(Titanic)

Titanic.data <- data.frame(
  Class = rep(Titanic.tables$Class, Titanic.tables$Freq),
  Sex = rep(Titanic.tables$Sex, Titanic.tables$Freq),
  Age = rep(Titanic.tables$Age, Titanic.tables$Freq),
  Survived = rep(Titanic.tables$Survived, Titanic.tables$Freq)
  )

library(rpart)

Titanic.tree <- rpart(Survived~.,data=Titanic.data)

Titanic.tree

plot(Titanic.tree)
text(Titanic.tree)
par(xpd=NA)
plot(Titanic.tree, uniform=TRUE)
text(Titanic.tree, use.n=TRUE, fancy=TRUE)