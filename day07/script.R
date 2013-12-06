setwd("~/dev/feg/day07")
bank.fulldata <- read.csv("bank-full.csv", sep=";")

bank.bunseki <- bank.fulldata[1:39752, ]
bank.test <- bank.fulldata[39753:45211, ]
bank.test$y <- NULL

set.seed(12345)
trainnum <- sample(1:39752, 27826)
bank.train <- bank.bunseki[trainnum, ]

validnum <- setdiff(1:39752, trainnum)
bank.valid <- bank.bunseki[validnum, ]

library(rpart)
library(partykit)
source("calcAR.R")
source("MakeTreeDiagram_0.0.1.R")

bank.tree <- rpart(
    y~job+marital+education+balance+previous+poutcome, 
    data=bank.train, 
    cp = -1, 
    maxdepth = 5, 
    minbucket = 100)
plot(as.party(bank.tree))
PROB <- predict(bank.tree, newdata=bank.train, type="prob")
PD <- PROB[, 2]
calcAR(X=PD, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2372974

PROBv <- predict(bank.tree, newdata=bank.valid, type="prob")
PDv <- PROBv[, 2]
calcAR(X=PDv, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2001005

1 - 0.2372974 / 0.2001005


PROB.test2 <- predict(bank.tree, newdata=bank.test, type="prob")
PY.test2 <- PROB.test2[,"yes"] #反応率=(yesの確率)
target2 <- rep(0, length=5459)
target2[PY.test2 >= 0.15] <- 1

sum(target2)

sum(target2 * PY.test2 * 1000) - sum(target2) * 150
# 210794.9もうかりそうという期待値

table(target2, ANS.test)

