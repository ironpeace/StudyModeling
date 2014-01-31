
setwd("~/dev/feg/day14/")

jpop <- read.csv("Jpop/Jpop.csv")
ans <- read.csv("Jpop/JpopAns.csv")

library(FNN)

train <- jpop[1:4,]
train$HHH <- NULL

test <- jpop[5:8,]
test$HHH <- NULL

cl <- jpop$HHH[1:4] # trainの答え

jpop.knn <- knn(train, test, cl)

# [1] 2 2 2 3         <- 5行目から８行目の答え
# attr(,"nn.index")
#      [,1]
# [1,]    4           <- １番目のユーザ（５番目）と一番近いユーザのインデックスが４。4番のユーザが２なので、５番目のユーザの答えは２になる。
# [2,]    4
# [3,]    4
# [4,]    1
# attr(,"nn.dist")
#         [,1]
# [1,] 6.557439
# [2,] 5.916080
# [3,] 2.645751
# [4,] 3.741657
# Levels: 1 2 3 5

# kオプションは「一番近い何人のユーザをとってくるか」という指定
# デフォルトは１になる
# 一番近いユーザが複数していされている場合は、多数決になる。
# 例：k=5にして、1,3,3,3,3となったら３が採用される

knn.reg(train, test, cl, k=2)

# 一番近いユーザのポイントからの選び方が、多数決でなく平均になる。



bank.fulldata <- read.csv("bank-full.csv", sep=";")

bank.bunseki <- bank.fulldata[1:39752, ]
bank.test <- bank.fulldata[39752:45211, ]
bank.test$y <- NULL

set.seed(12345)
trainnum <- sample(1:39752, 27826)
bank.train <- bank.bunseki[trainnum, ]

validnum <- setdiff(1:39752, trainnum)
bank.valid <- bank.bunseki[validnum, ]

library(randomForest)

set.seed(100)
bank.rf <- randomForest(y ~ age + job + marital + education + default, data=bank.train)
train.prob <- predict(bank.rf, newdata=bank.train, type="prob")
valid.prob <- predict(bank.rf, newdata=bank.valid, type="prob")

source("~/dev/feg/day04/calcAR.R")

calcAR(X=train.prob[,2], y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)

#                 [,1]
# no vs. yes 0.2047899

calcAR(X=valid.prob[,2], y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)

#                  [,1]
# no vs. yes 0.07477626

# OverFitting!!

# nodesize : 最後のノードが何個になるまで分割するか
# 大きな数字にするほど、Treeは小さくなる。オーバーフィッティングしにくくなる。
bank.rf <- randomForest(y ~ age + job + marital + education + default, data=bank.train, nodesize=50)
train.prob <- predict(bank.rf, newdata=bank.train, type="prob")
valid.prob <- predict(bank.rf, newdata=bank.valid, type="prob")

calcAR(X=train.prob[,2], y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
calcAR(X=valid.prob[,2], y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)

tuneRF(bank.train, bank.train$y, doBest=TRUE)

# 参照
# http://tjo.hatenablog.com/entry/2013/09/02/190449
bank.rf <- randomForest(y ~ age + job + marital + education + default, data=bank.train, mtry=4, nodesize=50)
train.prob <- predict(bank.rf, newdata=bank.train, type="prob")
valid.prob <- predict(bank.rf, newdata=bank.valid, type="prob")
calcAR(X=train.prob[,2], y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
calcAR(X=valid.prob[,2], y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)

bank.rf <- randomForest(y ~ age + job + marital + education + default, data=bank.train, ntree=2000)
train.prob <- predict(bank.rf, newdata=bank.train, type="prob")
valid.prob <- predict(bank.rf, newdata=bank.valid, type="prob")
calcAR(X=train.prob[,2], y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
calcAR(X=valid.prob[,2], y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
