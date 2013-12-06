setwd("~/dev/feg/day05")
bank.fulldata <- read.csv("bank-full.csv", sep=";")

bank.bunseki <- bank.fulldata[1:39752, ]
bank.test <- bank.fulldata[39753:45211, ]
bank.test$y <- NULL

39752*0.7

set.seed(12345)
trainnum <- sample(1:39752, 27826)
bank.train <- bank.bunseki[trainnum, ]

validnum <- setdiff(1:39752, trainnum)
bank.valid <- bank.bunseki[validnum, ]

library(rpart)
library(partykit)
source("calcAR.R")
source("MakeTreeDiagram_0.0.1.R")

# 02 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bank.tree02 <- rpart(y~., data=bank.train)
plot(as.party(bank.tree02))
print(bank.tree02)
MakeTreeDiagram(bank.tree02, "tree02.csv")
PD2 <- PROB02[, 2]
PROB02 <- predict(bank.tree02, newdata=bank.train, type="prob")
calcAR(X=PD2, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.5539828

PROB02v <- predict(bank.tree02, newdata=bank.valid, type="prob")
PD2v <- PROB02v[, 2]
calcAR(X=PD2v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.5510233


# 03 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bank.tree03 <- rpart(y~., data=bank.train, cp = -1, minbucket = 10)
#plot(as.party(bank.tree03))
PROB03 <- predict(bank.tree03, newdata=bank.train, type="prob")
PD3 <- PROB03[, 2]
calcAR(X=PD3, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.9533542

PROB03v <- predict(bank.tree03, newdata=bank.valid, type="prob")
PD3v <- PROB03v[, 2]
calcAR(X=PD3v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.7696851  <-- over fitting


# 04 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bank.tree04 <- rpart(y~., data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree04))
MakeTreeDiagram(bank.tree04, "tree04.csv")
PROB04 <- predict(bank.tree04, newdata=bank.train, type="prob")
PD4 <- PROB04[, 2]
calcAR(X=PD4, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.8008585

PROB04v <- predict(bank.tree04, newdata=bank.valid, type="prob")
PD4v <- PROB04v[, 2]
calcAR(X=PD4v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.8114267


# 05 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bank.tree05 <- rpart(y~age+education+balance, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree05))
PROB05 <- predict(bank.tree05, newdata=bank.train, type="prob")
PD5 <- PROB05[, 2]
calcAR(X=PD5, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1984882

PROB05v <- predict(bank.tree05, newdata=bank.valid, type="prob")
PD5v <- PROB05v[, 2]
calcAR(X=PD5v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1669796


# 06 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bank.tree06 <- rpart(y~age+campaign+balance+education+housing+marital, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree06))
PROB06 <- predict(bank.tree06, newdata=bank.train, type="prob")
PD6 <- PROB06[, 2]
calcAR(X=PD6, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2213745

PROB06v <- predict(bank.tree06, newdata=bank.valid, type="prob")
PD6v <- PROB06v[, 2]
calcAR(X=PD6v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1983344


# 07 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 分布のバランスのいい変数を利用
bank.tree07 <- rpart(y~campaign+balance+education+housing+marital, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree07))
PROB07 <- predict(bank.tree07, newdata=bank.train, type="prob")
PD7 <- PROB07[, 2]
calcAR(X=PD7, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2468179

PROB07v <- predict(bank.tree07, newdata=bank.valid, type="prob")
PD7v <- PROB07v[, 2]
calcAR(X=PD7v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1747638

MakeTreeDiagram(bank.tree07, "tree07.csv")

0.1747638 / 0.2468179


# 08 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 個人属性だけ利用
bank.tree08 <- rpart(y~age+job+marital+education, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree08))
PROB08 <- predict(bank.tree08, newdata=bank.train, type="prob")
PD8 <- PROB08[, 2]
calcAR(X=PD8, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1949179

PROB08v <- predict(bank.tree08, newdata=bank.valid, type="prob")
PD8v <- PROB08v[, 2]
calcAR(X=PD8v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.181586

0.181586 / 0.1949179
# 90%超えた

MakeTreeDiagram(bank.tree08, "tree08.csv")

PROB08t <- predict(bank.tree08, newdata=bank.test, type="prob")
PD8t <- PROB08t[, "yes"]
bank.test2 <- data.frame(bank.test, PD8t)
TARGET.test2 <- bank.test2[bank.test2$PD8t >= 0.15, ]
income <- sum(TARGET.test2$PD8t * 1000 - 150)

# 09 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 個人属性のうち年齢以外を利用
bank.tree09 <- rpart(y~job+marital+education, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree09))
PROB09 <- predict(bank.tree09, newdata=bank.train, type="prob")
PD9 <- PROB09[, 2]
calcAR(X=PD9, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1860786

PROB09v <- predict(bank.tree09, newdata=bank.valid, type="prob")
PD9v <- PROB09v[, 2]
calcAR(X=PD9v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1437085

0.1437085 / 0.1860786

# 10 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 取引情報だけを利用
bank.tree10 <- rpart(y~default+balance+housing+loan, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree10))
PROB10 <- predict(bank.tree10, newdata=bank.train, type="prob")
PD10 <- PROB10[, 2]
calcAR(X=PD10, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.201602

PROB10v <- predict(bank.tree10, newdata=bank.valid, type="prob")
PD10v <- PROB10v[, 2]
calcAR(X=PD10v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.110945

0.110945 / 0.201602

# 11 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 取引情報からdefaultは偏りが大きいので
bank.tree11 <- rpart(y~balance+housing+loan, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree11))
PROB11 <- predict(bank.tree11, newdata=bank.train, type="prob")
PD11 <- PROB11[, 2]
calcAR(X=PD11, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.201602

PROB11v <- predict(bank.tree11, newdata=bank.valid, type="prob")
PD11v <- PROB11v[, 2]
calcAR(X=PD11v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.110945

0.110945 / 0.201602

# 12 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 個人属性と取引情報。defaultとageは偏りが大きいからはずす。
bank.tree12 <- rpart(y~job+marital+education+balance+housing+loan, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree12))
PROB12 <- predict(bank.tree12, newdata=bank.train, type="prob")
PD12 <- PROB12[, 2]
calcAR(X=PD12, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2436639

PROB12v <- predict(bank.tree12, newdata=bank.valid, type="prob")
PD12v <- PROB12v[, 2]
calcAR(X=PD12v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1819828

0.1819828 / 0.2436639


# 13 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 個人属性だけ利用 + previous + poutcome
bank.tree13 <- rpart(y~age+job+marital+education+previous+poutcome, data=bank.train, cp = -1, maxdepth = 4, minbucket = 100)
plot(as.party(bank.tree13))
PROB13 <- predict(bank.tree13, newdata=bank.train, type="prob")
PD13 <- PROB13[, 2]
calcAR(X=PD13, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1958693

PROB13v <- predict(bank.tree13, newdata=bank.valid, type="prob")
PD13v <- PROB13v[, 2]
calcAR(X=PD13v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2100257

0.1958693 / 0.2100257

MakeTreeDiagram(bank.tree13, "tree13.csv")

# 14 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 個人属性だけ利用 + previous + poutcome
# Overfitting具合が低いのでmaxdepthをあげてみた
bank.tree14 <- rpart(y~age+job+marital+education+previous+poutcome, data=bank.train, cp = -1, maxdepth = 5, minbucket = 100)
plot(as.party(bank.tree14))
PROB14 <- predict(bank.tree14, newdata=bank.train, type="prob")
PD14 <- PROB14[, 2]
calcAR(X=PD14, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2204061

PROB14v <- predict(bank.tree14, newdata=bank.valid, type="prob")
PD14v <- PROB14v[, 2]
calcAR(X=PD14v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2150352

0.2204061 / 0.2150352

PROB14t <- predict(bank.tree14, newdata=bank.test, type="prob")
PD14t <- PROB14t[, "yes"]
bank.test2 <- data.frame(bank.test, PD14t)
TARGET.test2 <- bank.test2[bank.test2$PD14t >= 0.15, ]
income <- sum(TARGET.test2$PD14t * 1000 - 150)

# randomForestにtry ~~~~~~

install.packages("randomForest")
library(randomForest)

bank.forest <- randomForest(y ~ age + job + marital + education + balance + housing + loan + previous + poutcome, data=bank.train)
PD.forest <- predict(bank.forest, newdata=bank.test, type="prob")

varImpPlot(bank.forest)

importance(bank.forest)
# MeanDecreaseGini
# age              498.76167
# job              241.28292
# marital           66.37467
# education         95.50044
# balance          923.71329
# housing           43.77419
# loan              48.67445
# previous          92.40516
# poutcome          68.99437

bank.treeF <- rpart(y~age+job+balance, data=bank.train, cp = -1, maxdepth = 5, minbucket = 100)
plot(as.party(bank.treeF))
PROBF <- predict(bank.treeF, newdata=bank.train, type="prob")
PDF <- PROBF[, 2]
calcAR(X=PDF, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2205608

PROBFv <- predict(bank.treeF, newdata=bank.valid, type="prob")
PDFv <- PROBFv[, 2]
calcAR(X=PDFv, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1678837

# No.14のモデルを採用

PROB.test <- predict(bank.tree14, newdata=bank.test, type="prob")
PY.test <- PROB.test[,"yes"] #反応率=(yesの確率)
target <- rep(0, length=5459)
target[PY.test >= 0.15] <- 1

Income <- sum(target * PY.test * 1000)
Cost <- sum(target) * 150
Income - Cost
# 355922.9もうかりそうという期待値

#仮に全員にキャンペンーンをかけた時のCost
150*5459

ANS.test <- bank.fulldata$y[39753:45211]
table(target, ANS.test)

#        ANS.test
# target no   yes
#      0 1897 1171
#      1 1076 1315

(1076 + 1315) * 150 # cost : 358650
1315 * 1000 # income : 1315000
1315000 - 358650 # net : 956350
956350 / 355922.9 # 期待値の2.69倍の収益が出た

(1171+1315)*1000 - 5459*150 
# → 実際(全員をターゲットにした場合)に得られた収益は、1,667,150

(1315)*1000 - (1076 + 1315)*150 
# → モデル構築によって得られた収益は、959,350

# 黙って全員にキャンペーンした方が収益上げられたじゃん!



calcAR(X=PY.test, y=ANS.test, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1492325
# train が 0.2204061、valid が 0.2150352 だった。ちょっと下がってる。

# CAP曲線見ると、一番左のポイントがrandomを下回っていて
# 何か問題がありあそう。
# 一番左にポイントがあるということは、
# 一番反応率の高いもの。
# 60.5歳以上というのが一番反応率が高い。
# モデル上はそう考えたけど、実際は違うということ。

hist(bank.train$age)
hist(bank.test$age)
# 年齢の分布を比べてみると、testデータは60歳以上の分布が増えている。

table(bank.train$age, bank.train$y)
table(bank.test$age, ANS.test)

# 15 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No.14からageをはずしてみる
bank.tree15 <- rpart(y~job+marital+education+previous+poutcome, data=bank.train, cp = -1, maxdepth = 5, minbucket = 100)
plot(as.party(bank.tree15))
PROB15 <- predict(bank.tree15, newdata=bank.train, type="prob")
PD15 <- PROB15[, 2]
calcAR(X=PD15, y=bank.train$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2088178

PROB15v <- predict(bank.tree15, newdata=bank.valid, type="prob")
PD15v <- PROB15v[, 2]
calcAR(X=PD15v, y=bank.valid$y, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1827158

0.1827158 / 0.2088178

PROB.test2 <- predict(bank.tree15, newdata=bank.test, type="prob")
PY.test2 <- PROB.test2[,"yes"] #反応率=(yesの確率)
target2 <- rep(0, length=5459)
target2[PY.test2 >= 0.15] <- 1

sum(target * PY.test2 * 1000) - sum(target) * 150
# 153326.6もうかりそうという期待値

table(target2, ANS.test)

#         ANS.test
# target2   no  yes
#       0 2440 1481
#       1  533 1005

(533 + 1005) * 150 # cost : 230700
1005 * 1000 # income : 1005000
1005000 - 230700 # net : 774300
774300 / 153326.6 # 期待値の5.05倍の収益が出た

(1481+1005)*1000 - 5459*150 
# → 実際(全員をターゲットにした場合)に得られた収益は、1,667,150

(1005)*1000 - (533 + 1005)*150 
# → モデル構築によって得られた収益は、774,300

# 黙って全員にキャンペーンした方が収益上げられたじゃん!

calcAR(X=PY.test2, y=ANS.test, TARGET="yes", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.2558481

