setwd("~/dev/feg/kaggle")

date()
train <- read.csv("train_v2.csv")
date()
# 8分程度かかる

date()
test <- read.csv("test_v2.csv")
date()
# 12分くらいかかる

date()
train10k <- read.csv("train_sample.csv")
date()
# 20秒くらい

set.seed(12345)
trainnum <- sample(1:105471, 73830)
train.train <- train[trainnum, ]
validnum <- setdiff(1:105471, trainnum)
train.valid <- train[validnum, ]

train10knum <- sample(1:10000, 7000)
train10k.train <- train10k[train10knum, ]
valid10knum <- setdiff(1:10000, train10knum)
train10k.valid <- train10k[valid10knum, ]

# 変数を減らして行こう
# 前回結果で、以下のは採用決定
# f471
# f674
# f651
# f775
# f732
# f464

train.trainDroped <- train.train
train.trainDroped$f471 <- NULL
train.trainDroped$f674 <- NULL
train.trainDroped$f651 <- NULL
train.trainDroped$f775 <- NULL
train.trainDroped$f732 <- NULL
train.trainDroped$f464 <- NULL

# rpart
library(rpart)
library(partykit)

date()
train.tree02 <- rpart(loss~., data=train.trainDroped, maxdepth=3, cp=-1)
date()
# 4分で完了

plot(as.party(train.tree02))

train.trainDroped$f468 <- NULL
train.trainDroped$f536 <- NULL
train.trainDroped$f75 <- NULL
train.trainDroped$f212 <- NULL
train.trainDroped$f529 <- NULL
train.trainDroped$f662 <- NULL

date()
train.tree03 <- rpart(loss~., data=train.trainDroped, maxdepth=3, cp=-1)
date()
# 3分で完了

plot(as.party(train.tree03))

slimtrain <- data.frame(train$f471,
                        train$f674,
                        train$f651,
                        train$f775,
                        train$f732,
                        train$f464,
                        train$f468,
                        train$f536,
                        train$f75,
                        train$f212,
                        train$f529,
                        train$f662,
                        train$f533,
                        train$f211,
                        train$f376,
                        train$f273,
                        train$f527 - train$f528,
                        train$loss)

colnames(slimtrain) <- c("f471",
                         "f674",
                         "f651",
                         "f775",
                         "f732",
                         "f464",
                         "f468",
                         "f536",
                         "f75",
                         "f212",
                         "f529",
                         "f662",
                         "f533",
                         "f211",
                         "f376",
                         "f273",
                         "gold",
                         "loss")

set.seed(12345)
trainnum <- sample(1:105471, 73830)
validnum <- setdiff(1:105471, trainnum)
slimtrain.train <- slimtrain[trainnum, ]
slimtrain.valid <- slimtrain[validnum, ]

# カラム絞り込み完了
# とりあえずrpart回してみる

date()
slimtrain.tree01 <- rpart(loss~., data=slimtrain.train, maxdepth=3, cp=-1)
date()
# 12秒で完了

plot(as.party(slimtrain.tree01))

# 推定値算出
PRED01 <- predict(slimtrain.tree01, newdata=slimtrain.train)

# MAE算出
mean(abs(PRED01 - slimtrain.train$loss))
# 1.038696

PRED01.valid <- predict(slimtrain.tree01, newdata=slimtrain.valid)
mean(abs(PRED01.valid - slimtrain.valid$loss))
# 1.06981

# testデータにもgoldを設定しておく
test$gold <- test$f527 - test$f528
# 思った以上に時間かかる。数分？

date()
PRED01.test <- predict(slimtrain.tree01, newdata=test)
date()
# 1分程度かかる

PRED01.answer <- data.frame(test$id, PRED01.test)
names(PRED01.answer) <- c("id","loss")
write.csv(PRED01.answer, "answer20130219_01.csv", row.names=F)



