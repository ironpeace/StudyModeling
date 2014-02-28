setwd("~/dev/feg/kaggle")

# trainデータ読み込み
date()
train <- read.csv("train_v2.csv")
date()

# カラム絞り込み
slimtrain <- data.frame(train$f16,
                        train$f211,
                        train$f212,
                        train$f216,
                        train$f220,
                        train$f273,
                        train$f306,
                        train$f308,
                        train$f348,
                        train$f376,
                        train$f383,
                        train$f422,
                        train$f436,
                        train$f464,
                        train$f468,
                        train$f471,
                        train$f518,
                        train$f529,
                        train$f533,
                        train$f536,
                        train$f54,
                        train$f57,
                        train$f61,
                        train$f615,
                        train$f634,
                        train$f635,
                        train$f645,
                        train$f651,
                        train$f66,
                        train$f661,
                        train$f662,
                        train$f674,
                        train$f680,
                        train$f710,
                        train$f732,
                        train$f75,
                        train$f76,
                        train$f775,
                        train$f80,
                        train$f81,
                        train$f271,
                        train$f274,
                        train$f527 - train$f528,
                        train$loss)

colnames(slimtrain) <- c("f16",
                         "f211",
                         "f212",
                         "f216",
                         "f220",
                         "f273",
                         "f306",
                         "f308",
                         "f348",
                         "f376",
                         "f383",
                         "f422",
                         "f436",
                         "f464",
                         "f468",
                         "f471",
                         "f518",
                         "f529",
                         "f533",
                         "f536",
                         "f54",
                         "f57",
                         "f61",
                         "f615",
                         "f634",
                         "f635",
                         "f645",
                         "f651",
                         "f66",
                         "f661",
                         "f662",
                         "f674",
                         "f680",
                         "f710",
                         "f732",
                         "f75",
                         "f76",
                         "f775",
                         "f80",
                         "f81",
                         "f271",
                         "f274",
                         "gold",
                         "loss")

# lossの値を0/1に変換
slimtrain$loss <- ifelse(slimtrain$loss == 0, 0, 1)

library(rpart)
library(partykit)

# モデル作成
date()
slimtrain.tree01 <- rpart(as.factor(loss)~., data=slimtrain, maxdepth=3, cp=-1)
date()

# trainに対してモデル適用
RES.train <- predict(slimtrain.tree01, newdata=slimtrain)

# 0.5より大きかったら１としちゃう
train_0_1 <- ifelse(RES.train[,2]>0.5,1,0)

# MAEを確認
mean(abs(train_0_1 - train$loss))
# 0.7667036

# testデータ読み込み
date()
test <- read.csv("test_v2.csv")
date()

# goldカラム追加
test$gold <- test$f527 - test$f528

# テストデータにモデル適用
date()
RES.test <- predict(slimtrain.tree01, newdata=test)
date()

# 0.5より大きかったら１としちゃう
test_0_1 <- ifelse(RES.test[,2]>0.5,1,0)

ANS <- data.frame(test$id, test_0_1)
names(ANS) <- c("id","loss")
write.csv(ANS, "ans_20140228.csv", row.names=F)
# 完成
# MAE 0.80426 でした。


