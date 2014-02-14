setwd("~/dev/feg/kaggle")

date()
train <- read.csv("train_v2.csv")
date()
# 8分程度かかる

# 変数にlossというものがある
# このコンペはlossをあてろというもの。
# デフォルトしたとしても、担保で回収できたのなら、ロス０となる

# まずはlossを集計してみる
table(train$loss)

# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# 95688  1145  1297  1086  1038   685   573   565   487   312   316   234   211   188   157   140   129 
# 17    18    19    20    21    22    23    24    25    26    27    28    29    30    31    32    33 
# 123   100    73    83    76    72    48    48    45    24    46    35    33     8    27    15    29 
# 34    35    36    37    38    39    40    41    42    43    44    45    46    47    48    49    50 
# 7    12     6    29    10    23     8    23     4    16     8    12     7     5     6    10     5 
# 51    52    53    54    55    56    57    58    59    60    61    62    63    64    65    68    69 
# 6     1     3    11     1     2     2     3     8     3     5     4     1     8     2     1     5 
# 70    71    72    73    74    76    77    78    79    80    81    83    84    85    89    90    92 
# 2     1     1     1     2     1     1     3     8     1     1     1     4     1     5     3     1 
# 94    95    99   100 
# 4     1     1    35 

hist(train$loss)
hist(subset(train$loss, train$loss != 0))

# 何%分が担保でカバーできなかったかの数値のようだ・・・

# このコンペではMAEという指標でモデルを評価
# MAE = (モデル予測値　- 正解)の絶対値の平均値

# １０万件を１万件にしよう
x <- seq(10000) * 10
train_sample <- train[x, ]
write.csv(train_sample, file="train_sample.csv", row.names=F)

# 7:3でトレーニング用と検証用データに分けてみる

105471 * 0.7
# 73829.7

set.seed(12345)
trainnum <- sample(1:105471, 73830)
train.train <- train[trainnum, ]
validnum <- setdiff(1:105471, trainnum)
train.valid <- train[validnum, ]

# ひとまずrpartやってみよう
library(rpart)
library(partykit)

date()
train.tree01 <- rpart(loss~., data=train.train, maxdepth=3, cp=-1)
date()
# ８分程度かかる

summary(train.tree01)
plot(as.party(train.tree01))
# ここにでてくる変数は消さない方がいいだろう

# 推定値算出
RES <- predict(train.tree01, newdata=train.train)

# MAEを計算してみる
# MAE = (モデル予測値　- 正解)の絶対値の平均値
mean(abs(RES - train.train$loss))
# 1.422796

RES.valid <- predict(train.tree01, newdata=train.valid)
mean(abs(RES.valid - train.valid$loss))
# 1.446794
# Overfitしてなさそう!

# testやってみよう！
date()
test <- read.csv("test_v2.csv")
date()
# 12分くらいかかる

date()
RES.test <- predict(train.tree01, newdata=test)
date()
# 4分程度かかる

ANS <- data.frame(test$id, RES.test)
names(ANS) <- c("id","loss")
write.csv(ANS, "ans.csv", row.names=F)

# rpartの最初の分岐に使ったf471の分布を見てみる
plot(train$f471)
summary(train$f471)
plot(train$f471, train$loss)






