setwd("~/dev/feg/kaggle")

date()
train <- read.csv("train_v2.csv")
date()

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

write.csv(slimtrain, "slimtrain.csv")


# f16について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f16)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0   411800  1673000  4010000  4731000 70380000 

hist(slimtrain$f16)
# 0から７千万まで幅広く、数字が少ない領域に強く分布しているデータ

# 件数10%ずつ区切った単位で見てみる
quantile(slimtrain$f16, probs = seq(0, 1, 0.1))
hist(slimtrain$f16, breaks=quantile(slimtrain$f16, probs = seq(0, 1, 0.1)))
# 約1200万以下が９割を締める
# 1200万以下に絞って見てみる
hist(subset(slimtrain$f16, slimtrain$f16 < 12000000))

# lossとの相関を見てみる
plot(slimtrain$f16, slimtrain$loss)
#なんか反比例の関係っぽく見える

#単純に相関係数を見てみてもやっぱり超低い
cor(slimtrain$f16, slimtrain$loss)

# とりあえずこんなところかな。


subset_f16_under12M <- subset(slimtrain, slimtrain$f16 < 12000000)

pdf("f16_plot_under12M.pdf", paper="a4")
plot(subset_f16_under12M$f16, subset_f16_under12M$loss)
dev.off()

pdf("f16_hist_under12M.pdf", paper="a4")
hist(subset_f16_under12M$f16)
dev.off()


# f211について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f211)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   4.430   6.020   6.309   7.880  53.300     174 

hist(slimtrain$f211)
quantile(slimtrain$f211, probs = seq(0, 1, 0.1), na.rm=T)
#   0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 1.00  3.27  4.08  4.75  5.38  6.02  6.69  7.45  8.38  9.69 53.30 

# 9.7以下が9割。

hist(subset(slimtrain$f211, slimtrain$f211 < 13))
# 13以下に絞ってみると、正規分布っぽく見える

plot(slimtrain$f211, slimtrain$loss)
sub_f211_under_13 <- subset(slimtrain, slimtrain$f211 < 13)
plot(sub_f211_under_13$f211, sub_f211_under_13$loss)
 

pdf("f211_plot_under13.pdf", paper="a4")
plot(sub_f211_under_13$f211, sub_f211_under_13$loss)
dev.off()

pdf("f211_hist_under13.pdf", paper="a4")
hist(sub_f211_under_13$f211)
dev.off()



# f212について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f212)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   3.330   5.740   6.296   8.590  90.120     174 

hist(slimtrain$f212)
quantile(slimtrain$f211, probs = seq(0, 1, 0.1), na.rm=T)
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# 1.00  3.27  4.08  4.75  5.38  6.02  6.69  7.45  8.38  9.69 53.30 

# 9.69以下が9割以下
hist(subset(slimtrain$f212, slimtrain$f212 < 20))

plot(slimtrain$f212, slimtrain$loss)
sub_f212_under_20 <- subset(slimtrain, slimtrain$f212 < 20)
plot(sub_f212_under_20$f212, sub_f212_under_20$loss)


pdf("f212_plot_under20.pdf", paper="a4")
plot(sub_f212_under_20$f212, sub_f212_under_20$loss)
dev.off()

pdf("f212_hist_under20.pdf", paper="a4")
hist(sub_f212_under_20$f212)
dev.off()



# f216について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f216)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1   585000  1333000  1831000  2609000 22600000      101 

hist(slimtrain$f216)
quantile(slimtrain$f216, probs = seq(0, 1, 0.1), na.rm=T)
# 0%        10%        20%        30%        40%        50%        60%        70%        80% 
# 1.0   192716.2   441954.8   736484.2  1041131.2  1332709.5  1754360.6  2183546.3  2974073.0 
# 90%       100% 
#  4305502.6 22596886.0 

# 430万以下が9割以下
hist(subset(slimtrain$f216, slimtrain$f216 < 6000000))

plot(slimtrain$f216, slimtrain$loss)
sub_f216_under_6M <- subset(slimtrain, slimtrain$f216 < 6000000)
plot(sub_f216_under_6M$f216, sub_f216_under_6M$loss)


pdf("f216_plot_under6M.pdf", paper="a4")
plot(sub_f216_under_6M$f216, sub_f216_under_6M$loss)
dev.off()

pdf("f216_hist_under6M.pdf", paper="a4")
hist(sub_f216_under_6M$f216)
dev.off()



# f220について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f220)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  -2.990   1.320   1.400   1.523   1.490   3.770     182 

hist(slimtrain$f220)
quantile(slimtrain$f220, probs = seq(0, 1, 0.1), na.rm=T)
#    0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100% 
# -2.99  1.28  1.31  1.33  1.36  1.40  1.43  1.47  1.54  1.75  3.77 

# はずれ値的に負の数がありそう
sub_f220_over_0 <- subset(slimtrain, slimtrain$f220 >= 0)
# 約200件が負の数

hist(sub_f220_over_0$f220)

plot(slimtrain$f220, slimtrain$loss)
plot(sub_f220_over_0$f220, sub_f220_over_0$loss)


pdf("f220_plot_over0.pdf", paper="a4")
plot(sub_f220_over_0$f220, sub_f220_over_0$loss)
dev.off()

pdf("f220_hist_over0.pdf", paper="a4")
hist(sub_f220_over_0$f220)
dev.off()


# f273について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f273)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  0.0    435.7    705.2   1120.0   1253.0 230100.0      101 

hist(slimtrain$f273)
quantile(slimtrain$f273, probs = seq(0, 1, 0.1), na.rm=T)
#    0%        10%        20%        30%        40%        50%        60%        70%        80% 
# 0.000    299.629    395.050    473.380    562.446    705.235    899.428   1137.556   1596.444 
#        90%       100% 
#   2209.587 230099.000 

# 9割が約2200以下
sub_f273_under_2300 <- subset(slimtrain, slimtrain$f273 < 2300)

hist(sub_f273_under_2300$f273)

plot(slimtrain$f273, slimtrain$loss)
plot(sub_f273_under_2300$f273, sub_f273_under_2300$loss)


pdf("f273_plot_under2300.pdf", paper="a4")
plot(sub_f273_under_2300$f273, sub_f273_under_2300$loss)
dev.off()

pdf("f273_hist_under2300.pdf", paper="a4")
hist(sub_f273_under_2300$f273)
dev.off()



# f306について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f306)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0100  0.1079  0.1300  1.0000 

hist(slimtrain$f306)
quantile(slimtrain$f306, probs = seq(0, 1, 0.1), na.rm=T)

#   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0.00 0.00 0.00 0.00 0.01 0.01 0.02 0.07 0.24 0.50 1.00 

# 9割が0.5以下
sub_f306_under_0_5 <- subset(slimtrain, slimtrain$f306 < 0.5)

hist(sub_f306_under_0_5$f306)

plot(slimtrain$f306, slimtrain$loss)
plot(sub_f306_under_0_5$f306, sub_f306_under_0_5$loss)


pdf("f306_plot_under2300.pdf", paper="a4")
plot(sub_f306_under_0_5$f306, sub_f306_under_0_5$loss)
dev.off()

pdf("f306_hist_under2300.pdf", paper="a4")
hist(sub_f306_under_0_5$f306)
dev.off()



# f308について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f308)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0900  0.1065  0.1600  1.0000 

hist(slimtrain$f308)
quantile(slimtrain$f308, probs = seq(0, 1, 0.1), na.rm=T)

#   0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0.00 0.00 0.00 0.00 0.06 0.09 0.11 0.14 0.18 0.25 1.00 


# 9割が0.25以下
sub_f308_under_0_25 <- subset(slimtrain, slimtrain$f308 < 0.25)

hist(sub_f308_under_0_25$f308)

plot(slimtrain$f308, slimtrain$loss)
plot(sub_f308_under_0_25$f308, sub_f308_under_0_25$loss)



pdf("f308_plot_under0_25.pdf", paper="a4")
plot(sub_f308_under_0_25$f308, sub_f308_under_0_25$loss)
dev.off()

pdf("f308_hist_under0_25.pdf", paper="a4")
hist(sub_f308_under_0_25$f308)
dev.off()




# f348について調査~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(slimtrain$f348)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   6.565  10.220  21.450  37.320  94.650       5 

hist(slimtrain$f348)
quantile(slimtrain$f348, probs = seq(0, 1, 0.1), na.rm=T)

#       0%       10%       20%       30%       40%       50%       60%       70%       80%       90% 
#  0.000000  4.376685  5.993750  7.121440  8.357630 10.224015 14.840820 26.398845 44.483490 56.393185 
#      100% 
# 94.651220

plot(slimtrain$f348, slimtrain$loss)


# まとめてplotとhistをPDF出力

variables <- list("f16",
                  "f211",
                  "f212",
                  "f216",
                  "f220",
                  "f273",
                  "f306",
                  "f308",
                  "f348")

for(variable in variables) {
  filename <- paste(variable, "_plot.pdf", sep="")
  pdf(filename, paper="a4")
  #plot(slimtrain[,variable], main=paste(variable))
  plot(slimtrain[,variable], slimtrain$loss)
  dev.off()
}

for(variable in variables) {
  filename <- paste(variable, "_hist.pdf", sep="")
  pdf(filename, paper="a4")
  hist(slimtrain[,variable])
  dev.off()
}

# まとめて相関係数ゲット
for(variable in variables) {
  print(variable)
  print(cor(slimtrain[,variable], slimtrain$loss, use="pairwise.complete.obs"));
}


allvars <- list("f16",
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


cor_matrix <- matrix(nrow=length(allvars), ncol=length(allvars))

r <- 1
for(rv in allvars) {
  c <- 1
  for(cv in allvars) {
    cor_matrix[r, c] <- cor(slimtrain[,rv], slimtrain[,cv], use="pairwise.complete.obs")
    c <- c + 1
  }
  r <- r + 1
}

write.csv(cor_matrix, "cor_matrix.csv")
  
