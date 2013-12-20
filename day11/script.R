
library(sqldf)
setwd("~/dev/feg/day11")

Test1220 <- read.csv(file = "binary.csv")

View(Test1220)
str(Test1220)

# rank変数が数値なのを文字化(factor型にする)
# 数値だけど連続値ではない。カテゴリ扱いさせる。
Test1220$rank <- factor(Test1220$rank)
str(Test1220)

summary(Test1220)

plot(Test1220[1:3])
cor(Test1220[1:3])
#admit       gre       gpa
#admit 1.0000000 0.1844343 0.1782123
#gre   0.1844343 1.0000000 0.3842659
#gpa   0.1782123 0.3842659 1.0000000

aggregate(Test1220$gre, list(Test1220$admit), summary)
#Group.1 x.Min. x.1st Qu. x.Median x.Mean x.3rd Qu. x.Max.
#1       0  220.0     500.0    580.0  573.2     660.0  800.0
#2       1  300.0     540.0    620.0  618.9     680.0  800.0

aggregate(Test1220$gpa, list(Test1220$admit), summary)
#Group.1 x.Min. x.1st Qu. x.Median x.Mean x.3rd Qu. x.Max.
#1       0  2.260     3.080    3.340  3.344     3.610  4.000
#2       1  2.420     3.220    3.540  3.489     3.755  4.000

boxplot(Test1220$gre ~ Test1220$admit)
boxplot(Test1220$gpa ~ Test1220$admit)

prflrank <- 
  sqldf("SELECT rank,
          COUNT(*) AS Cnt,
          SUM(admit) AS AdmtCnt,
          ROUND(avg(admit), 4) * 100 AS AdmtPrcnt
        FROM Test1220
        GROUP BY rank")

prflrank

# rank Cnt AdmtCnt AdmtPrcnt
# 1    1  61      33     54.10
# 2    2 151      54     35.76
# 3    3 121      28     23.14
# 4    4  67      12     17.91
# AdmtPrcnt : 合格率
# ランクと合格率に相関がちゃんと見られることが分かる

Test1220$GRECut <- cut(Test1220$gre, breaks=c(200, 400, 500, 600, 700, 800))

aggregate(Test1220$gre, list(Test1220$GRECut), summary)

prflgre <-
  sqldf("SELECT GRECut, 
          COUNT(*) AS Cnt,
          SUM(admit) As ADmtCnt,
          ROUND(avg(admit), 4) * 100 AS AdmtPrcnt
        FROM Test1220
        GROUP By GRECut")

prflgre

# GRECut Cnt ADmtCnt AdmtPrcnt
# 1 (200,400]  31       4     12.90
# 2 (400,500]  68      14     20.59
# 3 (500,600] 127      40     31.50
# 4 (600,700] 117      43     36.75
# 5 (700,800]  57      26     45.61

logit01 <- glm(admit ~ gre, data=Test1220, family="binomial")

logit01$coefficients
str(logit01)
summary(logit01)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.901344   0.606038  -4.787 1.69e-06 ***
#  gre          0.003582   0.000986   3.633  0.00028 ***

# 合格率 = 1 / (1 + exp(-(-2.901344 + 0.003582 * gre)))


Test1220$AProb <- predict(logit01, newdata=Test1220, type="response")
head(Test1220)

exp(logit01$coefficients[2])
#      gre 
# 1.003589
# gre が１点あがったら、1.003589オッズ比あがる。

# オッズとは
# 確率の比較の仕方
# 確率を直接比較するのではなく、オッズ比にしてから比較する方が適切
# オッズ = 1 / (1 - 確率)
# 参考：「オッズ比一定の法則」


# こっからよく分かっていない。ここまで（12/20）



plot(Test1220$gre, Test1220$admit, xlim=c(200,800), yliim=c(0,1)) $range
par(new=T)
plot(Test1220$gre, Test1220$AProb, 
  pch=20, col="green", xlab="", ylab="",
  xlim=c(200,800), ylim=c(0,1))

prflgre$LOdds <- log(prflgre$AdmtPrcnt / (100 - prflgre$AdmtPrcnt))

greMedian <- aggregate(Test1220$gre, list(Test1220$GRECut), median)

prflgre2 <- cbind(prflgre, greMedian)

lmOdds <- lm(prflgre$LOdds ~ prflgre2$x, data=prflgre2)

plot(prflgre2$x, prflgre2$LOdds)
abline(lmOdds, lwd=1, lty=1, col="red")


