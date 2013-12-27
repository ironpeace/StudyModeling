library(tcltk)
library(sqldf)

setwd("~/dev/feg/day12")

Test1227 <- read.csv(file = "binary.csv")

Test1227$rank <- factor(Test1227$rank)

Test1227$GPACut <- cut(Test1227$gpa, breaks=c(2, 3, 3.25, 3.5, 3.75, 4))

head(Test1227)

boxplot(Test1227$gre ~ Test1227$admit)
boxplot(Test1227$gpa ~ Test1227$admit)

prflgpa <-
  sqldf("SELECT GPACut, 
          COUNT(*) AS Cnt,
          SUM(admit) As ADmtCnt,
          ROUND(avg(admit), 4) * 100 AS AdmtPrcnt
        FROM Test1227
        GROUP By GPACut")

prflgpa

logit02 <- glm(admit ~ gpa, data=Test1227, family="binomial")

logit02$coefficients

# モデルの回帰式
# 合格率 = 1 / (1 + exp(-(-4.357587 + 1.051109 * gra)))

Test1227$AProb <- predict(logit02, newdata=Test1227, type="response")

head(Test1227)

exp(logit02$coefficients[2])
#      gpa 
# 2.860821 
# goa が１点あがったら、2.860821オッズ比あがる。

plot(Test1227$gpa, Test1227$admit, xlim=c(2,4), ylim=c(0,1))
par(new=T)
plot(Test1227$gpa, Test1227$AProb, 
     pch=20, col="green", xlab="", ylab="",
     xlim=c(2,4), ylim=c(0,1))

# 緑のラインがモデルの回帰式を図示したもの

prflgpa$LOdds <- log(prflgpa$AdmtPrcnt / (100 - prflgpa$AdmtPrcnt))

gpaMedian <- aggregate(Test1227$gpa, list(Test1227$GPACut), median)

prflgpa2 <- cbind(prflgpa, gpaMedian)

lmOdds <- lm(prflgpa$LOdds ~ prflgpa2$x, data=prflgpa2)

plot(prflgpa2$x, prflgpa2$LOdds)
abline(lmOdds, lwd=1, lty=1, col="red")

# 線形性をチェック
# 赤ラインから離れているので、Logitにgpaは含めるべきでないと判断


#logit gre : 0.003582
#logit gpa : 1.051109


logit03 <- glm(admit ~ gpa + gre, data=Test1227, family="binomial")

logit03$coefficients

# モデルの回帰式
# 合格率 = 1 / (1 + exp(-(-4.949378063 + 0.754686856 * gra + 0.002690684 * gre)))

#logit01 gre : 0.003582
#logit02                    gpa : 1.051109
#logit03 gre : 0.002690684  gpa : 0.754686856

# logit01と02の、それぞれのβと、03のβの大きさ関係が大きく変わらないから、03はOK




