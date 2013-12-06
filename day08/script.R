setwd("~/dev/feg/day08")

?cars
data(cars)
View(cars)
str(cars)
summary(cars)
plot(cars$speed, cars$dist)
cor(cars$speed, cars$dist)

lm01 <- lm(dist ~ speed, data = cars)
summary(lm01)

plot(cars$speed, cars$dist)
abline(lm01, #線形回帰モデル
       lwd = 1, #線の太さの指定（小さいほど細い）
       lty = 1, #線のタイプの指定（実線）
       col = "red" #線の色
       )

#残差の確認
lm01$residuals
summary(lm01$residuals)

#モデル診断
par(mfrow=c(2,2))
plot(lm01)


#左上のみ出力
#残差と予測値（Fitted Values）のプロット。残さの全体像を把握するのに使う。
plot(lm01, which=1)

#指定値（モデルの予測値）
pred <- predict(lm01)

#残差
resid <- lm01$residuals

#実績値と推定値と残さをデータフレームcars2にする
cars2 <- cbind(cars, pred, resid)

#はずれ値の出力
print(cars[c(23,35,49), ])
subset(cars2, speed==14)

#残さが正規分布に従うと過程するので、その検証。
plot(lm01, which=2)

#leverage value
hat(cars)

cars3 <- cbind(cars2, hat(cars))
summary(cars3)
print(cars3[c(23, 35, 49), ])
subset(cars3, speed==14)
#0.06平均的なhat値の値。これより大きいとはずれ値。


lm01$coefficients

#係数の算出
Sxx <- sum((cars$speed - mean(cars$speed))^2)
# 説明変数とその平均からの差の平方和

xx <- cars$speed - mean(cars$speed)
xy <- cars$dist - mean(cars$dist)
Sxy <- sum(xx * xy)
#説明変数とその平均値からの差と目的変数とその平均値からの差の積和

beta <- Sxy / Sxx
alpha <- mean(cars$dist) - beta * mean(cars$speed)
#もしbeta=0ならば、alphaはyの平均値に一致する

#betaのstd. Error
((sum((cars$dist - lm01$fitted.values) ** 2) / (nrow(cars) - 2)) ** 0.5) / (Sxx ** 0.5 )
# [1] 0.4155128

# t value
beta / (((sum((cars$dist - lm01$fitted.values) ** 2) / (nrow(cars) - 2)) ** 0.5) / (Sxx ** 0.5))
# [1] 9.46399

# ** は累乗

# このt値の分布の位置を確認
x <- seq(-10,10,0.01)
plot(x, dt(x, df=48), type="1", ylab="density")
#追いつかず・・・・


par(mfrow=c(1,1))


kakigori <- read.csv("ice.csv")
str(kakigori)
summary(kakigori)
plot(kakigori$temperature, kakigori$ice)
cor(kakigori$temperature, kakigori$ice)
lmIce <- lm(ice ~ temperature, data = kakigori)
summary(lmIce)

plot(kakigori$temperature, kakigori$ice)
abline(lmIce, #線形回帰モデル
       lwd = 1, #線の太さの指定（小さいほど細い）
       lty = 1, #線のタイプの指定（実線）
       col = "red" #線の色
)

plot(lmIce, which = 1)
plot(lmIce, which = 2)

# 気温と電力需要で分析してみる

jma <- read.csv("jma.csv")
tepco <- read.csv("tepco.csv")

head(jma)
head(tepco)

#denki <- cbind(jma$avgtmp, tepco$sum)
denki <- data.frame(tmp=abs(jma$avgtmp - mean(jma$avgtmp)), denki=tepco$sum)
head(denki)
lmDenki <- lm(denki~tmp, data=denki)

plot(denki$tmp, denki$denki)
abline(lmDenki, #線形回帰モデル
       lwd = 1, #線の太さの指定（小さいほど細い）
       lty = 1, #線のタイプの指定（実線）
       col = "red" #線の色
)

plot(lmDenki, which=1)
plot(lmDenki, which=2)

