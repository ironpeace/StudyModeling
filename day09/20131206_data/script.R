#前回の復習
#線形回帰モデルを走らせる
setwd("~/dev/feg/day09/20131206_data")
tea <- read.csv(file = "tea.csv")

View(tea)
str(tea)
summary(tea)
plot(tea$temperature, tea$tea)
cor(tea$temperature, tea$tea)


lm01 <- lm(tea ~ temperature, data = tea)
summary(lm01)

plot(tea$temperature, tea$tea)
abline(lm01, lwd = 1, lty = 1, col = "red")

plot(lm01, which=1)

print(cbind(tea, hat(tea))[c(2,4,5), ])

plot(lm01, which=2)

print(cbind(tea, hat(tea))[c(2,5,8), ])

pred <- predict(lm01, interval="conf", level=0.95)

plot(tea$temperature, tea$tea, xlim=c(0,30), ylim=c(0,100))

par(new=T)
plot(tea$temperature, pred[,1], type="l", col="red", xlim=c(0,30), ylim=c(0,100), xlab="", ylab="")

par(new=T)
plot(tea$temperature, pred[,2], type="p", col="blue", pch=3, xlim=c(0,30), ylim=c(0,100),xlab="",ylab="")

par(new=T)
plot(tea$temperature, pred[,3], type="p", col="blue", pch=3, xlim=c(0,30), ylim=c(0,100),xlab="",ylab="")

test <- data.frame(temperature=10)
predict(lm01, newdata=test, interval="conf", level=0.95)


?cars
data(cars)
lmCar <- lm(dist ~ speed, data = cars)

testCar <- data.frame(speed = c(2, 15.4, 30, 50))
res <- 
  as.data.frame(
    predict(lmCar, newdata=testCar, interval="conf", level=0.95)
  )

res$dif <- res$upr - res$lwr
res
# fit       lwr        upr       dif
# 1  -9.714277 -21.73307   2.304513 24.037582
# 2  42.980000  38.60687  47.353135  8.746269
# 3 100.393168  87.43543 113.350908 25.915481
# 4 179.041343 149.80604 208.276646 58.470606

#diffの値が異なる。一番幅（diff）が小さいのが#2で、一番平均に近いやつ。
#平均に近いほどあたりやすい。平均がおおいほどあたりにくい。



a01 <- read.csv("Anscombe.csv")
mean(a01$x1)
mean(a01$x2)
mean(a01$x3)
mean(a01$x4)

mean(a01$y1)
mean(a01$y2)
mean(a01$y3)
mean(a01$y4)

cor(a01$x1, a01$y1)
cor(a01$x2, a01$y2)
cor(a01$x3, a01$y3)
cor(a01$x4, a01$y4)

lmA01 <- lm(a01$y1 ~ a01$x1, data=a01)
lmA02 <- lm(a01$y2 ~ a01$x2, data=a01)
lmA03 <- lm(a01$y3 ~ a01$x3, data=a01)
lmA04 <- lm(a01$y4 ~ a01$x4, data=a01)

lmA01$coefficients
lmA02$coefficients
lmA03$coefficients
lmA04$coefficients

par(mfrow=c(2,2))

plot(a01$x1, a01$y1)
abline(lmA01, lwd=1, lty=1, col="red")

plot(a01$x2, a01$y2)
abline(lmA02, lwd=1, lty=1, col="red")

plot(a01$x3, a01$y3)
abline(lmA03, lwd=1, lty=1, col="red")

plot(a01$x4, a01$y4)
abline(lmA04, lwd=1, lty=1, col="red")


print(a01[, c("x3","y3")])
a01[3, c("x3", "y3")]
#はずれ値を除く
b01 <- a01[-3,]
b01

lmB03 <- lm(b01$y3 ~ b01$x3, data=b01)
summary(lmB03)
lmB03$coefficients

plot(a01$x3, a01$y3, ylim=c(5,13))
abline(lmA03, lwd=1, lty=1, col="blue")
abline(lmB03, lwd=1, lty=1, col="red")
#はずれ値を外す前（青）と、外した後（赤）の比較










