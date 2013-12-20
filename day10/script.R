setwd("~/dev/feg/day10")

library(car)
?mtcars

data(mtcars)
head(mtcars)
str(mtcars)
summary(mtcars)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)
plot(mtcars)
plot(mtcars[1:5])
cor(mtcars$mpg, mtcars$disp)
cor(mtcars$mpg, mtcars$wt)
cor(mtcars[1:5])

lmDisp <- lm(mpg ~ disp, data = mtcars)
lmWt <- lm(mpg ~ wt, data = mtcars)

plot(mtcars$disp, mtcars$mpg)
abline(lmDisp, lwd = 1, lty = 1, col = "red")

plot(mtcars$wt, mtcars$mpg)
abline(lmWt, lwd = 1, lty = 1, col = "red")

lmDisp$residuals
summary(lmDisp$residuals)
plot(lmDisp, which=1)
plot(lmDisp, which=2)

lmWt$residuals
summary(lmWt$residuals)
plot(lmWt, which=1)
plot(lmWt, which=2)

predDisp = predict(lmDisp, interval="pred", lavel=0.95)
predWt = predict(lmWt, interval="pred", lavel=0.95)

predDisp2 = predict(lmDisp, interval="conf", lavel=0.95)
predWt2 = predict(lmWt, interval="conf", lavel=0.95)

plot(mtcars$disp, mtcars$mpg)
abline(lmDisp, lwd = 1, lty = 1, col = "red")
par(new = T)
plot(mtcars$disp, predDisp[,2], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="green")
par(new = T)
plot(mtcars$disp, predDisp[,3], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="green")
par(new = T)
plot(mtcars$disp, predDisp2[,2], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="blue")
par(new = T)
plot(mtcars$disp, predDisp2[,3], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="blue")


plot(mtcars$wt, mtcars$mpg)
abline(lmWt, lwd = 1, lty = 1, col = "red")
par(new = T)
plot(mtcars$wt, predWt[,2], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="green")
par(new = T)
plot(mtcars$wt, predWt[,3], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="green")
par(new = T)
plot(mtcars$wt, predWt2[,2], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="blue")
par(new = T)
plot(mtcars$wt, predWt2[,3], type="p", pch="x",
     xlimc=c(1,6),ylim=c(10, 35), xlab="", ylab="",col="blue")

#こっから重回帰

?Duncan
data(Duncan)
str(Duncan)
summary(Duncan)
plot(Duncan)

Duncan2 <- transform(Duncan, type2=ifelse(type=="bc",0,1))
table(Duncan2$type2, Duncan2$type)

lmBc <- lm(income ~ type2, data=Duncan2)
summary(lmBc)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   23.762      3.847   6.177 2.02e-07 ***
#   type2         33.946      5.268   6.444 8.26e-08 ***
# ブルーカラーでなければIncomeが33.946高まる

#1,0を入れ替える
Duncan2 <- transform(Duncan, type2=ifelse(type=="bc",1,0))
table(Duncan2$type2, Duncan2$type)

lmBc <- lm(income ~ type2, data=Duncan2)
summary(lmBc)


Duncan2 <- transform(Duncan, typebc=ifelse(type=="bc",1,0))
table(Duncan2$typebc, Duncan2$type)
Duncan2 <- transform(Duncan2, typewc=ifelse(type=="wc",1,0))
table(Duncan2$typewc, Duncan2$type)
Duncan2 <- transform(Duncan2, typeprof=ifelse(type=="prof",1,0))
table(Duncan2$typeprof, Duncan2$type)


mlm03 <- lm(income ~ education + type2, data=Duncan2)
summary(mlm03)

s_lm_edu        <- lm(income ~ education, data=Duncan2)
s_lm_pre        <- lm(income ~ prestige, data=Duncan2)
d_lm_wc_prf     <- lm(income ~ typewc+typeprof, data=Duncan2)
d_lm_type       <- lm(income ~ type, data=Duncan2)
d_lm_wc_prf_bc  <- lm(income ~ typewc+typeprof+typebc, data=Duncan2)
m_lm_edu_pre    <- lm(income ~ education+prestige, data=Duncan2)
m_lm_prf_type   <- lm(income ~ education+prestige+type, data=Duncan2)

summary(s_lm_edu)
# edu : 0.5949

summary(s_lm_pre)
# pre : 0.64968

summary(d_lm_wc_prf)
# wc : 26.905
# prf : 36.294

summary(d_lm_type)
# prf : 36.294
# wc : 26.905
# カテゴリ変数を勝手にダミー変数に変換してやってくれる

summary(d_lm_wc_prf_bc)
# wc : 26.905
# prf : 36.294
# bc : NA

summary(m_lm_edu_pre)
# edu : 0.03226
# pre : 0.62372

summary(m_lm_prf_type)
# edu : -0.1321
# pre : 0.8834
# prof : -7.2634
# wc : 19.3997


binary = read.csv("feg/binary.csv")
score = read.csv("feg/score.csv")

head(binary)
head(score)

str(binary)
str(score)

summary(binary)
summary(score)

plot(score)
cor(score)

plot(score$Score1, score$Score2)
abline(lm(Score2 ~ Score1, data=score), lwd = 1, lty = 1, col = "red")

plot(score$Score2, score$Score3)
abline(lm(Score3 ~ Score2, data=score), lwd = 1, lty = 1, col = "red")

plot(score$Score1, score$Score3)
abline(lm(Score3 ~ Score1, data=score), lwd = 1, lty = 1, col = "red")

lm312 <- lm(Score3 ~ Score1 + Score2, data=score)
summary(lm312)

