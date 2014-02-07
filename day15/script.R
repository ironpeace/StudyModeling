
setwd("~/dev/feg/day15")

x <- seq(-5, 5, by=0.1)   #X軸の設定（-5から5まで0.1刻み）
p <- 1 / (1 + exp(-x))   #ロジスティック関数
plot(x, p)                #図示
summary(p)                #pの分布の確認


# Z = 0 + beta * X
# alpha = 0 と固定してbetaを0,1,2のように変化させる
plot(x, 1/(1 + exp(-(0 + 0 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col='red')
par(new = T)
plot(x, 1/(1 + exp(-(0 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="blue") 
par(new = T)
plot(x, 1/(1 + exp(-(0 + 2 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="green")
par(new = T)
plot(x, 1/(1 + exp(-(0 + 5 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="yellow")

# beta = 1 alphaを-1,0,1のように変化させる
plot(x, 1/(1 + exp(-(-1 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="blue")
par(new = T)
plot(x, 1/(1 + exp(-(0 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="green")
par(new = T)
plot(x, 1/(1 + exp(-(1 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="red")


plot(x, 1/(1 + exp(-(-5 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="blue")
par(new = T)
plot(x, 1/(1 + exp(-(0 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="green")
par(new = T)
plot(x, 1/(1 + exp(-(5 + 1 * x))), ylab="p", ylim=c(0,1), xlim=c(-5, 5), col="red")


iris2 <- iris[1:100, c(1,5)] #必要なデータだけ抜粋
iris2$target <- ifelse(iris2$Species == "versicolor", 1, 0) #Speciesを0/1に変換
iris.lm <- lm(target ~ Sepal.Length, data=iris2) #線形回帰モデルの推定
plot(iris2$Sepal.Length, iris2$target) #散布図
abline(iris.lm, lwd=1, lty=1, col="red") #散布図に推定結果を上書き
#推定値の計算例
-2.6203 + 0.5703 * 6.5
# 1.08665 のような100％超えが出てしまう
# 線形回帰モデルは確率を推定するのは不便


#ロジスティック回帰モデルの推定
logit01 <- glm(target ~ Sepal.Length, data=iris2, family="binomial")

#推定結果に基づき、推定確率を返す
1 / (1 + exp(- (-27.831 + 5.140 * iris2$Sepal.Length)))

#散布図
plot(iris2$Sepal.Length, iris2$target, xlim=c(4,7), ylim=c(0,1), ylab="Probability", xlab="Sepal.Length")

#散布図に推定結果を上書き
par(new=T)
curve(1 / (1 + exp(-(-27.831 + 5.140 * x))), col="blue", xlim=c(4,7), ylim=c(0,1), lwd=2, lty=3, ylab="Probability", xlab="Sepal.Length")

# -> alpha:-27.831, belta:5.140 という値は、logit01の結果から取得している

# x=6.5の時、y=0.966 と、100％を超えない
# 確率を推定する場合はロジスティック回帰モデルの方が適切

