setwd("~/dev/feg/day13")

data(iris)
str(iris)

plot(iris$Sepal.Length, iris$Sepal.Width)

iris.mat1 <- as.matrix(iris[, 1:2]) # 左ふたつの変数を使う
iris.km1 <- kmeans(iris.mat1, 3)    # 3個に分ける
iris.km4 <- kmeans(iris.mat1, 4)    # 4個に分ける

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris.km1$cluster)
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris.km4$cluster)

# クラスタ番号
# Clustering vector:
#  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [49] 2 2 1 1 1 3 1 3 1 3 1 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 1 1 1 1 3 3 3 3 3 3 3 3 1 3 3 3 3 3 3 3 3 3
# [97] 3 3 3 3 1 3 1 1 1 1 3 1 1 1 1 1 1 3 3 1 1 1 1 3 1 3 1 3 1 1 3 3 1 1 1 1 1 3 3 1 1 1 3 1 1 1 3 1
# [145] 1 1 3 1 1 3

# クラスタの中心となる３つの点
# Cluster means:
#   Sepal.Length Sepal.Width
# 1     6.812766    3.074468
# 2     5.006000    3.428000
# 3     5.773585    2.692453

plot()

par(mfrow=c(1,2))
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris.km1$cluster, main="Clustering")
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, main="Answer")
par(mfrow=c(1,1))

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris.km1$cluster, main="Clustering")
points(iris.km1$centers, col=1:3, pch="+")

# ~~~~~~~~
jma <- read.csv("jma.csv")
tepco <- read.csv("tepco.csv")
jmatepco <- data.frame(temp=jma$avgtmp, tepco=tepco$sum / 2000)
str(jmatepco)
plot(jmatepco$tepco, jmatepco$temp)
jmatepco.mt <- as.matrix(jmatepco)
jmatepco.km <- kmeans(jmatepco.mt, 3)
plot(jmatepco$tepco, jmatepco$temp, col=jmatepco.km$cluster)
# 冬・秋春・夏の分割っぽくできたかも
# ~~~~~~~~

names(iris)
iris.mat2 <- as.matrix(iris[, c(1,3)])
iris.km2 <- kmeans(iris.mat2, 3)    # 3個に分ける
plot(iris$Sepal.Length, iris$Petal.Length, col=iris.km2$cluster, main="Clustering")
plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species, main="Answer")

iris.mat3 <- as.matrix(iris[, c(2,4)])
iris.km3 <- kmeans(iris.mat3, 3)    # 3個に分ける
par(mfrow=c(1,2))
plot(iris$Sepal.Width, iris$Petal.Width, col=iris.km3$cluster, main="Clustering")
plot(iris$Sepal.Width, iris$Petal.Width, col=iris$Species, main="Answer")
par(mfrow=c(1,1))

# 階層的クラスタリング
iris.dist <- dist(iris[,1:4])
iris.hc <- hclust(iris.dist)
plot(iris.hc)
# 隣にいるノードが近い。
# 一番近いノードから足していっている

library(car)
data(mtcars)
mtcars.d <- dist(mtcars)
mtcars.h <- hclust(mtcars.d)
plot(mtcars.h)
