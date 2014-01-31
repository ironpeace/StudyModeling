setwd("~/dev/feg/review")

# 以下のサイトからデータ取得
# http://archive.ics.uci.edu/ml/datasets/Bank+Marketing

# bankディレクトリ配下に格納
# -rw-r--r--@ 1 ITPUser  staff  4610348  2 14  2012 bank-full.csv
# -rw-r--r--@ 1 ITPUser  staff     3864  2 15  2012 bank-names.txt
# -rw-r--r--@ 1 ITPUser  staff   461474  2 14  2012 bank.csv
# -rw-r--r--@ 1 ITPUser  staff   579043  1  6 10:37 bank.zip

# 演習シナリオ
# A銀行では定期預金口座開設の電話営業をしている
# 電話をするには、１件あたりX円のコストがかかるが、
# 口座を解説してくれれば、１件あたり概算でY円の利益がある。
# 対象が数千人から数万人いるので、その中から電話をかけるべき人の
# リストを作成して、収益を最大化する

bank.sampledata <- read.csv("bank/bank.csv")

# １行目の確認
bank.sampledata[1,]

# カンマではなく、セミコロンで区切られているようなので、修正する
rm(bank.sampledata)
bank.sampledata <- read.csv("bank/bank.csv", sep=";")

# １行目の確認
bank.sampledata[1,]

bank.fulldata <- read.csv("bank/bank-full.csv", sep=";")

# 項目の確認
str(bank.fulldata)
names(bank.fulldata)
bank.fulldata[1:10, ] # 1行目から10行目までを参照

# Input variables:
# bank client data:
# 1 - age (numeric)
# 2 - job : type of job
#    (categorical:  'admin.','unknown','unemployed','management','housemaid',
#                   'entrepreneur','student','blue-collar','self-employed',
#                   'retired','technician','services') 
# 3 - marital : marital status 
#    (categorical: 'married','divorced','single'; note: 'divorced' means divorced or widowed)
# 4 - education (categorical: 'unknown','secondary','primary','tertiary')
# 5 - default: has credit in default? (binary: 'yes','no')
# 6 - balance: average yearly balance, in euros (numeric) 
# 7 - housing: has housing loan? (binary: 'yes','no')
# 8 - loan: has personal loan? (binary: 'yes','no')
#
# related with the last contact of the current campaign:
# 9 - contact: contact communication type 
#    (categorical: 'unknown','telephone','cellular') 
# 10 - day: last contact day of the month (numeric)
# 11 - month: last contact month of year
#    (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
# 12 - duration: last contact duration, in seconds (numeric)
# other attributes:
# 13 - campaign: number of contacts performed during this campaign and for this client 
#    (numeric, includes last contact)
# 14 - pdays: number of days that passed by after the client was last contacted from a previous campaign 
#    (numeric, -1 means client was not previously contacted)
# 15 - previous: number of contacts performed before this campaign and for this client (numeric)
# 16 - poutcome: outcome of the previous marketing campaign
#    (categorical: 'unknown','other','failure','success')
#
# Output variable (desired target):
#  17 - y - has the client subscribed a term deposit? (binary: 'yes','no')

# 年齢データの確認
bank.fulldata$age[1:20]
summary(bank.fulldata$age)
hist(bank.fulldata$age)
table(bank.fulldata$age)

# 綺麗なヒストグラム
hist(bank.fulldata$age, breaks=10)
hist(bank.fulldata$age, breaks=c(0,20,40,60,80,100))
hist(bank.fulldata$age, main="年齢の分布", xlab="年齢", ylab="件数")
hist(bank.fulldata$age, col="blue1")

# 職業データの確認
summary(bank.fulldata$job)
barplot(summary(bank.fulldata$job))
pie(summary(bank.fulldata$job))

# 口座残高データの確認
hist(bank.fulldata$balance)
# -> 偏りが大きすぎる。
hist(bank.fulldata$balance, breaks=c(-10000, 0, 100, 300, 1000, 110000))
# -> 効果無し
hist(log(bank.fulldata$balance, base=10))
# -> 綺麗に出るけど、残高マイナスのlogがとれない
# -> 負数のlogはとれないので無視され、警告が出る。

# 手動でカテゴリ変数に変換する

balance_cut <- cut(bank.fulldata$balance, 
                   breaks=c(-Inf, 0, 100, 300, 1000, 3000, 10000, Inf))
barplot(summary(balance_cut))
# -> Infは無限大の意味。両端の値を大きくして欠損を出さないようにしている。

balance_cut <- cut(bank.fulldata$balance, 
                   breaks=c(-Inf, 0, 100, 300, 1000, 3000, 10000, Inf), 
                   right=FALSE)
barplot(summary(balance_cut))
# -> オプションを追加して、cut区間を「x超y以下」の表現から、「x以上y未満」に変更

balance_cut <- cut(bank.fulldata$balance, 
                   breaks=c(-Inf, 0, 100, 300, 1000, 3000, 10000, Inf), 
                   right=FALSE,
                   labels=c("<0","0<100","100<300","300<1000","1000<3000","3000<10000","10000=<"))
barplot(summary(balance_cut))
# -> さらに表現を親切に

# bank.fulldataの数値を全てカテゴリ化したbank.work1というデータフレームを作成
bank.work1 <- bank.fulldata

bank.work1$age <- cut(bank.fulldata$age, 
                      breaks=c(10,20,30,40,50,60,70,Inf), 
                      right=FALSE, 
                      labels=c("10s","20s","30s","40s","50s","60s","Old"))

bank.work1$balance <- cut(bank.fulldata$balance, 
                          breaks=c(-Inf,0,100,300,1000,3000,10000,Inf), 
                          right=FALSE, 
                          labels=c("-0","0-100","100-300","300-1000"
                                   ,"1000-3000","3000-10000","10000-")) 

bank.work1$duration <- cut(bank.fulldata$duration, 
                           breaks=c(-Inf,0,100,200,300,400,500,1000,3000,10000,Inf), 
                           right=FALSE, 
                           labels=c("-0","0-100","100-200","200-300","300-400","400-500",
                                    "500-1000","1000-3000","3000-10000","10000-")) 

bank.work1$campaign <- cut(bank.fulldata$campaign, 
                           breaks=c(0,1,2,3,4,5,6,7,8,9,10,Inf), 
                           right=FALSE, 
                           labels=c("0-1","1-2","2-3","3-4","4-5","5-6",
                                    "6-7","7-8","8-9","9-10","10-")) 

bank.work1$pdays <- cut(bank.fulldata$pdays, 
                        breaks=c(-1,0,Inf), 
                        right=FALSE, 
                        labels=c("-1","0-")) 

bank.work1$previous <- cut(bank.fulldata$previous, 
                           breaks=c(0,1,5,Inf), 
                           right=FALSE, 
                           labels=c("0-1","1-5","5-")) 

# 目的変数yと説明変数（その他の変数）の関連を探る

# 分析用データ抽出
bank.bunseki <- bank.fulldata[1:39752, ]
bank.work2 <- bank.work1[1:39752, ]

# 予測用将来データ
bank.test <- bank.fulldata[39752:45211, ] 
bank.work9 <- bank.work1[39752:45211, ]

# データフレームから列ごと削除
bank.test$y <- NULL 
bank.work9$y <- NULL


createCrossChart <- function(var, x, bar_y, plot_y, ttl){
  table <- table(var, bank.work2$y)
  c <- barplot(summary(var), xlim=x, ylim=bar_y, axes=FALSE, ylab="", col=4)
  axis(2)
  par(new = T)
  plot(c, prop.table(table, 1)[, 2], type="o", lwd=3, axes=FALSE, 
       xlim=x, ylim=plot_y, xlab="", ylab="", col=2)
  axis(4)
  box()
  title(ttl)
}

createCrossChart(var=bank.work2$balance, x=c(0, 9), bar_y=c(0,15000), plot_y=c(0, 0.2), ttl="balance")
createCrossChart(var=bank.work2$age, x=c(0, 8), bar_y=c(0,20000), plot_y=c(0, 0.6), ttl="age")
createCrossChart(var=bank.work2$duration, x=c(0, 12), bar_y=c(0,15000), plot_y=c(0, 0.7), ttl="duration")
createCrossChart(var=bank.work2$campaign, x=c(0, 13), bar_y=c(0,17000), plot_y=c(0, 0.1), ttl="campaign")
createCrossChart(var=bank.work2$pdays, x=c(0, 3), bar_y=c(0,40000), plot_y=c(0, 0.1), ttl="pdays")
createCrossChart(var=bank.work2$previous, x=c(0, 4), bar_y=c(0,40000), plot_y=c(0, 0.1), ttl="previous")

# ランダムサンプリング
39752 * 0.7
# -> 27826.4
set.seed(12345)
trainnum <- sample(1:39752, 27826)
bank.train <- bank.bunseki[trainnum, ]
validnum <- setdiff(1:39752, trainnum)
bank.valid <- bank.bunseki[validnum, ]
