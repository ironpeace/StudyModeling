setwd("~/dev/feg/day02/")
bank.fulldata <- read.csv("bank-full.csv", sep=";")

hist(bank.fulldata$age)
hist(bank.fulldata$balance) #年収

hist(bank.fulldata$balance, breaks=c(-10000,0,300,1000,110000))
hist(log(bank.fulldata$balance, base=10))

balance_cut <- cut(bank.fulldata$balance, breaks=5)
barplot(summary(balance_cut))

balance_cut <- cut(bank.fulldata$balance, breaks=c(-Inf,0,100,300,1000,3000,10000,Inf))
barplot(summary(balance_cut))

balance_cut <- cut(bank.fulldata$balance, breaks=c(-Inf,0,100,300,1000,3000,10000,Inf), right=FALSE)
barplot(summary(balance_cut))

balance_cut <- cut(bank.fulldata$balance, breaks=c(-Inf,0,100,300,1000,3000,10000,Inf), right=FALSE, labels=c("0未満","0以上100未満","100以上300未満","300以上1000未満","1000以上3000未満","3000以上10000未満","10000以上"))
barplot(summary(balance_cut))

bank.work1 <- bank.fulldata

bank.work1$age <- cut(bank.fulldata$age, 
                      breaks=c(10,20,30,40,50,60,70,Inf), 
                      right=FALSE, 
                      labels=c("10s","20s","30s","40s","50s","60s","Old"))

barplot(summary(bank.work1$age))

bank.work1$balance <- cut(bank.fulldata$balance, 
                          breaks=c(-Inf,0,100,300,1000,3000,10000,Inf), 
                          right=FALSE, 
                          labels=c("0未満","0以上100未満","100以上300未満","300以上1000未満","1000以上3000未満","3000以上10000未満","10000以上")) 
                      
barplot(summary(bank.work1$balance))

bank.work1$duration <- cut(bank.fulldata$duration, 
                          breaks=c(-Inf,0,100,200,300,400,500,1000,3000,10000,Inf), 
                          right=FALSE, 
                          labels=c("-0","0-100","100-200","200-300","300-400","400-500","500-1000","1000-3000","3000-10000","10000-")) 

barplot(summary(bank.work1$duration))

bank.work1$campaign <- cut(bank.fulldata$campaign, 
                           breaks=c(0,1,2,3,4,5,6,7,8,9,10,Inf), 
                           right=FALSE, 
                           labels=c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-")) 

barplot(summary(bank.work1$campaign))

bank.work1$pdays <- cut(bank.fulldata$pdays, 
                          breaks=c(-1,0,Inf), 
                          right=FALSE, 
                          labels=c("-1","0-")) 

# tableでとった２行目以降をcutすればいいかも
#~~~~~~

bank.work1$pdaysOver0 <- cut(table(bank.fulldata$pdays)[1:559], 
                             break=c(0,50,100,150,200,250,300,350,400,Inf),
                             right=FALSE)

#~~~~~~

bank.work1$previous <- cut(bank.fulldata$previous, 
                           breaks=c(0,1,5,Inf), 
                           right=FALSE, 
                           labels=c("0-1","1-5","5-")) 



barplot(summary(bank.work1$previous))


# 39752行までがトレーニングデータ
bank.bunseki <- bank.fulldata[1:39752,]
bank.work2 <- bank.work1[1:39752,]

#test data
bank.test <- bank.fulldata[39753:45211,]
bank.work9 <- bank.work1[39753:45211,]

bank.test$y <- NULL
bank.work9$y <- NULL

table(bank.work2$balance, bank.work2$y)
mosaicplot(table(bank.work2$balance, bank.work2$y))

tab.bal <- table(bank.work2$balance, bank.work2$y)
write.csv(tab.bal, "bal.csv")

tab.age <- table(bank.work2$age, bank.work2$y)
write.csv(tab.age, "age.csv")

tab.day <- table(bank.work2$day, bank.work2$y)
write.csv(tab.day, "day.csv")

tab.duration <- table(bank.work2$duration, bank.work2$y)
write.csv(tab.duration, "duration.csv")

tab.campaign <- table(bank.work2$campaign, bank.work2$y)
write.csv(tab.campaign, "campaign.csv")

tab.previous <- table(bank.work2$previous, bank.work2$y)
write.csv(tab.previous, "previous.csv")

tab.job <- table(bank.work2$job, bank.work2$y)
write.csv(tab.job, "job.csv")


