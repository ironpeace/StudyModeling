print("Hello R World!")

hello <- function(){
  print("Hello R World!")
}

hello()

rm(hello)

setwd("~/dev/feg/day01/")

source("hello.R")

hello()

bank.sampledata <- read.csv("bank/bank.csv")

bank.sampledata[1,]

head(bank.sampledata)

rm(bank.sampledata)

bank.sampledata <- read.csv("bank/bank.csv", sep=";")

bank.sampledata[1,]

View(bank.sampledata)

head(bank.sampledata)

tail(bank.sampledata)

bank.fulldata <- read.csv("bank/bank-full.csv", sep=";")

head(bank.fulldata)

str(bank.fulldata) # structure

names(bank.fulldata)

bank.fulldata[1:10, ] # row 1~10 and all cols

bank.fulldata[1:10, 1:5] # row 1~10 and col 1~5

bank.fulldata$age[1:20]

bank.fulldata[["age"]][1:20]

bank.fulldata[[{1}]][1:20]

summary(bank.fulldata$age)

var(bank.fulldata$age)

sd(bank.fulldata$age)

hist(bank.fulldata$age)

table(bank.fulldata$age)

write.csv(table(bank.fulldata$age), "temp.csv")

hist(bank.fulldata$age, breaks=10)

hist(bank.fulldata$age, breaks=c(0,20,40,60,80,100))

hist(bank.fulldata$age, breaks=30)

hist(bank.fulldata$age, main="Age Range", xlab="Age", ylab="Count")

hist(bank.fulldata$age, col="blue1")

summary(bank.fulldata$job)

table(bank.fulldata$job)

barplot(summary(bank.fulldata$job))

pie(summary(bank.fulldata$job))

myloop <- function(){
  for(name in names(bank.fulldata)){
    #len <- length(bank.fulldata[[name]])
    #print(paste(name, len))
    write.csv(table(bank.fulldata[[name]]), paste("tableout/", name, ".csv", sep=""))
  }
}

myloop()

hist(bank.fulldata$balance, breaks=50)

