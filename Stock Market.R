library(class)
library(forecast)
library(dummies)
library(ggplot2)
library(caret)
stocks <- read.csv('ADANIPORTS.csv', header = TRUE)
stocks <- stocks[,-1:-3]
head(stocks)
View(stocks)
memory.limit(24000)
str(stocks)

stocks.subset <- stocks[c('Prev.Close', 'Open', 'High','Low','Last','Close','Volume','Increase')]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

stocks.subset.n <- as.data.frame(lapply(stocks.subset[,2:8],normalize))

View(stocks.subset.n)
summary(stocks.subset.n)
head(stocks.subset.n)

set.seed(125)

dat.d <- sample(1:nrow(stocks.subset.n), size=nrow(stocks.subset.n)*0.7,replace = FALSE)

train.stocks <-  stocks.subset.n[dat.d, ]
test.stocks  <-  stocks.subset.n[-dat.d, ]

train.stocks_labels <- stocks.subset.n[dat.d,1]
test.stocks_labels <- stocks.subset.n[-dat.d,1]

NROW(train.stocks_labels)

library(class)

knn.47 <- knn(train=train.stocks , test= test.stocks , cl=train.stocks_labels, k=47)

knn.48 <- knn(train=train.stocks , test= test.stocks , cl=train.stocks_labels, k=48)

ACC.47 <- 100 * sum(test.stocks_labels == knn.47) / NROW(test.stocks_labels)

ACC.48 <- 100 * sum(test.stocks_labels == knn.48) / NROW(test.stocks_labels)

ACC.47


i=1
k.optm=1
for(i in 1:48) {
  knn.mod <-knn (train=train.stocks, test=test.stocks, cl=train.stocks_labels, k=i)
 k.optm[i] <- 100 * sum (train.stocks_labels == knn.mod) / NROW(test.stocks_labels)
 k=i
 cat(k, '=',k.optm[i],'\n')
}

plot(k.optm, type='b', xlab = "K-VALUE", ylab = "Accuracy level")