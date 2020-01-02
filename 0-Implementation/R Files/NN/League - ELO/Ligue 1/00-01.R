library(neuralnet)
library("caret")
library(corrplot)
library(C50)
library(dummies)
library(gmodels)
library(Metrics)
library(neuralnet)
library(plyr)
library(rpart)
library(tree)
library(e1071)
library(rpart.plot)
library(fastDummies)
################################## Load Files #############################################
x <-
  read.csv(
    "C:\\Users\\User\\Documents\\Thesis\\Data\\Models\\LeagueELO\\Ligue 1\\ELO00-01.csv",
    stringsAsFactors = FALSE
  )
################################# Clean Data ##############################################
x$GBH   <- as.numeric(x$GBH)
x$GBD   <- as.numeric(x$GBD)
x$GBA   <- as.numeric(x$GBA)
x$IWH   <- as.numeric(x$IWH)
x$IWD   <- as.numeric(x$IWD)
x$IWA   <- as.numeric(x$IWA)
x$LBH   <- as.numeric(x$LBH)
x$LBD   <- as.numeric(x$LBD)
x$LBA   <- as.numeric(x$LBA)
x$SBH   <- as.numeric(x$SBH)
x$SBD   <- as.numeric(x$SBD)
x$SBA   <- as.numeric(x$SBA)
x$WHH   <- as.numeric(x$WHH)
x$WHD   <- as.numeric(x$WHD)
x$WHA   <- as.numeric(x$WHA)
x <- na.exclude(x)
################################## Rename Columns #########################################
colnames(x)[1] <- "Season"
################################ Create Dummy Vars ########################################
x <- cbind.data.frame(x, dummy(x$Home))
x <- cbind.data.frame(x, dummy(x$Away))
########################### Remove Cols After Dummy Vars ##################################
x$Home <- NULL
x$Away <- NULL
x$Season <- NULL
x$date <- NULL
x$FTR <- as.factor(x$FTR)
##################################### All Bookies #########################################
NNM <- x

set.seed(123)


NNM.rows <- nrow(NNM)
NNM.sample <- sample(NNM.rows, NNM.rows * 0.6)

NN.train <- NNM[NNM.sample,]
NN.test <- NNM[-NNM.sample,]

NN = neuralnet(FTR ~ ., NN.train, hidden = c(10,5), linear.output = T,stepmax = 1e6)

plot(NN)

comp <- compute(NN, NN.test[-1])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('A', 'D', 'H')[idx]

CrossTable(
  idx,
  NN.test$FTR,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################
NNM2 <- x[-c(7:15)]

set.seed(123)


NNM2.rows <- nrow(NNM2)
NNM2.sample <- sample(NNM2.rows, NNM2.rows * 0.6)

NN2.train <- NNM2[NNM2.sample,]
NN2.test <- NNM2[NNM2.sample,]

NN2 = neuralnet(FTR ~ ., NN2.train, hidden = c(10,5), linear.output = T,stepmax = 1e6)

plot(NN2)

comp <- compute(NN2, NN2.test[-1])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('A', 'D', 'H')[idx]

CrossTable(
  idx,
  NN2.test$FTR,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################
NNM3 <- x[-c(4:6,10:15)]

set.seed(123)


NNM3.rows <- nrow(NNM3)
NNM3.sample <- sample(NNM3.rows, NNM3.rows * 0.6)

NN3.train <- NNM3[NNM3.sample,]
NN3.test <- NNM3[NNM3.sample,]

NN3 = neuralnet(FTR ~ ., NN3.train, hidden = 3, linear.output = T)

plot(NN3)

comp <- compute(NN3, NN3.test[-1])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('A', 'D', 'H')[idx]

CrossTable(
  idx,
  NN3.test$FTR,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################
NNM4 <- x[-c(4:9, 13:15)]

set.seed(123)


NNM4.rows <- nrow(NNM4)
NNM4.sample <- sample(NNM4.rows, NNM4.rows * 0.6)

NN4.train <- NNM4[NNM4.sample,]
NN4.test <- NNM4[NNM4.sample,]

NN4 = neuralnet(FTR ~ ., NN.train, hidden = 3, linear.output = T)

plot(NN4)

comp <- compute(NN4, NN4.test[-1])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('A', 'D', 'H')[idx]

CrossTable(
  idx,
  NN4.test$FTR,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################
NNM5 <- x[-c(4:12)]

set.seed(123)


NNM5.rows <- nrow(NNM5)
NNM5.sample <- sample(NNM5.rows, NNM5.rows * 0.6)

NN5.train <- NNM5[NNM5.sample,]
NN5.test <- NNM5[NNM5.sample,]

NN5 = neuralnet(FTR ~ ., NN5.train, hidden = 3, linear.output = T)

plot(NN5)

comp <- compute(NN5, NN5.test[-1])
pred.weights <- comp$net.result
idx <- apply(pred.weights, 1, which.max)
pred <- c('A', 'D', 'H')[idx]

CrossTable(
  idx,
  NN5.test$FTR,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################