library("caret")
library(corrplot)
library(C50)
library(dummies)
library(gmodels)
library(Metrics)
library(neuralnet)
library(plDTr)
library(rpart)
library(tree)
library(e1071)
library(rpart.plot)
library(fastDummies)
################################## Load Files #############################################
x <-
  read.csv(
    "C:\\Users\\User\\Documents\\Thesis\\Data\\Models\\League\\BPL\\BPL00-01.csv",
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
################################ Create DummDT Vars ########################################
x <- cbind.data.frame(x, dummy(x$Home))
x <- cbind.data.frame(x, dummy(x$Away))
########################### Remove Cols After DummDT Vars ##################################
x$Home <- NULL
x$Away <- NULL
x$Season <- NULL
x$date <- NULL
##################################### All Bookies #########################################
DT1 <- x

set.seed(123)
DT1$FTR <- as.factor(DT1$FTR)

DT1.rows <- nrow(DT1)
DT1.sample <- sample(DT1.rows, DT1.rows * 0.6)

DT1.train <- DT1[DT1.sample, ]
DT1.test <- DT1[-DT1.sample, ]

DT1.model <- C5.0(DT1.train[, -1], DT1.train$FTR, trails = 100)

plot(DT1.model)
summary(DT1.model)

DT1.predict <- predict (DT1.model, DT1.test[, -1])
CrossTable(
  DT1.test$FTR,
  DT1.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)

##################################### Bet 365 OnlDT ########################################
DT2 <- x[-c(5:16)]

set.seed(123)
DT2$FTR <- as.factor(DT2$FTR)

DT2.rows <- nrow(DT2)
DT2.sample <- sample(DT2.rows, DT2.rows * 0.6)

DT2.train <- DT2[DT2.sample, ]
DT2.test <- DT2[-DT2.sample, ]

DT2.model <- C5.0(DT2.train[, -1], DT2.train$FTR, trails = 100)

plot(DT2.model)
summary(DT2.model)

DT2.predict <- predict (DT2.model, DT2.test[, -1])
CrossTable(
  DT2.test$FTR,
  DT2.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################################
DT3 <- x[-c(2:4, 8:16)]

set.seed(123)
DT3$FTR <- as.factor(DT3$FTR)

DT3.rows <- nrow(DT3)
DT3.sample <- sample(DT3.rows, DT3.rows * 0.6)

DT3.train <- DT3[DT3.sample, ]
DT3.test <- DT3[-DT3.sample, ]

DT3.model <- C5.0(DT3.train[, -1], DT3.train$FTR, trails = 100)


plot(DT3.model)
summary(DT3.model)

DT3.predict <- predict (DT3.model, DT3.test[, -1])
CrossTable(
  DT3.test$FTR,
  DT3.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###########################################################################################################
DT4 <- x[-c(2:7, 11:16)]

set.seed(123)
DT4$FTR <- as.factor(DT4$FTR)

DT4.rows <- nrow(DT4)
DT4.sample <- sample(DT4.rows, DT4.rows * 0.6)

DT4.train <- DT4[DT4.sample, ]
DT4.test <- DT4[-DT4.sample, ]

DT4.model <- C5.0(DT4.train[, -1], DT4.train$FTR, trails = 100)

plot(DT4.model)
summary(DT4.model)

DT4.predict <- predict (DT4.model, DT4.test[, -1])
CrossTable(
  DT4.test$FTR,
  DT4.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###########################################################################################################
DT5 <- x[-c(2:10, 14:16)]

set.seed(123)
DT5$FTR <- as.factor(DT5$FTR)

DT5.rows <- nrow(DT5)
DT5.sample <- sample(DT5.rows, DT5.rows * 0.6)

DT5.train <- DT5[DT5.sample, ]
DT5.test <- DT5[-DT5.sample, ]

DT5.model <- C5.0(DT5.train[, -1], DT5.train$FTR, trails = 100)

plot(DT5.model)
summary(DT5.model)

DT5.predict <- predict (DT5.model, DT5.test[, -1])
CrossTable(
  DT5.test$FTR,
  DT5.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
########################################################################################
DT6 <- x[-c(2:13)]

set.seed(123)
DT6$FTR <- as.factor(DT6$FTR)

DT6.rows <- nrow(DT6)
DT6.sample <- sample(DT6.rows, DT6.rows * 0.6)

DT6.train <- DT6[DT6.sample, ]
DT6.test <- DT6[-DT6.sample, ]

DT6.model <- C5.0(DT6.train[, -1], DT6.train$FTR, trails = 100)

plot(DT6.model)
summary(DT6.model)

DT6.predict <- predict (DT6.model, DT6.test[, -1])
CrossTable(
  DT6.test$FTR,
  DT6.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)