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
 
##########################################################################################################
DT3 <- x[-c(2:4, 8:22)]

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
DT4 <- x[-c(2:7, 11:22)]

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
DT5 <- x[-c(2:10, 14:22)]

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
DT6 <- x[-c(2:13,17:22)]

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
########################################################################################
DT7 <- x[-c(2:16,20:22)]

set.seed(123)
DT7$FTR <- as.factor(DT7$FTR)

DT7.rows <- nrow(DT7)
DT7.sample <- sample(DT7.rows, DT7.rows * 0.6)

DT7.train <- DT7[DT7.sample, ]
DT7.test <- DT7[-DT7.sample, ]

DT7.model <- C5.0(DT7.train[, -1], DT7.train$FTR, trails = 100)

plot(DT7.model)
summary(DT7.model)

DT7.predict <- predict (DT7.model, DT7.test[, -1])
CrossTable(
  DT7.test$FTR,
  DT7.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##########################################################################################
DT8 <- x[-c(2:19)]

set.seed(123)
DT8$FTR <- as.factor(DT8$FTR)

DT8.rows <- nrow(DT8)
DT8.sample <- sample(DT8.rows, DT8.rows * 0.6)

DT8.train <- DT8[DT8.sample, ]
DT8.test <- DT8[-DT8.sample, ]

DT8.model <- C5.0(DT8.train[, -1], DT8.train$FTR, trails = 100)

plot(DT8.model)
summary(DT8.model)

DT8.predict <- predict (DT8.model, DT8.test[, -1])
CrossTable(
  DT8.test$FTR,
  DT8.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###########################################################################################