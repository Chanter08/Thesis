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
    "C:\\Users\\User\\Documents\\Thesis\\Data\\Models\\ELO\\ELO00-01.csv",
    stringsAsFactors = FALSE
  )
################################# Clean Data ##############################################
x$GBH   <- as.numeric(x$GBH)
x$GBD   <- as.numeric(x$GBD)
x$GBA   <- as.numeric(x$GBA)
x$IWH   <- as.numeric(x$IWH)
x$IWD   <- as.numeric(x$IWD)
x$IWA   <- as.numeric(x$IWA)
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
##################################### All Bookies #########################################
y1 <- x

set.seed(123)
y1$FTR <- as.factor(y1$FTR)

y1.rows <- nrow(y1)
y1.sample <- sample(y1.rows, y1.rows * 0.6)

y1.train <- y1[y1.sample, ]
y1.test <- y1[-y1.sample, ]

y1.model <- C5.0(y1.train[, -1], y1.train$FTR, trails = 100)

plot(y1.model, sub = 142)
summary(y1.model)

y1.predict <- predict (y1.model, y1.test[, -1])
CrossTable(
  y1.test$FTR,
  y1.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##################################### Bet 365 Only ########################################
y2 <- x[-c(7:15)]

set.seed(123)
y2$FTR <- as.factor(y2$FTR)

y2.rows <- nrow(y2)
y2.sample <- sample(y2.rows, y2.rows * 0.6)

y2.train <- y2[y2.sample, ]
y2.test <- y2[-y2.sample, ]

y2.model <- C5.0(y2.train[, -1], y2.train$FTR, trails = 100)

plot(y2.model)
summary(y2.model)

y2.predict <- predict (y2.model, y2.test[, -1])
CrossTable(
  y2.test$FTR,
  y2.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)

##########################################################################################################
y3 <- x[-c(4:6, 10:15)]

set.seed(123)
y3$FTR <- as.factor(y3$FTR)

y3.rows <- nrow(y3)
y3.sample <- sample(y3.rows, y3.rows * 0.6)

y3.train <- y3[y3.sample, ]
y3.test <- y3[-y3.sample, ]

y3.model <- C5.0(y3.train[, -1], y3.train$FTR, trails = 100)


plot(y3.model)
summary(y3.model)

y3.predict <- predict (y3.model, y3.test[, -1])
CrossTable(
  y3.test$FTR,
  y3.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###########################################################################################################
y4 <- x[-c(4:9, 13:15)]

set.seed(123)
y4$FTR <- as.factor(y4$FTR)

y4.rows <- nrow(y4)
y4.sample <- sample(y4.rows, y4.rows * 0.6)

y4.train <- y4[y4.sample, ]
y4.test <- y4[-y4.sample, ]

y4.model <- C5.0(y4.train[, -1], y4.train$FTR, trails = 100)

plot(y4.model)
summary(y4.model)

y4.predict <- predict (y4.model, y4.test[, -1])
CrossTable(
  y4.test$FTR,
  y4.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###########################################################################################################
y5 <- x[-c(4:12)]

set.seed(123)
y5$FTR <- as.factor(y5$FTR)

y5.rows <- nrow(y5)
y5.sample <- sample(y5.rows, y5.rows * 0.6)

y5.train <- y5[y5.sample, ]
y5.test <- y5[-y5.sample, ]

y5.model <- C5.0(y5.train[, -1], y5.train$FTR, trails = 100)

plot(y5.model)
summary(y5.model)

y5.predict <- predict (y5.model, y5.test[, -1])
CrossTable(
  y5.test$FTR,
  y5.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
############################################################################################################