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
    "C:\\Users\\User\\Documents\\Thesis\\Data\\Models\\ELO\\ELO13-14.csv",
    stringsAsFactors = FALSE
  )
################################# Clean Data ##############################################
x$B365H <- as.numeric(x$B365H)
x$B365D <- as.numeric(x$B365D)
x$B365A <- as.numeric(x$B365A)
x$BWH   <- as.numeric(x$BWH)
x$BWD   <- as.numeric(x$BWD)
x$BWA   <- as.numeric(x$BWA)
x$IWH   <- as.numeric(x$IWH)
x$IWD   <- as.numeric(x$IWD)
x$IWA   <- as.numeric(x$IWA)
x$LBH   <- as.numeric(x$LBH)
x$LBD   <- as.numeric(x$LBD)
x$LBA   <- as.numeric(x$LBA)
x$PSH   <- as.numeric(x$PSH)
x$PSD   <- as.numeric(x$PSD)
x$PSA   <- as.numeric(x$PSA)
x$WHH   <- as.numeric(x$WHH)
x$WHD   <- as.numeric(x$WHD)
x$WHA   <- as.numeric(x$WHA)
x$VCH   <- as.numeric(x$VCH)
x$VCD   <- as.numeric(x$VCD)
x$VCA   <- as.numeric(x$VCA)
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
y2 <- x[-c(7:24)]

set.seed(123)
y2$FTR <- as.factor(y2$FTR)

y2.rows <- nrow(y2)
y2.sample <- sample(y2.rows, y2.rows * 0.6)

y2.train <- y2[y2.sample, ]
y2.test <- y2[-y2.sample, ]

y2.model <- C5.0(y2.train[, -1], y2.train$FTR, trails = 20)

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
y3 <- x[-c(4:6, 10:24)]

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
y4 <- x[-c(4:9, 13:24)]

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
y5 <- x[-c(4:12, 16:24)]

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
y6 <- x[-c(4:15, 19:24)]

set.seed(123)
y6$FTR <- as.factor(y6$FTR)

y6.rows <- nrow(y6)
y6.sample <- sample(y6.rows, y6.rows * 0.6)

y6.train <- y6[y6.sample, ]
y6.test <- y6[-y6.sample, ]

y6.model <- C5.0(y6.train[, -1], y6.train$FTR, trails = 100)

plot(y6.model)
summary(y6.model)

y6.predict <- predict (y6.model, y6.test[, -1])
CrossTable(
  y6.test$FTR,
  y6.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##############################################################################################################
y7 <- x[-c(4:18, 22:24)]

set.seed(123)
y7$FTR <- as.factor(y7$FTR)

y7.rows <- nrow(y7)
y7.sample <- sample(y7.rows, y7.rows * 0.6)

y7.train <- y7[y7.sample, ]
y7.test <- y7[-y7.sample, ]

y7.model <- C5.0(y7.train[, -1], y7.train$FTR, trails = 100)

plot(y7.model)
summary(y7.model)

y7.predict <- predict (y7.model, y7.test[, -1])
CrossTable(
  y7.test$FTR,
  y7.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
##############################################################################################################
y8 <- x[-c(4:21)]

set.seed(123)
y8$FTR <- as.factor(y8$FTR)

y8.rows <- nrow(y8)
y8.sample <- sample(y8.rows, y8.rows * 0.6)

y8.train <- y8[y8.sample, ]
y8.test <- y8[-y8.sample, ]

y8.model <- C5.0(y8.train[, -1], y8.train$FTR, trails = 100)

plot(y8.model)
summary(y8.model)

y8.predict <- predict (y8.model, y8.test[, -1])
CrossTable(
  y8.test$FTR,
  y8.predict,
  prop.c = FALSE,
  prop.r = FALSE,
  prop.chisq = FALSE
)
###############################################################################################################