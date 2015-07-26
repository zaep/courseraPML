## ----knitrSetup, echo=FALSE, message=FALSE-------------------------------
knitr::opts_knit$set(root.dir = "..")

## ----loadPkg, echo=FALSE,message=FALSE-----------------------------------
devtools::load_all()
data(pml_train)
data(pml_test)

## ----loadDataFromURL, eval=FALSE-----------------------------------------
#  pml_train <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
#  pml_test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

## ----splitData, warning=FALSE, message=FALSE-----------------------------
str(pml_train)

library(caret)

set.seed(126377)
train_idx <- createDataPartition(pml_train$classe, p = .6, list = FALSE)

training <- pml_train[train_idx, ]
testing <- pml_train[-train_idx, ]

## ----edaFactors----------------------------------------------------------
str(training[, sapply(training, is.factor)])

## ----transformFactors----------------------------------------------------
transformFactors <- function(data) {
  cols <- sapply(data, is.factor) & grepl("skewness|kurtosis|yaw", names(data))
  data[, cols] <- as.numeric(as.character(data[, cols]))
  data$user_name <- as.character(data$user_name)
  data$cvtd_timestamp <- lubridate::parse_date_time(data$cvtd_timestamp, "d/m/Y H:M")
  return(data)
}

training <- transformFactors(training)
str(training[, sapply(training, is.factor)])

## ----edaPCA--------------------------------------------------------------
# identify zero variance variables
nzv <- nearZeroVar(training, saveMetrics = TRUE)
#remove zero variance and non-numeric variables
train_num <- training[, !nzv$zeroVar & sapply(training, is.numeric)]
# scale and center, then perform PCA
pca_train <- prcomp(train_num[complete.cases(train_num), -1],
                     center = TRUE,
                     scale. = TRUE)
plot(pca_train)

## ----edaPCAPlot----------------------------------------------------------
library(ggplot2)
library(scales)

idxs <- train_num[complete.cases(train_num), "X"]

ggplot(data.frame(pc1 = pca_train$x[, 1], 
                  pc2 = pca_train$x[, 2], 
                  classe = training[training$X %in% idxs, "classe"]), 
       aes(pc1, pc2)) + 
  geom_point(aes(color = classe), size = 3) +
  ggtitle("First 2 principal components of the WLE dataset")

## ----dataPrePoc----------------------------------------------------------
train_sub <- training[, !nzv$zeroVar & 
                        !(names(training) %in% c("X", "user_name", "cvtd_timestamp",
                                                "new_window", "raw_timestamp_part_1",
                                                "raw_timestamp_part_2", "num_window"))]
train_sub <- train_sub[, !sapply(train_sub, function(x) any(is.na(x)))]
str(train_sub)

## ----resamplingTrainingset-----------------------------------------------
set.seed(7824)
train_small_idx <- createDataPartition(train_sub$classe, p = .1, list = FALSE)
train_small <- train_sub[train_small_idx, ]
validation <- train_sub[-train_small_idx, ]
nrow(train_small)

## ----modelFitting, message = FALSE---------------------------------------
set.seed(3377)
rf_fit <- train(classe ~ ., train_small, 
                method = "rf",
                tuneLength = 5,
                trControl = trainControl(method = "cv"))
rf_fit
plot(varImp(rf_fit))

set.seed(34671)
rpart_fit <- train(classe ~ ., train_small, 
                method = "rpart",
                tuneLength = 20,
                trControl = trainControl(method = "cv"))

(rf_acc <- confusionMatrix(predict(rf_fit, validation), validation$classe)$overall["Accuracy"])
(rpart_acc <- confusionMatrix(predict(rpart_fit, validation), validation$classe)$overall["Accuracy"])

## ----testsetAccuracy, warning = FALSE------------------------------------
testing <- transformFactors(testing)
confusionMatrix(predict(rf_fit, testing), testing$classe)

## ----obtainPredictions, warning=FALSE------------------------------------
pml_test <- transformFactors(pml_test)
submission <- predict(rf_fit, pml_test)
submission

