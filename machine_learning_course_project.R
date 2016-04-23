library(caret)
library(gbm)

# read in the training and testing sets
training.raw = read.csv("pml-training.csv",na.strings=c("","NA"))
testing.raw = read.csv("pml-testing.csv",na.strings=c("","NA"))

# reduce training sets, remove variables with more than 10k NAs
training.red <- training.raw[!colSums(is.na(training.raw)) > 10000]
# remove timestamp, window, name, and X variables
var.reduce <- grep("timestamp|window|name|X", names(training.red))
training.red <- training.red[,-var.reduce]

#split the training set into training and validation sets
inTrain <- createDataPartition(y = training.red$classe, p = 0.7, list = FALSE)
training <- training.red[inTrain,]
validation <- training.red[-inTrain,]

# train a random forest, lda, and gbm models to the training set
fit.rf <- train(classe ~ ., data=training, method="rf")
fit.lda <- train(classe ~ ., data=training, method="lda")
fit.gbm <- train(classe ~., data=training, method="gbm")

pred.rf <- predict(fit.rf, validation)
pred.lda <- predict(fit.lda, validation)
pred.gbm <- predict(fit.gbm, validation)

#test accuracies of each model
confusionMatrix(pred.rf, validation$classe) #99.2% accurate
confusionMatrix(pred.lda, validation$classe) #70% accurate
confusionMatrix(pred.gbm, validation$classe) #95.65 accurate

#predict test values using most accurate model (random forest)
pred.test <- predict(fit.rf, testing.raw)

#estimate out of sample error rate
(1-confusionMatrix(pred.rf,validation$classe)$overall["Accuracy"])*100 #0.83%

