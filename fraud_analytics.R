ccdata <- read.csv('creditcard.csv')

table(ccdata$Class)
prop.table(table(ccdata$Class))

# split data into train and test portions
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(ccdata$Class, p = .50,
                                  list = FALSE,
                                  times = 1)
trainSplit <- ccdata[ splitIndex,]
testSplit <- ccdata[-splitIndex,]

prop.table(table(trainSplit$Class))
prop.table(table(testSplit$Class))

# model using treebag
ctrl <- trainControl(method = "cv", number = 2)
tbmodel <- caret::train(Class ~ ., data = trainSplit, method = "treebag",
                        trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

library(pROC)
auc <- roc(testSplit$Class, pred)
print(auc)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

library(DMwR)

trainSplit$Class <- as.factor(trainSplit$Class)
trainSplit <- SMOTE(Class ~ ., trainSplit, perc.over = 100, perc.under=200)
trainSplit$Class <- as.numeric(trainSplit$Class)
prop.table(table(trainSplit$Class))

# evaluate the SMOTE performance
tbmodel <- caret::train(Class ~ ., data = trainSplit, method = "treebag",
                        trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'Class']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])

auc <- roc(testSplit$Class, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

