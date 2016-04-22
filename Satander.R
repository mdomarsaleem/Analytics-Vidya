rm(list=ls())
setwd("~/Satander Bank")

library(caret)
library(ROSE)
library(pROC)
library(ROCR)

train <- read.csv('train.csv')
test <- read.csv('test.csv')

str(train)

train$ID <- NULL
target = train$TARGET
train$TARGET <- NULL

table(is.na(train))

#Without any data modification 0.839

Standard_deviation <- apply(train,2,sd)
Variance  <- nearZeroVar(train,saveMetrics = T)
MultiCollinearity <- findLinearCombos(train)


##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)
train$TARGET <- target


##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

#Outliers <- DMwR::lofactor(train,k=10)
#out_index <- which(Outliers>3)

correlation <- cor(train)
findCorrelation(cor(train), cutoff = .999, verbose = FALSE)

anova <- aov(as.numeric(TARGET)~.,data=train)
summary(anova)

#For easeness
# Feature.Variables <- colnames(train)[-371]
Feature.Variables <- colnames(train)[-337]
target <- 'TARGET'
train[,target] = paste('X',train$TARGET,sep="")

index <- sample(1:nrow(train),10000)
validate <- train[index,]
train <- train[!(1:nrow(train) %in% index),]


train_over <- ovun.sample(TARGET ~ ., data = train, method = "over",N = 100000)$data
train_under <- ovun.sample(TARGET ~ ., data = train, method = "under",N = 30000)$data
train_both <- ovun.sample(TARGET ~ ., data = train, method = "both",N = 75000)$data
train_rose <- ROSE(TARGET ~ ., data = train, seed=1)$data

fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE,
                           classProbs = T,
                           summaryFunction=twoClassSummary)
system.time(
 glm <- train(x = train[,Feature.Variables],
            y= train[,target],
            method = 'glm',
            metric='ROC',
            tuneLength = 1,
            trControl = fitControl))
glm

system.time(rpart <- train(x = train[,Feature.Variables],
            y= train[,target],
            method = 'rpart',
            metric='ROC',
            tuneLength = 5,
            trControl = fitControl))
rpart

rpart2 <- train(x = train[,Feature.Variables],
            y= train[,target],
            method = 'rpart2',
            metric='ROC',
            tuneLength = 5,
            trControl = fitControl)
rpart2



rf <- train(x = train[,Feature.Variables],
             y= train[,target],
             method = 'rf',
             metric='ROC',
             tuneLength = 5,
            do.trace=T,
            ntree=50,
             trControl = fitControl)
rf
xgb <- train(x = train[,Feature.Variables],
            y= train[,target],
            method = 'xgbTree',
            metric='ROC',
            tuneLength = 3,
            trControl = fitControl)
xgb


roc.curve(validate[,target], predict(xgb,newdata=validate[,Feature.Variables],type='prob')[,2], plotit = T)

xgb_over <- train(x = train_over[,Feature.Variables],
             y= train_over[,target],
             method = 'xgbTree',
             metric='ROC',
             tuneLength = 3,
             trControl = fitControl)
roc.curve(validate[,target], predict(xgb_over,newdata=validate[,Feature.Variables],type='prob')[,2], plotit = T)

xgb_under <- train(x = train_under[,Feature.Variables],
             y= train_under[,target],
             method = 'xgbTree',
             metric='ROC',
             tuneLength = 3,
             trControl = fitControl)
roc.curve(validate[,target], predict(xgb_under,newdata=validate[,Feature.Variables],type='prob')[,2], plotit = T)

xgb_both <- train(x = train_both[,Feature.Variables],
             y= train_both[,target],
             method = 'xgbTree',
             metric='ROC',
             tuneLength = 3,
             trControl = fitControl)
roc.curve(validate[,target], predict(xgb_both,newdata=validate[,Feature.Variables],type='prob')[,2], plotit = T)

xgb_rose <- train(x = train_rose[,Feature.Variables],
             y= train_rose[,target],
             method = 'xgbTree',
             metric='ROC',
             tuneLength = 3,
             trControl = fitControl)
roc.curve(validate[,target], predict(xgb_rose,newdata=validate[,Feature.Variables],type='prob')[,2], plotit = T)

pred_rf <- predict(xgb,test,type='prob')[,2]
#############################Submission#######################################
submission <- data.frame(ID=test$ID, TARGET=pred_rf)
write.csv(submission, "xgb.csv",row.names = F)
