rm(list=ls())
setwd("~/Mckinsey")

#loading libraries
library(caret)
library(rpart)
library(C50)
library(e1071)
library(DMwR)
library(MASS)
library(randomForest)
library(xgboost)

#Creating a function to get missing values column wise
column_wise <- function(data) apply(data,2,function(x) sum(is.na(x)))

#Loading the data
#train <- read.csv('Train_psolI3n.csv')
test <- read.csv('Test_09JmpYa.csv')
Id <- test$Email_ID
rm(test)

train_imp <- read.csv('train_imp.csv')
test_imp  <- read.csv('test_imp.csv')

#Structure of file
str(train)

#Initial File Conversion
#train$Email_ID <-  NULL
#train$Email_Type <- as.factor(train$Email_Type)
#train$Email_Source_Type <- as.factor(train$Email_Source_Type)
#train$Email_Campaign_Type <- as.factor(train$Email_Campaign_Type)
#train$Time_Email_sent_Category <- as.factor(train$Time_Email_sent_Category)
# target <- train$Email_Status
# train$Email_Status <- NULL

#test$Email_ID <- NULL
#test$Email_Type <- as.factor(test$Email_Type)
#test$Email_Source_Type <- as.factor(test$Email_Source_Type)
#test$Email_Campaign_Type <- as.factor(test$Email_Campaign_Type)
#test$Time_Email_sent_Category <- as.factor(test$Time_Email_sent_Category)

#Data Understanding and missing patterns
#column_wise(combi)

#summary(train)

#Missing Value Imputation
#combi = mice::complete(mice::mice(combi))
#train[is.na(train)] <- 1

# Dummy <- dummies::dummy.data.frame(combi,omit.constants=TRUE,dummy.classes = 'factor')
# train_imp <- Dummy[1:68353,]
# test_imp <- Dummy[68354:114331,]
# train_imp$Email_Status <- target

#Feature Engineering
train_imp$New_User <- ifelse(train_imp$Total_Past_Communications==0,1,0)
#train$New_User <- ifelse(train$Total_Past_Communications==0,1,0)
train_imp$Low_Start <- ifelse(train_imp$Subject_Hotness_Score<0.25,1,0)
#train$Low_Start <- ifelse(train$Subject_Hotness_Score<0.25,1,0)

test_imp$Low_Start <- ifelse(test_imp$Subject_Hotness_Score<0.25,1,0)
#test$Low_Start <- ifelse(test$Subject_Hotness_Score<0.25,1,0)
test_imp$New_User <- ifelse(test_imp$Total_Past_Communications==0,1,0)
#test$New_User <- ifelse(test$Total_Past_Communications==0,1,0)

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train_imp)) {
  if (length(unique(train_imp[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train_imp[[f]] <- NULL
    test_imp[[f]] <- NULL
  }
}

################################# Anova Analysis ##################################
anova <- aov(as.numeric(Email_Status)~.,data=train_imp)


################################# Data Smoting for class imbalance ##################################

train_imp$Email_Status <- as.factor(train_imp$Email_Status)

test <- train_imp[h,]
train <- train_imp[!(1:nrow(train_imp) %in% h),]
data_SMOTE <- SMOTE(Email_Status~.,data=train,perc.over=600,perc.under=200)
data_SMOTE2 <- SMOTE(Email_Status~.,data=data_SMOTE,perc.over=300)

################################# For easeness##################################

Feature.Variables <- colnames(train_imp)[c(-24)]
target <- 'Email_Status'

################################# Bulding Stacking using different algorithms ##################################

train_imp$Email_Status <- as.factor(train_imp$Email_Status)
Formula <-as.formula(paste('Email_Status',paste(Feature.Variables,collapse =  '+'),sep = "~"))
folds <- sample(1:5,size=nrow(train_imp),replace = T)
for(i in 1:5){
  tst <- train_imp[folds==i,]
  tra <- train_imp[folds != i,]
  rpart <- rpart(Formula,data=tra,cp=0.001,method='class')
  train_imp[folds==i,'prediction_rpart'] <- predict(rpart,tst,type='class') 
  #Accuracy of model is 81.30
  cat("\n##In model C50 \n",i)
  C50 <- C5.0(Formula,data=tra,trails=20,rules=F)
  train_imp[folds==i,'prediction_C50'] <- predict(C50,tst,type='class')
  #Accuracy of model is 81.23
  cat("\n##In model lda \n",i)
  lda<-lda(Formula,data=tra) 
  pred_lda <- predict(lda,tst)
  pred_lda$class[is.na(pred_lda$class)] <- 0
  train_imp[folds==i,'prediction_lda'] <- pred_lda$class
  #Accuracy of model is 80.40
  cat("\n##In model nnet \n",i)
  nnet<-nnet(Formula,data=tra,size = 5,decay = 0.1) 
  train_imp[folds==i,'prediction_nnet'] <- as.factor(predict(nnet,tst,type='class'))
  #Accuracy of model is 80.9
  cat("\n##In model naive bayes\n",i)
  nb<-naiveBayes(Formula,data=tra,k=10) #Summarize the model
  train_imp[folds==i,'prediction_nb'] <- predict(nb,tst,type='class')
  #Accuracy of model is 80.39
  cat("\n##In model random Forest \n",i)
  rf <- randomForest(Formula,data=tra,mtry=3,ntree=100)
  train_imp[folds==i,'prediction_rf'] <- predict(rf,tst,type='class')
  #Accuracy of model is 81.16
  cat("\n##In model XB \n",i)
  xgb <- xgboost(data = data.matrix(sapply(train_imp[,Feature.Variables],as.numeric)), label = as.numeric(train_imp[,target])-1, max.depth = 8,  
                 eta = 0.01, nthread = 2, nround = 200, objective = "multi:softmax",num_class=3)
  train_imp[folds==i,'prediction_xgb'] <- as.factor(predict(xgb, data.matrix(tst[,Feature.Variables])))
  #Accuracy of model is 82.1542
}
stack <- data.frame(sapply(train_imp[,c(27:33,24)], as.numeric)) 

#A 2 nd layer of xgboost using stacking features
models <- colnames(stack)[-8]
h<-sample(nrow(stack),20000)
dval<-xgb.DMatrix(data=data.matrix(stack[h,models]),label=as.numeric(stack[h,target])-1)
dtrain<-xgb.DMatrix(data=data.matrix(stack[-h,models]),label=as.numeric(stack[-h,target])-1)

watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softmax", 
                booster             = "gbtree",
                seed                = 19,
                eta                 = 0.01,
                max_depth           = 3, 
                subsample           = 0.7,
                #colsample_bytree    = 0.7, 
                num_class           = 3,
                base_score         = 0,
                eval_metric = "merror",
                min_child_weight    = 1
)


clf <- xgb.train(data       = dtrain,
                 nrounds             = 15,
                 params              = param,
                 #verbose             = 1,
                 watchlist           = watchlist,
                 early.stop.round =   25,
                 maximize            = FALSE)

#Bulding Models for test data
rpart <- rpart(Formula,data=train_imp,cp=0.001,method='class')
test_imp[,'prediction_rpart'] <- predict(rpart,test_imp,type='class') 

cat("\n##In model C50 \n")
C50 <- C5.0(Formula,data=train_imp,trails=20,rules=F)
test_imp[,'prediction_C50'] <- predict(C50,test_imp,type='class')

cat("\n##In model lda \n")
lda<-lda(Formula,data=train_imp) 
pred_lda <- predict(lda,test_imp)
pred_lda$class[is.na(pred_lda$class)] <- 0
test_imp[,'prediction_lda'] <- pred_lda$class

cat("\n##In model nnet \n")
nnet<-nnet(Formula,data=train_imp,size = 5,decay = 0.1) 
test_imp[,'prediction_nnet'] <- as.factor(predict(nnet,test_imp,type='class'))

cat("\n##In model naive bayes\n")
nb<-naiveBayes(Formula,data=train_imp,k=10) #Summarize the model
test_imp[,'prediction_nb'] <- predict(nb,test_imp,type='class')

cat("\n##In model random Forest \n")
rf <- randomForest(Formula,data=train_imp,mtry=3,ntree=100,do.trace=T)
test_imp[,'prediction_rf'] <- predict(rf,test_imp,type='class')

cat("\n##In model XB \n")
xgb <- xgboost(data = data.matrix(sapply(train_imp[,Feature.Variables],as.numeric)), label = as.numeric(train_imp[,target])-1, max.depth = 8,  
               eta = 0.01, nthread = 2, nround = 200, objective = "multi:softmax",num_class=3)
test_imp[,'prediction_xgb'] <- as.factor(predict(xgb, data.matrix(test_imp[,Feature.Variables])))

#Final Predictions
stack_test <- data.frame(sapply(test_imp[,models], as.numeric)) 
predictions <- predict(clf,data.matrix(stack_test))

######################### Identifying the best tune with 5 fold cros validation ##################################

train_imp[,target] = paste('X',train_imp$Email_Status,sep="")
fitControl <- trainControl(method = "cv",
                           number = 5,
                           allowParallel = TRUE,
                           classProbs = T)

system.time(rpart <- train(x = train_imp[,Feature.Variables],
                           y= train_imp[,target],
                           method = 'rpart',
                           tuneGrid = expand.grid(cp=c(0.01,0.001,0.007,0.0007,0.0009)),
                           trControl = fitControl))
rpart
#Best tune for data is cp = 0.001


system.time(C50 <- train(x = train_imp[,Feature.Variables],
                         y= train_imp[,target],
                         method = 'C5.0',
                         tuneLength = 3,
                         trControl = fitControl))
C50
#Best Tune For data is trials = 20, model = tree and winnow = FALSE. 


system.time(bag <- train(x = train_imp[,Feature.Variables],
                         y= train_imp[,target],
                         method = 'treebag',
                         do.trace=50,
                         ntree=250,
                         trControl = fitControl))
bag
# No tune acc of 79.9

system.time(svm <- train(x = train_imp[,Feature.Variables],
                         y= train_imp[,target],
                         method = 'svmRadial',
                         trControl = fitControl))
svm
# Best tune For data is 

system.time(nb <- train(x = train_imp[,Feature.Variables],
                        y= train_imp[,target],
                        method = 'nb',
                        trControl = fitControl))
nb
# Best tune For data is were fL = 0, usekernel = TRUE and adjust = 1. 

system.time(nnet <- train(x = train_imp[,Feature.Variables],
                          y= train_imp[,target],
                          method = 'nnet',
                          trace =T,
                          trControl = fitControl))
nnet
# Best tune For data is  size = 5 and decay = 0.1.


system.time(glmnet <- train(x = train_imp[,Feature.Variables],
                            y= train_imp[,target],
                            method = 'glmnet',
                            trControl = fitControl))
glmnet
# Best tune For data is alpha = 0.1 and lambda = 0.02128847. 

system.time(rf <- train(x = train_imp[,Feature.Variables],
                        y= train_imp[,target],
                        method = 'rf',
                        do.trace=50,
                        tuneGrid = expand.grid(mtry=c(3)),
                        ntree=50,
                        trControl = fitControl))
rf
# Best tune For data is ntree = 50,mtry = 3


system.time(xgb <- train(x = train_imp[,Feature.Variables],
                         y= train_imp[,target],
                         method = 'xgbTree',
                         tuneLength = 4,
                         trControl = fitControl))
xgb
# Best tune For data is nrounds = 200, max_depth = 2, eta = 0.4, gamma = 0, colsample_bytree = 0.8 and min_child_weight = 1

############################################################################
#XGB Using Linear Regression and finding an optimal threshold points

tra = data.frame(sapply(train_imp[,c(-24)], as.numeric))
tst = data.frame(sapply(test_imp, as.numeric))
tst[is.na(tst)] <- 0
a = 0

Fitness <- function(x){
  for(i in 1:5){
    index <- sample(1:length(pred_xgb),10000)
    prediction = ifelse(pred_xgb>x[2],2,ifelse(pred_xgb>x[1],1,0))
    a <- a + sum(diag(table(prediction[index],train_imp[index,'Email_Status'])))/10000
  }
  return(-abs(a/5))
}

folds <- sample(1:5,size=nrow(train_imp),replace = T)

for(i in 1:5){
  dtrain  <-xgb.DMatrix(data=data.matrix(tra[!(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[!(folds %in% i),target]))
  dval<-xgb.DMatrix(data=data.matrix(tra[(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[(folds %in% i),target]))
  
  watchlist<-list(val=dval,train=dtrain)
  param <- list(  objective           = "reg:linear", 
                  booster             = "gbtree",
                  seed                = 19,
                  eta                 = 0.01,
                  max_depth           = 4, 
                  subsample           = 0.7,
                  eval_metric         ='rmse',
                  colsample_bytree    = 0.7,
                  min_child_weight    = 5
  )
  
  
  clf <- xgb.train(data       = dtrain,
                   nrounds             = 200,
                   params              = param,
                   verbose             = FALSE,
                   watchlist           = watchlist,
                   early.stop.round =   25
  )
  pred_xgb <- predict(clf,newdata=dval)
  
  cat('Optimisation happening for fold',i,'  ')
  optimisation <- stats::optim(c(1,2),Fitness, method = "Nelder-Mead")
  cat(optimisation$par,'Accuracy=',optimisation$value)
}

##################################Special Status for the series
for(i in 1:5){
  dval1<-xgb.DMatrix(data=data.matrix(tra[(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[(folds %in% i),target] > 0))
  dtrain1<-xgb.DMatrix(data=data.matrix(tra[!(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[!(folds %in% i),target]>0))
  
  dval2<-xgb.DMatrix(data=data.matrix(tra[(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[(folds %in% i),target] > 1))
  dtrain2<-xgb.DMatrix(data=data.matrix(tra[!(folds %in% i),Feature.Variables]),label=as.numeric(train_imp[!(folds %in% i),target]>1))
  
  watchlist1<-list(val=dval1,train=dtrain1)
  watchlist2<-list(val=dval2,train=dtrain2)
  param <- list(  objective           = "binary:logistic", 
                  booster             = "gbtree",
                  seed                = 19,
                  eta                 = 0.01,
                  max_depth           = 15, 
                  #subsample           = 0.7,
                  #colsample_bytree    = 0.7, 
                  #num_class           = 3,
                  #base_score         = 0,
                  #eval_metric = "merror",
                  min_child_weight    = 5
  )
  
  
  clf1 <- xgb.train(data       = dtrain1,
                    nrounds             = 400,
                    params              = param,
                    verbose             = 0,
                    watchlist           = watchlist,
                    early.stop.round =   10,
                    maximize            = FALSE)
  clf2 <- xgb.train(data       = dtrain2,
                    nrounds             = 100,
                    params              = param,
                    verbose             = 0,
                    watchlist           = watchlist,
                    early.stop.round =   10,
                    maximize            = FALSE)
  train_imp[folds==i,'prediction_xgb_1'] <-  as.numeric(predict(clf1,dval1))
  
  train_imp[folds==i,'prediction_xgb_2'] <-  as.numeric(predict(clf2,dval2))
  
}


################################# XGB preparation ##################################

tra = data.frame(sapply(train_imp[,c(-24)], as.numeric))
tst = data.frame(sapply(test_imp, as.numeric))
tst[is.na(tst)] <- 0

h<-sample(nrow(train_imp),20000)
dval<-xgb.DMatrix(data=data.matrix(tra[h,Feature.Variables]),label=as.numeric(train_imp[h,target]))
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,Feature.Variables]),label=as.numeric(train_imp[-h,target]))
dtest<-xgb.DMatrix(data=data.matrix(tst[,Feature.Variables]),missing = 0)

watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "multi:softmax", 
                booster             = "gbtree",
                seed                = 209,
                eta                 = 0.4,
                max_depth           = 2, 
                subsample           = 0.7,
                colsample_bytree    = 0.8, 
                num_class           = 3,
                base_score         = 0,
                eval_metric = "merror",
                min_child_weight    = 1
)

clf <- xgb.train(data       = dtrain,
                 nrounds             = 200,
                 params              = param,
                 #verbose             = 1,
                 watchlist           = watchlist,
                 early.stop.round =   25,
                 maximize            = FALSE)

pred_rf <- as.numeric(predict(xgb,data.matrix(test_imp)))-1
#############################Submission#######################################
submission <- data.frame(Email_ID=Id, Email_Status=pred_rf)
write.csv(submission, "xgb.csv",row.names = F)
