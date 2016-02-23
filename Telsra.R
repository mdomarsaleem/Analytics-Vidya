rm(list=ls())
setwd("~/telstra")

library(dplyr)
library(readr)
library(sqldf)
library(caret)
library(xgboost)
library(methods)
#Reading the data
# combi2 <- read.csv("~/telstra/combi2.csv", stringsAsFactors=FALSE)
train <- read.csv("~/telstra/train.csv", stringsAsFactors=FALSE)
test <- read.csv("~/telstra/test.csv", stringsAsFactors=FALSE)
event_type <- read.csv("~/telstra/event_type.csv", stringsAsFactors=FALSE)
log_feature <- read.csv("~/telstra/log_feature.csv", stringsAsFactors=FALSE)
resource_type <- read.csv("~/telstra/resource_type.csv", stringsAsFactors=FALSE)
severity_type <- read.csv("~/telstra/severity_type.csv", stringsAsFactors=FALSE)

log <- read.csv('log_dummy.csv',stringsAsFactors = F)
log$X = NULL
log$location = NULL
log[] <- lapply(log, as.numeric)
log$count= as.numeric(apply(log[,-1],1,sum))
log$log_count <- log(log$count+1)


target <- train$fault_severity
train$fault_severity = NULL
combi <- rbind(train[,-3],test)
location <- summarise(group_by(combi,location),count = n())
dd <- left_join(combi,event_type,"id")
location <- left_join(location,sqldf('select location,count(distinct(event_type)) ,count(event_type) from dd group by location '),by="location")

dd <- left_join(combi,log_feature,"id")
location <- left_join(location,sqldf('select location,count(distinct(log_feature)),count(log_feature),min(volume),max(volume),sum(volume),avg(volume) from dd group by location '),by="location")


dd <- left_join(combi,resource_type,"id")
location <- left_join(location,sqldf('select location,count(distinct(resource_type)),count(resource_type) from dd group by location '),by="location")

dd <- left_join(combi,severity_type,"id")
location <- left_join(location,sqldf('select location,count(distinct(severity_type)),count(severity_type) from dd group by location '),by="location")

log_train <- log[log$id %in% train$id,]
log_train$target <- target
feature.names <- colnames(log_train)[c(-1,-388)]
target <- 'target'
tra = as.data.frame(lapply(log_train,function(x) log(x+1)))

idf <- data.frame(apply(log_train,2,function(x) sum(as.numeric(x>0))))
idf$names <- row.names(idf)
idf$value <- idf$apply.log_train..2..function.x..sum.as.numeric.x...0...
idf$apply.log_train..2..function.x..sum.as.numeric.x...0... = NULL

for(i in idf$names){
  tra[,i] = tra[,i]*idf[idf$names==i,'value']
}

# solution = train$fault_severity
# train$fault_severity = NULL
# 
# combi = rbind(train,test)

create_dummy <- function(dataset,variable){
  for(level in levels(as.factor(variable))){
    dataset[,paste(level)] <- 0
  }
  dataset <- dataset[!duplicated(dataset$id),]
  return (dataset)
}

update_dummy <- function(dataset,data){
  for(i in dataset[,1]){
    records = data[data$id==i,]
    for(j in records[,2]){
      dataset[dataset[,1]==i,j]<-1
    }
  }
  return (dataset)
}

update_volume_dummy <- function(dataset,data){
  for(i in unique(dataset[,1])){
    records = data[data$id==i,]
    for(j in records[,3]){
      dataset[dataset[,1]==i,j]<-records[records[,3]==j,4]
    }
  }
  return (dataset)
}

combi <- create_dummy(combi,event_type$event_type)
combi <- update_dummy(combi,event_type)

combi <- create_dummy(combi,resource_type$resource_type)
combi <- update_dummy(combi,resource_type)

combi <- create_dummy(combi,severity_type$severity_type)
combi <- update_dummy(combi,severity_type)

log <- create_dummy(combi,log_feature$log_feature)
log <- update_dummy(log,log1)
combi <-update_volume_dummy(combi,log_feature)
myfun <- function(x) unique(x)[which.max(table(x))]
combi$loc <- as.numeric(sapply(strsplit(combi$location," "), "[[", 2))
plot(loc,train$fault_severity)
app = data.frame(apply(train_dummy,2,var))
nonZeros = row.names.data.frame(app)[app!=0]
nonZeros = nonZeros[-2]
train1 <- train_dummy[,nonZeros]
rd <- lm.ridge(target ~ ., data=train_dummy[,nonZeros], lambda=0.5)
train1 <- train1[,(abs(rd$coef) > quantile(abs(rd$coef), 0.25))]


rpart = rpart(fault_severity~.,data = tra,cp=0.0001)
feature.names = levels(rpart$frame$var)[2:76]

train <- read.csv("~/telstra/train3.csv", stringsAsFactors=FALSE)
test <- read.csv("~/telstra/test3.csv", stringsAsFactors=FALSE)
location <- read.csv("location.csv",stringsAsFactors = FALSE)

train = merge(train,location,by.x = "location",by.y="location",all.x = T)
test = merge(test,location,by.x = "location",by.y="location",all.x = T)

train <- left_join(train,cluster)
test<- left_join(test,cluster)
tra = data.frame(sapply(train[,c(-1,-2)], as.numeric))
tst = data.frame(sapply(test[,c(-1,-2)], as.numeric))

tra$mean_1 = tra$mean_resource/tra$resource_count+tra$mean_severity/tra$severity_count
tst$mean_1 = tst$mean_resource/tst$resource_count+tst$mean_severity/tst$severity_count
tra$mean_2 = tra$mean_resource/tra$resource_count+tra$mean_event_train/tra$event_count
tst$mean_2 = tst$mean_resource/tst$resource_count+tst$mean_event_train/tst$event_count
tra$mean_3 = tra$mean_event_train/tra$event_count+tra$mean_severity/tra$severity_count
tst$mean_3 = tst$mean_event_train/tst$event_count+tst$mean_severity/tst$severity_count

tra[is.na(tra)] <- 0 
tst[is.na(tst)] <- 0

library(FSelector)
weights <- information.gain(fault_severity~., tra)
weights <- gain.ratio(fault_severity~., tra)
weights <-symmetrical.uncertainty(fault_severity~., tra)
print(weights)
subset <- cutoff.biggest.diff(weights)
subset <- cutoff.k(weights, 50)
f <- as.simple.formula(subset, "fault_severity")

tra = train[,-2]
tra$target <- as.factor(paste('C',tra$target,sep=""))
feature.names <- colnames(tst)
target <-c('fault_severity')

#########################Model Building################################
LogLoss <- function (data, lev = NULL, model = NULL) 
{
  probs <- pmax(pmin(as.numeric(data$T), 1 - 1e-15), 1e-15)
  logPreds <- log(probs)        
  log1Preds <- log(1 - probs)
  real <- (as.numeric(data$obs) - 1)
  out <- c(mean(real * logPreds + (1 - real) * log1Preds)) * -1
  names(out) <- c("LogLoss")
  out
}

colnames(train) <- make.names(names(train))
fitControl <- trainControl( method = "cv",
                            number = 3,
                            repeats = 3,
                            allowParallel=TRUE,
                            classProbs = T,
                            summaryFunction = mnLogLoss)

rpart <- train(fault_severity~.,data = tra,method = "rpart",trControl = fitControl,
               metric="logLoss",tuneLength =2)
C50 <- train(fault_severity~.,data = tra,method = "C5.0",trControl = fitControl,
             metric="logLoss",tuneLength =20)
gbm <- train(fault_severity~.,data = tra,method = "gbm",trControl = fitControl,
             metric="logLoss",tuneLength =2)
xgb <- train(fault_severity~.,data = tra,method = "xgbTree",trControl = fitControl,
             metric="logLoss",tuneLength =3)
svm <- train(fault_severity~.,data = tra,method = "svmRadial",trControl = fitControl,
             metric="logLoss",tuneLength =3)
rpart2 <- train(fault_severity~.,data = tra,method = "rpart2",trControl = fitControl,
                metric="logLoss",tuneLength =3)
rf <- train(target~.,data = tra[,c(-1,-2)],method = "rf",trControl = fitControl,
            metric="logLoss",tuneGrid=expand.grid(mtry = c(2,40,80,100,120)),maximize=F, importance = TRUE)


clf = train(tra[,feature.names], 
            tra[,"fault_severity"],method="rf",tuneGrid =expand.grid(mtry = 100),
            trainControl= trainControl(method = "repeatedcv",number = 5, repeats = 3),
            metric = "logLoss",ntree= 100,importance = TRUE,do.trace = T )

pred <- predict(rpart,tst,type="prob")
submission <- data.frame(id=test$id,predict_0=pred$C0,predict_1=pred$C1,predict_2=pred$C2)
cat("saving the submission file\n")
write_csv(submission, "rf.csv")

#################################XGB############################
test[is.na(test)] <- 0

dissimilarity(data.matrix(tra[h,feature.names]), method = "cosine")
h<-sample(nrow(tra),1000)
dval<-xgb.DMatrix(data=data.matrix(tra[h,feature.names]),label=tra[h,target])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,feature.names]),label=tra[-h,target])
dtest<-xgb.DMatrix(data=data.matrix(tst[,feature.names]))

watchlist<-list(val=dval,train=dtrain)
# dtrain<-xgb.DMatrix(data=data.matrix(tra[,feature.names]),label=tra[,target])
 param <- list(  objective           = "multi:softprob", 
                booster             = "gbtree",
                seed                = 19,
                eta                 = 0.01,
                max_depth           = 15, 
                subsample           = 0.7,
                colsample_bytree    = 0.7, 
                num_class           = 3,
                base_score         = 0,
                "eval_metric" = "mlogloss",
                min_child_weight    = 10
)



clf <- xgb.cv(data            = dtrain,
               nrounds             = 1000,
               params              = param,
               verbose             = 1,
               nfold              = 5,
               early.stop.round =   10,
               watchlist           = watchlist,
               maximize            = FALSE)


clf <- xgb.train(data       = dtrain,
              nrounds             = 640,
              params              = param,
              #verbose             = 1,
              watchlist           = watchlist,
              early.stop.round =   25,
              maximize            = FALSE)

pred = predict(clf,dtest)
a <- seq(1,33513,3)
predict_0 = pred[a]
predict_1 = pred[a+1]
predict_2 = pred[a+2]

# I pretended this was a regression problem and some predictions may be outside the range
submission <- data.frame(id=test$id,predict_0=predict_0,predict_1=predict_1,predict_2=predict_2)
cat("saving the submission file\n")
write_csv(submission, "xgb.csv")
importance_matrix <- xgb.importance(feature.names, model = clf)
xgb.plot.importance(importance_matrix)


pred = cbind(predict_0,predict_1,predict_2)
new <- function(X){
  val = matrix(0,nrow = 1000,ncol=3)
  val[,1] = X[1]*pred[,1] + X[2]*pred[,2] + X[3]*pred[,3]
  val[,2] = X[4]*pred[,1] + X[5]*pred[,2] + X[6]*pred[,3]
  val[,3] = X[7]*pred[,1] + X[8]*pred[,2] + X[9]*pred[,3]
  return(logLoss(act,val))
}
optim(c(1,0,0,0,1,0,0,0,1),new, method = "Nelder-Mead")
optim(c(1,0,0,0,1,0,0,0,1),new, method = "BFGS")
optim(c(1,0,0,0,1,0,0,0,1),new, method = "CG")
optim(c(1,0,0,0,1,0,0,0,1),new, method = "L-BFGS-B")
optim(c(1,0,0,0,1,0,0,0,1),new, method = "SANN")
optim(c(1,0,0,0,1,0,0,0,1),new, method = "Brent")
library(pracma)
fminsearch(new,c(0,0,0,0,0,0,0,0,0),minimize = T,dfree = T)
