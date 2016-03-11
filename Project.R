rm(list=ls(all=TRUE))
setwd("~/INSOFE/Project")

library(MASS)
library(mice)
library(rpart)
library(DMwR)
library(reshape2)
library(data.table)
library(caret)


Year_wise <- function(data, variable=NULL){
  if(is.null(variable)){
    for(i in levels(as.factor(data$STAT_PROFILE_DATE_YEAR))){
      cat(i,table(is.na(agent[data$STAT_PROFILE_DATE_YEAR==i,])),'\n')
    }
  } else{
    for(i in levels(as.factor(data$STAT_PROFILE_DATE_YEAR))){
      cat(i,table(is.na(agent[data$STAT_PROFILE_DATE_YEAR==i,variable])),'\n')
    }
  }
}

column_wise <- function(data) apply(data,2,function(x) sum(is.na(x)))

agent <- read.csv('agency_final.csv',stringsAsFactors = F,header = T)
Final_data <-  read.csv('final.csv',header=T)
agent <- agent[agent$STAT_PROFILE_DATE_YEAR!=2015,]

agent[agent == 99999] <- NA

#Missing values wit respective to Growth_rate_3year
Year_wise(agent,'GROWTH_RATE_3YR')

#We have 2005,2006,2007 with all NA's
agent <- agent[agent$STAT_PROFILE_DATE_YEAR>2007,]

#Missing values wit respective to LOSS_RATIO
Year_wise(agent,'LOSS_RATIO')
table(agent$LOSS_RATIO==99998)   #Positive loss 
median(agent[agent$LOSS_RATIO>0 & agent$LOSS_RATIO<9000,'LOSS_RATIO'],na.rm=T)
agent$LOSS_RATIO[agent$LOSS_RATIO==99998] <- 0.4824585

table(agent$LOSS_RATIO==99997)  #Negative Loss
median(agent[agent$LOSS_RATIO<0,'LOSS_RATIO'],na.rm=T)
agent$LOSS_RATIO[agent$LOSS_RATIO==99997] <- -0.2549541

summary(agent$LOSS_RATIO)

#Missing values wit respective to LOSS_RATIO_3YR
Year_wise(agent,'LOSS_RATIO_3YR')

#Identify the variables with maximum NA's
app <- data.frame(column_wise(agent))

#Variables with least missing values
good_variables <- row.names.data.frame(app)[app<20000]

#good data set
good_data <- agent[,good_variables]

#Corelationplot
numeric <- colnames(good_data)[5:20]
corrplot::corrplot(cor(na.omit(good_data[,numeric])))


good_data$QTY_Growth <- (good_data$POLY_INFORCE_QTY- good_data$PREV_POLY_INFORCE_QTY)/(good_data$PREV_POLY_INFORCE_QTY+1)
good_data$AMOUNT_Growth <-  (good_data$WRTN_PREM_AMT- good_data$PREV_WRTN_PREM_AMT)/(good_data$PREV_WRTN_PREM_AMT+1)



d = good_data[,c('AGENCY_ID','PROD_ABBR','STATE_ABBR','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth')]
# magent <- dcast(d, AGENCY_ID+PROD_ABBR~STAT_PROFILE_DATE_YEAR,value.var = 'GROWTH_RATE_3YR',mean)
magent <- dcast(setDT(d), AGENCY_ID+PROD_ABBR+STATE_ABBR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth'),mean)
magent <- data.frame(magent)

#Plotting
corrplot::corrplot(cor(na.omit(magent[,c(-1,-2,-3)])))

# fit model
formula <- paste('GROWTH_RATE_3YR_mean_2014~',paste(colnames(magent[,c(-1,-10,-17,-24,-31,-38)]),collapse = "+"))
anova <- aov(GROWTH_RATE_3YR_mean_2014~.,magent[,c(-1,-17,-24,-31,-38)])
summary(anova)

linear <- lm(GROWTH_RATE_3YR_mean_2014~GROWTH_RATE_3YR_mean_2013 ,magent)
summary(linear)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(linear, las = 1)

#Getting collinearity
library(car)
vif(linear)
outlierTest(linear)
outliers = as.numeric(names(outlierTest(linear)[[1]]))

#interaction Between Quantity and the Amount
formula = paste(formula , '+I(WRTN_PREM_AMT_mean_2008/(QTY_Growth_mean_2008+1))',collapse = "+")
linear <- lm(as.formula(formula) ,magent[-outliers,])
summary(linear)

#Robust linear regreesoion
Robust <- rlm(as.formula(formula) ,magent)
summary(Robust)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(Robust, las = 1)


#Quantilt linear Regression
library(quantreg)
qr<- rq(GROWTH_RATE_3YR_mean_2014~. ,data = magent[,c(-1,-17,-24,-31,-38)])
summary(qr)


table(is.na(good_data$PREV_WRTN_PREM_AMT) & is.na(good_data$PREV_POLY_INFORCE_QTY))

rm(agent)

# Final_data <- knnImputation(good_data[,numerical],scale = T,k = 10)
# Final_data <- cbind(Final_data,good_data[,categorical])
# system.time(imp <- mice(good_data[,numerical], m=5, maxit=2, printFlag=TRUE) )
# Final_data <- complete(imp, "long", include=TRUE)
# write.csv(Final_data,"final.csv",row.names = F)

clean <- na.omit(good_data)

pair <- md.pairs(good_data)
pair <- data.frame(pair[67:88])

Final_data$Missing_Prev <- ifelse(is.na(good_data$PREV_WRTN_PREM_AMT) & is.na(good_data$PREV_POLY_INFORCE_QTY),1,0)
Final_data$Missing_gain <- ifelse(is.na(good_data$LOSS_RATIO_3YR) & is.na(good_data$GROWTH_RATE_3YR),1,0)
Final_data$Missing_agent <- ifelse(is.na(good_data$AGENCY_APPOINTMENT_YEAR),1,0)
# 
# 
# d = agent[,c('AGENCY_ID','PROD_ABBR','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','RETENTION_POLY_QTY')]
# # magent <- dcast(d, AGENCY_ID+PROD_ABBR~STAT_PROFILE_DATE_YEAR,value.var = 'GROWTH_RATE_3YR',mean)
# magent <- dcast(setDT(d), AGENCY_ID+PROD_ABBR+PROD_LINE+STATE_ABBR+AGENCY_APPOINTMENT_YEAR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','RETENTION_POLY_QTY'),mean)
# 
# # colnames(magent)[3:12] <- paste('Y',colnames(magent)[3:12],sep  = "")
# a <- na.omit(magent)
# 
# 
# # fit model
# linear <- lm(Y2014~.,agent=magent[,6:12])
# summary(linear)
# stepAIC(linear,direction = 'both')

#########################Details on clean data################################

clean$GROWTH_RATE_3YR <- ifelse(clean$GROWTH_RATE_3YR<(-0.09),"low",ifelse(clean$GROWTH_RATE_3YR>0.05,"High","No_Change"))
clean$GROWTH_RATE_3YR <- as.factor(clean$GROWTH_RATE_3YR)
categorical <- colnames(good_data[,c(1,2,3,4,16,21,22)])
numerical <- colnames(good_data[-c(1,2,3,4,16,21,22)])


c <- data.frame(lapply(clean[,categorical] , factor))
clean <- cbind(clean[,numerical],c)
rm(c)
clean$range <- clean$MAX_AGE - clean$MIN_AGE

anova <- aov(as.numeric(GROWTH_RATE_3YR)~.-AGENCY_ID,data=clean)
summary(anova)

corrplot::corrplot(cor(clean[,numerical]))

library(ggplot2)
ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=PREV_WRTN_PREM_AMT,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=AGENCY_APPOINTMENT_YEAR,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_y_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=NB_WRTN_PREM_AMT,y=POLY_INFORCE_QTY*2))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_log10()+scale_y_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=PREV_WRTN_PREM_AMT,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_sqrt()

train <- clean[clean$STAT_PROFILE_DATE_YEAR<2014,]
#Validation <- clean[clean$STAT_PROFILE_DATE_YEAR==2013,]
Test <- clean[clean$STAT_PROFILE_DATE_YEAR==2014,]
install.packages('caret')

fitControl <- trainControl(method = "cv",
                           number = 3,
                           repeats = 1)

rpart <- train(GROWTH_RATE_3YR~.-AGENCY_ID
               ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
               method = 'rpart',
               tuneGrid = expand.grid(cp=c(0.00005,0.0001,.0005)),
               trControl = fitControl)
rpart

C50 <- train(GROWTH_RATE_3YR~.-AGENCY_ID
             ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
             method = 'C5.0',
             tuneLength = 3,
             trControl = fitControl)
C50

svm <- train(GROWTH_RATE_3YR~.-AGENCY_ID
             ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
             method = 'svmRadial',
             tuneLength = 3,
             trControl = fitControl)
svm

ada<- train(GROWTH_RATE_3YR~.-AGENCY_ID
            ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
            method = 'ada',
            tuneLength = 3,
            trControl = fitControl)
ada

adaboost <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                  ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
                  method = 'AdaBag',
                  tuneLength = 3,
                  trControl = fitControl)

Extratrees <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                    ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
                    method = 'extraTrees',
                    tuneLength = 3,
                    trControl = fitControl)

glmboost <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                  ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
                  method = 'glmboost',
                  tuneLength = 3,
                  trControl = fitControl)

glmnet <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
                method = 'glmnet',
                tuneLength = 3,
                trControl = fitControl)

nnet <- train(GROWTH_RATE_3YR~.-AGENCY_ID
              ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
              method = 'nnet',
              tuneLength = 3,
              trControl = fitControl)

knn <- train(GROWTH_RATE_3YR~.-AGENCY_ID
             ,data=rbind(train,Validation),
             method = 'knn',
             tuneLength = 3,
             trControl = fitControl)

Naive <- train(GROWTH_RATE_3YR~.-AGENCY_ID
               ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
               method = 'nb',
               tuneLength = 3,
               trControl = fitControl)

parRF <- train(GROWTH_RATE_3YR~.-AGENCY_ID
               ,data=rbind(train,Validation),
               method = 'parRF',
               tuneLength = 3,
               trControl = fitControl)

randomGLM <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                   ,data=rbind(train,Validation),
                   method = 'randomGLM',
                   tuneLength = 3,
                   trControl = fitControl)

treebag <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                 ,data=rbind(train,Validation),
                 method = 'treebag',
                 tuneLength = 3,
                 trControl = fitControl)

RandomForest <- train(GROWTH_RATE_3YR~.-AGENCY_ID
                      ,data=train[train$STAT_PROFILE_DATE_YEAR>2012,],
                      method = 'rf',
                      tuneLength = 3,
                      ntree = 20,
                      do.trace = T,
                      trControl = fitControl)

library(rpart.plot)
library(rattle)
fancyRpartPlot(rpart$finalModel)
rpart

pred_test <- predict(rpart,Test)
regr.eval(Test$GROWTH_RATE_3YR, pred_test)
variable_imp3 <- data.frame(rpart$variable.importance)

############################Predictions on imputed data#########################
c <- data.frame(apply(good_data[,categorical],2,function(x) as.factor(x)))
Imputed <- cbind(Final_data,c)
rm(c)
train <- Imputed[Imputed$STAT_PROFILE_DATE_YEAR<2013,]
Validation <- Imputed[Imputed$STAT_PROFILE_DATE_YEAR==2013,]
Test <- Imputed[Imputed$STAT_PROFILE_DATE_YEAR==2014,]


rpart <- rpart(GROWTH_RATE_3YR~.,data=train)
pred_validation <- predict(rpart,Validation)
regr.eval(Validation$GROWTH_RATE_3YR, pred_validation)

pred_test <- predict(rpart,Test)
regr.eval(Test$GROWTH_RATE_3YR, pred_test)



