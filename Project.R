rm(list=ls(all=TRUE))
setwd("~/INSOFE/Project")

library(MASS)
library(mice)
library(rpart)
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

agent <- read.csv('agency_final.csv',stringsAsFactors = F,header = T,na.strings = "99999")
#Final_data <-  read.csv('final.csv',header=T)
agent <- agent[agent$STAT_PROFILE_DATE_YEAR!=2015,]

#Missing values wit respective to Growth_rate_3year
Year_wise(agent,'GROWTH_RATE_3YR')

#We have 2005,2006,2007 with all NA's
agent <- agent[agent$STAT_PROFILE_DATE_YEAR>2007,]
agent <- agent[!((is.na(agent$GROWTH_RATE_3YR))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]
agent <- agent[!((is.na(agent$WRTN_PREM_AMT))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]
agent <- agent[!((is.na(agent$POLY_INFORCE_QTY))*(agent$STAT_PROFILE_DATE_YEAR ==2014)),]

#Missing values wit respective to LOSS_RATIO
Year_wise(agent,'LOSS_RATIO')
table(agent$LOSS_RATIO==99998)   #Positive loss 
median(agent[agent$LOSS_RATIO>0 & agent$LOSS_RATIO<9000,'LOSS_RATIO'],na.rm=T)
agent$LOSS_RATIO[agent$LOSS_RATIO==99998] <- 0.4824585

table(agent$LOSS_RATIO==99997)  #Negative Loss
median(agent[agent$LOSS_RATIO<0,'LOSS_RATIO'],na.rm=T)
agent$LOSS_RATIO[agent$LOSS_RATIO==99997] <- -0.2549541

#Imputing the 3 year growth and loss ratio growth AGENCY_APPOINTMENT_YEAR
new_agent <- which(agent$STAT_PROFILE_DATE_YEAR < agent$AGENCY_APPOINTMENT_YEAR+3)
agent[new_agent,'LOSS_RATIO_3YR'] <- 0
agent[new_agent,'GROWTH_RATE_3YR'] <- 0

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
magent <- dcast(setDT(d), AGENCY_ID+PROD_ABBR+STATE_ABBR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','POLY_INFORCE_QTY','WRTN_PREM_AMT','QTY_Growth','AMOUNT_Growth'),fun=mean)
magent <- data.frame(magent)
column_wise(magent)
pattern = rsem::rsem.pattern(magent)$mispat


library(ggplot2)
ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=PREV_WRTN_PREM_AMT,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=AGENCY_APPOINTMENT_YEAR,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_y_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=NB_WRTN_PREM_AMT,y=POLY_INFORCE_QTY*2))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_log10()+scale_y_sqrt()

ggplot(clean[clean$STAT_PROFILE_DATE_YEAR>2010,],aes(x=PREV_WRTN_PREM_AMT,y=POLY_INFORCE_QTY))+
  geom_point(aes(color=GROWTH_RATE_3YR))+scale_x_sqrt()


#Plotting
corrplot::corrplot(cor(na.omit(magent[,c(-1,-2,-3)])))

# fit model
formula <- paste('GROWTH_RATE_3YR_mean_2014~',paste(colnames(magent[,c(-1,-10,-17,-24,-31,-38)]),collapse = "+"))
anova <- aov(GROWTH_RATE_3YR_mean_2014~.,magent[,c(-1,-17,-24,-31,-38)])
summary(anova)

#Imputation
library(mice)
new <- mice(magent[,c(-1,-17,-24,-31,-38)])

linear <- lm(GROWTH_RATE_3YR_mean_2014~. ,magent[,c(-1,-17,-24,-31,-38)])
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

#########################Details on clean data################################


fitControl <- trainControl(method = "cv",
                           number = 3,
                           repeats = 3)

rpart <- train(GROWTH_RATE_3YR_mean_2014~.
               ,data=magent,
               method = 'rpart',
               tuneGrid = expand.grid(cp=c(0.0001,0.0002,0.0004,0.00001,0.00005)),
               trControl = fitControl)
rpart
plotcp(rpart$finalModel)

glmboost <-  train(GROWTH_RATE_3YR_mean_2014~.
                   ,data=magent,
                  method = 'glmboost',
                  tuneLength = 10,
                  trControl = fitControl)
glmboost

glmnet <-  train(GROWTH_RATE_3YR_mean_2014~.
                 ,data=magent,
                method = 'glmnet',
                tuneLength = 10,
                trControl = fitControl)
glmnet

nnet <-  train(GROWTH_RATE_3YR_mean_2014~.
               ,data=magent,
              method = 'nnet',
              tuneLength = 10,
              trControl = fitControl)
nnet

RandomForest <- train(GROWTH_RATE_3YR_mean_2014~.
                    ,data=magent,
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



