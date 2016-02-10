rm(list=ls(all=TRUE))
setwd("~/INSOFE/Project")

library(data.table)
library(glmnet)
library(MASS)
library(DMwR)


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

categorical <- colnames(good_data[,c(1,2,3,4,21,22)])
numerical <- colnames(good_data[-c(1,2,3,4,21,22)])

table(is.na(good_data$PREV_WRTN_PREM_AMT) & is.na(good_data$PREV_POLY_INFORCE_QTY))

Final_data <- knnImputation(good_data[,numerical],scale = T,k = 10)
Final_data <- cbind(Final_data,good_data[,categorical])

Final_data$Missing_Prev <- ifelse(is.na(good_data$PREV_WRTN_PREM_AMT) & is.na(good_data$PREV_POLY_INFORCE_QTY),1,0)
Final_data$Missing_gain <- ifelse(is.na(good_data$LOSS_RATIO_3YR) & is.na(good_data$GROWTH_RATE_3YR),1,0)
Final_data$Missing_agent <- ifelse(is.na(good_data$AGENCY_APPOINTMENT_YEAR),1,0)


d = agent[,c('AGENCY_ID','PROD_ABBR','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','RETENTION_POLY_QTY')]
# magent <- dcast(d, AGENCY_ID+PROD_ABBR~STAT_PROFILE_DATE_YEAR,value.var = 'GROWTH_RATE_3YR',mean)
magent <- dcast(setDT(d), AGENCY_ID+PROD_ABBR+PROD_LINE+STATE_ABBR+AGENCY_APPOINTMENT_YEAR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','RETENTION_POLY_QTY'),mean)

# colnames(magent)[3:12] <- paste('Y',colnames(magent)[3:12],sep  = "")
a <- na.omit(magent)


# fit model
linear <- lm(Y2014~.,agent=magent[,6:12])
summary(linear)
stepAIC(linear,direction = 'both')

train <- good_data[good_data$STAT_PROFILE_DATE_YEAR<2013,]
Validation <- good_data[good_data$STAT_PROFILE_DATE_YEAR==2013,]
Test <- good_data[good_data$STAT_PROFILE_DATE_YEAR==2014,]







