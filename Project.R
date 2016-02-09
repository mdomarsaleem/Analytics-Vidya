rm(list=ls(all=TRUE))
setwd("~/INSOFE/Project")

library(data.table)
library(glmnet)
library(MASS)
agent <- read.csv('agency_final.csv',stringsAsFactors = F,header = T)
agent <- agent[agent$STAT_PROFILE_DATE_YEAR!=2015,]

agent[agent == 99999] <- NA

for(i in levels(as.factor(agent$STAT_PROFILE_DATE_YEAR))){
  cat(i,table(is.na(agent[agent$STAT_PROFILE_DATE_YEAR==i,])),'\n')
}

app <- data.frame(apply(agent,2,function(x) sum(is.na(x))))

#Variables with least missing values
good_variables <- row.names.data.frame(app)[app<40000]
app
#good data set
good_data <- agent[,good_variables]



d = agent[,c('AGENCY_ID','PROD_ABBR','GROWTH_RATE_3YR','STAT_PROFILE_DATE_YEAR','RETENTION_POLY_QTY')]
# magent <- dcast(d, AGENCY_ID+PROD_ABBR~STAT_PROFILE_DATE_YEAR,value.var = 'GROWTH_RATE_3YR',mean)
magent <- dcast(setDT(d), AGENCY_ID+PROD_ABBR+PROD_LINE+STATE_ABBR+AGENCY_APPOINTMENT_YEAR~STAT_PROFILE_DATE_YEAR,value.var = c('GROWTH_RATE_3YR','RETENTION_POLY_QTY'),mean)

# colnames(magent)[3:12] <- paste('Y',colnames(magent)[3:12],sep  = "")
a <- na.omit(magent)


# fit model
linear <- lm(Y2014~.,agent=magent[,6:12])
summary(linear)
stepAIC(linear,direction = 'both')


Validation <- good_data[good_data$STAT_PROFILE_DATE_YEAR=2013,]
Test <- good_data[good_data$STAT_PROFILE_DATE_YEAR=2014,]







