#clearing the global enviornment
rm(list = ls())

#loading the required packages

library(lubridate)
library(dplyr)
library(ggplot2)
library(lattice)
library(InformationValue)
library(ROCR)
library(caret)
library(ROSE)
library(psych)
library(class)
library(tidyr) 
library(stringi)

# #setting the directory
# setwd("F:/hackathon/L&T")

#Reading the file
train <- read.csv("F:/hackathon/L&T/train.csv")
test <- read.csv("F:/hackathon/L&T/test.csv")

#Understanding the data
str(train)
head(train)
tail(train)
summary(train)
names(train)
View(train)
dim(train)
describe(train)

#converting the DateOfBirth variable into age with system date

train$Date.of.Birth <- dmy(train$Date.of.Birth)
train$Date.of.Birth <- as.numeric(Sys.Date())-as.numeric(train$Date.of.Birth)
class(train$Date.of.Birth)

train$Date.of.Birth <- train$Date.of.Birth/365
Neg <- as.numeric(Sys.Date())/365
train$Date.of.Birth <- ifelse(train$Date.of.Birth<0,Neg,train$Date.of.Birth)

#Converting the DisbursalDate
train$DisbursalDate <- dmy(train$DisbursalDate)
train$DisbursalDate <- as.numeric(Sys.Date())-as.numeric(train$DisbursalDate)
train$DisbursalDate <- train$DisbursalDate/365
train$DisbursalDate <- ifelse(train$DisbursalDate<0,Neg,train$DisbursalDate)

    #making a new variable for the flag variables
    # train$All_Flag <- (train$MobileNo_Avl_Flag+train$Aadhar_flag+train$PAN_flag+train$VoterID_flag+
    #                      train$Driving_flag+train$Passport_flag)/6


# Encoding the Employment type as factor 
train[train == ""] <- NA # putting NA's into blank spaces
colSums(is.na(train)) #Checking that blank has been filled with na's or not

train$Employment.Type <- as.numeric(train$Employment.Type,
                                              levels = c('Self employed','Salaried'),
                                              labels = c(1,2))

#Converting perform_CNS.score.Description

train$PERFORM_CNS.SCORE.DESCRIPTION <- ifelse(train$PERFORM_CNS.SCORE.DESCRIPTION == "A-Very Low Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION == "B-Very Low Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION == "C-Very Low Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION == "D-Very Low Risk","1",
                                              ifelse(train$PERFORM_CNS.SCORE.DESCRIPTION == "E-Low Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION =="F-Low Risk"| train$PERFORM_CNS.SCORE.DESCRIPTION =="G-Low Risk","2",
                                                     ifelse(train$PERFORM_CNS.SCORE.DESCRIPTION == "H-Medium Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION == "I-Medium Risk","3",
                                                            ifelse(train$PERFORM_CNS.SCORE.DESCRIPTION == "J-High Risk"| train$PERFORM_CNS.SCORE.DESCRIPTION == "K-High Risk","4",
                                                                   ifelse(train$PERFORM_CNS.SCORE.DESCRIPTION == "L-Very High Risk"|train$PERFORM_CNS.SCORE.DESCRIPTION == "M-Very High Risk","5","6")))))


train$PERFORM_CNS.SCORE.DESCRIPTION <- as.numeric(train$PERFORM_CNS.SCORE.DESCRIPTION)
str(train)


#conversion of Average account age into years

train$AVERAGE.ACCT.AGE1 <- substr(train$AVERAGE.ACCT.AGE,1,1)
train$AVERAGE.ACCT.AGE2 <- ifelse(stri_length(train$AVERAGE.ACCT.AGE) == 9,substr(train$AVERAGE.ACCT.AGE,5,6),substr(train$AVERAGE.ACCT.AGE,5,7))
class(train$AVERAGE.ACCT.AGE2)
train$AVERAGE.ACCT.AGE1 <- as.numeric(train$AVERAGE.ACCT.AGE1)
train$AVERAGE.ACCT.AGE2 <- as.numeric(train$AVERAGE.ACCT.AGE2)

train$AVERAGE.ACCT.AGE <- train$AVERAGE.ACCT.AGE1 + train$AVERAGE.ACCT.AGE2/12
train$AVERAGE.ACCT.AGE1 <- NULL
train$AVERAGE.ACCT.AGE2 <- NULL

#conversion of Credit history length into years
train$CREDIT.HISTORY.LENGTH1 <- substr(train$CREDIT.HISTORY.LENGTH,1,1)
train$CREDIT.HISTORY.LENGTH2 <- ifelse(stri_length(train$CREDIT.HISTORY.LENGTH) == 9,substr(train$CREDIT.HISTORY.LENGTH,5,6),substr(train$CREDIT.HISTORY.LENGTH,5,7))
train$CREDIT.HISTORY.LENGTH1 <- as.numeric(train$CREDIT.HISTORY.LENGTH1)
train$CREDIT.HISTORY.LENGTH2 <- as.numeric(train$CREDIT.HISTORY.LENGTH2)

train$CREDIT.HISTORY.LENGTH <- train$CREDIT.HISTORY.LENGTH1+train$CREDIT.HISTORY.LENGTH2/12
train$CREDIT.HISTORY.LENGTH1 <- NULL
train$CREDIT.HISTORY.LENGTH2 <- NULL


#Removing the not required variables
train$UniqueID <- NULL
train$branch_id <- NULL
train$State_ID <- NULL 
train$Employee_code_ID <- NULL
train$supplier_id <- NULL
train$manufacturer_id <- NULL
train$Current_pincode_ID <- NULL
train$NO.OF_INQUIRIES <- NULL

    ###As i have made a new variable from all this hence removing it
    # train$MobileNo_Avl_Flag <- NULL
    # train$Aadhar_flag <- NULL
    # train$VoterID_flag <-NULL
    # train$PAN_flag <- NULL
    # train$Driving_flag <- NULL
    # train$Passport_flag <- NULL

#creating the function for audit report on Numerical variabls

mystat_num <- function(x){
  n= length(x)
  nmiss=sum(is.na(x))
  nmiss_pct=mean(is.na(x))
  nmissprop <- (nmiss/n)*100
  sum=sum(x,na.rm = T)
  mean=mean(x,na.rm = T)
  median=median(x,na.rm = T)
  std_dev=sd(x,na.rm = T)
  Cv= sd(x,na.rm = T)/mean(x,na.rm = T)
  variance= var(x,na.rm = T)
  range=max(x,na.rm = T)-min(x,na.rm = T)
  pctl = quantile(x,p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n, Nmiss=nmiss, Nmiss_perc=nmiss_pct,prop=nmissprop, Sum=sum, Mean=mean, Median=median, Std_dev=std_dev,
           CV=Cv, Variance=variance, range=range, pctl=pctl))
}


str(train)

#Vector of numerical variables
data <- sapply(train,FUN = is.numeric)
numdata <- train[data]

#Applying above defined function on variables
numstat <- data.frame(sapply(numdata,FUN=mystat_num))

#missing value treatment for numerical variable
sapply(train,function(x) sum(is.na(x))) 

miss_treat_num <- function(x){
  x[is.na(x)]=median(x,na.rm = T)  #replace missing value with mean
  return(x)
}

train1 <- data.frame(apply(numdata,2,FUN = miss_treat_num))
sapply(train1,function(x) sum(is.na(x))) 

#Outlier treatmet
outlier_treat <- function(x){
  UC1=quantile(x,p=0.99,na.rm = T)
  LC1=quantile(x,p=0.01,na.rm = T)

  x=ifelse(x>UC1,UC1,x)
  x=ifelse(x<LC1,LC1,x)
  return(x)
}

train1 <- data.frame(sapply(train1,FUN = outlier_treat))
View(train1)


#corelation
cor_mat<-data.frame(cor(train1))
write.csv(cor_mat, "cor_mat.csv")

str(train1)

#converting loans_default into factors
train1$loan_default <- as.factor(train1$loan_default)

#Building models for training datasets
names(train)
fit <- glm(loan_default~.,data = train1,family = binomial(logit))

#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

#now prediction is done
train1 <- cbind(train1,prob=predict(fit,type = "response"))
View(train1)

Concordance(train1$loan_default,train1$prob)

somersD(train1$loan_default,train1$prob)
table(train1$loan_default)
AUROC(train1$loan_default,train1$prob)

cut1 <- optimalCutoff(train1$loan_default,train1$prob, optimiseFor = "Both", returnDiagnostics = T)

ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)

ks_table<-ks_stat(train1$loan_default, train1$Prob, returnKSTable=TRUE)


#--------------------------------------------------------------------------------------------------------------------
###Prediction on test Data


#converting the DateOfBirth variable into age with system date
test$Date.of.Birth <- dmy(test$Date.of.Birth)
test$Date.of.Birth <- as.numeric(Sys.Date())-as.numeric(test$Date.of.Birth)
class(test$Date.of.Birth)

test$Date.of.Birth <- test$Date.of.Birth/365
Neg <- as.numeric(Sys.Date())/365
test$Date.of.Birth <- ifelse(test$Date.of.Birth<0,Neg,test$Date.of.Birth)

#Converting the DisbursalDate
test$DisbursalDate <- dmy(test$DisbursalDate)
test$DisbursalDate <- as.numeric(Sys.Date())-as.numeric(test$DisbursalDate)
test$DisbursalDate <- test$DisbursalDate/365
test$DisbursalDate <- ifelse(test$DisbursalDate<0,Neg,test$DisbursalDate)

# Encoding the Employment type as factor 
test[test == ""] <- NA # putting NA's into blank spaces
colSums(is.na(test)) #Checking that blank has been filled with na's or not

test$Employment.Type <- as.numeric(test$Employment.Type,
                                    levels = c('Self employed','Salaried'),
                                    labels = c(1,2))

#Converting perform_CNS.score.Description

test$PERFORM_CNS.SCORE.DESCRIPTION <- ifelse(test$PERFORM_CNS.SCORE.DESCRIPTION == "A-Very Low Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION == "B-Very Low Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION == "C-Very Low Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION == "D-Very Low Risk","1",
                                              ifelse(test$PERFORM_CNS.SCORE.DESCRIPTION == "E-Low Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION =="F-Low Risk"| test$PERFORM_CNS.SCORE.DESCRIPTION =="G-Low Risk","2",
                                                     ifelse(test$PERFORM_CNS.SCORE.DESCRIPTION == "H-Medium Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION == "I-Medium Risk","3",
                                                            ifelse(test$PERFORM_CNS.SCORE.DESCRIPTION == "J-High Risk"| test$PERFORM_CNS.SCORE.DESCRIPTION == "K-High Risk","4",
                                                                   ifelse(test$PERFORM_CNS.SCORE.DESCRIPTION == "L-Very High Risk"|test$PERFORM_CNS.SCORE.DESCRIPTION == "M-Very High Risk","5","6")))))


test$PERFORM_CNS.SCORE.DESCRIPTION <- as.numeric(test$PERFORM_CNS.SCORE.DESCRIPTION)

#conversion of Average account age into years

test$AVERAGE.ACCT.AGE1 <- substr(test$AVERAGE.ACCT.AGE,1,1)
test$AVERAGE.ACCT.AGE2 <- ifelse(stri_length(test$AVERAGE.ACCT.AGE) == 9,substr(test$AVERAGE.ACCT.AGE,5,6),substr(test$AVERAGE.ACCT.AGE,5,7))
class(test$AVERAGE.ACCT.AGE2)
test$AVERAGE.ACCT.AGE1 <- as.numeric(test$AVERAGE.ACCT.AGE1)
test$AVERAGE.ACCT.AGE2 <- as.numeric(test$AVERAGE.ACCT.AGE2)

test$AVERAGE.ACCT.AGE <- test$AVERAGE.ACCT.AGE1 + test$AVERAGE.ACCT.AGE2/12
test$AVERAGE.ACCT.AGE1 <- NULL
test$AVERAGE.ACCT.AGE2 <- NULL

#conversion of Credit history length into years
test$CREDIT.HISTORY.LENGTH1 <- substr(test$CREDIT.HISTORY.LENGTH,1,1)
test$CREDIT.HISTORY.LENGTH2 <- ifelse(stri_length(test$CREDIT.HISTORY.LENGTH) == 9,substr(test$CREDIT.HISTORY.LENGTH,5,6),substr(test$CREDIT.HISTORY.LENGTH,5,7))
test$CREDIT.HISTORY.LENGTH1 <- as.numeric(test$CREDIT.HISTORY.LENGTH1)
test$CREDIT.HISTORY.LENGTH2 <- as.numeric(test$CREDIT.HISTORY.LENGTH2)

test$CREDIT.HISTORY.LENGTH <- test$CREDIT.HISTORY.LENGTH1+test$CREDIT.HISTORY.LENGTH2/12
test$CREDIT.HISTORY.LENGTH1 <- NULL
test$CREDIT.HISTORY.LENGTH2 <- NULL



#Removing the not required variables
test$UniqueID <- NULL
test$branch_id <- NULL
test$State_ID <- NULL 
test$Employee_code_ID <- NULL
test$supplier_id <- NULL
test$manufacturer_id <- NULL
test$Current_pincode_ID <- NULL
test$NO.OF_INQUIRIES <- NULL

#creating the function for audit report on Numerical variabls

mystat_num <- function(x){
  n= length(x)
  nmiss=sum(is.na(x))
  nmiss_pct=mean(is.na(x))
  nmissprop <- (nmiss/n)*100
  sum=sum(x,na.rm = T)
  mean=mean(x,na.rm = T)
  median=median(x,na.rm = T)
  std_dev=sd(x,na.rm = T)
  Cv= sd(x,na.rm = T)/mean(x,na.rm = T)
  variance= var(x,na.rm = T)
  range=max(x,na.rm = T)-min(x,na.rm = T)
  pctl = quantile(x,p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n, Nmiss=nmiss, Nmiss_perc=nmiss_pct,prop=nmissprop, Sum=sum, Mean=mean, Median=median, Std_dev=std_dev,
           CV=Cv, Variance=variance, range=range, pctl=pctl))
}



#Vector of numerical variables
data <- sapply(test,FUN = is.numeric)
numdata <- test[data]

#Applying above defined function on variables
numstat <- data.frame(sapply(numdata,FUN=mystat_num))
str(test)

#missing value treatment for numerical variable
sapply(test,function(x) sum(is.na(x))) 

miss_treat_num <- function(x){
  x[is.na(x)]=median(x,na.rm = T)  #replace missing value with mean
  return(x)
}

test1 <- data.frame(apply(numdata,2,FUN = miss_treat_num))
sapply(test1,function(x) sum(is.na(x))) 

#Outlier treatmet
outlier_treat <- function(x){
  UC1=quantile(x,p=0.99,na.rm = T)
  LC1=quantile(x,p=0.01,na.rm = T)
  
  x=ifelse(x>UC1,UC1,x)
  x=ifelse(x<LC1,LC1,x)
  return(x)
}

test1 <- data.frame(sapply(test1,FUN = outlier_treat))
View(test1)

#corelation
cor_mat<-data.frame(cor(test1))
write.csv(cor_mat, "cor_mat.csv")

str(test1)


#validating Building
test2<- cbind(test1, Prob=predict(fit,newdata=test1, type="response")) 
View(test2)
sapply(test2,function(x) sum(is.na(x))) 
test2$prob <- ifelse(test2$Prob>0.2154124, 1,0)
sum(test2$prob)

#Creating Deciles
decLocations <- quantile(test2$Prob, probs = seq(0.1,0.9,by=0.1),na.rm = T)
test$decile <- findInterval(test$Prob,c(-Inf,decLocations, Inf))
names(test)

#Decile Analysis Reports
require(sqldf)

fit_test_DA <- sqldf("select decile, count(decile) as count, min(Prob1) as Min_prob
                     , max(Prob1) as max_prob 
                     from test2
                     group by decile
                     order by decile desc")

write.csv(fit_test_DA,"fit_test_DA1.csv",row.names = F)

test <- read.csv("F:/hackathon/L&T/test.csv")
loan_pred <- as.data.frame(cbind(test$UniqueID,test2$prob))
names(loan_pred) <- c("UniqueID","loan_default")
write.csv(loan_pred, "loan_pred.csv",row.names = F)
#--------------------------------------------------------------------



