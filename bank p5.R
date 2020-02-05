library(dplyr)
setwd("C:/Users/venugopal/Downloads")
getwd()

train=read.csv("bank-full_train.csv",stringsAsFactors = FALSE,header = T ) #31647,18
test=read.csv("bank-full_test.csv",stringsAsFactors = FALSE,header = T ) #13564,17

#Step 1: Imputing NA values in the datasets.

apply(train,2,function(x)sum(is.na(x)))

#There exist no NA values in train dataset.

apply(test,2,function(x)sum(is.na(x)))

##Step 2:Data Preparation
##Combining both train n test datasets prior to data preparation.

test$y=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))

glimpse(all_data) #45211,19var

##Creating dummy variables by combining similar categories for variable job(char type)

t=table(all_data$job)
sort(t)

final=round(prop.table(table(all_data$job,all_data$y),1)*100,1)
final


s=addmargins(final,2) #add margin across Y
sort(s[,1])

View(s)


all_data=all_data %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(all_data)

##Making dummies for variable marital

t=table(all_data$marital)
sort(t)

all_data=all_data %>% 
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)
glimpse(all_data)

##Making dummies for variable education

t=table(all_data$education)
sort(t)

all_data=all_data %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)
glimpse(all_data)


##Making dummies for varible default

table(all_data$default)

all_data$default=as.numeric(all_data$default=="yes")

##Making dummies for variable housing

table(all_data$housing)

all_data$housing=as.numeric(all_data$housing=="yes")
glimpse(all_data)

##Making dummies for variable loan

table(all_data$loan)

all_data$loan=as.numeric(all_data$loan=="yes")
glimpse(all_data)

##Making dummies for variable contact

t=table(all_data$contact)
sort(t)

all_data=all_data %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)
glimpse(all_data)

##Making dummies for variable month

table(all_data$month)

#lets convert into percentage across months.

finalmnth=round(prop.table(table(all_data$month,all_data$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])


#may taken as base var

all_data=all_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)
glimpse(all_data)

##Making dummies for variable outcome

t=table(all_data$poutcome)
sort(t)

#unknown as base var

all_data=all_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)
glimpse(all_data)

##Thus data preparation is done and we will now seperate both test n train data.

glimpse(all_data)
table(all_data$y)
table(train$y)
all_data$y=as.numeric(all_data$y=="yes")
table(all_data$y)
glimpse(all_data)


##Separating test and train:
  
  train=all_data %>% 
  filter(data=='train') %>% 
  select(-data) #31647,34

test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-y)

##Lets view the structure of test n train datasets:
  
  glimpse(train) #31647,34
  
  glimpse(test) #13564,33
  
  ##now lets divide the train dataset in the ratio 75:25.
  
  set.seed(5)
  s=sample(1:nrow(train),0.75*nrow(train))
  train_75=train[s,] #23735,34
  test_25=train[-s,]#7912,34
  
  
  #3.Model Buliding
  library(car)
  for_vif=lm(y~.,data = train)
  summary(for_vif)
  
  ##In order to take care of multi collinearity,we remove variables whose VIF>5,as follows:
    
    t=vif(for_vif)
  sort(t,decreasing = T)[1:5]
  
  ##Removing variable edu_sec
  
  for_vif=lm(y~.-edu_sec,data=train)
  t=vif(for_vif)
  sort(t,decreasing = T)[1:5]
  
  summary(for_vif)
  #now lets remove edu_sec in train data
  colnames(train)
fit_train = train %>%
  select(-edu_sec)
colnames(fit_train)

fit=glm(y~.,family = "binomial",data=fit_train) #32 predictor var
summary(fit) 

##Now lets remove all variables whose p value is >0.05 using step function.

fit=step(fit)

##lets check the remaining significant variables

names(fit$coefficients) #25 significant var

##lets build final logistic model on significant variables on dataset fit_train

fit_final=glm(y~balance + housing + loan + duration + campaign + ID + 
                job_3 + job_5 + divorced + single + edu_primary + 
                co_cellular + co_tel + month_1 + month_2 + month_3 + month_4 + 
                month_5 + month_6 + poc_success + poc_failure + poc_other ,data=fit_train,family="binomial")
summary(fit_final)
names(fit_final$coefficients) 

##Thus logistic regression model is successfully built.
##Now lets make predict scores

train$score=predict(fit_final,newdata = train,type="response")
#score means Pi
#lets see how the score (Pi ) behaves.

library(ggplot2)
ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()
  

##Step 4. Finding Cutoff value and Perfomance measurements of the model.
##lets find cutoff based on these probability scores.

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
for (i in cutoffs){
  predicted=as.numeric(train$score>i)
  
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}
## lets remove the dummy data cotaining top row in data frame cutoff_data
cutoff_data=cutoff_data[-1,]
#we now have 100 obs in df cutoff_data

#lets calculate the performance measures:sensitivity,specificity,accuracy, KS and precision.
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP, #total positives and negatives
         Sn=TP/P, #sensitivity
         Sp=TN/N, #specificity
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P
  ) %>% 
  select(-P,-N)
#lets view cutoff dataset:
  
  #View(cutoff_data)
  #Lets find cutoff value based on ks MAXIMUM.
KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
KS_cutoff

test$score=predict(fit_final,newdata =test,type = "response")

test$left=as.numeric(test$score>KS_cutoff)#if score is greater dan cutoff then true(1) else false(0)
table(test$left)

test$leftfinal=factor(test$left,levels = c(0,1),labels=c("no","yes"))
table(test$leftfinal)

write.csv(test$leftfinal,"P5_sub_1.csv")
mean