install.packages("dplyr")
library(dplyr)
setwd("D:/edvancer/r files/Data/banking/")
getwd()
train=read.csv("bank-full_train.csv",stringsAsFactors = FALSE,header = T ) #31647,18
test=read.csv("bank-full_test.csv",stringsAsFactors = FALSE,header = T ) 
apply(train,2,function(x)sum(is.na(x)))
apply(test,2,function(x)sum(is.na(x)))
test$y=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))
glimpse(all_data)
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
t=table(all_data$marital)
sort(t)
all_data=all_data %>% 
  mutate(divorced=as.numeric(marital=="divorced"),
         single=as.numeric(marital=="single")
  ) %>% 
  select(-marital)
glimpse(all_data)
t=table(all_data$education)
sort(t)
all_data=all_data %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)
glimpse(all_data)
table(all_data$default)
all_data$default=as.numeric(all_data$default=="yes")
table(all_data$housing)
all_data$housing=as.numeric(all_data$housing=="yes")
glimpse(all_data)
table(all_data$loan)
all_data$loan=as.numeric(all_data$loan=="yes")
glimpse(all_data)
t=table(all_data$contact)
sort(t)
all_data=all_data %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)
glimpse(all_data)
table(all_data$month)
finalmnth=round(prop.table(table(all_data$month,all_data$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])
all_data=all_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)
glimpse(all_data)
t=table(all_data$poutcome)
sort(t)
all_data=all_data %>% 
  mutate(poc_success=as.numeric(poutcome =="success"),
         poc_failure=as.numeric(poutcome =="failure"),
         poc_other=as.numeric(poutcome =="other"))%>% 
  select(-poutcome)
glimpse(all_data)
table(all_data$y)
table(train$y)
all_data$y=as.numeric(all_data$y=="yes")
table(all_data$y)
glimpse(all_data)
train=all_data %>% 
  filter(data=='train') %>% 
  select(-data) #31647,34

test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-y)
glimpse(train)
glimpse(test)
set.seed(5)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,] #23735,34
test_25=train[-s,]#7912,34
library(car)
for_vif=lm(y~.,data=train)
summary(for_vif)
t=vif(for_vif)
sort(t,decreasing = T)[1:5]
for_vif=lm(y~.-edu_sec,data=train)
t=vif(for_vif)
sort(t,decreasing = T)[1:5]
summary(for_vif)
colnames(train)
fit_train=train %>% 
  select(-edu_sec)
#1 omited
colnames(fit_train)
fit=glm(y~.,family = "binomial",data=fit_train) #32 predictor var
summary(fit) 
fit=step(fit)

names(fit$coefficients) 
fit_final=glm(y~balance + housing + loan + duration + campaign + ID + 
                job_3 + job_5 + divorced + single + edu_primary + 
                co_cellular + co_tel + month_1 + month_2 + month_3 + month_4 + 
                month_5 + month_6 + poc_success + poc_failure + poc_other ,data=fit_train,family="binomial")
summary(fit_final)
(fit_final$coefficients) 
train$score=predict(fit_final,newdata = train,type="response")
library(ggplot2)
ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()
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
View(cutoff_data)
KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

KS_cutoff
test$score=predict(fit_final,newdata =test,type = "response")
test$left=as.numeric(test$score>KS_cutoff)
table(test$left)
test$leftfinal=factor(test$left,levels = c(0,1),labels=c("no","yes"))
table(test$leftfinal)
write.csv(test$leftfinal,"Kavya_G_P5_part2.csv")
test_25$score=predict(fit_final,newdata =test_25,type = "response")
table(test_25$y,as.numeric(test_25$score>KS_cutoff))
table(test_25$y)
a=(770+5888)/7912
a
1-a
library(pROC)
roccurve=roc(test_25$y,test_25$score) #real outcome and predicted score is plotted
plot(roccurve)
auc(roccurve)
