#University Admission Prediction using Logistic Regression

#Load source data
Admission<-read.csv("C:/Users/Janani Selvaraj/Desktop/college explorer/admission.csv")
names(Admission)

#Here serial number is not required to predict the Admission,so removing the column serial number
Admission<-Admission[-1]
names(Admission)

#converting Chance of Admit data into 0 and 1
Admission$Chance.of.Admit<-ifelse(Admission$Chance.of.Admit>0.5,1,0)
Admission$Chance.of.Admit

#EDA Process
summary(Admission)

#Data cleaning
#Missing values
sum(is.na(Admission))
#no missing values

#Outliers
boxplot(Admission)
boxplot(Admission$`GRE.Score`)$out
boxplot(Admission$`TOEFL.Score`)$out
boxplot(Admission$University.Rating)$out
boxplot(Admission$SOP)$out

boxplot(Admission$LOR)$out
unique(boxplot(Admission$LOR)$out)

quantiles<-quantile(Admission$LOR,probs=c(0.25,0.75))
quantiles

range<-1.5*IQR(Admission$LOR)
range

Admission_new<-subset(Admission,Admission$LOR>(quantiles[1]-range)&Admission$LOR<(quantiles[2]+range))
Admission_new

unique(boxplot(Admission_new$LOR)$out)
unique(boxplot(Admission_new$CGPA)$out)

summary(factor(Admission_new$University.Rating))
summary(factor(Admission_new$SOP))
summary(factor(Admission_new$LOR))
summary(factor(Admission_new$Research))   

#Dividing data into train and test
set.seed(200)

names(Admission_new)
library(caTools)

#Partioning the data into train and test

sample<-sample.split(Admission_new$Chance.of.Admit,SplitRatio=0.70)
sample

train_data<-subset(Admission_new,sample==TRUE)
train_data

test_data<-subset(Admission_new,sample==FALSE)
test_data
#model building
AdmissionMod<-glm(Chance.of.Admit~.,data=train_data,family="binomial")
summary(AdmissionMod)

#Predicting the test_data values by applying model
pred<-predict(AdmissionMod,test_data,type="response")
pred

results<-ifelse(pred>0.5,1,0)
results

table(test_data$Chance.of.Admit,results)
(2+136)/(2+8+0+136)
#94% Accuracy

library(neuralnet)
n <- names(train_data)
f <- as.formula(paste("Chance.of.Admit ~", paste(n[!n %in% "Chance.of.Admit"], collapse = " + ")))
nn <- neuralnet(f,data=train_data,hidden=c(5,3),linear.output=T)
plot(nn)
pr.nn <- compute(nn,test_data[])

pr.nn_ <- pr.nn$net.result*(max(Admission$Chance.of.Admit)-min(Admission$Chance.of.Admit))+min(Admission$Chance.of.Admit)
test.r <- (test_data$Chance.of.Admit)*(max(Admission$Chance.of.Admit)-min(Admission$Chance.of.Admit))+min(Admission$Chance.of.Admit)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_data)
#we then compare the two MSEs
lm.fit <- glm(Chance.of.Admit~., data=train_data)
summary(lm.fit)
pr.lm <- predict(lm.fit,test_data)
MSE.lm <- sum((pr.lm - test_data$Chance.of.Admit)^2)/nrow(test_data)

print(paste(MSE.lm,MSE.nn))
par(mfrow=c(1,2))

plot(test_data$Chance.of.Admit,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test_data$Chance.of.Admit,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
plot(test_data$Chance.of.Admit,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test_data$Chance.of.Admit,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))



library(boot)

set.seed(200)
lm.fit <- glm(Chance.of.Admit~.,data=Admission)
cv.glm(Admission,lm.fit,K=10)$delta[1]



library(plyr) 
set.seed(450)
cv.error <- NULL
k <- 10
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  cv.error <- NULL
  
  index <- sample(1:nrow(Admission),round(0.9*nrow(Admission)))
  train.cv <- train_data
  test.cv <- train_data
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[])
  pr.nn <- pr.nn$net.result*(max(Admission$Chance.of.Admit)-min(Admission$Chance.of.Admit))+min(Admission$Chance.of.Admit)
  
  test.cv.r <- (test.cv$Chance.of.Admit)*(max(Admission$Chance.of.Admit)-min(Admission$Chance.of.Admit))+min(Admission$Chance.of.Admit)
  cv.error <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}
mean(cv.error)
boxplot(cv.error,xlab='Chance.of.Admit',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
