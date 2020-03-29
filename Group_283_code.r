
#reading the dataset
mydata <- read.table("/Users/shreeyeshchauhan/Downloads/my-dataset/credit_train.csv", header = T, sep = ',')
str(mydata)
summary(mydata)

#removing 514 columns
library(dplyr)
MM<- mydata[-seq(nrow(mydata),nrow(mydata)-513),]

#deleting dulicates
MM1 <- MM %>% distinct(MM$Customer.ID, .keep_all=TRUE)
summary(MM1)

#cleaning the column name
library(janitor)
MM2= clean_names(MM1)
summary(MM2)

#chi-square test
chisq.test(MM2$loan_status, MM2$loan_status)


summary(MM2)
head(MM2)

#removing 0 
MM2$newcs <- ifelse(MM2$credit_score >800, MM2$credit_score /10, MM2$credit_score)

MM3<-MM2

#creating new categories
MM3$current_job_year <- ifelse((MM3$years_in_current_job == ('< 1 year') 
                                | MM3$years_in_current_job == ('1 year') 
                                | MM3$years_in_current_job ==('2 years')  
                                | MM3$years_in_current_job == ('3 years')   
                                | MM3$years_in_current_job == ('4 years')),'0-4 ',
                               ifelse((MM3$years_in_current_job == ('5 years') 
                                       | MM3$years_in_current_job ==('6 years')
                                       | MM3$years_in_current_job == ('7 years') 
                                       | MM3$years_in_current_job ==('8 years')
                                       | MM3$years_in_current_job == ('9 years') 
                                       | MM3$years_in_current_job == ('n/a')),'5-9',
                                      '>=10'))

head(MM3$current_job_year)   


MM3$Loan_Purpose <- ifelse((MM3$purpose == ('Business Loan') 
                            | MM3$purpose == ('renewable_energy') 
                            | MM3$purpose ==('small_business')  
                            | MM3$purpose == ('Debt Consolidation')   
                            | MM3$purpose == ('major_purchase')),'Business',
                           ifelse((MM3$purpose == ('Buy House') 
                                   | MM3$purpose ==('Home Improvements')
                                   | MM3$purpose == ('moving')) , 'Home', 
                                  ifelse((MM3$purpose == ('Buy a Car') 
                                          | MM3$purpose ==('Educational Expenses')
                                          | MM3$purpose == ('Medical Bills')
                                          | MM3$purpose ==('Take a Trip')
                                          | MM3$purpose ==('vacation')
                                          | MM3$purpose ==('wedding')), 'Personal',
                                         'Other')))

head(MM3$Loan_Purpose)

#replacing 99999999 to NA and NA to mean
MM3$CLA = ifelse(MM3$current_loan_amount==99999999,NA,MM3$current_loan_amount)
MM3$CLA [is.na(MM3$CLA)] <- mean(MM3$CLA, na.rm = TRUE)
head(MM3$CLA)


#creating dummies
install.packages("dummies")
library(dummies)
str(MM3)
MM3 = dummy.data.frame(MM3,names = c("term"))
MM3 = dummy.data.frame(MM3,names = c("home_ownership"))
MM3 = dummy.data.frame(MM3,names = c("Loan_Purpose"))
MM3 = dummy.data.frame(MM3,names = c("current_job_year")) 

sapply(MM2, function(x) sum(is.na(x)))

str(MM3)
library(dplyr)

#mean of newcs group by loan status
MM3 <- MM3 %>% group_by(loan_status) %>% mutate(newcs= ifelse(is.na(newcs), mean(newcs, na.rm= TRUE), newcs))
MM3$newcs <- round(MM3$newcs, 0)
aggregate(MM3$newcs, list(MM3$loan_status), mean)

#mean of annual income group by loan status
MM3 <- MM3 %>% group_by(loan_status) %>% mutate(annual_income= ifelse(is.na(annual_income), mean(annual_income, na.rm= TRUE), annual_income))
aggregate(MM3$annual_income, list(MM3$loan_status), mean)

#mean of maximum open credit group by loan status
MM3 <- MM3 %>% group_by(loan_status) %>% mutate(maximum_open_credit= ifelse(is.na(maximum_open_credit), mean(maximum_open_credit, na.rm= TRUE), maximum_open_credit))
aggregate(MM3$maximum_open_credit, list(MM3$loan_status), mean)

#median of brankrupties group by loan status
MM3 <- MM3 %>% group_by(loan_status) %>% mutate(bankruptcies= ifelse(is.na(bankruptcies), median(bankruptcies, na.rm= TRUE), bankruptcies))
aggregate(MM3$bankruptcies, list(MM3$loan_status), median)

#median of tax liens group by loan status
MM3 <- MM3 %>% group_by(loan_status) %>% mutate(tax_liens= ifelse(is.na(tax_liens), median(tax_liens, na.rm= TRUE), tax_liens))
aggregate(MM3$tax_liens, list(MM3$loan_status), median)

#converting to 0 and 1
MM3$loan_status=ifelse(MM3$loan_status== 'Charged Off', 0 , 1 )

str(MM3)

MM4<- MM3
MM5<-MM4

#dropping the columns
MM3[,c("mm_customer_id")] <- list(NULL)
str(MM3)
MM3 <- MM3[,-c(1,2,4,6,7,9,10,14,17,20,21,22,25,28)]
MM3 <- MM3[,-c(9,17,20)]
MM3 <- MM3[,-c(11,12,13)]
MM3 <- MM3[,-c(2)]

str(MM5)

MM3<-KNN
print(MM4)
print(MM3)


#splitting data train and test
MM3=MM3[sample(nrow(MM3)),]
select.data = sample (1:nrow(MM3), 0.8*nrow(MM3))
train.data=MM3[select.data,]
test.data=MM3[-select.data,]

train.label=train.data$loan_status
test.label=test.data$loan_status

#logistic regression model
full=glm(loan_status~., data=train.data,family=binomial())
summary(full)

base=glm(loan_status~newcs, data = train.data,family = binomial())
summary(base)

forwardmodel = step(base,scope=list(upper = full, lower=~1),direction = "forward", trace = FALSE)
summary(forwardmodel)

backwardmodel = step (full,scope=list(upper = full, lower=~1),direction = "backward", trace = FALSE)
summary(backwardmodel)

bestsubsetmodel=step(base,scope=list(upper = full, lower=~1),direction = "both", trace = FALSE)
summary(bestsubsetmodel)

#finding probabilty and accuracy
prob=predict(bestsubsetmodel, type="response", newdata=test.data)

for(i in 1:length(prob)){
  if(prob[i]>0.4){
    prob[i]=1
  }else{
    prob[i]=0
  }
}

accuracy(test.label,prob)

accuracy_1<-table(test.data$loan_status,prob>0.4)
accuracy_1
sum(diag(accuracy_1))/sum(accuracy_1)


MM4<-MM3
MM3$loan_status<- factor(MM3$loan_status)

str(MM3)

#normalizing the data
MM3<-KNN
MM3 <- sapply(MM3, is.numeric)
normalized = (MM3-min(MM3))/(max(MM3)-min(MM3))
print(normalized)
MM3<-normalized
summary(MM3)


MM3<-lapply(MM3,scale)

#splitting the data
MM3=MM3[sample(nrow(MM3)),]
select.data = sample (1:nrow(MM3), 0.8*nrow(MM3))
train.data=MM3[select.data,]
test.data=MM3[-select.data,]


lss<-MM3[select.data,1]
ls<-MM3[-select.data,1]


summary(MM3)

install.packages("class")
library(class)
install.packages('Metrics', dependencies = TRUE)
library(Metrics)

#KNN model
str(MM3)
knn.1<- knn(train.data,test.data,cl=lss,k=3)
accuracy(knn.1,ls)

knn.5<- knn(train.data,test.data,cl=lss,k=5)
accuracy(knn.5,ls)

knn.10<- knn(train.data,test.data,cl=lss,k=10)
accuracy(knn.10,ls)

knn.15<- knn(train.data,test.data,cl=lss,k=15)
accuracy(knn.15,ls)

knn.21<- knn(train.data,test.data,cl=lss,k=21)
accuracy(knn.21,ls)

knn.99<- knn(train.data,test.data,cl=lss,k=99)
accuracy(knn.99,ls)

knn.50<- knn(train.data,test.data,cl=lss,k=50)
accuracy(knn.50,ls)







