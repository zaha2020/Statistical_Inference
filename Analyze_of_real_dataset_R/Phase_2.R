#--------------------------QO--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------
df2<-read.csv("C:\\Users\\z_hab\\OneDrive\\Desktop\\prozheestenbat\\StudentsPerformance.csv")
View(df2)
data1 <- df2[ , ! names(df2) %in% c("X")]
View(data1)

ncol(data1)
nrow(data1)
is.null(data1)  
any(is.na(data1))


View(data1)
#--------------------------Q1--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------  
#I Choosed sex with tw0 level & Fjob with 5 level for two categorical variable(I had just two cat that have
#more than two level, they are Fjob & Mjob ,I choose Fjob)
#---
unique(data1$sex)
unique(data1$Fjob)

#process
df = data.frame(data1$sex,data1$Fjob)
Table = table(data1$sex,data1$Fjob) 
count=addmargins(Table)[1:2,]
count

#p_had
p <- count
p[1,]=round((p[1,]/p[1,6]),3)
p[2,]=round((p[2,]/p[2,6]),3)
p

#Conditons for inference

i <- 1:5
CFI=lapply(i,p,count,FUN = function(i,p,count) {
  np     = round(count[,i]*p[,i])
  n_p  = round(count[,i]*(1-p[,i]))
  print(paste(" female and male if Fjob::",names(count[1,])[i],np[1],np[2], n_p[1],n_p[2]))
} )




#CI

i <- 1:5
CI=lapply(i,p,count,FUN = function(i,p,count) {
  pointestimate   = p[1,i]-p[2,i]
  SE<-sqrt(sum(p[,i]*(1-p[,i])/count[,i]))
  z_star = qnorm(0.025,lower.tail = FALSE)
  CI<- c(pointestimate-z_star*SE , pointestimate+z_star*SE)
  print(paste("CI for proportion of female and male  if Fjob::",
              names(count[1,])[i],'is (',round(CI,2)[1],',',round(CI,2)[2],')'))
})


#------------------------------B-----------------------------------------

#-------------------------chi-square-------------------------------------
#chi-squares
# H0 : sex & Fjob are independent
# HA :sex & Fjob are dependent


#observation
observation_dep=addmargins(Table)
observation_dep


#expected
expected_dep=observation_dep
for (r in seq(1,nrow(expected_dep))) {
  for (c in seq(1,ncol(expected_dep))) {
    expected_dep[r,c] = round(expected_dep[r,ncol(expected_dep)]*expected_dep[nrow(expected_dep),c]/expected_dep[nrow(expected_dep),ncol(expected_dep)])
  }
}
expected_dep


#chi-square
chi_square=sum(((observation_dep[1:2,1:5]-expected_dep[1:2,1:5])^2)/(expected_dep[1:2,1:5 ]))
chi_square

R<-2
C<-5
df<-(R-1)*(C-1)
df


#p-value
p_value=pchisq(chi_square,4,lower.tail = FALSE)
p_value


#-------------------------pool for proportion-----------------------------

## H0 : the proportation of Fjob in TWO groupS ( female & male)is same
## HA : the proportation of Fjob in TWO groupS ( female & male)isn't same
#Conditons for p-pool

i <- 1:5
POOL=lapply(i,count,FUN = function(i,count) {
  p_pool  = sum(count[,i])/sum(count[,6])
  np_pool     = round(count[1,6]*p_pool)
  n_p_pool  = round(count[1,6]*(1-p_pool))
  np_pool_2     = round(count[2,6]*p_pool)
  n_p_pool_2  = round(count[2,6]*(1-p_pool))
  print(paste("female & male if Fjob::",names(count[1,])[i],
              np_pool,n_p_pool,
              np_pool_2,n_p_pool_2))
})

#pvalue
i <- 1:5
PVALUE_P=lapply(i,count,FUN = function(i,count) {
  p_pool  = sum(count[,i])/sum(count[,6])
  pointestimate   = p[1,i]-p[2,i]
  SE_pool = sqrt(((p_pool*(1-p_pool))/count[1,6])+((p_pool*(1-p_pool))/count[2,6]))
  z  = abs((pointestimate-0)/SE_pool)
  pvalue   = 2*pnorm(z,lower.tail = FALSE)
  print(paste("pvalue of female & male if Fjob::",
              names(count[1,])[i],': P_value = ',round(pvalue,2)))
})


#--------------------------Q2--------------------------------
#-------------------------------------------------------------
#------------------------------------------------------------  
## I select variables «internet»  
#three ways.

# I devide 14 pepole in to two groups and for each group I do simulation with 70 
#repeating  and the I calcute differnce of two p-had and calcuting pvalue for two side 

#  (we have 2 side Test)
# H0 : pyes - pno  =0
# HA : pyes - pno !=0

#-------------------------------first_way-------------------------------------------
library(dplyr)
set.seed(546)
sample1_sim = sample_n(data1,14)$internet
sample1_sim

boott1 = c()

for(j in 1:10){
  re_sample <- sample1_sim[1:7]
  pro = ifelse(re_sample=="yes",1,0) #calcute p(internet l female)
  boott1 = append(boott1, pro)
}
boott1

#p_had1
p_had1=sum(boott1==1)/length(boott1)
p_had1

boott2 = c()

for(j in 1:10){
  re_sample <- sample1_sim[8:14]
  pro = ifelse(re_sample=="yes",1,0) #calcute p(internet l female)
  boott2 = append(boott2, pro)
}
boott2

#p_had2
p_had2=sum(boott2==1)/length(boott2)
p_had2

#we use p_pool for calcute hypothesis test

p_pool  = (sum(boott1==1)+sum(boott2==1))/140
SE_pool = sqrt(((p_pool*(1-p_pool))/70)+((p_pool*(1-p_pool))/70))
pointestimate1=p_had1-p_had2
z  = (pointestimate1-0)/SE_pool
z
pvalue1   = 2*pnorm(z,lower.tail = TRUE)
pvalue1

#------------------second-way---------------------------
#second_way (ready_package)

# we assume that  p =0.5 as Null hypothesis
#  (we have 1 side Test)
# H0 : p  = 0.5
# HA : p > 0.5
set.seed(546)
sample2_sim = sample_n(data1,15)$internet
sample2_sim


source("http://bit.ly/dasi_inference") #if it doesn't connect ,please use proxy

inference(sample2_sim,est="proportion",type="ht",success = "yes",
          method = "simulation",null=0.5,
          alternative = "greater")


#--------------------third way without seperate two groups with one side------------------
boott = c()

for(j in 1:10){
  i <- sample(1:15,15, replace = TRUE)
  re_sample <- sample2_sim[i]
  pro = ifelse(re_sample=="yes",1,0) #calcute p(internet l female)
  boott = append(boott, pro)
}
boott

p_had=sum(boott==1)/length(boott)
p_had

p=0.5# when we dont konw the exact value of p we set 0.5
n=length(boott)
SE=sqrt((0.5*0.5)/150)
SE
z_s = (p_had-p)/SE
pValue = pnorm(z_s,lower.tail = FALSE)
pValue



#--------------------------Q3--------------------------------
#-------------------------------------------------------------
#--------------------------A---------------------------------- 

#we assume that all data is inpoulation so we calcute expected  

#because the question said,use probability 

df = prop.table(table((data1$Fjob)))
inpopulation<-df
inpopulation


library(dplyr)
set.seed(123)
sample1 = sample_n(data1,100) #(This is randomly selected()
sample1
sample2=data1[150:249,] #(This is selected with purpose it means that we don't select it randomly)
sample2


#sample1
df_sample1 = data.frame(sample1$Fjob)
df_sample1


#sample1_observation
Table_sample1 = table((df_sample1))
observation<-Table_sample1[1:5]
observation

#sample1_expected
expected1 = round((inpopulation[1:5]*100))
expected1



#chi_square
chi_square_=sum(((observation[1:5]-expected1[1:5])^2)/(expected1[1:5]))
chi_square_

#p-value_sample1
DF<-5-1
DF

p_value_1=pchisq(chi_square_,4,lower.tail = FALSE)
p_value_1


#sample2((sample with purpose))

df_sample2 = data.frame(sample2$Fjob)
df_sample2


#sample2-observation
Table_sample2= table(df_sample2)
observation1<-Table_sample2[1:5]
observation1

#expected2=expected1
expected2 = round((inpopulation[1:5]*100))
expected2

#expected2=expected1
#sample2-chi_square
chi_square_2=sum(((observation1[1:5]-expected1[1:5])^2)/(expected1[1:5]))
chi_square_2


#sample2-pvalue
Df<-5-1
Df
p_value_2=pchisq(chi_square_2,4,lower.tail = FALSE)
p_value_2

#compare 2 p_value together with one expected value but one of sapmle has bias

p_value_1

p_value_2

#--------------------------B----------------------------------
#I select romantic
Table3 = table(data1$romantic,data1$Fjob) 
print(Table3)

#observation
observation3=addmargins(Table3)
observation3


#expected
expected3=observation3
for (r in seq(1,nrow(expected3))) {
  for (c in seq(1,ncol(expected3))) {
    expected3[r,c] = round(expected3[r,ncol(expected3)]*expected3[nrow(expected3),c]/expected3[nrow(expected3),ncol(expected3)])
  }
}
expected3


#chi_square
chi_square3=sum(((observation3[1:2,1:5]-expected3[1:2,1:5])^2)/(expected3[1:2,1:5 ]))
chi_square3


#p-value
R<-2
C<-5
DF2<-(R-1)*(C-1)
DF2

p_value3=pchisq(chi_square3,4,lower.tail = FALSE)
p_value3



#--------------------------Q4--------------------------------
#-------------------------------------------------------------
#--------------------------A---------------------------------- 
#G2 NUMERICAL(response)    
#G1 & G3(expantory)
#(in pdf)
#--------------------------B--------------------------------- 
library(ggplot2)
#total
#model1 <- lm(G2 ~ G1 + G3, data = data1)
#model1

#for each explantory variable
#a
model2 <- lm(G2 ~ G3 , data = data1)
model2

model3 <- lm(G2 ~ G1 , data = data1)
model3

#b
#Y_TOTAL=  1.6065 +(0.3830)G1+(0.5171 )G3

#OUR_GOAL

#Y_G2= 3.0849+(0.7269)G3
#Y_G2= 1.6236+(0.9877)G1

#summary(model1)
summary(model2)
summary(model3)

#c
#(G2,G3)
ggplot(data1, aes(x = G3, y = G2)) +
  geom_point(color="darkblue") +
  stat_smooth(method = 'lm',
              col = "red",
              se = FALSE, linetype="dashed",
              formula = 'y ~ x')+
  labs(title = "linear regression curve fitting(G3lG2)  ")+
  theme(plot.title = element_text(hjust = 0.5))

#(G2,G1)
ggplot(data1, aes(x = G1, y = G2)) +
  geom_point(color="darkblue") +
  stat_smooth(method = 'lm',
              col = "red",
              se = FALSE, linetype="dashed",
              formula = 'y ~ x')+
  labs(title = "linear regression curve fitting(G3lG1)  ")+
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------C----------------------------------

#summary(model1)
summary(model2)

summary(model3)

#--------------------------D----------------------------------

#compare two model
#first::Adjusted R-squared

library(rsq)
rsq(model2,adj=TRUE)
rsq(model3,adj=TRUE)
#Adjusted R-squared(G3):  0.837 
#Adjusted R-squared(G1):  0.7234 


#second::anova

#anova(model1)

anova(model2)
anova(model3)


#--------------------------E----------------------------------
#GOOD_ PREDICTOR=G3&G1 

#--------------------------F----------------------------------

#install.packages("caret")
library(ggplot2)
library(caret)
library(dplyr)


set.seed(123)
sample_f = sample_n(data1,100)
sample_f

#------------------------first_way for sampling------------------------------
#a
sample_f_=select(sample_f ,G2,G1,G3)
index_4 = sample(1:nrow(sample_f_), size = .90 * nrow(sample_f_))

#train,test
train_4 = sample_f_[index_4, ]
train_4
nrow(train_4)
test_4 = sample_f_[-index_4, ]


#full_model
#fit <- lm(G2 ~ G1 + G3, data = train_4)
#summary(fit)


#model2
fit2 <- lm(G2 ~ G3, data = train_4)
summary(fit2)

#model3
fit3 <- lm(G2 ~ G1 , data = train_4)
summary(fit3)

#condition
#1.linearity(ok)
#2.Nearly   normal   residuals
hist(fit2$residuals,xlab = "Residuals")


qqnorm(fit2$residuals,main="qqnorm")
qqline(fit2$residuals,col="Blue") ## adds the line to the plot

# 3.Constant variability

#(G2,G3)
ggplot(train_4, aes(x = G3, y = G2)) +
  geom_point(color="darkblue") +
  stat_smooth(method = 'lm',
              col = "red",
              se = FALSE, linetype="dashed",
              formula = 'y ~ x')+
  labs(title = "linear regression curve fitting(G3lG2)  ")+
  theme(plot.title = element_text(hjust = 0.5))


#(G2,G1)
ggplot(train_4, aes(x = G1, y = G2)) +
  geom_point(color="darkblue") +
  stat_smooth(method = 'lm',
              col = "red",
              se = FALSE, linetype="dashed",
              formula = 'y ~ x')+
  labs(title = "linear regression curve fitting(G3lG1)  ")+
  theme(plot.title = element_text(hjust = 0.5))



#met condition


fit2 <- lm(G2 ~ G3, data = train_4)
summary(fit2)

#model3
fit3 <- lm(G2 ~ G1 , data = train_4)
summary(fit3)


#------------------------second_way for sampling---------------------------
# creating training data as 90% of the dataset
#random_sample <- createDataPartition(sample_f1$G2,p = 0.90, list = FALSE)
#random_sample
# generating training dataset
# from the random_sample
#training_dataset  <- sample_f1[random_sample, ]
# generating testing dataset
#testing_dataset <- sample_f1[-random_sample, ]



#b
#CI_MODEL2
#df=n-k-1=90-1-1=88

tstar=qt(0.95+(1-0.95)/2,df=88)
b1_model2=fit2$coefficients[2]
b1_model2
SE_b1_model2 = summary(fit2)$coefficients[2,2]
SE_b1_model2

CI_MODEL2= c(b1_model2-tstar*SE_b1_model2,b1_model2+tstar*SE_b1_model2 )
CI_MODEL2


#CI_MODEL3
#df=n-k-1=90-1-1=88

tstar=qt(0.95+(1-0.95)/2,df=88)
b1_model3=fit3$coefficients[2]
b1_model3
SE_b1_model3 = summary(fit3)$coefficients[2,2]  
SE_b1_model3

CI_MODEL3= c(b1_model3-tstar*SE_b1_model3,b1_model3+tstar*SE_b1_model3 )
CI_MODEL3



#c

x_test=select(test_4,G3)
x_test
# predicting the target variable
predictions2 <- predict(fit2, x_test)
predictions2



#predic(G1_test)

x_test1=select(test_4,G1)
predictions3 <- predict(fit3,x_test1)
predictions3




#d
# success rate
pre_df= data.frame(x_test,predictions2)
pre_df

#ei=actual-predict
pre_df["abs_diff"]=abs(pre_df$G3 - pre_df$predictions2)
pre_df

sucsses_rate=nrow(pre_df[pre_df["abs_diff"]<2,])/nrow(pre_df)
print(paste("accuracy of model on G3  is",round(sucsses_rate*100,2),'%'))


#sucsses_rate_model2
pre_df_= data.frame(x_test1,predictions3)
pre_df_

#ei
pre_df_["abs_diff"]=abs(pre_df_$G1 - pre_df_$predictions3)
pre_df_

sucsses_rate1=nrow(pre_df_[pre_df_["abs_diff"]<2,])/nrow(pre_df_)
print(paste("accuracy of model on G3  is",round(sucsses_rate1*100,2),'%'))


# computing model performance metrics(fit2)
data.frame( Rsquared    = round(R2(predictions2, test_4$G2),2),
            RMSE = round(RMSE(predictions2, test_4$G2),2))


# computing model performance metrics(fit3)
data.frame( Rsquared    = round(R2(predictions3, test_4$G2),2),
            RMSE = round(RMSE(predictions3, test_4$G2),2))


#--------------------------Q5--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------
library(dplyr)
data2=data1
data2$sex <- as.numeric(factor(data1$sex))
data2$school = as.numeric(factor(data1$school))
data2$Fjob = as.numeric(factor(data1$Fjob))
data2$Mjob = as.numeric(factor(data1$Mjob))
data2$internet = as.numeric(factor(data1$internet ))
data2$romantic=as.numeric(factor(data1$romantic ))

#corr(x,y)
y=data2$G2
x=select(data2,age,romantic,sex,Fjob,Mjob,goout,absences,failures,health,school,
           studytime,internet,G1,G3)

cor(x,y)

#response G2
#EXPANTORY
#G1,G3,failures,studytime

#firstway
library(GGally)

library(ggplot2)
ggpairs(data1, columns = c("G2", "failures", "studytime","G1","G3")
        , title = "Bivariate analysis of revenue expenditure by the British household"
        , upper = list(continuous = wrap("cor",
                                         size = 3)),
        lower = list(continuous = wrap("points",
                                       alpha = 0.3,
                                       size = 0.1)))
#secondway
library(dplyr)
df2=select(data1,G2, failures, studytime,G1,G3)
ggpairs(
  df2, columns =1:5,
  upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)))

#--------------------------B----------------------------------
model5 <- lm(G2 ~ G1 + G3+ failures, data = data1)
model5

#--------------------------c----------------------------------
summary(model5)$r.squared
#--------------------------D----------------------------------
summary(model5)
#--------------------------E----------------------------------
#backward
#adjusted R2

#FULL
model6 <- lm(G2 ~ G1 + G3+ failures, data = data1)

library(rsq)
rsq(model6,adj=TRUE)

#STEP1
model5_ <- lm(G2 ~ G1 + G3, data = data1)
rsq(model5_,adj=TRUE)

model5__ <- lm(G2 ~ G1 + failures, data = data1)
rsq(model5__,adj=TRUE)

model5___ <- lm(G2 ~ G3+ failures, data = data1)
rsq(model5___ ,adj=TRUE)

#STOP because full model has biggest R2

#P-VALUE

#FULL
summary(model6)
#stop because all of p-valure are smaller than 0.05 
#so our best model is full model


#--------------------------F----------------------------------

#1. Linearity
#FIRST way
plot(model6$residuals ~ data1$G1)
plot(model6$residuals ~ data1$G3)
plot(model6$residuals ~ data1$failures)


#secondway
#G1
datam <- fortify(model6)
ggplot(data = datam) + 
  geom_hline(yintercept = 0, color = "red", size = 1.5) + 
  geom_point(aes(x = G1, y = .resid), size = 3, color = "blue") + 
  labs(x = "G1", y = "residual (score of G2)",
       title = "residual plot, modeling G2 with G1") + 
  theme(plot.title = element_text(hjust = 0.6))

#G3
datam <- fortify(model6)
ggplot(data = datam) + 
  geom_hline(yintercept = 0, color = "red", size = 1.5) + 
  geom_point(aes(x = G3, y = .resid), size = 3, color = "blue") + 
  labs(x = "G3", y = "residual (score of G2)",
       title = "residual plot, modeling G2 with G3") + 
  theme(plot.title = element_text(hjust = 0.6))

#failures
datam <- fortify(model6)
ggplot(data = datam) + 
  geom_hline(yintercept = 0, color = "red", size = 1.5) + 
  geom_point(aes(x = failures, y = .resid), size = 3, color = "blue") + 
  labs(x = "failures", y = "residual (score of G2)",
       title = "residual plot, modeling G2 with failures") + 
  theme(plot.title = element_text(hjust = 0.6))




#2.Nearly   normal   residuals
hist(model6$residuals)

qqnorm(model6$residuals,main="qqnorm")
qqline(model6$residuals,col="Blue") ## adds the line to the plot


# 3.Constant variability
plot(model6$residuals ~ model6$fitted.values ,main="Residuals vs. fitted" 
     ,xlab="model6$fitted",
     ylab="model6$residuals")

#absolute
plot(abs(model6$residuals) ~ model6$fitted.values
,main="Absolute value of residuals vs.fitted" 
     ,xlab="model6$fitted",
     ylab="model6$residuals")

#--------------------------G----------------------------------
#5-fold cross validation


#Split the dataset into 5 subsets randomly
#Use 4 subsets for training the model
#Test the model against that one subset which was left in the previous step
#Repeat the above steps for 5 times i.e., until the model is not trained and tested on all subsets
#Generate overall prediction error by taking the average of prediction errors in every case

library(ggplot2)
library(caret)

train_control <- trainControl(method = "cv", number = 5)
                             

# training the model by assigning G2 column
# as target variable and rest other column
# as independent variable

model7 <- train(G2 ~ G1 + G3+ failures, data = data1,method = "lm",trControl = train_control)

# printing model performance metrics
# along with other details
print(model7)
#--------------------------Q6--------------------------------
#-------------------------------------------------------------
#--------------------------A----------------------------------

#internet(response)
#age,romantic,sex,G1,G2,G3(explantory)

library(dplyr)
data2=data1
#data2$internet = as.numeric(factor(data1$internet))
#data2$internet

data2$internet = factor(data2$internet)


model_lg=glm(internet ~ age+romantic+sex+G1+G2+G3,data=data2, family=binomial)
summary(model_lg)

# two significant predictor(age,romantic:yes)

#intercept$log_odds & odds_ratio

#intercept
intercept=model_lg$coefficients[1]
intercept

#log_odds_age
log_odds_age=model_lg$coefficients[2]
log_odds_age

odds_ratio_age=round(exp(log_odds_age),3)
odds_ratio_age

#log_odds_romanticyes
log_odds_romanticyes=model_lg$coefficients[3]
log_odds_romanticyes


odds_ratio_romanticyes=round(exp(log_odds_romanticyes),3)
odds_ratio_romanticyes

#log_odds_sexM
log_odds_sexM=model_lg$coefficients[4]
log_odds_sexM

odds_ratio_sexM=round(exp(log_odds_sexM),3)
odds_ratio_sexM

#log_odds_G1
log_odds_G1=model_lg$coefficients[5]
log_odds_G1

odds_ratio_G1=round(exp(log_odds_G1),3)
odds_ratio_G1

#log_odds_G2
log_odds_G2=model_lg$coefficients[6]
log_odds_G2

odds_ratio_G2=round(exp(log_odds_G2),3)
odds_ratio_G2

#log_odds_G3
log_odds_G3=model_lg$coefficients[7]
log_odds_G3

odds_ratio_G3=round(exp(log_odds_G3),3)
odds_ratio_G3


#--------------------------B----------------------------------
#sex
OR=round(exp(model_lg$coefficients[4]),2)
OR


#I use the formule of our lessons for plot the OR
# because I want to calcute
#OR=(P(internet l male )/[1-p(internet l male)]) /
#(P(internet l female)/[1-p(internet l female)])


#our x_axis in plot P(internet l female)
p_internet_if_female = seq(0,1,0.01)
p_internet_if_female 

#our y_axis is (P(internet l male )

p_internet_if_male <- c()

for (i in p_internet_if_female) {
  p_male = ifelse(i==1,1,(OR*(i/(1-i)))/(1+OR*(i/(1-i)))) #calcute p(internet l male) #PAGE 10 IN VIDEO 27
  p_internet_if_male  = round(c(p_internet_if_male,p_male),2)
}
p_internet_if_male

#data_frame
df_OR= data.frame(p_internet_if_female,p_internet_if_male)
df_OR


#two_way for plot
#plot
plot(df_OR$p_internet_if_female, df_OR$p_internet_if_male, pch = 19,
     lwd = 1,  main = "OR Curve",col = "blue",xlab="p(internet l female)",
     ylab="p(internet l male)")+
abline(a = 0, b = 1,col = "black",lty = "dashed",lwd=2)            
       
       

#ggplot
library(ggplot2)
ggplot(aes(x=p_internet_if_female, y=p_internet_if_male ),data=df_OR) +
  geom_smooth(method = 'loess',formula = 'y ~ x',color="blue",size = 2)+
  geom_segment(aes(x = 0.00, y = 0.00, xend = 1.00, yend = 1.00,),
               linetype="dashed",color="black",size = 1)+
  labs(title =  "OR Curve ",x="p(internet l female)",y="p(internet l male)")+
  theme(plot.title = element_text(hjust = 0.5))
       
 
#--------------------------C----------------------------------
#install.packages("pROC")
#install.packages("plotROC")
#install.packages('ROCR')
library(ROCR)
#library(plotROC)
library(ggplot2)
library(pROC)
library(caret)
library(dplyr)


dff=select(data2,internet,romantic,sex,age,G2,G1,G3)


#train,test
set.seed(123)
index = sample(1:nrow(dff), size = .85 * nrow(dff))
train = dff[index, ]
#nrow(train)
test = dff[-index, ]
#nrow(test)

#----------------------
#set.seed(1)
#random_sample1 <- createDataPartition(dff$internet,p = 0.85, list = FALSE)
#random_sample1
# generating training dataset
# from the random_sample
#train  <- dff[random_sample1, ]
#test <- dff[-random_sample1, ]
#-------------------------------------------------


#fit_model
logistic_model=glm(internet ~ age+romantic+sex+G1+G2+G3,train, family=binomial)
summary(logistic_model)

#use model to make predictions on test set
pred_prob= predict(logistic_model,test,type="response")



# Converting from probability to actual output

test$pred_class <- ifelse(pred_prob >= 0.85, "yes", "no")
test$pred_class

# Generating the classification table
conusion_table <- table(test$internet, test$pred_class)
conusion_table


#roc <- roc(test$internet,pred_prob)


#test$internet=ytest
pred = prediction(pred_prob, test$internet)
roc = performance(pred,"tpr","fpr")
print(roc)
auc <- round(auc(test$internet, pred_prob),2)

#create ROC plot

plot(roc,  lwd = 4,main=paste0('ROC Curve ', '(AUC = ', auc, ')'),colors="blue",pch = 19)+
abline(a = 0, b = 1,col = "black",lty = "dashed",lwd=2)  


#--------------------------D----------------------------------
#summary(logistic_model)
#ROMANTIC & Age
summary(model_lg)

#--------------------------E----------------------------------
#ROMANTIC & Age

logistic_model2=glm(internet ~ age+romantic,data2, family=binomial)
summary(logistic_model2)


#--------------------------F----------------------------------
#install.packages('e1071', dependencies=TRUE)
library(caret)
library('e1071')
dff2=select(data2,internet,romantic,age)

#train,test
set.seed(123)
index_6 = sample(1:nrow(dff2), size = .85 * nrow(dff2))
train6 = dff[index_6, ]
#nrow(train)
test6 = dff[-index_6, ]
#nrow(test)



#utility   of   different   outcomes 
tpv<-  1    
tnv <-  1      
fnv <- -2     
fpv <- -6


threshold= seq(0,1,0.1)

U <- c()

for (i in threshold) {
  pred_prob2= predict(logistic_model2,test)
  test$pred_class <- factor(ifelse(pred_prob2 >= i, "yes", "no"),levels = c('yes','no'))
  conusion_table <- confusionMatrix(test$internet, test$pred_class)$table
  U_P = ((tpv*conusion_table[2,2])+(fpv*conusion_table[1,2])+(tnv*conusion_table[1,1])+(fnv*conusion_table[2,1])) 
  U  = c(U,U_P)
}

U

#data_frame
df_utility= data.frame(threshold,U)
df_utility

#MAX_utility
MAX <-(max(df_utility$U))
MAX

#PLOT  
#ggplot
library(ggplot2)
ggplot(aes(x=threshold, y=U ),data=df_utility) +
  geom_smooth(method = 'loess',formula = 'y ~ x',color="blue",size = 2)+
  labs(title =  "Utility Curve ",x="P",y="U")+
  geom_point(data=df_utility, 
             aes(x=0.6,y=MAX ), 
             color='red',
             size=3)+  theme(plot.title = element_text(hjust = 0.5))


#--------------------------Q7--------------------------------
#-------------------------------------------------------------
#processing(add new column)
library(dplyr)

data1$sumofscores =  data1$G1+data1$G2+data1$G3

data1$academic_probation <- ifelse(data1$sumofscores < 25, 1, 0)
View(data1)

#select without sum of score
dff7=select(data1,academic_probation , age,romantic,sex,Fjob,Mjob,goout,absences,failures,health,
              studytime,internet,G1,G2,G3)

#train_test
index7 = sample(1:nrow(dff7), size = .80 * nrow(dff7))
train7 = dff7[index7, ]
test7 = dff7[-index7, ]

#fit_model
train7$academic_probation = factor(train7$academic_probation)
logistic_model7=glm(academic_probation ~ age+romantic+sex+Fjob+Mjob+goout+absences+failures+health+
                 studytime+internet+G1+G2+G3,train7, family=binomial)
summary(logistic_model7)


#predict on test data
pred_prob7= predict(logistic_model7,test7,type="response")
pred_prob7

# Generating the classification table
test7$pred_class <- ifelse(pred_prob7 >= 0.80, 1, 0)
test7$pred_class


#conusion_table
conusion_table7 <- table(test7$academic_probation, test7$pred_class)
conusion_table7

TN=conusion_table7[1,1]
TN
FP=conusion_table7[1,2]
FP
FN=conusion_table7[2,1]
FN
TP=conusion_table7[2,2]
TP


#accuracy of model on test data
accuracy_test <- sum(diag(conusion_table7))/sum(conusion_table7)*100
print(paste("accuracy of model on test data is",round(accuracy_test,2),'%'))
           


