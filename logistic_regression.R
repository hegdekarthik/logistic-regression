rm(list = ls())

library(readxl)
German_Credit <- read_excel("Documents/German Credit.xlsx", 
                            sheet = "All Data")
View(German_Credit)

plot(German_Credit$Creditability, German_Credit$CreditAmt)

hist(German_Credit$CreditAmt, col = 'blue')

cutpoint <- c(0,500,1000,1500,2000,2500,5000,7500,10000,15000,20000)

Credit_cat <- with(German_Credit, cut(German_Credit$CreditAmt, cutpoint, right=T))

table(Credit_cat)

table_1 <- with(German_Credit, table(Credit_cat, German_Credit$Creditability))
table_2 <- prop.table(table_1,1)
table_3 <- cbind(table_2, table(Credit_cat))
round(table_3,2)

colnames(table_3)

table_3[,c(2,3)]


#Running a logistic regression
fit1.lr <- glm(Creditability ~ CreditAmt, data=German_Credit, family=binomial(link=logit))
summary.glm(fit1.lr)

names(fit1.lr)

#Probablity
with(German_Credit,plot(CreditAmt, fit1.lr$fitted.values, pch=19, cex=0.4, col='dark blue'))
fit1.lr$linear.predictors
max(fit1.lr$fitted.values)
min(fit1.lr$fitted.values)

x1 = exp(1.229-0.000119*500)/(1+exp(1.229-0.000119*500))
x2 = (exp(1.229-0.000119*600))/(1+exp(1.229-0.000119*600))
x3 = (exp(1.229-0.000119*700))/(1+exp(1.229-0.000119*700))

y1 = (exp(1.229-0.000119*4000))/(1+exp(1.229-0.000119*4000))
y2 = (exp(1.229-0.000119*6000))/(1+exp(1.229-0.000119*6000))
y3 = (exp(1.229-0.000119*8000))/(1+exp(1.229-0.000119*8000))

z1 = (exp(1.229-0.000119*15000))/(1+exp(1.229-0.000119*15000))
z2 = (exp(1.229-0.000119*20000))/(1+exp(1.229-0.000119*20000))
z3 = (exp(1.229-0.000119*25000))/(1+exp(1.229-0.000119*25000))

df1 = c(x1,x2,x3)
df1
df_1=c(1-x1,1-x2,1-x3)
df1/df_1

df2 = (c(y1,y2,y3))
df2
df_2 = c(1-y1,1-y2,1-y3)
df2/df_2

df3 = (c(z1,z2,z3))
df3
df_3 = c(1-z1,1-z2,1-z3)
df3/df_3

## Fit regression for duration

fit2.lr <- glm(Creditability ~ DurCredit, data=German_Credit, family=binomial(link=logit))
summary.glm(fit2.lr)

a1 = (exp(1.666351-0.037538*12))/(1+exp(1.666351-0.037538*12))
a1

a2 = (exp(1.666351-0.037538*30))/(1+exp(1.666351-0.037538*30))
a2

#Multiple Logistic Regression
fit3.lr <- glm(Creditability~CreditAmt + DurCredit, data=German_Credit, family=binomial(link=logit))
summary.glm(fit3.lr)



#Calculations
b1 = exp(1.670-(0.000023*15000)-(0.03412*12))/(1+exp(1.670-(0.000023*15000)-(0.03412*12)))
b1

b2 = exp(1.670-(0.000023*20000)-(0.03412*9))/(1+exp(1.670-(0.000023*20000)-(0.03412*9)))
b2

nd = data.frame(CreditAmt = 15000, DurCredit=12)
predict(fit3.lr, nd, type = 'response')

#############
library(pROC)
roc(German_Credit$Creditability, fit2.lr$fitted.values)
plot.roc(German_Credit$Creditability, fit2.lr$fitted.values)

library(ggplot2)
library(ROCR)

predict2 <- predict(fit2.lr, type = 'response')
ROCRpred2 <- prediction(as.numeric(predict2),as.numeric(German_Credit$Creditability))

#Another Model
fit4.logistic <- glm(Creditability ~ DurCredit + as.factor(SexMS), data=German_Credit, family=binomial(link=logit))
summary.glm(fit4.logistic)

#Another
fit5.logistic <- glm(Creditability ~ DurCredit + as.factor(SexMS), data=German_Credit, family=binomial(link=logit))
summary.glm(fit5.logistic)

str(German_Credit)

#PseudoR2 Test
library(DescTools)
PseudoR2(fit4.logistic)

#hoslem.test()
library(ResourceSelection)
hoslem.test(German_Credit$Creditability, fit1.lr$fitted.values, g=10)
