                           # Linear regrassion part2 #
library("ISLR")
Carseats
attach(Carseats)
lm.Sales=lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age)
lm.Sales
#doing prediction 
predict(lm.Sales)[1]
#it gives prediction of sales of 1st store

#residual or error in prediction
residuals(lm.Sales)[1]
#it gives error make in prediction of sales of 1st store
 Sales[1]

 # by above method we can say that my predicted sales where 7.3 my actual sale was 9.5 where error made in prediction was 2.19
 plot(Sales,predict(lm.Sales))
#this is the plot of actual vs predicted sales  
cor(Sales,predict(lm.Sales))
cor(Sales,predict(lm.Sales))^2 
hist(residuals(lm.Sales))     
#show that how much error is made
summary(lm.Sales)


## predictive modling##
#using traing and Test
#for that we split our data in two part traing and test

set.seed(1)
index=sample(400,100)
index
Carseats.test=Carseats[index,]
dim(Carseats.test)
Carseats.training=Carseats[-index,]
dim(Carseats.training)
lm.Sales.train=lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age,data = Carseats.training)
summary(lm.Sales.train)
predict(lm.Sales.train,newdata = Carseats.test)
plot(Carseats.test$Sales,predict(lm.Sales.train,newdata = Carseats.test))
cor(Carseats.test$Sales,predict(lm.Sales.train,newdata = Carseats.test))
cor(Carseats.test$Sales,predict(lm.Sales.train,newdata = Carseats.test))^2 # R^2
#it mean for my training set 0.87% variation and on test set 0.84% varation due to price

#checking Multicolinearity
library("rms")
vif(lm.Sales)
#here vif are low therefore there is no Multicolinearity between variables
