### Logestic reggresion ####
Default=read.csv(file.choose())
head(Default)
dim(Default)
names(Default)
attach(Default)
summary(Default)
default.rate=125/(125+625)
default.rate
plot(Status,Credit.Score)
#plot explains defaulter has lower credit score as compare to Non defaulter 
#also Defualter has more set of variables set around 
#creating logistic reg modle
glm(Status~Credit.Score,data = Default,family ="binomial")
#here we are finding porb on No-default
summary(glm(Status~Credit.Score,data = Default,family ="binomial"))
#credit score is significant 

#multipal logestic reg modle
summary(glm(Status~Credit.Score+EMI.Ratio,data = Default,family ="binomial"))
#as we can see that Emi ratio is significant but very low
summary(glm(Status~Credit.Score+EMI.Ratio+Work.Exp,data = Default,family ="binomial"))
#after adding work exp in model Emi ratio loses its significance (that mean there is multicolinearity presnet in it) 

library("rms")
vif((glm(Status~Credit.Score+EMI.Ratio+Work.Exp,data = Default,family ="binomial")))
#here vif is greater than 5 which is issue of concern
#to see the relation
plot(Work.Exp,Credit.Score)
cor(Work.Exp,Credit.Score)
#as we see that (Work.Exp,Credit.Score) are strongly correlated 
#so haveing both in same model cancel out each other so we can remove one of them
summary(glm(Status~EMI.Ratio+Work.Exp,data = Default,family ="binomial"))
#now we can see that my Emi ratio and work exp are significant for the model
logistic.Status=glm(Status~EMI.Ratio+Work.Exp,data = Default,family ="binomial")
logistic.Status
exp(0.3854)
#means for every extra year of work exp the odd of being non defaul increas by 47%
plot(Status,Work.Exp)
logistic.Status$fitted.values
#gives the prob
plot(Status,logistic.Status$fitted.values)
#plot showes the prob of being defualt or not
Status.predicted=ifelse(logistic.Status$fitted.values<0.95,"Default","No")
#here  0.95 are the cutoff 
summary(Status.predicted)
table(Status,Status.predicted)
summary(Status)
sensitivity=114/125
sensitivity
specifcity=367/656
specifcity
#as we increse or decrese the cutoff sensitivity and specifcity changes
library("pROC")
roc(Status,logistic.Status$fitted.values)
#
plot.roc(Status,logistic.Status$fitted.values)
#good roc cruve have high specificity and high sensitivty  which can be find by area under roc cruve
# area under the roc curve is as same as the Rsuq in regression 

