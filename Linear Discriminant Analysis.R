               ## Linear Discriminant Analysis ####
library("MASS")
?lda
Default=read.csv(file.choose())
attach(Default)
lda.Status=lda(Status~EMI.Ratio+Work.Exp,data = Default,CV=TRUE)
lda.Status
lda.Status=lda(Status~EMI.Ratio+Work.Exp,data = Default)
lda.Status
lda.Status$posterior
#It gives the posterior prob of default and no defualt
plot(Status,lda.Status$posterior[,2])
#we can change the cutoff by setting status predictor
Status.predicted=ifelse(lda.Status$posterior[,2]<0.95,"Default","No")
table(Status,Status.predicted)
table(Status)
sensitivity=101/125
sensitivity
specifcity=451/656
specifcity
roc(Status,lda.Status$posterior[,2])
plot.roc(Status,lda.Status$posterior[,2])
lda.Status$class
table(Status,lda.Status$class)
#why its no give me right prediction? coz here the cutoff of class is 0.5 
lda.Status=lda(Status~EMI.Ratio+Work.Exp,data = Default)
lda.Status
#we can set our prior prob by own
lda.Status=lda(Status~EMI.Ratio+Work.Exp,prior=c(0.5,0.5),data = Default)
lda.Status
#to see what happed to our posterior prob due change in prior prob
lda.Status=lda(Status~EMI.Ratio+Work.Exp,prior=c(0.5,0.5),data = Default,CV=TRUE)
lda.Status$posterior
plot(Status,lda.Status$posterior[,2])
table(Status,lda.Status$class)
