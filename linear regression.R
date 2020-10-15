                # Linear redrassion part1#
library("ISLR")
Carseats
?Carseats
names(Carseats)
head(Carseats)
attach(Carseats)
hist(Sales)
summary(Sales)
# To check the relation between Sales & Price Via Scatter plot
plot(Price,Sales)
#Corelation between Sales & Price
cor(Price,Sales)
# Building a model between Sales & Price
lm(Sales~Price) # where my Sales is dependent Variable of price
#to see a regrassion line in scatter plot
abline(13.64192   ,  -0.05307)
#Every Unite increase in price there is 0.53 decrease in sales
# to see the summary of the model 
summary(lm(Sales~Price))
#conclusions based on summary 
#as we see that T-vale = <2e-16 *** which is less than 0.05 therefore 
#Price is a significant predictor of Sales how much with is given byy R seq vale which is 0.198 that means price is 0.19 or Almost 20% significant to sales
# to see price based on ShelveLoc
plot(ShelveLoc,Sales)
# As we can see in graph As ShelveLoc Good has highest Sales among all
# Where as Bad quality of ShelveLoc reduce the sales almost 50% on avg
#Now we see how significant ShelveLoc are for over Sales
summary(lm(Sales~ShelveLoc))
#ShelveLoc is a significant predictor of Sales (how mucch) which is given byy R seq value which is 2e-16 that means ShelveLoc is 0.317 or Almost 31% significant to sales
summary(ShelveLoc)
#as we see that there are diffrent categories in which ShelveLoc are divided
#so to Convert the Categorical  data in quantitative date we add a dummey variables for each cat



# Multiple regression model
lm(Sales~Price+ShelveLoc)
summary(lm(Sales~Price+ShelveLoc))
#as we see the model is signifcant and Pice and ShelveLoc both combine 54% responsible for my Sales

#Model usin log 
lm(log(Sales[Sales>0])~log(Price[Sales>0]))  #Since Sales cannot be neg therefore we take Sales>0
#log(Price[Sales > 0]) gives slope =  -0.8266   mean 1% increase in price lead to Decreass of 0.82


# making model where all Varable are present 
lm(Sales~.,data = Carseats)
summary(lm(Sales~.,data = Carseats))
# as we can see that population,education,urban,us are not siginficant for our model
#where all other variables are significant by 87%
#that means 87% variaction in data is explaind by model

# creating modle of only siginfcant variables
lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age)
summary(lm(Sales~CompPrice+Income+Advertising+Price+ShelveLoc+Age))
#t-value tell the no of std error the coffeicent avway form zero 
#ie my  Pice is 35 sd away form zero 
#by looking at t-value we say that  which variable is more significant for model
