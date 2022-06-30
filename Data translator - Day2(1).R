########################################
## R Exercise DAY 2 ##
########################################

## 1. HYPOTHESES TESTING

qatar<-read.csv("qatar.csv")

## Qatar example
t.test(qatar$income[qatar$country=="kor"],qatar$income[qatar$country=="qat"])


kbo<-read.csv("baseball.csv",fileEncoding = "euc-Kr")

## kbo example
t.test(kbo$관중수[kbo$홈=="KIA"],kbo$관중수[kbo$홈=="삼성"])

## What about LG vs. 두산?
t.test(kbo$관중수[kbo$홈=="LG"],kbo$관중수[kbo$홈=="두산"])

## What about Saturday vs. Sunday?
t.test(kbo$관중수[kbo$요일=="토"],kbo$관중수[kbo$요일=="일"])

##### Useful codes
aggregate(kbo$관중수,by=list(kbo$홈),mean)
byhome_mean<-aggregate(kbo$관중수,by=list(kbo$홈),mean)
byhome_mean[order(byhome_mean$x),]
byhome_mean[order(byhome_mean$x,decreasing=TRUE),]
write.csv(byhome_mean,"byhome_mean.csv")


gapminder=read.csv("gapminder.csv")

## Life span example
t.test(gapminder$lifeExp[gapminder$year==2007],gapminder$lifeExp[gapminder$year==2002])
t.test(gapminder$lifeExp[gapminder$year==2007]-gapminder$lifeExp[gapminder$year==2002])


## 2. SIMPLE REGRESSION

restaurant<-read.csv("restaurant.csv")
summary(restaurant)

## Running a regression model
hist(restaurant$sale)
hist(restaurant$ad_spend)
cor(restaurant$sale,restaurant$ad_spend)
plot(restaurant$sale,restaurant$ad_spend)

## Running a regression model
model1<-lm(sale~ad_spend,restaurant)
summary(model1)

## A. Interpretation & prediction
## What is the predicted sales when ad_spending is 0?
## What is the predicted increment in sales whan ad_spending increases by 1?
## What is the predicted sales when ad_spending is 0.5?
## What is the residual when ad_spending is 0.5?

## Useful codes
fitted(model1) # Fitted value for all the observations
resid(model1) # Residual for all the observations
coef(model1)%*%c(1,0.5) # Model prediction

## B. Addressing model fit
plot(fitted(model1),restaurant$sale)
abline(0,1,col=2)

## C. Hypothesis testing




## 3. Multiple regression
cor(restaurant)
pairs(restaurant)
hist(restaurant$price)
hist(restaurant$compet)

## Running a regression model
model2<-lm(sale~ad_spend+price+compet,restaurant)
summary(model2)

## A. Interpretation & Prediction
fitted(model2)
resid(model2)
coef(model2)%*%c(1,2,2,10) 
## DIY1. Can you make a prediction when ad=8.5, price=3, compet=4?

## B. Model fit
plot(fitted(model1),restaurant$sale)
abline(0,1,col=2)
plot(fitted(model2),restaurant$sale)
abline(0,1,col=2)

## C. Hypotheses testing



## 4. Regression - Popularity

pop<-read.csv("popularity.csv")
head(pop)
cor(pop$Feet.size,pop$Height)
cor(pop$Height,pop$pop)
cor(pop$Feet.size,pop$pop)
plot(pop$Feet.size,pop$pop)

## Simple regression model
model0<-lm(pop~Feet.size,pop)
summary(model0)

## Multiple regression model
model1<-lm(pop~Age+Height+Feet.size+Salary,pop)
summary(model1)

table(pop$Blood)
pop$Blood<-as.factor(pop$Blood) ## Making this as a categorical variable
model2<-lm(pop~Age+Height+Feet.size+Salary+Blood,pop)
summary(model2)

pop$Blood<-relevel(pop$Blood,ref="B")  ## Changing the baseline.
model3<-lm(pop~Age+Height+Feet.size+Salary+Blood,pop)
summary(model3)



## 5. Regression - Apartment