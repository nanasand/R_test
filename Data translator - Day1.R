################################
# Data translator program Exercise: DAY 1 ##
################################


## 1. STARTING R PROGRAMMING

#### 1.A Very basic commands

1+1 #ctrl+enter
1+1 # you can write anything after #
2*2 
5/3


###### How can we make a vector of numbers (a series of numbers)?
(1,2,3,4,5,6,7,8) ## NO
c(1,2,3,4,5,6,7,8)
c(1:8)
seq(1,8)
seq(1,8,by=2)
c(2,4,6,0,3,3,4,5)

###### Operations of vectors
c(2,4,6,0,3,3,4,5)+1
c(2,4,6,0,3,3,4,5)*2
c(1,3,2,4)+c(5,7,8,6)

###### Characters vs. Numbers
Apple # it will be seen as a command without ""
"Apple"

c("Apple","Delecious")

#### 1.B. Loading the data
###### Before you do anything, please make sure you downloaded the data and the code in one folder.
###### Then, you need to set the WORKING DIRECTORY: "SESSION => Choose Directory"

kbo<-read.csv("baseball.csv",fileEncoding = "euc-kr")
head(kbo)
summary(kbo)
View(kbo)
names(kbo)

## You can check it out on top-right.

kbo$날짜
kbo$요일
kbo$홈
kbo$방문
kbo$구장
kbo$관중수

kbo$관중수[c(1:5)]
kbo$관중수[c(2,4,6,8)]

kbo$관중수[kbo$요일=="토"]
kbo$관중수[kbo$요일=="토"|kbo$요일=="일"]
kbo$관중수[kbo$요일=="토"&kbo$요일=="일"]
kbo$관중수[kbo$요일!="토"]
kbo$관중수[kbo$홈=="kia"]
kbo$관중수[kbo$홈=="KIA"]

## DIY: 토요일날 기아경기 관중수?
kbo$관중수[kbo$요일=="토"&kbo$홈=="KIA"]


kbo$관중수[kbo$관중수>25000]
kbo$관중수[kbo$관중수>=25000]
kbo$관중수[kbo$관중수<3117]
kbo$관중수[kbo$관중수<=3117]

## DIY: 10000에서 20000사이 관중수?
kbo$관중수[10000<kbo$관중수&kbo$관중수<20000]

## 2. DESCRIPTIVE ANALYSES
#### 2.A. Representative value: Mean, Median, Mode

qatar<-read.csv("qatar.csv")
View(qatar)
qatar$country
qatar$income

mean(qatar$income)
median(qatar$income)

mean(qatar$income[qatar$country=="qat"])
mean(qatar$income[qatar$country=="kor"])
###### No R funtion for mode. I made my own function. Once you run the following codes you can use "modes" function.
##Frequency
table(qatar$income)
modes<-function(x){
T<-table(x)
T[which.max(T)]
}
modes(qatar$income)


###### Mean - Korea vs. Qatar
mean(qatar$income[qatar$country=="kor"])
mean(qatar$income[qatar$country=="qat"])
barplot(
  c(mean(qatar$income[qatar$country=="kor"]),
    mean(qatar$income[qatar$country=="qat"])),
  names=c("KOREA","QATAR"),main="AVERAGE INCOME"
)

###### DIY1. Median - Korea vs. Qatar
barplot(
  c(median(qatar$income[qatar$country=="kor"]),
    median(qatar$income[qatar$country=="qat"])),
  names=c("KOREA","QATAR"),main="Median INCOME"
)



######## Median for Korea?
######## Median for Qatar?
######## Bar plot that compares the two?


#### 2.B. Distribution and Dispersion
hist(qatar$income)
hist(qatar$income[qatar$country=="kor"])
hist(qatar$income[qatar$country=="qat"])
hist(log(qatar$income[qatar$country=="qat"]))



range(qatar$income)
diff(range(qatar$income))
quantile(qatar$income,c(0,0.5,1))
quantile(qatar$income,seq(0,1,by=0.1))

###### DIY2. Make the two separate histograms: Korea vs. Qatar 

sd(qatar$income)
sd(qatar$income[qatar$country=="kor"])
sd(qatar$income[qatar$country=="qat"])



#### C. Correlation
gapminder<-read.csv("gapminder.csv")
View(gapminder)
head(gapminder)
hist(gapminder$gdpPercap)
hist(log(gapminder$gdpPercap))
hist(gapminder$lifeExp)
cor(gapminder$gdpPercap,gapminder$lifeExp)
cor(log(gapminder$gdpPercap),gapminder$lifeExp)
plot(gapminder$gdpPercap,gapminder$lifeExp)
plot(log(gapminder$gdpPercap),gapminder$lifeExp)

gapminder[1,]
gapminder[c(1:10),]
gapminder[,1]
gapminder[,c(1:2)]
gapminder[c(1:2),c(1:2)]

gapminder[,c(3:6)]
cor(gapminder[,c(3:6)])
pairs(gapminder[,c(3:6)])

###### DIY3. Compute correlation between GDP and population in Asia. Make a scatter plot as well.

hist(gapminder$gdpPercap[gapminder$continent=="Asia"])
hist(log(gapminder$gdpPercap[gapminder$continent=="Asia"]))
hist(gapminder$pop[gapminder$continent=="Asia"])
cor(gapminder$gdpPercap[gapminder$continent=="Asia"],gapminder$pop[gapminder$continent=="Asia"])
plot(log(gapminder$gdpPercap[gapminder$continent=="Asia"]),gapminder$pop[gapminder$continent=="Asia"])
