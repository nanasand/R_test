# Sales By Brand
# Prepare Color info. according to Brand
PtsCol = rep("",300)
PtsCol[SalesByBrand$Brand=="Coke"] = "red"
PtsCol[SalesByBrand$Brand=="Pepsi"] = "blue"
PtsCol[SalesByBrand$Brand=="Sprite"] = "green"
# Scatter
plot(SalesByBrand$Price, SalesByBrand$Sales, col=PtsCol, pch=10,
     main="Sales vs. Price", xlab="Price", ylab="Sales")
# Interaction Effect Regression
out = lm(Sales ~ Price + as.factor(Brand) + Price*as.factor(Brand), SalesByBrand)
summary(out)


#Coke : 138.34     -21.6P
#Pepsi: 138.34-128.0162-21.6P+22.04P=10.33+0.44P
#Sprite:138.34-56.9-21.6P+10P= 81.44-11.6P


# Amusement Park
# Regression
out = lm(overall ~ rides + games + wait + clean + distance + num.child +
           distance*wait + num.child*wait, AmusementPark)
summary(out)
# Is there a child or not 
AmusementPark$child01 = 0
AmusementPark$child01[AmusementPark$num.child>0] = 1
# Simple Version Regression
out = lm(overall ~ rides + games + wait + clean + distance + child01 +
           distance*wait + child01*wait, AmusementPark)
summary(out)

# Admission
# Logistic Regression
out = glm(admit~gre+gpa+rank, admission, family=binomial)
summary(out)
# simple Linear Regression
out = lm(admit~gre+gpa+rank, admission)
summary(out)

############################### Titanic
# Survived: 생존유무
# Pclass: 티켓 클래스 (등급)
# Sex: 성별
# Age: 연령
# SSA: 동반 탑승 형제자매/배우자 수
# PCA: 동반 탑승 부모/자녀 수
# Fare: 요금

# 변수명 단순화
colnames(titanic)[6:7] = c("SSA","PCA")

# 로지스틱 회귀분석
out = glm(Survived ~ Pclass + Sex + Age + SSA + PCA + Fare, titanic, family=binomial)
summary(out)

# 분포확인
hist(titanic$Fare, breaks=20)
hist(log(titanic$Fare+1), breaks=20)

# 로지스틱 회귀분석 (로그변환후)
out = glm(Survived ~ Pclass + Sex + Age + SSA + PCA + log(Fare+1), titanic, family=binomial)
summary(out)

# 일부 변수만 사용한 로지스틱 회귀분석 (로그변환후)
out = glm(Survived ~ PCA, titanic, family=binomial)
summary(out)

############################### Lake Huron
# 패키지 설치
install.packages("forecast")
# 패키지 호출
library(forecast)

# 선그래프
plot(LakeHuron)
# 시계열인가?
acf(LakeHuron)
Box.test(LakeHuron, type="Ljung", lag=1)

# AR(1)
out = Arima(LakeHuron, c(1,0,0))
pred = forecast(out, h=20)
plot(pred, main="Prediction for Future 20 Years")
# AR(3)
out = Arima(LakeHuron, c(3,0,0))
pred = forecast(out, h=20)
plot(pred, main="Prediction for Future 20 Years")

# AR 몇을 선택해야하는가?
# AR(1)에서 시작
out = Arima(LakeHuron, c(1,0,0))
resid = residuals(out)
acf(resid)
Box.test(resid, type="Ljung", lag=1)
# AR(2) 시도
out = Arima(LakeHuron, c(2,0,0))
resid = residuals(out)
acf(resid)
Box.test(resid, type="Ljung", lag=1)

############################### Airline Passengers
# 선그래프
plot(AirPassengers, type="l")
# 시계열인가?
acf(AirPassengers)
Box.test(AirPassengers, type="Ljung", lag=1)

# AR(1) model
out = Arima(AirPassengers, c(1,0,0))
pred = forecast(out, h=50)
plot(pred, main="Prediction for Future 50 Months")

# 비정상(non-stationary)과 계절성(seasonality) 고려
out = Arima(log(AirPassengers), order=c(1,1,0),
            seasonal=list(order=c(1,1,0), period=12))
pred = forecast(out, h=50)
plot(pred, main="Prediction for Future 50 Months")

############################### Coke Revenue
# 선그래프
plot(cokerev, type="l")
# 시계열인가?
acf(cokerev$revenue)
Box.test(cokerev$revenue, type="Ljung", lag=1)

# AR(1)
out = Arima(cokerev$revenue, c(1,0,0))
pred = forecast(out, h=20)
plot(pred, main="Prediction for Future 20 Quarters")

# 비정상(non-stationary)과 계절성(seasonality) 고려
out = Arima(log(cokerev$revenue), order=c(1,1,0),
            seasonal=list(order=c(1,1,0), period=4))
pred = forecast(out, h=10)
plot(pred, main="Prediction for Future 10 Quarters")

############################### CA Housing
# 패키지 호출
library(tree)

# 데이터 정리
CAhousing$AveBedrms = CAhousing$totalBedrooms/CAhousing$households
CAhousing$AveRooms = CAhousing$totalRooms/CAhousing$households
CAhousing$AveOccupancy = CAhousing$population/CAhousing$households
logMedVal = log(CAhousing$medianHouseValue)
CAhousing = CAhousing[,-c(4,5,9)]

# CART 분석
catree = tree(logMedVal ~ ., data=CAhousing)
plot(catree, col=8, lwd=2)
text(catree)

# 패키지 호출
library(randomForest)

# RF 분석
carf = randomForest(logMedVal ~ ., data=CAhousing,
                    ntree=250, nodesize=25,
                    importance=TRUE)

# 변수별 중요도 시각화
varImpPlot(carf, type=1, pch=21, bg="navy",
           main='RF variable importance')

# 예측값
yhattree = predict(catree, CAhousing)
yhatrf = predict(carf, CAhousing)

# 예측값과 실제값의 상관관계 비교