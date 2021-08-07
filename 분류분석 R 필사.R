library(ISLR)
names(Smarket)
getwd()
setwd("G:/")
write.csv(Smarket,"Smarket.csv")
Smarket=Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#로지스틱 회귀모델을 적합하여 Lag1~Lag5 와 volume 이용하여 direction 예측 

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family = binomial)
summary(glm.fit)

coef(glm.fit)

summary(glm.fit)$coef

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)

mean(glm.pred==Direction)

#데이터 분류해서 예측 

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

#2005년 이전의 관측치 셋만 사용하여 로지스틱 회귀모델 적합

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.probs

#예측값들을 계산하고 실제 움직임 방향과 비교 

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]='Up'

table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)


#Lag1 과 Lag2 가지고 적합

glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)

glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.probs

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]='Up'

table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)


#선형 판별분석을 이용하여 에측 

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit


plot(lda.fit)

lda.pred=predict(lda.fit,Smarket.2005)
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

#이차 판별분석 

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.pred=predict(qda.fit,Smarket.2005)
names(qda.pred)

qda.class=qda.pred$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


#knn 인접 caravan 보험자료 이용

Caravan=Caravan


dim(Caravan)

attach(Caravan)
summary(Purchase)

#데이터를 표준화 하여 모든변수들이 평균이 0이고 표준편차가 1이되게 해서 비교 가능한 스케일로 변경

library(class)

standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)

knn.pred

mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)
