
setwd("C:\\Users\\benoit\\Desktop\\ENS\\ENSAE\\2A\\S2\\S�ries temp\\Projets")

#Requirements
library(lmtest)
library(zoo)
library(tseries)
library(readr)
library(xtable)
library(ellipse)
library(forecast)

#Chargement des donn�es
#Chargement des donn�es
data <- read.csv("valeurs_mensuelles.csv",sep=";",header = TRUE)

data<-na.omit(data)
data <- data[2:nrow(data),] #le readcsv a mis une ligne de caracteres au debut du coup je l'enleve
colnames(data) <- c("Date","valeur","code")
data <- data[,c("Date","valeur")]

test <- data[1:2,]
train <- data[3:nrow(data),]

#Stats desc
summary(data$valeur)

#Question 2
serie_indice = stats::ts(train[seq(dim(train)[1],1),]$valeur, start=c(1990,1), end=c(2018,11), frequency=12)
plot.ts(serie_indice,ylab="S�rie Brute")

#La s�rie n'a pas l'air d'�tre stationnaire � l'oeil.
#v�rifions d'abord par un acf (non formel) puis par
# un test de Dickey Fuller: 

#acf
acf(serie_indice, lag.max=349)
#La lente d�croissance des autocorrelations (significativement diff�rentes de 0) indique
#de la non stationnarit�.

#DF
adf.test(serie_indice) #on ne rejette pas la non stationnarit�

#PP test
pp.test(serie_indice) #ne rejette pas non plus la stationnarit�.

#KPSS test
kpss.test(serie_indice) #On rejette la stationnarit� du processus

#tranformation pour rendre stationnaire.
Dt<-diff(serie_indice) #diff � l'ordre 1
plot(Dt, ylab="S�rie diff�renci�e")

acf(Dt, lag.max = 50)
pacf(Dt,lag.max = 50, main = "")
adf.test(Dt) #�a c'est ok pour k=0,1,2,3,4,5,6
#PP test
pp.test(Dt)

#KPSS test
kpss.test(Dt)

#Question 3
plot.ts(Dt,ylab="S�rie Diff�renci�e")


#Partie 2 
#Quesion 4 : 



#Identification du mod�le 
#arima (1,1,0)
arima(serie_indice,c(1,1,0))
bic=AIC(arima(serie_indice,c(1,1,0)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,0)(1,1,0)
arima(serie_indice, c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
bic=AIC(arima(serie_indice, c(1,1,0), seasonal=list(order=c(1,1,0),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(1,1,0)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,0),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,0),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(1,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,1),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(1,1,1),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(0,1,1)
model <- arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12)),k = log(length(serie_indice)))
bic

#SARIMA_12 (1,1,1)(2,1,1)
arima(serie_indice, c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
bic=AIC(arima(serie_indice, c(1,1,1), seasonal=list(order=c(2,1,1),period=12)),k = log(length(serie_indice)))
bic


#Etude des r�sidus du mod�le retenu
resi <- residuals(model)
plot(density(resi)) #pas mal
ks.test(resi,"pnorm",0,1)
acf(resi) #fantastique
pacf(resi) #fantastique
ret=c(1:12)
Box.test.2(model,nlag=ret,type="Ljung-Box",fitdf=3)


#plot des r�sidues par rapport � la normale
resi <- residuals(model)
densit�_residus = density(resi)
densit�_th�orique = dnorm(densit�_residus$x, mean=mean(resi),sd=sd(resi))
hist(resi, breaks = 100,main = "", xlab = "R�sidus de l'estimation
     par SARIMA (1,1,1)(0,1,1)", ylab = "Probabilit�",freq=FALSE)
lines(densit�_residus,xaxt="n",yaxt="n",ylab="",xlab="",main="",col = "blue",lwd = 2)
lines(densit�_residus$x,densit�_th�orique,xaxt="n",yaxt="n",ylab="",xlab="",main="",col =
        "blue",lwd=2,lty = 2)


#Autocorr des r�sidus
acf(resi, lag.max = 100, main = "")


#significativit� des coefs du mod�le ?
coeftest(model) #Tous � 1%


# Superposition des ann�es en radar

tr <- function(x){
  return(paste(x,"01",sep="-"))
}

train$Date <- lapply(train$Date,tr)
train$Date <- lapply(train$Date,as.Date)

train$month <- lapply(train$Date,month)
train$year <- lapply(train$Date,year)
toradar <- train[,c("year","month","valeur")]


te <- t(toradar[toradar$year == 1990,]$valeur)
for (i in 1991:2017){
  aux <- t(toradar[toradar$year == i,]$valeur)
  te<-rbind(te,aux)
}
allobs<-data.frame(te)
colnames(allobs) <- c("Dec","Nov","Oct","Sep","Aout","Jui","Juin","Mai","Avr","Mar","Fev","Jan")
rownames(allobs) <- seq(1990:2017)+1990


s <- data.frame(t(allobs))
s <- cbind(data.frame(rownames(s)),s)
colnames(s) <- c("Label",seq(1990:2017)+1990)

chartJSRadar(s, showToolTipLabel=TRUE,main = "Radar plot de la s�rie originale",labelSize = 14,polyAlpha=0.025)

#Partie 3


#Question 7

#Pr�diction de XT+1 et XT+2:
#t+1
x_1=predict(model, n.ahead=2)$pred[1]
#t+2
x_2=predict(model, n.ahead=2)$pred[2]

#Variance estim�e des r�sidus du mod�le
sigma2=var(residuals(model))

#Param�tres qui interviennent dans la variance de la pr�vision de XT+2
theta1 = 0.8617
phi1 = 0.3231

#Matrice de variance-covariance
V2=sigma2*(1+(1+phi1-theta1)^2)
cov=sigma2*(1+phi1-theta1)
#Matrice Sigma de la formule de l'ellipse d�termin�e uestion 5
Sigma=matrix(c(sigma2,cov,cov,V2),nrow=2, ncol=2)
plot(ellipse(Sigma, centre=c(x_1,x_2)), type="l", main="", xlab = "XT+1", ylab = "XT+2")
points (x_1,x_2,pch =4)


### Pr�vision de D�cembre 2018 et Janvier 2019:

vecteur_1819 = data
apredirets = ts(vecteur_1819[seq(dim(vecteur_1819)[1],1),]$valeur, start=c(1990,1), end=c(2019,1), frequency=12)

serie_indice = ts(train[seq(dim(train)[1],1),]$valeur, start=c(1990,1), end=c(2018,11), frequency=12)
model <- arima(serie_indice, c(1,1,1), seasonal=list(order=c(0,1,1),period=12))
predictions = forecast(model,2)

plot(apredirets[(349-12):349], type = "l", lty=c(1,2,2,3), xlab = "", ylab = "Cok�faction et Raffinage", ylim = c(60,150), xaxt='n', lwd=2, main='')
axis(1,at=c(1,4,8,13),labels=c("janv-2018","avril-2018","ao�t-2018","janv-2019"),cex.axis=.8)
lines(seq(12,13,1), predictions$mean, "l", col = 'blue' , lwd = 2)
lines(seq(12,13,1), predictions$lower[,2], 'l', col='blue' ,lty = 2,lwd=1.5)
lines(seq(12,13,1), predictions$upper[,2], 'l', col = 'blue', lty = 2,lwd=1.5)
legend("topleft", c("S�rie observ�e", "Pr�visions", "Intervalle � 95%"), lwd=1, col =c("black","blue","blue"),lty=c(1,1,2),cex=.8)

