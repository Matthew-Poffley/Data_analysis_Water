#Brecon looping

library(readxl)
library(fpp2)

#Brecon 3

Error.B3<-rep(NA,2592)
for(k in 1:2592){
  Brecon3.fit<-auto.arima(Brecon3[seq(from=k,length=336)])
  j <- k+336
  Error.B3[k]<-Brecon3[j]-forecast(Brecon3.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.B3))
plot(Error.B3,type="l",main="Brecon 3: Oct-Nov 16")
sum(Error.B3>.5)
abline(h=.5,col="red")
Dates[which(Error.B3>.5)+336]
T2[which(Error.B3>.5)+336]

#Brecon 4

Error.B4<-rep(NA,2592)
for(k in 1:2592){
  Brecon4.fit<-auto.arima(Brecon4[seq(from=k,length=336)])
  j <- k+336
  Error.B4[k]<-Brecon4[j]-forecast(Brecon4.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.B4))
plot(Error.B4,type="l",main="Brecon 4: Oct-Nov 16")
sum(Error.B4>.5)
abline(h=.5,col="red")
Dates[which(Error.B4>.5)+336]
T2[which(Error.B4>.5)+336]

#Brecon 5

Error.B5<-rep(NA,2592)
for(k in 1:2592){
  Brecon5.fit<-auto.arima(Brecon5[seq(from=k,length=336)])
  j <- k+336
  Error.B5[k]<-Brecon5[j]-forecast(Brecon5.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.B5))
plot(Error.B5,type="l",main="Brecon 5: Oct-Nov 16")
sum(Error.B5>.5)
abline(h=.5,col="red")
Dates[which(Error.B5>.5)+336]
T2[which(Error.B5>.5)+336]

#Brecon 6

Error.B6<-rep(NA,2592)
for(k in 1:2592){
  Brecon6.fit<-auto.arima(Brecon6[seq(from=k,length=336)])
  j <- k+336
  Error.B6[k]<-Brecon6[j]-forecast(Brecon6.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.B6))
plot(Error.B6,type="l",main="Brecon 6: Oct-Nov 16")
sum(Error.B6>.5)
abline(h=.5,col="red")
Dates[which(Error.B6>.5)+336]
T2[which(Error.B6>.5)+336]

#Cotswold 1

Error.c1<-rep(NA,2592)
for(k in 1:2592){
  Cotswold1.fit<-auto.arima(Cotswold1[seq(from=k,length=336)])
  j <- k+336
  Error.c1[k]<-Cotswold1[j]-forecast(Cotswold1.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c1))
plot(Error.c1,type="l",main="Cotswold1: Oct-Nov 16")
sum(Error.c1>.5)
abline(h=.5,col="red")
Dates[which(Error.c1>.5)+336]
T2[which(Error.c1>.5)+336]

#Cotswold 2

Error.c2<-rep(NA,2592)
for(k in 1:2592){
  Cotswold2.fit<-auto.arima(Cotswold2[seq(from=k,length=336)])
  j <- k+336
  Error.c2[k]<-Cotswold2[j]-forecast(Cotswold2.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c2))
plot(Error.c2,type="l",main="Cotswold2: Oct-Nov 16")
sum(Error.c2>.5)
abline(h=.5,col="red")
Dates[which(Error.c2>.5)+336]
T2[which(Error.c2>.5)+336]

#Cotswold 3

Error.c3<-rep(NA,2592)
for(k in 1:2592){
  Cotswold3.fit<-auto.arima(Cotswold3[seq(from=k,length=336)])
  j <- k+336
  Error.c3[k]<-Cotswold3[j]-forecast(Cotswold3.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c3))
plot(Error.c3,type="l",main="Cotswold3: Oct-Nov 16")
sum(Error.c3>.5)
abline(h=.5,col="red")
Dates[which(Error.c3>.5)+336]
T2[which(Error.c3>.5)+336]

#Cotswold 4

Error.c4<-rep(NA,2592)
for(k in 1:2592){
  Cotswold4.fit<-auto.arima(Cotswold4[seq(from=k,length=336)])
  j <- k+336
  Error.c4[k]<-Cotswold4[j]-forecast(Cotswold4.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c4))
plot(Error.c4,type="l",main="Cotswold4: Oct-Nov 16")
sum(Error.c4>.5)
abline(h=.5,col="red")
Dates[which(Error.c4>.5)+336]
T2[which(Error.c4>.5)+336]

#Cotswold 5

Error.c5<-rep(NA,2592)
for(k in 1:2592){
  Cotswold5.fit<-auto.arima(Cotswold5[seq(from=k,length=336)])
  j <- k+336
  Error.c5[k]<-Cotswold5[j]-forecast(Cotswold5.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c5))
plot(Error.c5,type="l",main="Cotswold5: Oct-Nov 16")
sum(Error.c5>.5)
abline(h=.5,col="red")
Dates[which(Error.c5>.5)+336]
T2[which(Error.c5>.5)+336]

#Cotswold 6

Error.c6<-rep(NA,2592)
for(k in 1:2592){
  Cotswold6.fit<-auto.arima(Cotswold6[seq(from=k,length=336)])
  j <- k+336
  Error.c6[k]<-Cotswold6[j]-forecast(Cotswold6.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.c6))
plot(Error.c6,type="l",main="Cotswold6: Oct-Nov 16")
sum(Error.c6>.5)
abline(h=.5,col="red")
Dates[which(Error.c6>.5)+336]
T2[which(Error.c6>.5)+336]

#Mendip 1

Error.m1<-rep(NA,2592)
for(k in 1:2592){
  Mendip1.fit<-auto.arima(Mendip1[seq(from=k,length=336)])
  j <- k+336
  Error.m1[k]<-Mendip1[j]-forecast(Mendip1.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.m1))
plot(Error.m1,type="l",main="Mendip1: Oct-Nov 16")
sum(Error.m1>.5)
abline(h=.5,col="red")
Dates[which(Error.m1>.5)+336]
T2[which(Error.m1>.5)+336]

#Mendip 2

Error.m2<-rep(NA,2592)
for(k in 1:2592){
  Mendip2.fit<-auto.arima(Mendip2[seq(from=k,length=336)])
  j <- k+336
  Error.m2[k]<-Mendip2[j]-forecast(Mendip2.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.m2))
plot(Error.m2,type="l",main="Mendip2: Oct-Nov 16")
sum(Error.m2>.5)
abline(h=.5,col="red")
Dates[which(Error.m2>.5)+336]
T2[which(Error.m2>.5)+336]

#Mendip 4

Error.m4<-rep(NA,2592)
for(k in 1:2592){
  Mendip4.fit<-auto.arima(Mendip4[seq(from=k,length=336)])
  j <- k+336
  Error.m4[k]<-Mendip4[j]-forecast(Mendip4.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.m4))
plot(Error.m4,type="l",main="Mendip4: Oct-Nov 16")
sum(Error.m4>.5)
abline(h=.5,col="red")
Dates[which(Error.m4>.5)+336]
T2[which(Error.m4>.5)+336]


#Mendip 5

Error.m5<-rep(NA,2592)
for(k in 1:2592){
  Mendip5.fit<-auto.arima(Mendip5[seq(from=k,length=336)])
  j <- k+336
  Error.m5[k]<-Mendip5[j]-forecast(Mendip5.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.m5))
plot(Error.m5,type="l",main="Mendip5: Oct-Nov 16")
sum(Error.m5>.5)
abline(h=.5,col="red")
Dates[which(Error.m5>.5)+336]
T2[which(Error.m5>.5)+336]

#Quantock 1

Error.q1<-rep(NA,2592)
for(k in 1:2592){
  Quantock1.fit<-auto.arima(Quantock1[seq(from=k,length=336)])
  j <- k+336
  Error.q1[k]<-Quantock1[j]-forecast(Quantock1.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q1))
plot(Error.q1,type="l",main="Quantock1: Oct-Nov 16")
sum(Error.q1>.5)
abline(h=.5,col="red")
Dates[which(Error.q1>.5)+336]
T2[which(Error.q1>.5)+336]

#Quantock 2

Error.q2<-rep(NA,2592)
for(k in 1:2592){
  Quantock2.fit<-auto.arima(Quantock2[seq(from=k,length=336)])
  j <- k+336
  Error.q2[k]<-Quantock2[j]-forecast(Quantock2.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q2))
plot(Error.q2,type="l",main="Quantock2: Oct-Nov 16")
sum(Error.q2>.5)
abline(h=.5,col="red")
Dates[which(Error.q2>.5)+336]
T2[which(Error.q2>.5)+336]

#Quantock 2

Error.q2<-rep(NA,2592)
for(k in 1:2592){
  Quantock2.fit<-auto.arima(Quantock2[seq(from=k,length=336)])
  j <- k+336
  Error.q2[k]<-Quantock2[j]-forecast(Quantock2.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q2))
plot(Error.q2,type="l",main="Quantock2: Oct-Nov 16")
sum(Error.q2>.5)
abline(h=.5,col="red")
Dates[which(Error.q2>.5)+336]
T2[which(Error.q2>.5)+336]

#Quantock 3

Error.q3<-rep(NA,2592)
for(k in 1:2592){
  Quantock3.fit<-auto.arima(Quantock3[seq(from=k,length=336)])
  j <- k+336
  Error.q3[k]<-Quantock3[j]-forecast(Quantock3.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q3))
plot(Error.q3,type="l",main="Quantock3: Oct-Nov 16")
sum(Error.q3>.5)
abline(h=.5,col="red")
Dates[which(Error.q3>.5)+336]
T2[which(Error.q3>.5)+336]

#Quantock 4

Error.q4<-rep(NA,2592)
for(k in 1:2592){
  Quantock4.fit<-auto.arima(Quantock4[seq(from=k,length=336)])
  j <- k+336
  Error.q4[k]<-Quantock4[j]-forecast(Quantock4.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q4))
plot(Error.q4,type="l",main="Quantock4: Oct-Nov 16")
sum(Error.q4>.5)
abline(h=.5,col="red")
Dates[which(Error.q4>.5)+336]
T2[which(Error.q4>.5)+336]

#Quantock 5

Error.q5<-rep(NA,2592)
for(k in 1:2592){
  Quantock5.fit<-auto.arima(Quantock5[seq(from=k,length=336)])
  j <- k+336
  Error.q5[k]<-Quantock5[j]-forecast(Quantock5.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q5))
plot(Error.q5,type="l",main="Quantock5: Oct-Nov 16")
sum(Error.q5>.5)
abline(h=.5,col="red")
Dates[which(Error.q5>.5)+336]
T2[which(Error.q5>.5)+336]

#Quantock 6

Error.q6<-rep(NA,2592)
for(k in 1:2592){
  Quantock6.fit<-auto.arima(Quantock6[seq(from=k,length=336)])
  j <- k+336
  Error.q6[k]<-Quantock6[j]-forecast(Quantock6.fit,h=1)$mean[[1]]
  if((k/100)==round(k/100,0)){print(k)}
}

mean(abs(Error.q6))
plot(Error.q6,type="l",main="Quantock6: Oct-Nov 16")
sum(Error.q6>.5)
abline(h=.5,col="red")
Dates[which(Error.q6>.5)+336]
T2[which(Error.q6>.5)+336]
