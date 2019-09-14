#Functional Data analysis
#function defined on interval, data collected at dicrete points. How to resolve?

install.packages("fda")
library(fda) # load package

data("CanadianWeather") # load data
attach(CanadianWeather) # attach data
?CanadianWeather

CanadianWeather$province
names(CanadianWeather$province)

temp<-CanadianWeather$dailyAv[,,"Temperature.C"]
#temperature for all 365 days over 35 regions



id.toronto <- which(names(CanadianWeather$province)=="Toronto")

day <- c(1:365)

plot(day, temp[,1], ylab="Temperature")

index<-10
plot(day, temp[,index], ylab="Temperature")
CanadianWeather$province[index]


temp.toronto <- CanadianWeather$dailyAv[,id.toronto,1]
plot(day, temp.toronto, xlab="Day", ylab="Temperature", type='l', ylim=range(temp))

for (idx in 1:35){
  lines(day, temp[,idx])
}
dim(temp)


matplot(day, temp,main = "Canada",xlab="Day", ylab="Temperature", type='l', col="light blue")

abline(v=100)
day0<-100
hist(temp[day0,], xlab="Temperature", main=paste("Temp for day ", day0))


m0<-mean(temp[day0,])
var(temp[day0,])
s0<-sd(temp[day0,])#same as above


abline(v=m0, lty=1,lwd = 2, col="red")
abline(v=m0+s0, lty=2,lwd = 2, col="red")
abline(v=m0-s0, lty=2,lwd = 2, col="red")

rowavg<-rowMeans(temp) #avg of temps for each day
std<-apply(temp, 1,sd) #finds standard dev
stdUp<-rowavg+std #1 std dev higher than mean
stdLow<-rowavg-std #1 std dev lower than mean

matplot(day, temp,main = "Canada",xlab="Day", ylab="Temperature", type='l', col="light blue")
lines(day,rowavg,col="red", lwd="2")
lines(day, stdUp, col="red", lty=2,lwd=2)
lines(day, stdLow, col="red", lty=2, lwd=2)

lines(day, rowavg+2*std, col="red", lty=3,lwd=2)
lines(day, rowavg-2*std, col="red", lty=3, lwd=2)

y<-temp[day0,]
day1<-200
z<-temp[day1,]

par(mfrow=c(1,1))
hist(y)
hist(z)

cov(y,z)
cor(y,z)

k <- matrix(0, 365, 365)#stores covariances

for(day0 in 1:365){
  y<-temp[day0,]
  for(day1 in 1:365){
    z<-temp[day1,]
    k[day0,day1]<-cov(y,z)
  }
}

image(day,day,k, xlab = "day 1", ylab = "day 2")


temp.toronto <- CanadianWeather$dailyAv[,id.toronto,1]
plot(day, temp.toronto, xlab="Day", ylab="Temperature", ylim=range(temp))

avg_temp<-mean(temp)
abline(h=avg_temp, col="black", lwd = 2)

lm_toronto<-lm(temp.toronto~day)
lines(lm_toronto$fitted.values,col="red", lwd=2)

day2<-day**2
lm_toronto<-lm(temp.toronto~day+day2)
lines(lm_toronto$fitted.values,col="green", lwd=2)

day3<-day**3
lm_toronto<-lm(temp.toronto~day+day2+day3)
lines(lm_toronto$fitted.values,col="blue", lwd=2)
"Third Degree"
mean((predict(lm_toronto)-temp.toronto)**2)


day4<-day**4
lm_toronto<-lm(temp.toronto~day+day2+day3+day4)
lines(lm_toronto$fitted.values,col="purple", lwd=3)

"Fourth degree"
mean((predict(lm_toronto)-temp.toronto)**2)

day5<-day**5
lm_toronto<-lm(temp.toronto~day+day2+day3+day4+day5)
lines(lm_toronto$fitted.values,col="orange", lwd=2)
"Fifth degree"
mean((predict(lm_toronto)-temp.toronto)**2)


#------------------------------------------------------------------
tt<- seq(0,1,len=101)
plot(tt, rep(1, 101), ylim=c(0,2), type ="l", ylab="monomial basis")

lines(tt,tt,col="red")
lines(tt,tt**2,col="blue")
lines(tt,tt**3, col="green")
mean(tt)
mean(tt*tt^2)

plot(tt, rep(1, 101), ylim=c(-2,3), type ="l", ylab="monomial basis")
lines(tt,cos(2*pi*tt), col="red")
lines(tt,sin(2*pi*tt), col="blue")

mean(cos(2*pi*tt)*sin(4*pi*tt))

#-------------------------------------------------------------------

temp.toronto <- CanadianWeather$dailyAv[,id.toronto,1]
plot(day, temp.toronto, xlab="Day", ylab="Temperature", ylim=range(temp))

avg_temp<-mean(temp)
abline(h=avg_temp, col="black", lwd = 2)


lm_toronto<-lm(temp.toronto~sin(day/365)+cos(day/365))
lines(lm_toronto$fitted.values,col="red", lwd=2)

lm_toronto<-lm(temp.toronto~sin(day/365)+cos(day/365)+sin(2*day/365)+cos(2*day/365))
lines(lm_toronto$fitted.values,col="green", lwd=2)
"2nd"
mean((predict(lm_toronto)-temp.toronto)**2)

lm_toronto<-lm(temp.toronto~sin(day/365)+cos(day/365)+sin(2*day/365)+cos(2*day/365)+sin(4*day/365)+cos(4*day/365))
lines(lm_toronto$fitted.values,col="blue", lwd=2)
"3rd"
mean((predict(lm_toronto)-temp.toronto)**2)

lm_toronto<-lm(temp.toronto~sin(day/365)+cos(day/365)+sin(2*day/365)+cos(2*day/365)+sin(4*day/365)+cos(4*day/365)+sin(3*day/365)+cos(3*day/365))
lines(lm_toronto$fitted.values,col="purple", lwd=2)
"4th"
mean((predict(lm_toronto)-temp.toronto)**2)