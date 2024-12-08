# file name: NofOne
#
# Rome 6th December 2024
#
path<-getwd() # current directory
#
#-------------------------------------------------------------------------------
# Packages and files
#-------------------------------------------------------------------------------
#
library(haven)
#
#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------
#
mydata<-as.data.frame(read_sav("Illustrative example data.sav")) # read data
head(mydata) # inspect data
par(mfrow=c(1,2))
plot(mydata$studyday,mydata$pain,type="l",col=1,lwd=2,xlab="day",ylab="pain")
plot(mydata$studyday,mydata$PA_24h,type="l",col=1,lwd=2,xlab="day",ylab="PA")
#
#-------------------------------------------------------------------------------
# Is the outcome stationary?
#-------------------------------------------------------------------------------
#
window<-5 # temporal width of time window
n.days<-nrow(mydata) # total number of days available
n.window<-round(n.days/window) # number of windows 
stat.df<-data.frame(window=seq(1:n.window),mean=rep(NA,n.window),sd=rep(NA,n.window))
d<-1
for (i in 1:(n.window)) {
  stat.df$mean[i]<-mean(mydata$PA_24h[d:(d+window-1)])
  stat.df$sd[i]<-sd(mydata$PA_24h[d:(d+window-1)])
  d<-d+window
}
model.mean<-lm(mean~window,data=stat.df)
summary(model.mean)
model.sd<-lm(sd~window,data=stat.df)
summary(model.sd)
par(mfrow=c(1,2))
plot(stat.df$window,stat.df$mean,pch=19,col="blue",xlab="window",ylab="mean")
abline(model.mean,col="red",lwd=2)
plot(stat.df$window,stat.df$sd,pch=19,col="blue",xlab="window",ylab="sd")
abline(model.sd,col="red",lwd=2)
#
#-------------------------------------------------------------------------------
# Decomposition
#-------------------------------------------------------------------------------
#
PA_24h.ts<-ts(mydata$PA_24h,frequency=7) # we need to introduce a window for stl
PA_24h.dec<-stl(PA_24h.ts,s.window=5)
plot(PA_24h.dec)
#
#-------------------------------------------------------------------------------
# Autocorrelation 
#-------------------------------------------------------------------------------
#
PA_24h.ts<-ts(mydata$PA_24h,frequency=1) # transform outcome in a ts object
par(mfrow=c(1,2))
acf(PA_24h.ts,lwd=2) # autocorrelation
pacf(PA_24h.ts,lwd=2) # autocorrelation corrected for previous lags (partial acf)
#
#-------------------------------------------------------------------------------
# Create lagged variables and add to data frame
#-------------------------------------------------------------------------------
#
Lag1<-c()
Lag1[1]<-NA
for (i in 2:n.days) {
  Lag1[i]<-mydata$PA_24h[i-1]
}
#
Lag2<-c()
Lag2[1]<-NA
Lag2[2]<-NA
for (i in 3:n.days) {
  Lag2[i]<-mydata$PA_24h[i-2]
}
#
mydata$Lag1<-Lag1
mydata$Lag2<-Lag2
#
#-------------------------------------------------------------------------------
# Autocorrelation of residuals
#-------------------------------------------------------------------------------
#
model<-lm(PA_24h~Lag1+Lag2,data=mydata)
par(mfrow=c(1,2))
residuals.ts<-ts(model$residuals,frequency=1) # transform residuals in a ts object
acf(residuals.ts,lwd=2) # autocorrelation
pacf(residuals.ts,lwd=2) # autocorrelation corrected for previous lags (partial acf)
#
#-------------------------------------------------------------------------------
# Dynamic regression
#-------------------------------------------------------------------------------
#
model<-lm(PA_24h~Lag1+Lag2+pain,data=mydata)
summary(model)


