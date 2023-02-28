
crimes1<-read.csv('criminalobs.csv')
View(crimes1)
head(crimes1)

#crimes<- subset(crimes1, select = c('Date', 'Year', 'Updated.On', 'Block', 'Primary.Type', 'Description', 'Arrest', 'Domestic', 'District', 'Ward'))
#View(crimes)

crimes1$Date<- as.Date(crimes1$Date,"%m/%d/%Y")

library(ggplot2)
library(xts)
library(forecast)
library(dygraphs)

# daily plot + acf +pacf
ggplot(data=crimes1,aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x=crimes1$Date, y=crimes1$n))+theme_bw()+labs(x="Year", y="Obs.", title= "Crimes per Day")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
acf(crimes1$n, lag.max = 366)
pacf(crimes1$n, lag.max = 366)
tsdisplay(crimes1$n,lag.max = 366)

# daily diff plot + acf +pacf
ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$Diff))+geom_line(aes(x=crimes1$Date, y=crimes1$Diff))+theme_bw()+labs(x="Day", y="Diff. Obs.", title= "Crimes per Day Diff")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
acf(crimes1$Diff, lag.max = 366)
pacf(crimes1$Diff, lag.max = 366)
tsdisplay(crimes1$Diff,lag.max = 366)

#m1 = arima(0,1,1)
m1<-arima(crimes1$Diff, order = c(0,0,1))
m1
m1f<- forecast(m1)
m1f
plot(m1f)

#m2 = arima (2,1,1) daily
m2<-auto.arima(crimes1$n)
m2
m2f<- forecast(m2)
m2f
plot(m2f)

#m3 = arima(1,1,3) daily
m3<-auto.arima(crimes1$Diff)
m3
m3f<- forecast(m3)
m3f
plot(m3f)

crimes2<-read.csv("crimeschi2.csv")

crimes2<- aggregate(.~Year.Week.,crimes2, FUN=sum)
crimes2$WeekNum<-ts(crimes2$Year.Week., frequency = 52)
#crimes2$Year.Week.<- crimes2$Year.Week.

#basic time series data
ggplot(crimes2)+geom_line(aes(x=crimes2$WeekNum, y=crimes2$n))

# weekly plot + acf +pacf
ggplot(crimes2,aes(x=crimes2$WeekNum, y=crimes2$n))+geom_line(aes(x=crimes2$WeekNum, y=crimes2$n))+theme_bw()+labs(x="Week", y="Obs.", title= "Crimes per Week")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
acf(crimes2$n, lag.max = 55)
pacf(crimes2$n, lag.max = 55)
tsdisplay(crimes2$n,  lag.max = 53)

# weekly diff plot + acf +pacf
ggplot(crimes2,aes(x=crimes2$WeekNum, y=crimes2$Diff))+geom_line(aes(x=crimes2$WeekNum, y=crimes2$Diff))+theme_bw()+labs(x="Week", y="Diff. Obs.", title= "Crimes per Week Diff")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
acf(crimes2$Diff, lag.max = 55)
pacf(crimes2$Diff, lag.max = 55)
tsdisplay(crimes2$Diff,  lag.max = 53)

#m4 = arima weekly
m4 <- auto.arima(crimes2$n)
m4
m4f<- forecast(m4)
m4f
plot(m4f)
ggplot()+geom_line(aes(x=(1:989), y=m4f$fitted))

ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x= crimes1$Date, y=m2f$fitted, color="blue"))

#regr
regr<-lm(crimes1$n ~ crimes1$Date)
summary(regr)
fit<- predict.lm(regr)
ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x=crimes1$Date, y=fit, color='red'))

?accuracy

#diff obs + m1 + mean
ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$Diff))+geom_line(aes(x=crimes1$Date, y=crimes1$Diff))+geom_line(aes(x= crimes1$Date, y=m1f$fitted, color="M1"))+geom_line(aes(x=crimes1$Date, y=mean(crimes1$Diff), color='Regr'))+theme_bw()+labs(x="Year", y="Diff. Obs.", title= "Crimes per Day Diff ARIMA M1")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
ggplot()+geom_point(aes(x=crimes1$Date, y=m1f$residuals))+geom_line((aes(x=crimes1$Date, y=mean(m1f$residuals), color="Mean", size = 1)))+theme_bw()+labs(x="Year", y="Residuals", title= "Crimes per Day Residuals M1")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
accuracy(m1f)

#obs + m2 + regr
ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x=crimes1$Date, y=crimes1$n))+geom_line(aes(x= crimes1$Date, y=m2f$fitted, color="M2"))+geom_line(aes(x=crimes1$Date, y=fit, color='Regr'))+theme_bw()+labs(x="Year", y="Obs.", title= "Crimes per Day ARIMA M2")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
ggplot()+geom_point(aes(x=crimes1$Date, y=m2f$residuals))+geom_line((aes(x=crimes1$Date, y=mean(m2f$residuals), color="Mean", size=1)))+theme_bw()+labs(x="Year", y="Residuals", title= "Crimes per Day Residuals M2")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
accuracy(m2f)

#diff obs + m3 + mean
ggplot(crimes1,aes(x=crimes1$Date, y=crimes1$Diff))+geom_line(aes(x=crimes1$Date, y=crimes1$Diff))+geom_line(aes(x= crimes1$Date, y=m3f$fitted, color="M3"))+geom_line(aes(x=crimes1$Date, y=mean(crimes1$Diff), color="Regr"))+theme_bw()+labs(x="Year", y="Diff. Obs.", title= "Crimes per Day Diff ARIMA M3")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
ggplot()+geom_point(aes(x=crimes1$Date, y=m3f$residuals))+geom_line((aes(x=crimes1$Date, y=mean(m3f$residuals), color="Mean", size=1)))+theme_bw()+labs(x="Year", y="Residuals", title= "Crimes per Day Residuals M3")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
accuracy(m3f)

#regr
wreg<- lm(crimes2$n~crimes2$WeekNum)
fit3<- predict.lm(wreg)

#weekly obs + m4 + regr
ggplot(crimes2)+geom_line(aes(x=crimes2$WeekNum, y=crimes2$n))+geom_line(aes(x=crimes2$WeekNum, y=m4f$fitted , color="M4"))+geom_line(aes(x=crimes2$WeekNum, y=fit3, color="Regr"))+theme_bw()+labs(x="Week", y="Observances of Crime", title= "Weekly Crime + Model") + theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
ggplot()+geom_point(aes(x=crimes2$WeekNum, y=m4f$residuals))+geom_line((aes(x=crimes2$WeekNum, y=mean(m4f$residuals), color="Mean", size = 1)))+theme_bw()+labs(x="Week", y="Residuals", title= "Crimes per Week Residuals M4")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
accuracy(m4f)

#decompose
crimes3<- crimes2[!(crimes2$WeekNum > 942),]
crimes3$WeekNum<- ts(crimes3$WeekNum)
crimes3$n<- ts(crimes3$n)
y<- ts(crimes3$n, frequency = 52)
yeardiff<- diff(crimes3$n, lag = 52, differences = 1)
x<- ts(yeardiff, frequency = 52)
decom<-decompose(y, 'multiplicative')
decom2<-decompose(x, 'multiplicative')
plot(decom)
plot(decom2)
?xts

# weekly  plot + acf +pacf
ggplot()+geom_line(aes(x=crimes3$WeekNum, y=crimes3$n)) +theme_bw()+labs(x="Week", y="Obs.", title= " Crimes per Week")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
dygraph(y, main = "Crimes per Week", xlab = "Week", ylab = "Observations")
acf(y, lag.max=55)
pacf(y, lag.max=55)
tsdisplay(y,  lag.max = 53)

#weekly w/ yearly diff plot + acf + pacf
ggplot()+geom_line(aes(x=c(1:890), y=yeardiff))+theme_bw()+labs(x="Week", y="Diff. Obs.", title= " Crimes per Week Diff")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
dygraph(x)
acf(yeardiff, lag.max = 55)
pacf(yeardiff, lag.max = 55)
tsdisplay(x,  lag.max = 53)

#m5 = sarima (1,1,1)(1.1.0)[52]
m5<- auto.arima(y)
m5
m5f<- forecast(m5)
plot(m5f, main = 'SARIMA(1,1,1)(1.1.0)[52] + Prediction', xlab = 'Years', ylab = 'Crime Obs.')+lines(x=crimes4$yrs, y=crimes4$n)
crimes4<- crimes2[!(crimes2$WeekNum <= 942),]
crimes4$Year.Week.<- as.vector(crimes4$Year.Week.)
crimes4$yrnum<- substr(crimes4$Year.Week.,1,nchar(crimes4$Year.Week.)-3)
crimes4$nmm<- substr(crimes4$Year.Week.,6,nchar(crimes4$Year.Week.))
crimes4$nmm<- as.numeric(crimes4$nmm)
crimes4$nmm<- crimes4$nmm/52
crimes4$yrnum<- as.numeric(crimes4$yrnum)
crimes4$yr<- crimes4$yrnum + crimes4$nmm
crimes4$yrs<- crimes4$yr - 2000

#substr(a$data,1,nchar(a$data)-3)

#m6
m6<-auto.arima(x)
m6
m6f<- forecast(m6)
plot(m6f)
?plot

#regr2
regr2<- lm(crimes3$n~crimes3$WeekNum, crimes3)
summary(regr2)
fit2<- predict.lm(regr2)

# crimes per week + sarima (m5) + regr2 plot
ggplot()+geom_line(aes(x=crimes3$WeekNum, y=crimes3$n))+geom_line(aes(x=crimes3$WeekNum, y=m5f$fitted), color= "blue")+theme_bw()+labs(x="Week", y="Obs. of Crime", title= " Crimes per Week + SARIMA")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))+geom_line(aes(x=crimes3$WeekNum, y=fit2),color="red")
tsdisplay(m5f$residuals,lag.max = 52)
#tsdisplay(m5f$fitted,lag.max = 52)

#add fit and residuals to data set
crimes3$m5f<-m5f$fitted
crimes3$resid<-crimes3$n-crimes3$m5f

#residual plot of m5
ggplot()+geom_point(aes(x=crimes3$WeekNum, y=m5$residuals), size =2)+geom_line(aes(x=crimes3$WeekNum, y=0), color="Blue", size = 2)+theme_bw()+labs(x="Week", y="Residuals", title= " Crimes per Week M5 Residuals")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"), legend.text = element_text(face= "bold", size = 14))
m5
accuracy(m5f)
?theme

# obs + sarima (m6) + mean
ggplot()+geom_line(aes(x=c(1:890), y=yeardiff))+geom_line(aes(x=c(1:890), y=m6f$fitted),color = "red")+geom_line(aes(x=c(1:890), y=mean(yeardiff)), color= "blue")+theme_bw()+labs(x="Week", y="Diff. Obs.", title= " Crimes per Week SARIMA Diff")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"))
ggplot()+geom_point(aes(x=c(1:890), y=m6f$residuals), size = 2)+geom_line(aes(x=crimes3$WeekNum, y=mean(m6f$residuals), color="Mean"), size = 2)+theme_bw()+labs(x="Week", y="Residuals", title= " Crimes per Week M6 Residuals")+ theme(axis.text.x = element_text(angle = 90),  axis.text = element_text(size = 12), axis.title = element_text(size = 16, face= "bold"), plot.title = element_text(size = 18, face= "bold"), legend.text = element_text(face= "bold", size = 14))
accuracy(m6f)
tsdisplay(m6f$residuals,lag.max = 52)

# run a acf and pacf on the residuals

m1 
m2
m3
m4
m5
m6
accuracy(m1)
accuracy(m2)
accuracy(m3)
accuracy(m4)
accuracy(m5)
accuracy(m6)

acf(m5f$residuals, lag.max = 53)
pacf(m5f$residuals, lag.max = 53)



