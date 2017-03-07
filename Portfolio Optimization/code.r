#1
df<-data.frame(read.csv('asset_data.txt',skip=0,header=TRUE))
dim(df)
#find and match the data on weekly basis
df.x <- df[!is.na(df$fed.rate),]
#convert string to date
df.x$date <- as.Date(df.x$date, format="%Y-%m-%d")
#find the first and last date
head(df.x,1)
tail(df.x,1)
#plot
plot(df$date,df$fed.rate,xlab='Date',ylab='Fed Rate',las=TRUE,main='Interest Rate',type='l')
?plot

#2
#Split the tranning and testing sets
data.training<-df.x[df.x$date < '2014-01-01',]
data.testing <- df.x[df.x$date >= '2014-01-01',]

#3
#convert fed rate in tranning and testing sets to decimal
data.training$fed.rate<-data.training$fed.rate/100
data.testing$fed.rate<-data.testing$fed.rate/100
#calculate return
spy.return <- diff(data.training$close.spy)/data.training$close.spy[1:569]
tlt.return <- diff(data.training$close.tlt)/data.training$close.tlt[1:569]
#plot return
par(mfrow=c(2,1))
plot(data.training$date[2:570],spy.return,type='l',xlab='Date',ylab='SPY Return')
abline(h=0,lty=3,col='red')

plot(data.training$date[2:570],tlt.return,type='l',xlab='Date',ylab='TLT Return')
abline(h=0,lty=3,col='red')

#4
#Check normanality
par(mfrow=c(2,1))
qqnorm(spy.return,main='S&P500')
qqline(spy.return)
qqnorm(tlt.return,main='Tresury')
qqline(tlt.return)

?cor
#5
cor(spy.return,tlt.return)
roll.cor<-c()
for (i in 1:length(spy.return)) {
  
  roll.cor[i]<- cor(spy.return[i:(i+23)],tlt.return[i:(i+23)])
}

plot(data.training$date[2:570],roll.cor,xlab='Date',ylab='Correlation',type='l')
abline(h=0,lyt=3,col='red')

#6
#step1
data.training
er.spy = spy.return-data.training$fed.rate[2:570]/52
er.tlt = tlt.return-data.training$fed.rate[2:570]/52
#step2
g.spy<-c()
g.tlt<-c()
g.spy[1]<-100
g.tlt[1]<-100
for (i in 2:nrow(data.training)){
  g.spy[i] = g.spy[i-1]*(1+er.spy[i-1])
}
for (i in 2:nrow(data.training)){
  g.tlt[i] = g.tlt[i-1]*(1+er.tlt[i-1])
}
#step3
number_y <- length(spy.return)/52
#step4
CAGR.spy <- (g.spy[length(g.spy)]/g.spy[1])^(1/number_y)-1
CAGR.tlt <- (g.tlt[length(g.tlt)]/g.tlt[1])^(1/number_y)-1
length(g.spy)
#step5
v.spy <- (sqrt(52)*sd(er.spy))
v.tlt <- (sqrt(52)*sd(er.tlt))
#step6
SR.spy<-CAGR.spy/v.spy
SR.tlt<-CAGR.tlt/v.tlt

#7
QSeven <- function(weights,A1=spy.return,A2=tlt.return,
                   IR=data.training$fed.rate[2:570]){
SR_v <- c()
for (i in 1:length(weights)){
  r_port.spy <- weights[i]*A1
  r_port.tlt <- (1-weights[i])*A2
  r_port_all <- r_port.spy+r_port.tlt
  r_er = r_port_all-IR/52
  g<-c()
  g[1]<-100
  for (r in 2:length(IR)){
    g[r] = g[r-1]*(1+r_er[r])
  }
  number_y <- length(r_port.spy)/52
  CAGR <- (g[length(g)]/g[1])^(1/number_y)-1
  v <- (sqrt(52)*sd(r_er))
  SR<-CAGR/v
  SR_v[i]<-SR
  }
 return(SR_v)
}
#testing the function
w<-c(0.1,0.3)
QSeven(weights=w,A1=spy.return,A2=tlt.return,IR=data.training$fed.rate[2:570])
#curve function
curve(QSeven, from = 0, to = 1, las=T, xlab = 'Weights', 
      ylab = 'Sharpe Ratio', main='Ratios/Weights')

#8
#find the best porfolio weights
best_port <- optimize(QSeven,c(0,1),maximum=TRUE)
best_port

#9
spy.return.test <- diff(data.testing$close.spy)/data.testing$close.spy[1:42]
tlt.return.test <- diff(data.testing$close.tlt)/data.testing$close.tlt[1:42]
port.return.test<-best_port$maximum*spy.return.test+(1-best_port$maximum)*tlt.return.test

er.spy.test <- spy.return.test-data.testing$fed.rate[2:43]/52
er.tlt.test <- tlt.return.test-data.testing$fed.rate[2:43]/52
er.port.test <- port.return.test-data.testing$fed.rate[2:43]/52

g.spy.test<-c()
g.tlt.test<-c()
g.port.test<-c()
g.spy.test[1]<-100
g.tlt.test[1]<-100
g.port.test[1]<-100
for (i in 2:nrow(data.testing)){
  g.spy.test[i] = g.spy.test[i-1]*(1+er.spy.test[i-1])
  g.tlt.test[i] = g.tlt.test[i-1]*(1+er.tlt.test[i-1])
  g.port.test[i] = g.port.test[i-1]*(1+er.port.test[i-1])
}


plot(data.testing$date,g.spy.test,type='l',col='red',main='Excess Index')
lines(x=data.testing$date,y=g.tlt.test,col='yellow')
lines(x=data.testing$date,y=g.port.test,col='blue')
abline(h=100, col='grey', lty=3)
legend('bottomright',lty=1,legend=c('S&P500','Tresury','Portfolio'),
       col=c('red','yellow','blue'))

#10
#find the last point of index
tail(g.spy.test,1)
tail(g.tlt.test,1)
tail(g.port.test,1)

