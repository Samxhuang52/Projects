
n1 <- sample(1:5000,100,replace=FALSE)
n1
n2 <- sample(1:5000,100,replace=FALSE)
n2
m2 <- which(is.element(n1,n2)==TRUE)
m2
Estimate <- ((length(n1)*length(n2))/length(m2))

Estimate
##


simu <- function(N=5000,n1=100,n2=100,times=1000){
  m2.all <-c()
  NLP.all <-c()
  for (i in 1:times){
    nn1 <- sample(1:N,n1,replace=FALSE)
    nn2 <- sample(1:N,n2,replace=FALSE)
    m2 <- which(is.element(nn1,nn2)==TRUE)
    Nlp <- ((length(nn1)*length(nn2))/length(m2))
    m2.all<-append(m2.all,length(m2))
    NLP.all<-append(NLP.all,Nlp)
     }
  o1<-data.frame('M2'=m2.all,'Nlp'=NLP.all)
  o2 <- times
  return(list(o1,o2))
}
##histogram##
x<-simu()
hist(x[[1]]$Nlp,freq=TRUE, las=TRUE, 
     col="green", 
     main=paste("Recapture Simulation", sep=""), 
     xlab="Nlp", ylab="frequency")

abline(v=5000, col="firebrick", lty=2, lwd=2)
axis(side=1, at=5000, label="N=5000", col="red", las=TRUE)


y <- length(which(x[[1]]$Nlp == Inf))/1000
y


Nc <- c()
for (i in 1:1000){
  Nc<-append(Nc,(101*101/(x[[1]]$M2[i]+1)-1))
}
hist(Nc,freq=TRUE, las=TRUE, 
     col="green", 
     main=paste("Recapture Simulation", sep=""), 
     xlab="Nlp", ylab="frequency")

abline(v=5000, col="firebrick", lty=2, lwd=2)
axis(side=1, at=5000, label="N=5000", col="red", las=TRUE)


noinf<-length((which(x[[1]]$Nlp != Inf)))
Nlp.mean <-(sum(which(x[[1]]$Nlp!=Inf)/noinf))-5000
Nlp.mean
Nc.mean <- mean(Nc)-5000
Nc.mean
##Nc Better###


large.simu<-function(N=10000,times=1000,size=seq(from=100, to=5000, by=50)){
  n.o <- c()
  bias.o <-c()
  var.o <-c()
  nc.o<- c()

  for (i in 1:length(size)){
    for (t in 1:times){
      nn1 <- sample(1:N,size[i],replace=FALSE)
      nn2 <- sample(1:N,size[i],replace=FALSE)
      n1 <- as.numeric(nn1)
      n2 <- as.numeric(nn2)
     ?is.element
      m2 <- which(is.element(n1,n2)==TRUE)
      nc <- ((length(n1)+1)*(length(n2)+1)/(length(m2)+1))-1
      nc.o<- append(nc.o,nc)
    }
    bias.o <- append(bias.o,(mean(nc.o)-N))
    var.o <- append(var.o, var(nc.o))
  }
  n.o
  return(list(data.frame('n'=size,'bias'=bias.o,'var'=var.o),N))
  
}
ls<-large.simu()

###plot###
par(mfrow=c(2,1))
plot(ls[[1]]$n, ls[[1]]$bias, type="b", main="bias and sample size", xlab="n", ylab="bias",col = "black", lwd = 1)
abline(h=0, col="firebrick", lty=2, lwd=2)
axis(side=4, at=0, label="0", col="red", las=TRUE)
plot(ls[[1]]$n, ls[[1]]$var, type="b", main="bias and var", xlab="n", ylab="var",col = "black", lwd = 1)
abline(h=0, col="red", lty=3, lwd=3)
axis(side=4, at=0, label="0", col="red", las=TRUE)

ls()
save.image("Xinglin Huang.RData")
