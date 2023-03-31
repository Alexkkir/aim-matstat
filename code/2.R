# N1
# (i)
N=1000
eta=sample(1:6,size=N,prob=c(0.5,rep(0.1,5)),replace=TRUE)
hist(eta)
table(eta)
m=c(0,seq(from=-1,to=1,by=0.5))
s=c(1,rep(0.1,5))
X=rep(NA,N)
for (k in 1:N){
  X[k]=rnorm(1,mean=m[eta[k]],sd=s[eta[k]])
}

# (ii)
hist(X)
?hist
p=function(x){
  y=0.5*dnorm(x,0,1)
  for (j in 0:4){
    y=y+0.1*dnorm(x,mean=m[j+2],sd=s[j+2])
  }
  return(y)
}
hist(X,freq=FALSE,ylim=c(0,max(sapply(u,p))))
u=seq(from=-3.5,to=3.5,length=1000)
lines(sapply(u,p)~u,type="l",col="red")

hist(X,freq=FALSE,ylim=c(0,max(sapply(u,p))),breaks="FD")
u=seq(from=-3.5,to=3.5,length=1000)
lines(sapply(u,p)~u,type="l",col="red")

hist(X,freq=FALSE,ylim=c(0,max(sapply(u,p))),breaks="Scott")
u=seq(from=-3.5,to=3.5,length=1000)
lines(sapply(u,p)~u,type="l",col="red")


for (k in 10:20){
  H=hist(X,breaks=k,plot=FALSE)
  plot(H,main=paste(k,";",length(H$breaks)))
}

for (k in 10:20){
  H=hist(X,breaks=seq(from=min(X),to=max(X),length=k),plot=FALSE)
  plot(H,main=paste(k,";",length(H$breaks)))
}

H=hist(X,breaks=c(min(X),-3,-2,-1,0,1,2,max(X)))

# (iii)
methods=c("Sturges","FD","Scott")
mise=rep(NA,length(methods))
Q=10000
u=seq(from=-2,to=2,length=Q)
for (k in 1:length(methods)){
  H=hist(X,breaks=methods[k],plot=F)
  p_n=function(x){
    H$density[max(which(H$breaks<x))]
  }
  aux=function(x){
    (p_n(x)-p(x))^2
  }
  mise[k]=mean(sapply(u,aux))*4
}
mise

hist(X,breaks=1000,freq=FALSE,ylim=c(0,max(sapply(u,p))))
u=seq(from=-3.5,to=3.5,length=1000)
lines(sapply(u,p)~u,type="l",col="red")


# N2
x=faithful$eruptions
density(x)
?density
# (i)
kernels=eval(formals(density.default)$kernel)
plot(density(x,bw=0.3348))
for (k in 2:length(kernels)){
  lines(density(x,bw=0.3348,kernel=kernels[k]),col=k)
}
legend("topleft",legend=kernels,col=1:length(kernels),cex=0.5,lty=1)

plot(density(x))
for (k in 2:length(kernels)){
  lines(density(x,kernel=kernels[k]),col=k)
}
legend("topleft",legend=kernels,col=1:length(kernels),cex=0.5,lty=1)


I=rep(NA,length(kernels))
for (k in 1:length(kernels)){
  I[k]=density(x,kernel=kernels[k],bw=0.3348,give.Rkern=TRUE)
}
I[2]/I

# (ii)
band=c("nrd","nrd0","ucv","bcv","SJ")
plot(density(x,bw=band[1]),ylim=c(0,0.6))
for (k in 2:length(band)){
  lines(density(x,bw=band[k]),col=k)
}
legend("topleft",legend=band,col=1:length(band),cex=0.5,lty=1)

# (iii)
install.packages("mixtools")
library(mixtools)
?normalmixEM
d=normalmixEM(x)
d$lambda
d$mu
d$sigma
plot(d)
plot(d,density=TRUE)
d$ft

d=normalmixEM(x,k=5,maxit=5000)
plot(d,density=TRUE)

# (iv)
d=normalmixEM(x)
d$lambda
d$mu
d$sigma
library(rootSolve)
f=function(x){
  dnorm(x,mean=d$mu[1],sd=d$sigma[1])-dnorm(x,mean=d$mu[2],sd=d$sigma[2])
}
root_em=uniroot(f,interval=d$mu)$root
cl1_em=x[which(x<=root_em)]
cl2_em=x[which(x>root_em)]

d=density(x)
plot(density(x))
aux=cbind(d$x,d$y)
aux=aux[which(aux[,1]>2 & aux[,1]<4),]
root_kde=aux[which.min(aux[,2]),1]
cl1_kde=x[which(x<=root_kde)]
cl2_kde=x[which(x>root_kde)]
plot(density(cl1_em))
lines(density(cl1_kde),col="red")
