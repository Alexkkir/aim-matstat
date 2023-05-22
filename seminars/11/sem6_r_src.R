x=read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(x)
install.packages("Information")
library(Information)
?create_infotables
x$rank=factor(x$rank)
IV=create_infotables(data=x,y="admit",bins=10)
IV$Tables$gpa
plot_infotables(IV,"gre")
plot_infotables(IV,IV$Summary$Variable[1:3],same_scale=FALSE)
MultiPlot
res=matrix(data=NA,nrow=9,ncol=2)
for (k in 4:12){
  IV=create_infotables(data=x,y="admit",bins=k)
  res[k-3,]=c(IV$Summary[1,2],IV$Summary[2,2])
}
plot(4:12,res[,1],type="b",ylim=c(min(res),max(res)))
points(4:12,res[,2],type="b",col="blue")
IV=create_infotables(data=x,y="admit",bins=11)
IV$Tables$gre
gre_int=c(420,480,520,540,580,600,620,660,700,740,801)
IV=create_infotables(data=x,y="admit",bins=10)
IV$Tables$gpa
gpa_int=c(2.9,3.04,3.17,3.31,3.39,3.49,3.61,3.75,3.94,5)
average=matrix(data=NA,nrow=11,ncol=10)
for (j in 1:11){
  for (k in 1:10){
    ind=which(x$gre<gre_int[j] & x$gpa<gpa_int[k])
    average[j,k]=mean(x$admit[ind])
  }
  x$gre[ind]=1000
}
average
x=read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
x$rank=factor(x$rank)
for (k in 1:length(x$admit)){
  ind1=min(which(x$gre[k]<gre_int))
  ind2=min(which(x$gpa[k]<gpa_int))
  res[k]=average[ind1,ind2]
}
res
install.packages("PRROC")
library(PRROC)
plot(roc.curve(scores.class0=res,weights.class0=x$admit,curve=TRUE))
logist=glm(admit~.,data=x,family="binomial")
ans=predict(logist,type="response")
plot(roc.curve(scores.class0=ans,weights.class0=x$admit,curve=TRUE))
install.packages("ROCR")
library(ROCR)
pred=prediction(ans,x$admit)
plot(performance(pred,"tpr","fpr"),colorize=FALSE)

?mtcars
head(mtcars)
x=mtcars$disp
y=mtcars$mpg
plot(x,y)
L=lm(y~x)
abline(L,col="red")
summary(L)
L=lm(mpg~disp+hp+drat+wt+qsec,data=mtcars)
summary(L)
matr=mtcars[,3:7]
install.packages("Hmisc")
library(Hmisc)
matrr=rcorr(as.matrix(matr),type="spearman")
install.packages("corrplot")
library(corrplot)
corrplot(matrr$r,type="upper",p.mat=matrr$P,sig.level=0.05)
L=lm(mpg~wt+qsec,data=mtcars)
summary(L)
plot(L)
install.packages("scatterplot3d")
library(scatterplot3d)
ss=scatterplot3d(mtcars$wt,mtcars$qsec,mtcars$mpg,angle=145)
ss$plane3d(L,col="red",lty.box = "solid")

install.packages("fANCOVA")
library(fANCOVA)
loess.as(mtcars[c("wt","qsec")],mtcars[,1])
band=c("gcv","aicc")
mse=rep(NA,length(band))
for (k in 1:length(band)){
  model=loess.as(mtcars[c("wt","qsec")],mtcars[,1],criterion=band[k],plot=T)
  mse[k]=mean((mtcars[,1]-model$fitted)^2)
}
mse

band=c("gcv","aicc")
mse=rep(NA,length(band))
for (k in 1:length(band)){
  model=loess.as(mtcars[c("wt","hp")],mtcars[,1],criterion=band[k],plot=T)
  mse[k]=mean((mtcars[,1]-model$fitted)^2)
}
mse

x=mtcars$hp
y=mtcars$mpg
install.packages("np")
library(np)
NW=npreg(txdat=x,tydat=y)
fitted(NW)
plot(NW,col="red")
points(x,y)
mean((y-fitted(NW))^2)
ker=c("gaussian","epanechnikov")
band=c("cv.ls","cv.aic")
type=c("lc","ll")
mse=array(data=NA,c(length(ker),length(band),length(type)))
n=1
y=y[order(x)]
x=sort(x)
for (i in 1:length(ker)){
  for (j in 1:length(band)){
    for (k in 1:length(type)){
      NW=npreg(txdat=x,tydat=y, ckertype=ker[i],bw=band[j],regtype=type[k])
      mse[i,j,k]=mean((y-fitted(NW))^2)
      if (i*j*k==1){
        plot(NW)
        points(x,y)
      }
      else{
        n=n+1
        points(x,fitted(NW),col=n,type="l")
      }
    }
  }
}
mse
L=lm(mpg~hp,data=mtcars)
mean((y-fitted(L))^2)
