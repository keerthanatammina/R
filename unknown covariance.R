x=read.csv(choose.files())
x
attach(x)
N=dim(x)[1]
P=dim(x)[2]
sigma=matrix(c(1000,100,100,20),nrow=2)
sigma
sigma.in=solve(sigma)
sigma%*%sigma.in
sigma.in
mu=c(170,70)
mu
xbar=apply(x,MARGIN=2,FUN=mean)
xbar=colMeans(x)
xbar
chi.test=N *t(xbar-mu)%*%sigma.in%*%(xbar-mu)
chi.test
chi.crit=qchisq(0.05,P,lower.tail=FALSE)
chi.crit
Decision=ifelse(chi.test>chi.crit,"REJECT THE NULL HYPOTHESIS","FAIL TO REJECT THE NULL HYPOTHESIS")
Decision