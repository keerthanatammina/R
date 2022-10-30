data=read.csv(choose.files())
attach(data)
data
x=cbind(x1,x2,x3,x4)
y=na.omit(cbind(data[,5],data[,6],data[,7],data[,8]))
x
y
N1=nrow(x)
N2=nrow(y)
p=ncol(x)
sigma=matrix(c(5,3,2,4,3,20,7,1,2,7,25,8,4,1,8,16),nrow = 4)
sigma
sigma.in=solve(sigma)
sigma.in
xbar=apply(x,MARGIN = 2,FUN=mean)
xbar
ybar=apply(y,MARGIN=2,FUN=mean)
ybar
chi.test=(((N1*N2)/(N1+N2))*t(xbar-ybar)%*%sigma.in%*%(xbar-ybar))
chi.test
chi.crit=qchisq(0.01,p,lower.tail = FALSE)
chi.crit
Decision=ifelse(p<=0.01,"REJECT THE NULL HYPOTHESIS")
Decision