library('scatterplot3d')
library('rgl')
library('car')
factor=as.factor(species)
plot3d(Sepal.Length,Petal.Length,Petal.Width,point.col="blue",surface=FALSE,groups=factor)