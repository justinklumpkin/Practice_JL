#senate roll call data

project <- function(v,u){
  return((as.vector(t(v)%*%u) * u)/(norm(as.matrix(u),"F")**2))
  
}

v<- c(3,4)
u<- c(-1,-1)
project(v,u)


senate <- read.csv('F:/Data Science Training/senate_homals.csv')
head(senate)

summary(senate)

X <- senate[,-c(1,2)]
ix <- which(!duplicated(X))
X <- as.matrix(X[ix,])

X <- scale(X,center=TRUE,scale=FALSE)
X <- X/norm(X, type = 'F')


senators <- senate[ix,1]
party <- senate[ix,2]

pca_senate <- prcomp(X)

ix_D <- which(party == "(D)")
ix_R <- which(party == "(R)")
ix_I <- which(party == "(I)")


plot(pca_senate$x[,1], pca_senate$x[,2], type='n')
points(pca_senate$x[ix_D,1], pca_senate$x[ix_D,2], pch=16, col='blue')
points(pca_senate$x[ix_R,1], pca_senate$x[ix_R,2], pch=16, col='red')
points(pca_senate$x[ix_I,1], pca_senate$x[ix_I,2], pch=16, col='green')

#pairs(pca_senate$x[,1],pca_senate$x[,2],pca_senate$x[,3])
