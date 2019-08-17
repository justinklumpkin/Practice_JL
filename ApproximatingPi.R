
approximate_pi<-function(darts=500){
dx <- runif(darts, -1,1)
dy <- runif(darts,-1,1)
cbind(x,y) # this is just so you can see it



incircle<- 0
for(i in 1:darts){
  if(dx[i]**2 +dy[i]**2 <= 1){
    incircle<- incircle+1
  }
}

areaOfSquare<- 4
pi1<- incircle/darts *areaOfSquare
#pi1
#incircle
return(pi1)
}

