mcd <- read.csv("F:/Data Science Training/mcd_nutrition.csv")
x<-as.matrix(mcd[,-1])
items<-mcd[,1]

z<- (scale(x,center=TRUE, scale=TRUE))
z<- t(apply(z,1,FUN= function(x) {x/norm(as.matrix(x),"F")}))


get_query_index <- function(items, Z, query_item) {
  return(which(items==query_item))
}

recommend2 <- function(items, Z, query_index) {
  candidates<-Z[-query_index,,drop=FALSE]
  rtn<-which.max((Z[query_index,,drop=FALSE]%*%t(candidates)))
  if(rtn>=query_index){
    rtn=rtn+1
  }
  return(as.character(items[rtn]))
}

recommend <- function(items, Z, query_index) {
  candidates<-items[-query_index]
  cos_dist<-Z[query_index,,drop=FALSE]%*%t(Z[-query_index,,drop=FALSE])
  
  return(as.character(candidates[sort(cos_dist, decreasing=TRUE, index.return=TRUE)$ix[1]]))
}

recommendTopFive <- function(items, Z, query_index) {
  candidates<-items[-query_index]
  cos_dist<-Z[query_index,,drop=FALSE]%*%t(Z[-query_index,,drop=FALSE])
  
  return(as.character(candidates[sort(cos_dist, decreasing=TRUE, index.return=TRUE)$ix[1:5]]))
}

recommendTopK <- function(items, Z, query_index, k=5) {
  candidates<-items[-query_index]
  cos_dist<-Z[query_index,,drop=FALSE]%*%t(Z[-query_index,,drop=FALSE])
  
  return(as.character(candidates[sort(cos_dist, decreasing=TRUE, index.return=TRUE)$ix[1:k]]))
}