Floor<-function(v,digits=0){
  tmp<-v*10^digits
  v1<-floor(tmp)/10^digits
  v1
}

Round<-function(v,digits=0){
  posneg = sign(v)
  z = abs(v)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

get_density<- function(x, y, n = 100) {
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
}
categorize<-function(x,n1){
  x1<-cut(x,quantile(x,seq(0,1,by=1/n1),na.rm=TRUE)-c(0.01,rep(0,n1)),dig.lab = 5)
  return(x1)
}
categorize2<-function(v,n=10,cut.off=0.05){
     qv1<-quantile(v,cut.off,na.rm=TRUE)
     qv2<-quantile(v,1-cut.off,na.rm=TRUE)
     rslt<-cut(v,seq(qv1,qv2,by=(qv2-qv1)/n))
     return(rslt)
}
                
fastAUC <- function(class, probs) { 
      x <- probs 
      y <- class 
      x1 = x[y==1]; n1 = length(x1);  
      x2 = x[y==0]; n2 = length(x2); 
      r = rank(c(x1,x2))   
      auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2 
      return(auc) 
} 

convertColumnName<-function(a){
  colnames(a)<-gsub(" ",".",colnames(a))
  colnames(a)<-gsub("#",".",colnames(a))
  colnames(a)<-gsub("-",".",colnames(a))
  colnames(a)<-gsub("[(]",".",colnames(a))
  colnames(a)<-gsub("[)]",".",colnames(a))
  colnames(a)<-gsub("[[]",".",colnames(a))
  colnames(a)<-gsub("[]]",".",colnames(a))
  colnames(a)<-gsub("[/]",".",colnames(a))
  colnames(a)<-gsub("[:]",".",colnames(a))
  colnames(a)<-gsub("%",".",colnames(a))
  colnames(a)<-gsub("_",".",colnames(a))
  colnames(a)<-gsub("P.AR.Coating..F.","P.Front.PECVD",colnames(a))
  return(a)
}
                
calTimeDiff <- function(x, y,units="secs"){return (difftime(ymd_hms(x), ymd_hms(y), units=units))}


                
                
