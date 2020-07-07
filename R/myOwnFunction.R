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
plot_contour<-function(x,y,z,n1,n2, xlab="",ylab="",title=""){
  r0<-na.omit(data.frame(x,y,z))
  x<-r0$x; y<-r0$y; z<-r0$z
  x1<-cut(x,quantile(x,seq(0,1,by=1/n1))-c(0.1,rep(0,n1)),dig.lab = 5)
  y1<-cut(y,quantile(y,seq(0,1,by=1/n2))-c(0.1,rep(0,n2)),dig.lab = 5)
  z1<-tapply(z,interaction(x1, y1, sep=  "", lex.order = TRUE),mean)
  c1<-tapply(z,interaction(x1, y1, sep=  "", lex.order = TRUE),length)
  z1[c1<length(x)/(n1*n2*20)]<-NA
  x2<-names(table(x1))
  y2<-names(table(y1))
  Eff<-matrix(z1,n2,n1)
  plot_ly(x=x2,y=y2,z=~Eff,type="contour",contours = list(showlabels = TRUE)) %>%
  layout(xaxis = list(title=xlab), yaxis = list(title=ylab), title=title)
}

plotTrend<-function(v,date,v2=NULL, type="mean",ylim=NULL,col=1,main="",las=1,axes=T,ylab=NA,ylab2=NA,xlab="Date",pch=1){
  tt<-date
  if(type=="mean"){
    v1<-tapply(v,tt,function(v)mean(na.omit(v)))
  } else {  v1<-tapply(v,tt,function(v)median(na.omit(v)))}
  if(is.null(ylim)==TRUE){ylim<-c(min(v1,na.rm=TRUE),max(v1,na.rm=TRUE))}  
  plot(v1,ylim=ylim,xaxt="n",ylab=ylab,xlab=xlab,cex=.5,col=col,main=main,axes=axes,pch=pch)
  points(v1,col=col,cex=.5)
  lines(v1,col=col)
  if(axes==T){axis(1,1:length(v1), tapply(date,tt,function(v)v[1]),las=las)}
  if(is.null(v2)==FALSE){
    par(new=T)
    if(type=="mean"){
      v3<-tapply(v2,tt,function(v)mean(na.omit(v)))
    } else {  v3<-tapply(v2,tt,function(v)median(na.omit(v)))}
    if(is.null(ylim)==TRUE){ylim<-c(min(v3,na.rm=TRUE),max(v3,na.rm=TRUE))}  
    plot(v3,ylim=ylim,xaxt="n",ylab="",xlab="",cex=.5,col=col+1,main=main,axes=F,pch=pch)
    points(v3,col=col+1,cex=.5)
    lines(v3,col=col+1)
    if(axes==T){axis(1,1:length(v3), tapply(date,tt,function(v)v[1]),las=las)}
    axis(side=4,col=col+1,col.axis=col+1)
    mtext(side=4,line=2,ylab2,col=col+1) 
  }
}
                                                                                  
plotScatter1<-function(v1,v2,date,type="mean",ylim=NULL,xlim=NULL,col=1,main="",las=1,axes=T,ylab="",xlab="",pch=1,labels=FALSE){
  tt<-date
  if(type=="mean"){
    v10<-tapply(v1,tt,function(v)mean(na.omit(v)))
    v20<-tapply(v2,tt,function(v)mean(na.omit(v)))
  } else {  
    v10<-tapply(v1,tt,function(v)median(na.omit(v)))
    v20<-tapply(v2,tt,function(v)median(na.omit(v)))
  }
  if(is.null(ylim)==TRUE){ylim<-c(min(v20,na.rm=TRUE),max(v20,na.rm=TRUE))}
  if(is.null(xlim)==TRUE){xlim<-c(min(v10,na.rm=TRUE),max(v10,na.rm=TRUE))}
  
  plot(v10,v20,xlim=xlim,ylim=ylim,ylab=ylab,xlab=xlab,cex=.5,col=col,main=main,axes=axes,pch=pch)
  if(labels==TRUE){text(v10,v20,labels=names(v10),cex=.8,pos=4)}
  
}

plotScatter2<-function(v1,v2,n,type="mean",ylim=NULL,xlim=NULL,col=1,main="",las=1,axes=T,ylab="",xlab="",pch=1,plotLoess=FALSE){
  n1<-floor(length(v1)/n)
  tt<-c(rep(1:n1,rep(n,n1)),rep(n1+1,length(v1)-n*n1))
  if(type=="mean"){
    v10<-as.numeric(tapply(v1,tt,function(v)mean(na.omit(v))))
    v20<-as.numeric(tapply(v2,tt,function(v)mean(na.omit(v))))
  } else {  
    v10<-as.numeric(tapply(v1,tt,function(v)median(na.omit(v))))
    v20<-as.numeric(tapply(v2,tt,function(v)median(na.omit(v))))
  }
  cnt<-as.numeric(tapply(v1,tt,function(v)length(na.omit(v))))
  if(is.null(ylim)==TRUE){ylim<-c(min(v20,na.rm=TRUE),max(v20,na.rm=TRUE))}
  if(is.null(xlim)==TRUE){xlim<-c(min(v10,na.rm=TRUE),max(v10,na.rm=TRUE))}
  
  naIdx<-which(is.na(v10)==TRUE|is.na(v20)==TRUE)
  if(length(naIdx)>0){v10<-v10[-naIdx];v20<-v20[-naIdx]}
  
  plot(v10,v20,xlim=xlim,ylim=ylim,ylab=ylab,xlab=xlab,cex=.5,col=col,main=main,axes=axes,pch=pch)
  if(plotLoess==TRUE){
    v21<-v20[order(v10)]
    v11<-v10[order(v10)]
    cnt1<-cnt[order(v10)]
    lines(v11,predict(loess(v21~v11,span=0.8,weights=cnt1)),col=5) 
  }
}

plotScatter3<-function(v1,v2,n,type="mean",ylim=NULL,xlim=NULL,col=1,main="",las=1,axes=T,ylab="",xlab="",pch=1,labels=FALSE){
  rr<-na.omit(data.frame(v1,v2))
  v1<-rr$v1; v2<-rr$v2
  v11<-cut(v1,quantile(v1,seq(0,1,by=1/n))-c(0.1,rep(0,n)),dig.lab = 5)
  if(type=="mean"){
    v10<-tapply(v1,v11,function(v)mean(na.omit(v)))
    v20<-tapply(v2,v11,function(v)mean(na.omit(v)))
  } else {  
    v10<-tapply(v1,v11,function(v)median(na.omit(v)))
    v20<-tapply(v2,v11,function(v)median(na.omit(v)))
  }
  if(is.null(ylim)==TRUE){ylim<-c(min(v20,na.rm=TRUE),max(v20,na.rm=TRUE))}
  if(is.null(xlim)==TRUE){xlim<-c(min(v10,na.rm=TRUE),max(v10,na.rm=TRUE))}
  
  plot(v10,v20,xlim=xlim,ylim=ylim,ylab=ylab,xlab=xlab,cex=.5,col=col,main=main,axes=axes,pch=pch)
  if(labels==TRUE){text(v10,v20,labels=names(v10),pos=4,cex=.8)}
  return(list(x=v10,y=v20))
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


                
                
