
Floor<-function(v,digits=0){tmp<-v*10^digits; v1<-floor(tmp)/10^digits; return(v1)}
Round<-function(v,digits=0){tmp<-v*10^digits; idx<-which(tmp-floor(tmp)>=0.5); v1<-floor(tmp)/10^digits; if(length(idx)>0){v1[idx]<-(floor(tmp[idx])+1)/10^digits} ;  return(v1)}
mape<-function(v1,v2){abs((v2-v1)/v1)}
get_density<- function(x, y, n = 100) {
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
}

mean0<-function(v)mean(na.omit(v))
sum0<-function(v)sum(na.omit(v))
