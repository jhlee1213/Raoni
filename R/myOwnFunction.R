Floor<-function(v,digits=0){tmp<-v*10^digits; v1<-floor(tmp)/10^digits; return(v1)}

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

mean0<-function(v)mean(na.omit(v))

sum0<-function(v)sum(na.omit(v))

library(moments)
skewness0<-function(v)skewness(na.omit(v))
