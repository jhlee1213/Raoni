if(!is.element("moments", installed.packages()[,1])){install.packages("moments")}
library(moments)

skew0<-function(v)skewness(na.omit(v))

mean0<-function(v)mean(na.omit(v))

sd0<-function(v)sd(na.omit(v))

sum0<-function(v)sum(na.omit(v))

