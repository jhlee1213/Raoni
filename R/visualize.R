#####################################################################################################################
#                                                                                                                   #
#                                            EDA분석을 위한 Plot Function                                           #
#                                                                                                                   #
#####################################################################################################################
library(ggplot2)

#1. 산점도 
scatter <- function(data, x, y, class=NULL, h = NA, v = NA, cex=0.5,title=""){
  
  class <- deparse(substitute(class))
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))     

  if(class!="NULL"){
  	cl<-data[,class]
     cl<-as.factor(as.character(cl))
  	levels<-unique(cl)
  	color <- palette(rainbow(length(levels))) 
	color_index <- sort(as.integer(levels))
  } else{ class<-NULL} 
  
  ggplot(data = data, aes_string(x = x, y = y, color = class)) +
    geom_point(size = cex) +
    {if(is.null(class)==FALSE) scale_color_manual(values = color[color_index])} + 
    {if (is.na(v) == F) geom_vline(xintercept = v, col = "red",linetype="dashed")} +
    {if (is.na(h) == F) geom_hline(yintercept = h, col = "red",linetype="dashed")} + 
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

#2. 히스토그램
histogram <- function(data, x, bins=10, class=NULL,  h = NA, v= NA,alpha=0.3,title=""){
  
  class <- deparse(substitute(class))
  x <- deparse(substitute(x))
  if(class!="NULL"){
  	cl<-data[,class]
     cl<-as.factor(as.character(cl))
  	levels<-unique(cl)
  	color <- palette(rainbow(length(levels))) 
     color_index <- sort(as.integer(levels))
  } else{class<-NULL} 
  ggplot(data = data, aes_string(x = x, fill = class, col = class)) +
    geom_histogram(position = "identity", alpha = alpha, bins = bins) +
    {if(is.null(class)==FALSE) scale_color_manual(values = color[color_index] )}+
    {if(is.null(class)==FALSE) scale_fill_manual(values = color[color_index] )}+
    {if (is.na(v) == FALSE) geom_vline(xintercept = v, col = "red",linetype="dashed")} +
    {if (is.na(h) == FALSE) geom_hline(yintercept = h, col = "red",linetype="dashed")} +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}


