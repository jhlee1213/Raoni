library(parallel)
library(doParallel)
library(data.table)

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)


## read data using multi-core

fileList<-paste(getwd(),list.files(recursive=TRUE),sep="/")
datL<-foreach(k = 1:length(fileList)) %dopar% {
  tmp<-as.data.frame(data.table::fread(fileList[k],fill=T))
  tmp[tmp==""]<-NA
  cat(k,"\n");flush.console();
  return(tmp)
}
dat0<-as.data.table(rbindlist(datL,fill=TRUE))
rm(datL)
gc()
fwrite(dat0,paste0("alskdjfladkjf.csv"),na=NA)



## calculate missing rate

cmiss<-colSums(is.na(dat0))/nrow(dat0)
dat0<-dat0[,cmiss<0.9]
rmiss<-rowSums(is.na(dat0))/ncol(dat0)
dat0<-dat0[rmiss<0.5,]

