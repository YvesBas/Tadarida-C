library(data.table)
DirW="C:/Users/Yves Bas/Documents/RSDB_HF"
SDW=list.dirs(DirW,recursive=F)

for (i in 1:length(SDW))
{
  SSDWtemp=list.dirs(SDW[i],recursive=F)
  SSDWtemp0=list.dirs(SDW[i],recursive=F,full.names=F)
  VerD=subset(SSDWtemp,substr(SSDWtemp0,1,3)=="ver")
  if(length(VerD)>1)
  {
  file.remove(VerD[2:length(VerD)])
    unlink(VerD[2:length(VerD)], recursive=TRUE)
  }
  print(paste(i,"/",length(SDW),Sys.time()))
}