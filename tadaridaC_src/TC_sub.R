library(data.table)
DirTC_All="E:/DataSandor/TC"
DirTC_sub="E:/DataSandor/TCsub"

dir.create(DirTC_sub)

TClist=list.files(DirTC_All,pattern=".tc$",full.names=T)

#Prefix=substr(basename(TClist),1,28)
#barplot(table(Prefix),las=2)

FileInfo=tstrsplit(basename(TClist),split="_2019")
Prefix=FileInfo[[1]]

for (i in 1:nlevels(as.factor(Prefix)))
{
  NewDir=paste0(DirTC_sub,"/",levels(as.factor(Prefix))[i])
  dir.create(NewDir)
  TCtocopy=subset(TClist,Prefix==levels(as.factor(Prefix))[i])
  NewName=paste0(NewDir,"/",basename(TCtocopy))
  file.copy(from=TCtocopy,to=NewName)
  print(paste(i,Sys.time()))
}
  
