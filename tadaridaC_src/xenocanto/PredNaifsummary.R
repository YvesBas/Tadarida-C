library(data.table)

PredDir="mnt/XC/ClassifNaifAllWn"
Outdir="mnt/XC/PredsumAllWn"
LimitNbFiles=100

PredFiles=list.files(PredDir,pattern="PredClassif",full.names=T)
PredFiles=PredFiles[1:min(length(PredFiles),LimitNbFiles)]
Pinfo=tstrsplit(PredFiles,split="_")
Pnum=max(as.numeric(gsub(".csv","",Pinfo[[3]])))
Pdate=Pinfo[[2]][length(Pinfo[[2]])]

SpeciesList=vector()
for (i in 1:length(PredFiles))
{
  temp=fread(PredFiles[i])
  SpeciesList=unique(c(SpeciesList,unique(temp$Species)))
}

PerfPerSpecies=data.frame()
for (j in 1:length(SpeciesList))
{
  print(paste(SpeciesList[j],Sys.time()))
  DataSp=list()
  for (k in 1:length(PredFiles))
  {
    temp=fread(PredFiles[k])
    DataSp[[k]]=subset(temp,temp$Species==SpeciesList[j])
  }
  DataSpj=rbindlist(DataSp,use.names=T,fill=T)
  testcol=match(SpeciesList[j],names(DataSpj))
  DataSpj$ScoreSp=as.data.frame(DataSpj)[,testcol]
  ScoreAgg=aggregate(DataSpj$ScoreSp
                     ,by=c(list(DataSpj$Filename),list(DataSpj$CallNum))
                     ,FUN=mean)
  FileInfo=tstrsplit(ScoreAgg$Group.1,split="-")
  SplitNum=gsub(".wav","",FileInfo[[4]])
  SplitNum=gsub("L","",SplitNum)
  SplitNum=gsub("R","",SplitNum)
  Slot=floor((as.numeric(SplitNum)-1)/10)
  FileNum=FileInfo[[3]]
  ScoreMaxPerSlot=aggregate(ScoreAgg$x,by=c(list(FileNum),list(Slot))
                            ,FUN=max)
  
  ScoreMaxPerSlot$Rate=NA
  for (l in 1:nrow(ScoreMaxPerSlot))
  {
    SAl=subset(ScoreAgg,(FileNum==ScoreMaxPerSlot$Group.1[l])&
                 (Slot==ScoreMaxPerSlot$Group.2[l])&
                 (ScoreAgg$x==ScoreMaxPerSlot$x[l]))
    SpMax=vector()
    for (m in 1:nrow(SAl))
    {
      Datam=subset(DataSpj,DataSpj$Filename==SAl$Group.1[[m]])
      Spmmax=subset(Datam,Datam$ScoreSp==max(Datam$ScoreSp))
      Spmmax2=apply(Spmmax[,8:ncol(Spmmax)],1,function(x) max(x,na.rm=T))
      Spmmax3=mean(Spmmax2==Spmmax$ScoreSp)
      SpMax=c(SpMax,Spmmax3)
    }
    ScoreMaxPerSlot$Rate[l]=mean(SpMax)
  }
ScoreMaxPerSlot$Species=SpeciesList[j]
    PerfPerSpecies=rbind(PerfPerSpecies,ScoreMaxPerSlot)
  
}

fwrite(PerfPerSpecies,paste0(Outdir,"/PerfSp_",Pdate,"_",Pnum,".csv"),sep=";")
