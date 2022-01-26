library(data.table)
library(randomForest)

DataTAtot=fread("mnt/XC/tab/dataTAtotF.csv")
#DataTAtot[is.na(DataTAtot)]=0
NSp=50
ParamToExclude=c("Filename","Species","FileNum","Version","CallNum"
                 ,"SampleRate","FileTA")
Ntree=10
op <- options(digits.secs = 3)
OutputDir="mnt/XC/ClassifNaifAllWn"
dir.create(OutputDir)

FileData=tstrsplit(DataTAtot$Filename,split="-")
DataTAtot$Species=paste(FileData[[1]],FileData[[2]])
table(DataTAtot$Species)
DataTAtot$FileNum=FileData[[3]]
DataTAtot$X10=as.numeric(FileData[[5]]=="x10.wav")
DataTAtot$X10[is.na(DataTAtot$X10)]=0
table(DataTAtot$X10)

ListSp=unique(DataTAtot$Species)


for (h in 1:100000)
{
  print(paste(h,Sys.time()))
  DateStored=Sys.Date()
  SelSp=sample(ListSp,min(NSp,length(ListSp)),replace=F)
  
  DataTA=subset(DataTAtot,DataTAtot$Species %in% SelSp)
  
  
  Param=names(DataTA)
  Param=subset(Param,!Param %in% ParamToExclude)
  #print(Param)
  CorrSpFiles=unique(data.frame(Species=DataTA$Species,Files=DataTA$FileNum))
  table(CorrSpFiles$Species)
  ListSpSel=unique(CorrSpFiles$Species)
  TrainFiles=sample(as.character(CorrSpFiles$Files),round(length(CorrSpFiles$Files)*0.62))
  CorrTrain=subset(CorrSpFiles,CorrSpFiles$Files %in% TrainFiles)
  MissingSpecies=subset(ListSpSel,!ListSpSel %in% CorrTrain$Species)
  if(length(MissingSpecies)>0){
    for (i in 1:length(MissingSpecies))
    {
      CorrSp=subset(CorrSpFiles,CorrSpFiles$Species==MissingSpecies[i])
      TrainFiles=c(TrainFiles,sample(as.character(CorrSp$Files),1))
    }
  }

  DataTrain=subset(DataTA,DataTA$FileNum %in% TrainFiles)
  DataTest=subset(DataTA,!DataTA$FileNum %in% TrainFiles)
  print(dim(DataTrain))
  #print(table(DataTrain$Species))
  NbCallMin=min(table(DataTrain$Species))
  Predictors=subset(DataTrain,select=Param)
  Predictors[is.na(Predictors)]=0
  
  #print(summary(Predictors),max=2000)
  
  #test=apply(Predictors,MARGIN=1,function(x) sum(is.na(x)))
  #test2=apply(Predictors,MARGIN=2,function(x) sum(is.na(x)))
  print(dim(Predictors))  
  
  Sys.time()
  Mod1=randomForest(y=as.factor(DataTrain$Species),x=Predictors,ntree=Ntree
                    ,sampsize=NbCallMin)
  Sys.time()
  
  save(Mod1,file=paste0(OutputDir,"/ClassifNaif_",DateStored,"_",h,".learner"))
  
  DataTest[is.na(DataTest)]=0
  Pred1=predict(Mod1,newdata=DataTest,type="prob")
  DataPred=subset(DataTest,select=c("Species","FileNum","X10","Filename"
                                    ,"CallNum","FreqMP","Dur"))
  DataPred=cbind(DataPred,Pred1)
  fwrite(DataPred,file=paste0(OutputDir,"/PredClassifNaif_",DateStored,"_",h,".csv"))
}
