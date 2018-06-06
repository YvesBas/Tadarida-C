library(randomForest)
library(data.table)
PredAdd=select=c("FreqM","Tstart","Tend","NbCris","Ind"
                 ,"Duree","OrderNum","Overlap","FreqDiff","FreqRatio")
args="tabase3HF_France_IdConc.csv"
args[2]="SpeciesList.csv"
args[3]="IdExhaustifsTriees.csv"

IdConc1=fread(args[1])
IdConc2=fread(args[3])
IdConc2sel=subset(IdConc2,select=names(IdConc1))
IdConc=rbind(IdConc1,IdConc2sel)

SpeciesList=fread(args[2])


IdConc=IdConc[order(IdConc$Group.1,IdConc$Order),]
IdConc$OrderNum=as.numeric(gsub("N","",IdConc$Order))
IdConc$Duree=IdConc$Tend-IdConc$Tstart

test=match("003760_0_20160907_033641_676.wav",levels(as.factor(IdConc$Group.1)))


VraiSp=vector()
Overlap=vector()
FreqDiff=vector()
FreqRatio=vector()
IdConc2=IdConc[0,]
for (i in 1:nlevels(as.factor(IdConc$Group.1)))
{
  IdSub=subset(IdConc,IdConc$Group.1==levels(as.factor(IdConc$Group.1))[i])
  
  ListSp=vector()
  TS=5
  TE=0
  FreqDom=999
  for (j in 1:nrow(IdSub))
  {
    VraiSp=c(VraiSp,!(IdSub$IdMan[j] %in% ListSp))
    ListSp=c(ListSp,IdSub$IdMan[j])
    TSO=max(TS,IdSub$Tstart[j])
    TEO=min(TE,IdSub$Tend[j])
    OverlapTemp=max(0,TEO-TSO)/max(1e-3,TE-TS)
    Overlap=c(Overlap,OverlapTemp)
    TS=min(TS,IdSub$Tstart[j])
    TE=max(TE,IdSub$Tend[j])
    
    if(j==1)
    {
      FreqDiff=c(FreqDiff,999)
      FreqRatio=c(FreqRatio,999)
      FreqInit=IdSub$FreqM[j]
    }else{
      FreqDiff=c(FreqDiff,abs(IdSub$FreqM[j]-FreqInit))
      FreqRatio=c(FreqRatio,IdSub$FreqM[j]/FreqInit)
    }
  
    
  }
  
  IdConc2=rbind(IdConc2,IdSub)
  
  if(i%%1000==1)
  {
    print(paste(i,nlevels(as.factor(IdConc$Group.1)),Sys.time()))
  }

    
}

IdConc2$VraiSp=VraiSp
IdConc2$Overlap=Overlap
IdConc2$FreqDiff=FreqDiff
IdConc2$FreqRatio=FreqRatio



ColIdConcSp=match(SpeciesList$Nesp,colnames(IdConc2))
ColIdConcSpSel=unique(ColIdConcSp[!is.na(ColIdConcSp)])
IdConcSp=IdConc[,..ColIdConcSpSel]

ProbaMax=aggregate(IdConcSp
                   ,by=list(IdConc$Group.1), FUN=max)
colnames(ProbaMax)[2:ncol(ProbaMax)]=paste0(colnames(ProbaMax)[2:ncol(ProbaMax)],"_Max")

IdConc_pourRF=merge(IdConc2,ProbaMax,by="Group.1")
if(nrow(IdConc2)!=nrow(IdConc_pourRF)){stop("problem w merge")}

ColPred=c(PredAdd,colnames(IdConcSp)
             ,colnames(ProbaMax)[2:ncol(ProbaMax)])

boxplot(IdConc_pourRF$OrderNum~IdConc_pourRF$VraiSp)
boxplot(IdConc_pourRF$Ind~IdConc_pourRF$VraiSp)
boxplot(IdConc_pourRF$FreqRatio~IdConc_pourRF$VraiSp,ylim=c(0,6))
boxplot(IdConc_pourRF$Leppun~IdConc_pourRF$VraiSp)
boxplot(IdConc_pourRF$noise~IdConc_pourRF$VraiSp)

Predictors=IdConc_pourRF[,..ColPred]

Sys.time()
ClassifNbSp=randomForest(x=Predictors,y=as.factor(IdConc_pourRF$VraiSp)
                            ,importance=T) 
Sys.time()

varImpPlot(ClassifNbSp,cex=0.6)

save(ClassifNbSp,file=paste0("CNS_",substr(args[1],1,nchar(args[1])-4),".learner"))


