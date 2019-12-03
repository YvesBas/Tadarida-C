library(data.table)
AUC1=fread("./VigieChiro/ROC/RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc_AUC.csv")
AUC2=fread("./VigieChiro/ROC/RSDB_HF_tabase3HF_sansfiltre_IdTot_woSR_IdConc_AUC.csv")
Id1=fread("RSDB_HF_tabase3HF_sansfiltre_IdTot_wiSR_IdConc.csv")
Id2=fread("RSDB_HF_tabase3HF_sansfiltre_IdTot_woSR_IdConc.csv")
SpTarget="Phogri"

barplot(AUC1$AUC-AUC2$AUC,names.arg=AUC1$Espece,las=2,cex.names=0.6)
boxplot(AUC1$AUC-AUC2$AUC)

Id1S=subset(Id1,Id1$IdMan==SpTarget)
Id2S=subset(Id2,Id2$IdMan==SpTarget)
Discrep1=subset(Id1S,Id1S$SpMaxF2!=SpTarget)
Discrep2=subset(Id2S,Id2S$SpMaxF2!=SpTarget)

Discrep12=subset(Discrep1,!(Discrep1$Group.1 %in% Discrep2$Group.1))
Discrep21=subset(Discrep2,!(Discrep2$Group.1 %in% Discrep1$Group.1))


for (i in 1:nlevels(as.factor(Id1$IdMan)))
{
  SpTarget=levels(as.factor(Id1$IdMan))[i]
  Id1S=subset(Id1,Id1$IdMan==SpTarget)
  Id2S=subset(Id2,Id2$IdMan==SpTarget)
  Id1F=subset(Id1,Id1$IdMan!=SpTarget)
  Id2F=subset(Id2,Id2$IdMan!=SpTarget)
  
  if(nrow(Id1S)>20)
  {
    Id1TP=subset(Id1S,Id1S$SpMaxF2==SpTarget)
    TP1=as.data.frame(subset(Id1TP,select=SpTarget))[,1]
    Id1FP=subset(Id1F,Id1F$SpMaxF2==SpTarget)
    FP1=as.data.frame(subset(Id1FP,select=SpTarget))[,1]
    Id1FN=subset(Id1S,Id1S$SpMaxF2!=SpTarget)
    FN1=as.data.frame(subset(Id1FN,select=SpTarget))[,1]
    Id2TP=subset(Id2S,Id2S$SpMaxF2==SpTarget)
    TP2=as.data.frame(subset(Id2TP,select=SpTarget))[,1]
    Id2FP=subset(Id2F,Id2F$SpMaxF2==SpTarget)
    FP2=as.data.frame(subset(Id2FP,select=SpTarget))[,1]
    Id2FN=subset(Id2S,Id2S$SpMaxF2!=SpTarget)
    FN2=as.data.frame(subset(Id2FN,select=SpTarget))[,1]
    print(
      boxplot(list(TP1,FP1,FN1,TP2,FP2,FN2),main=SpTarget
              ,names=c(length(TP1),length(FP1),length(FN1)
                       ,length(TP2),length(FP2),length(FN2)))
    )
    
  }
  
  
}

