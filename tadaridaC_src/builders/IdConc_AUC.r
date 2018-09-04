library(data.table)
library(pROC)
FIdConc="ProbEspHF_C2_PF"

IdConc=fread(paste0(FIdConc,".csv"))

if(sum(grepl("IdMan",names(IdConc)))==0)
{
  IdConc$IdMan=IdConc$valid.espece
}

ListSp=levels(as.factor(IdConc$IdMan))

ROClist=list()
AUCtot=vector()

for (i in 2:length(ListSp))
{
  Label=(IdConc$IdMan==ListSp[i])
  testSp=match(ListSp[i],names(IdConc))
  ScoreSp=IdConc[,..testSp]
  #boxplot(as.data.frame(ScoreSp)[,1]~as.factor(Label))
  ROCSp=roc(Label,as.data.frame(ScoreSp)[,1])
  ROClist=c(ROClist,ROCSp)
  plot(ROCSp$sensitivities,ROCSp$specificities,type="l",main=ListSp[i])
  AUCSp=auc(ROCSp)
  AUCtot=c(AUCtot,AUCSp)
  print(paste(ListSp[i],AUCSp))
  }

AUCtable=data.frame(Espece=ListSp[2:length(ListSp)],AUC=AUCtot)

fwrite(AUCtable,paste0("./VigieChiro/ROC/",FIdConc,"_AUC.csv"))
save(ROClist,file=paste0("./VigieChiro/ROC/",FIdConc,"_ROC.RData"))       
