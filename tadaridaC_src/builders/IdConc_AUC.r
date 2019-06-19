library(data.table)
library(pROC)
FIdConc="ProbEspC3_2019-03-25_G7"
GroupingSp=T
SpeciesList=fread("SpeciesList.csv")
DiscardRSDB=T
ColD="Group.1" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3



IdConc=fread(paste0(FIdConc,".csv"))
testD=match(ColD,colnames(IdConc))

if(DiscardRSDB)
{
  Sys.time()
  FilesRSDB=list.files("./RSDB_HF",recursive=T
                       ,full.names=T,pattern=".wav$")
  Sys.time()
  IdConc=subset(IdConc,!(as.data.frame(IdConc)[,testD] %in% basename(FilesRSDB)))
}




if(sum(grepl("IdMan",names(IdConc)))==0)
{
  if(sum(grepl("IdMan",names(IdConc)))==0)
  {
    IdConc$IdMan=IdConc$ValidId
  }else{
    IdConc$IdMan=IdConc$valid.espece
  }}

if(GroupingSp)
{
  test=match(IdConc$IdMan,SpeciesList$Esp)
  IdConc$IdMan=SpeciesList$Nesp2[test]
}


ListSp=levels(as.factor(IdConc$IdMan))
ListSp=subset(ListSp,ListSp!="")

ROClist=list()
AUCtot=vector()

for (i in 1:length(ListSp))
{
  Label=(IdConc$IdMan==ListSp[i])
  testSp=match(ListSp[i],names(IdConc))
  ScoreSp=IdConc[,..testSp]
  #boxplot(as.data.frame(ScoreSp)[,1]~as.factor(Label))
  ROCSp=roc(Label,as.data.frame(ScoreSp)[,1])
  ROClist=c(ROClist,ROCSp)
  png(filename=paste0("C:/Users/Yves Bas/Documents/VigieChiro/AUC/",ListSp[i],"_"
                      ,FIdConc,".png"),res=100)
  
  print(    plot(ROCSp,type="l",main=ListSp[i]
                 ,grid=c(0.1, 0.1),grid.col=c("green", "red"),xlim=c(0,1),ylim=c(0,1)
                 ,add=F)
  )
  dev.off()
  plot(ROCSp$sensitivities,ROCSp$specificities,type="l",main=ListSp[i]
       ,grid=c(0.1, 0.2))
  AUCSp=auc(ROCSp)
  AUCtot=c(AUCtot,AUCSp)
  print(paste(ListSp[i],AUCSp))
}

AUCtable=data.frame(Espece=ListSp,AUC=AUCtot)

fwrite(AUCtable,paste0("./VigieChiro/ROC/",FIdConc,"_AUC.csv"),sep=";")
save(ROClist,file=paste0("./VigieChiro/ROC/",FIdConc,"_ROC.RData"))       
