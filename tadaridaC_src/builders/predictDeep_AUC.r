library(data.table)
library(pROC)
#FIdConc="RSDB_HF_tabase3HF_sansfiltre_IdTot_woSR_IdConc"
#FIdConc="ProbIdConc_2021-02-16"
FIdConc="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point_du_5_janvier_2022/tests_classifiers/11247/predictions_11247_74_11247_jeutest10181.csv"
GroupingSp=T
SpeciesList=fread("C:/Users/yvesb/Downloads/classes11200.csv")
DiscardRSDB=F
#ColD="Filename" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3
ColD="fichier wav" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3
ColV="classe attendue"
AUCgraphDir="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point_du_5_janvier_2022/tests_AUC"


dir.create(AUCgraphDir)
IdConc=fread(FIdConc)
testD=match(ColD,colnames(IdConc))

FIdConc=gsub(".csv","",basename(FIdConc))
FIdConc=gsub(".txt","",FIdConc)


if(DiscardRSDB) #obsolete
{
  Sys.time()
  FilesRSDB=list.files("./RSDB_HF",recursive=T
                       ,full.names=T,pattern=".wav$")
  Sys.time()
  IdConc=subset(IdConc,!(as.data.frame(IdConc)[,testD] %in% basename(FilesRSDB)))
}




if(sum(grepl("IdMan",names(IdConc)))==0)
{
  IdV=subset(IdConc,select=ColV)
    IdConc$IdMan=as.data.frame(IdV)[,1]
}
#else{
#   IdConc$IdMan=IdConc$valid.espece
#}

if(GroupingSp)
{
  test=match(IdConc$IdMan,SpeciesList$Esp)
  IdConc$IdMan=SpeciesList$Nesp2[test]
}


ListSp=levels(as.factor(IdConc$IdMan))
ListSp=subset(ListSp,ListSp!="")
ListSp=subset(ListSp,ListSp %in% names(IdConc))

#IdConc=subset(IdConc,!is.na(IdConc$IdMan))

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
  png(filename=paste0(AUCgraphDir,"/",ListSp[i]
                      ,"_AUC.png"),res=100)
  
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

fwrite(AUCtable,paste0(AUCgraphDir,"/",FIdConc,"_AUC.csv"),sep=";")
save(ROClist,file=paste0(AUCgraphDir,"/",FIdConc,"_ROC.RData"))       
