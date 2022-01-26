library(data.table)
library(pROC)
#FIdConc="RSDB_HF_tabase3HF_sansfiltre_IdTot_woSR_IdConc"
#FIdConc="ProbIdConc_2021-02-16"
DirPred="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point_du_5_janvier_2022/tests_classifiers/"
Pattern="_jeutest10181"
GroupingSp=T
SpeciesList=fread("C:/Users/yvesb/Downloads/classes11200.csv")
DiscardRSDB=F #obsolete
#ColD="Filename" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3
ColD="fichier wav" #indicate where names of data identifier; "Filename" if from C1 Classifier, "Group.1" from C3
ColV="classe attendue"
AUCgraphDir="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point_du_5_janvier_2022/tests_FileAUC"
PerFiles=T #par fichier si TRUE, par cri si FALSE
FPw=5 #ration de pondération entre faux-positifs et faux négatifs (pas au point, à revoir)


dir.create(AUCgraphDir)

IdFiles=list.files(DirPred,pattern=Pattern,recursive=T,full.names=T)
IdFiles=subset(IdFiles,substr(basename(IdFiles),1,12)=="predictions_")

for (h in 1:length(IdFiles)){
  print(basename(IdFiles[h]))
  
  
  IdConc=fread(IdFiles[h])
  
  if(nrow(IdConc)>0)
  {
    testD=match(ColD,colnames(IdConc))
    
    FIdConc=gsub(".csv","",basename(IdFiles[h]))
    FIdConc=gsub(".txt","",FIdConc)
    
    
    if(DiscardRSDB) #obsolete
    {
      Sys.time()
      FilesRSDB=list.files("./RSDB_HF",recursive=T
                           ,full.names=T,pattern=".wav$")
      Sys.time()
      IdConc=subset(IdConc,!(as.data.frame(IdConc)[,testD] %in% basename(FilesRSDB)))
    }
    
    
    
    
    #if(sum(grepl("IdMan",names(IdConc)))==0)
    #{
      IdV=subset(IdConc,select=ColV)
      IdConc$IdMan=as.data.frame(IdV)[,1]
    #}
    
    IdD=subset(IdConc,select=ColD)
    IdConc$file=as.data.frame(IdD)[,1]
    
    #else{
    #   IdConc$IdMan=IdConc$valid.espece
    #}
    
    if(GroupingSp)
    {
      test=match(IdConc$IdMan,SpeciesList$Esp)
      IdConc$IdMan=SpeciesList$Nesp2[test]
      IdConc=subset(IdConc,!is.na(IdConc$IdMan))
    }
    
    
    
    ListSp=levels(as.factor(IdConc$IdMan))
    ListSp=subset(ListSp,ListSp!="")
    #ListSp=subset(ListSp,ListSp %in% names(IdConc))
    
    #IdConc=subset(IdConc,!is.na(IdConc$IdMan))
    
    ROClist=list()
    AUCtot=vector()
    AUCwtot=vector()
    Ntest=vector()
    for (i in 1:length(ListSp))
    {
      Label=(IdConc$IdMan==ListSp[i])
      testSp=match(ListSp[i],names(IdConc))
      if(is.na(testSp)) #regroupement (normalement)
      {
        ListGroup=subset(SpeciesList$Esp,SpeciesList$Nesp2==ListSp[i])
        ListGroup=subset(ListGroup,ListGroup %in% names(IdConc))
        IdGroup=subset(IdConc,select=ListGroup)
        ScoreSp=apply(IdGroup,1,sum)
      }else{
        ScoreSp=IdConc[,..testSp]
      }
      
      if(PerFiles){
        Fscore=aggregate(ScoreSp,by=list(IdConc$file),max)
        names(Fscore)=c("Group.1","x")
        #Fscore=subset(Fscore,select="x")
        Flabel=aggregate(Label,by=list(IdConc$file),max)
        #Flabel=subset(Flabel,select="x")
        #boxplot(as.data.frame(ScoreSp)[,1]~as.factor(Label))
        ROCSp=roc(as.logical(Flabel$x),Fscore$x)
        Ntest=c(Ntest,sum(Flabel$x))
      }else{
        ROCSp=roc(Label,as.data.frame(ScoreSp)[,1])
        Ntest=c(Ntest,sum(Label))
      }
      ROCWsp=ROCSp # pour stocker une courbe roc pondérée mais c'est pas au point
      ROCWsp$sensitivities=1-(1-ROCSp$sensitivities)/FPw
      #plot(ROCWsp$sensitivities,ROCSp$sensitivities)
      ROCWsp$sensitivities[length(ROCWsp$sensitivities)]=0
      
      ROClist=c(ROClist,ROCSp)
      png(filename=paste0(AUCgraphDir,"/",ListSp[i],"_"
                          ,FIdConc,"_AUC.png"),res=100)
      
      print(    plot(ROCSp,type="l",main=ListSp[i]
                     ,grid=c(0.1, 0.1),grid.col=c("green", "red"),xlim=c(0,1),ylim=c(0,1)
                     ,add=F)
      )
      dev.off()
      plot(ROCSp$sensitivities,ROCSp$specificities,type="l",main=ListSp[i]
           ,grid=c(0.1, 0.2),xlim=c(0,1))
      AUCSp=auc(ROCSp)
      AUCtot=c(AUCtot,AUCSp)
      AUCwtot=c(AUCwtot,auc(ROCWsp))
      
      print(paste(ListSp[i],AUCSp))
    }
    
    
    AUCtable=data.frame(Espece=ListSp,AUC=AUCtot,N=Ntest)
    
    fwrite(AUCtable,paste0(AUCgraphDir,"/",FIdConc,"_AUC.csv"),sep=";")
    save(ROClist,file=paste0(AUCgraphDir,"/",FIdConc,"_ROC.RData"))       
  }
}
