library(data.table)
library(lubridate)
library(tuneR)
library(seewave)
library(xlsx)
library(sound)
Tabase=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
#IdConc=fread("RSDB_HF_tabase3HF_sansfiltre_IdTot_IdConc.csv")
#SpTarget="Pleaur"
SpTarget=c(#"Ple"
           #,"Pip"
           #,"Myo","Barbar",
  #"Eptser"
  #,"Hypsav","Minsch"
           #"Myoalc","Myobec","Myobra",
  #"Myocap"
  #,"Myoema","Myomyo"
          #,"Myomys","Myonat","Myodau","Myobly","Myobra","Myodas"
           #,"Nyclas","Nyclei","Nycnoc",
  #,"Pipkuh"
  #,"Pipnat"
           "Pippip"
  #,"Pippyg"
           #,"Pleaur"
  #,"Pleaus","Plemac"
           #,"Rhieur","Rhihip"
           #,"Rhifer","Tadten","Vesmur"
          )
RSDB="./RSDB_HF"
DirQuiz="./chiros/quizMyotis2008"
SRmin=300000
SpeciesList=fread("SpeciesList.csv") #for species groups
CodesConfiance=c("POSSIBLE","PROBABLE","SUR","SUR","SUR")
NperSp=10
Neighbours=F
TolConf=2

LW=list.files(RSDB,pattern=".wav$",recursive=T,full.names=T)


TabaseTriSR=subset(Tabase,Tabase$SampleRate>=SRmin)
FileSR=unique(TabaseTriSR$Filename)
FileSR=gsub(".wav","",FileSR)


for (h in 1:length(SpTarget))
{
  print(SpTarget[h])
  print(Sys.time())
  #test=match(SpTarget[h],SpeciesList$Esp)
  test=grepl(SpTarget[h],SpeciesList$Esp)
  #SpGroup=SpeciesList$Nesp[test]
  SpGroup=subset(SpeciesList$Nesp,test)
  #IdSp=subset(IdConc,IdConc$IdMan %in% (SpGroup))
  #SpGroup=subset(SpGroup,SpGroup %in% colnames(IdConc))
  #IdSpScores=subset(IdSp,select=SpGroup)
  #ScoreGroup=apply(IdSpScores,MARGIN=1,max)
  #MaxI=max(ScoreGroup)
  

  #IdSel=IdSp[0,]
  #for (i in 1:NperSp)
  #{
    #IdSub=subset(IdSp,(ScoreGroup>(i-1)*MaxI/NperSp)&
                  # (ScoreGroup<=(i)*MaxI/NperSp))
    #Fsub=unique(IdSub$Group.1)
      Tsub=subset(Tabase,Tabase$Espece %in% c(SpTarget[h],SpGroup))
    if(nrow(Tsub)>0)
    {
      Tagg=aggregate(Tsub$Indice,by=list(Tsub$Filename),max)
      Tsel=subset(Tagg$Group.1,Tagg$x>max(Tagg$x)-TolConf-1)
      FSel=Tsel[sample.int(length(Tsel),NperSp, replace=T)]      
        }
    
  
  FSel=gsub(".wav","",FSel)
  
  #IdOSp=subset(IdConc,!(IdConc$Group.1 %in% IdSp$Group.1))
  #ScoreSp=subset(IdOSp,select=SpGroup)
  #ScoreGroup2=apply(ScoreSp,MARGIN=1,max)
  #ScoreRel=ScoreSp/IdOSp$Ind
  #IdOSp=IdOSp[order(ScoreGroup2,decreasing=T),]
  #FOSp=unique(IdOSp$Group.1)
  
  #FSel=c(IdSel$Group.1,FOSp[1:NperSp])
  
  TimeSel=ymd_hms(substr(FSel,nchar(FSel)-18,nchar(FSel)-4))
  PrefSel=substr(FSel,1,nchar(FSel)-23)
  PrefTb=substr(Tabase$Filename,1,nchar(Tabase$Filename)-23)
  
  if(Neighbours)
  {
  Fadd=vector()
  for (i in 1:length(FSel))
  {
    Tbsub=subset(Tabase,PrefTb==PrefSel[i])
    TimeSub=ymd_hms(substr(Tbsub$Filename,nchar(Tbsub$Filename)-22,nchar(Tbsub$Filename)-8))
    test=abs(TimeSub-TimeSel[i])
    Tbsub2=subset(Tbsub,test<6)
    Fneighbour=unique(Tbsub2$Filename)
    Fneighbour=subset(Fneighbour,Fneighbour!=FSel[i])
    Fadd=c(Fadd,Fneighbour)
  }
  
  
  FSel=c(FSel,Fadd)
  }
  
  #IdConcSel=subset(IdConc,IdConc$Group.1 %in% FSel)
  FSel=unique(FSel)
  FSel=FSel[order(FSel)]
  Ids=vector()
  Lieu=vector()
  Auteur=vector()
  for (i in 1:length(FSel))
  {
    Tsub=subset(Tabase,Tabase$Filename==paste0(FSel[i],".wav"))
    Tagg=aggregate(Tsub$Indice,by=list(Tsub$Espece),max)
    Identif=""
    for (j in 1:nrow(Tagg))
    {
      Identif=paste0(Identif,Tagg$Group.1[j]," ")
      Conf=CodesConfiance[Tagg$x[j]]
      Identif=paste0(Identif,Conf)
      if(j<nrow(Tagg)){Identif=paste0(Identif,", ")}
    }
    Ids=c(Ids,Identif)
    Lieu=c(Lieu,paste0(Tsub$Site[1],", ",Tsub$Zone[1]))
    Auteur=c(Auteur,Tsub$Auteur[1])
  }
  TabQuiz=data.frame(Fichier=FSel,Ids,Lieu,Auteur)
  TabQuizWoR=TabQuiz
  TabQuizWoR$Ids=NULL
  
  LSel=subset(LW,basename(LW) %in% paste0(FSel,".wav"))
  
  dir.create(DirQuiz)
  dir.create(paste0(DirQuiz,"/",SpTarget[h]))
  NewName=paste0(DirQuiz,"/",SpTarget[h],"/",basename(LSel))
  file.copy(from=LSel,to=NewName)
  
  dir.create(paste0(DirQuiz,"/",SpTarget[h],"_Norm"))
  
  for (i in 1:length(NewName))
  {
    tempW=readWave(NewName[i])  
    tempCL=normalize(tempW,level=0.4,unit="16")
    FreqInit=length(tempCL)/duration(tempCL)
    if(FreqInit>100000){FreqOut=FreqInit/10}else{FreqOut=FreqInit}
    NameNorm=gsub(SpTarget[h],paste0(SpTarget[h],"_Norm"),NewName[i])
    savewav(tempCL,f=FreqOut,filename=NameNorm)
  tempSound=loadSample(NameNorm)
    saveSample(tempSound,filename=NameNorm,overwrite=T)
    }
  
  LAS=list.files(paste0(DirQuiz,"/",SpTarget[h]),full.names=T,recursive=T)
  file.remove(LAS)
  unlink(paste0(DirQuiz,"/",SpTarget[h]),recursive=T) #to remove the empty directory
  write.xlsx(TabQuizWoR
             , file=paste0(DirQuiz,"/",SpTarget[h],"_Norm/0_InfosFichiers_"
                           ,SpTarget[h],".xlsx")
             , sheetName="sans_reponses", row.names=FALSE)
  write.xlsx(TabQuiz
             , file=paste0(DirQuiz,"/",SpTarget[h],"_Norm/0_InfosFichiers_"
                           ,SpTarget[h],".xlsx")
             , sheetName="avec_reponses", append=TRUE, row.names=FALSE)

}
