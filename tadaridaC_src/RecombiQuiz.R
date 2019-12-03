DirQuiz="./chiros/quizRLC"
DirQuizNew="./chiros/quizRLC_groupes"
Tri=fread("./chiros/groupesMyotis.csv")
Nsites=10

dir.create(DirQuizNew)
ListGroupes=unique(Tri$Problematique)

for (i in 1:length(ListGroupes))
{
  
  Trisub=subset(Tri$Espece,Tri$Problematique==ListGroupes[i])
  LW=vector()
  for (j in 1:length(Trisub))
  {
    LWsub=list.files(paste0(DirQuiz,"/",Trisub[j],"_Norm"),full.names=T,recursive=T
                     ,pattern=".wav$")
    LW=c(LW,LWsub)
  }
  
  Pref=substr(basename(LW),1,nchar(basename(LW))-23)
  Lpref=unique(Pref)
  Lpref=Lpref[sample.int(length(Lpref))]
  for (k in 1:ceiling(length(Lpref)/Nsites))
  {
    NewDir=paste0(DirQuizNew,"/",ListGroupes[i],"_",k)
    dir.create(NewDir)
    Lprefsub=Lpref[((k-1)*Nsites+1):min(length(Lpref),k*Nsites)]
    ToCopy=subset(LW,Pref %in% Lprefsub)
    NewName=paste0(NewDir,"/",basename(ToCopy))
    file.copy(from=ToCopy,to=NewName)
  }
}
