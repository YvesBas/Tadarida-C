DirQuiz="./chiros/quizMyotis2008"
DirQuizNew="./chiros/quizMyotis2008combi"
#Tri=fread("./chiros/groupesMyotis.csv")
Nsites=6

dir.create(DirQuizNew)

LW=list.files(paste0(DirQuiz),full.names=T,recursive=T
              ,pattern=".wav$")

Pref=substr(basename(LW),1,nchar(basename(LW))-23)
Lpref=unique(Pref)
Lpref=Lpref[sample.int(length(Lpref))]
for (k in 1:ceiling(length(Lpref)/Nsites))
{
  NewDir=paste0(DirQuizNew,"/Quiz_",k)
  dir.create(NewDir)
  Lprefsub=Lpref[((k-1)*Nsites+1):min(length(Lpref),k*Nsites)]
  ToCopy=subset(LW,Pref %in% Lprefsub)
  NewName=paste0(NewDir,"/",basename(ToCopy))
  file.copy(from=ToCopy,to=NewName)
}

