DirToTidy="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/donnees_LPO/bruits"
DirOut="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/donnees_LPO/bruits_tidy"
Depth=2

LW=list.files(DirToTidy,full.names=T,recursive=T)


NewN=gsub(" ","_",LW)
NewN=gsub(",","_",NewN)
NewN=gsub("é","e",NewN)
NewN=gsub("è","e",NewN)

NewN=gsub(DirToTidy,DirOut,NewN)

dir.create(DirOut)

dirs=vector()
for (i in 1:length(NewN))
{
  dirs=c(dirs,dirname(NewN))
  dirs=c(dirs,dirname(dirname(NewN)))
}
dirU=unique(dirs)
dirU=dirU[order(dirU)]
for (k in 1:length(dirU))
{
  dir.create(dirU[k],showWarnings=F)
}

file.rename(from=LW,to=NewN)

