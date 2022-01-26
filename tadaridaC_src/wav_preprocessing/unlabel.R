library(data.table)

DirW="C:/Users/yvesb/Documents/www/wav de jeutest10183"


LW=list.files(DirW,full.names=T)

NameList=tstrsplit(LW,split="--")

NewName=NameList[[3]]

file.rename(from=LW,to=paste0(DirW,"/",NewName))
