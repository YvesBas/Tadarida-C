library(data.table)
DirIT="E:/DataSandor/IdTots"
DirTC="E:/DataSandor/TC"

dir.create(DirTC)

LIT=list.files(DirIT,pattern="IdTot.csv$",full.names=T)

for (i in 1:length(LIT))
{
  IdTot=fread(LIT[i])
  FTC=levels(as.factor(IdTot$Group.1))
  print(paste(i,Sys.time()))
  for (j in 1:length(FTC))
  {
    idtc=subset(IdTot,IdTot$Group.1==FTC[j])
    NameTC=paste0(DirTC,"/",gsub(".wav",".tc",FTC[j]))
    fwrite(idtc,NameTC)
  }
  
}
