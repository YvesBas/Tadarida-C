library(data.table)

DataOut=fread("Data12.csv")
Sample=300
Tag="RSDB7030"

ColSp="Group.2"
ColSp="classe predite"
ColF="Group.1"
ColF="fichier wav"
ColC="pourcentage prediction"

DataF=subset(DataOut,select=ColF)
names(DataF)="file"

DataSp=subset(DataOut,select=ColSp)
names(DataSp)="species"

DataC=subset(DataOut,select=ColC)
names(DataC)="score"

DataOut=cbind(DataOut,DataF,DataSp,DataC)

NbSpF=aggregate(DataOut$species,by=list(DataOut$file),length)
summary(NbSpF$x)

DataOut$ClassC=round(DataOut$score/10)
DataOut$site=substr(DataOut$file,1,24)

ClassList=unique(DataOut$ClassC)
ListSp=unique(DataOut$species)
ListSites=unique(DataOut$site)




Fsel=vector()
Sp=vector()
Class=vector()
while (length(Fsel)<Sample)
{
  Spi=sample(ListSp,1)
  Classi=sample(ClassList,1)
  Sitei=sample(ListSites,1)
  DataSpi=subset(DataOut,(DataOut$species==Spi)&(DataOut$ClassC==Classi)&(DataOut$site==Sitei))
  if(nrow(DataSpi)>0){
  Fi=subset(NbSpF,NbSpF$Group.1 %in% DataSpi$file)
Fseli=sample(Fi$Group.1,prob=1/Fi$x,size=1)
  Fsel=c(Fsel,Fseli)
  Sp=c(Sp,Spi)
  Class=c(Class,Classi)
print(length(Fsel))
  }
}
DataSample=data.frame(Fsel,Sp,Class,Tag,Order=c(1:Sample))
DataSample=DataSample[order(DataSample$Fsel),]
DataSample

fwrite(DataSample,paste0("./Tadarida/Dbas_deepL/",Tag,".csv"),sep=";")
