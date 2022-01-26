library(data.table)

DataOut=fread("./Tadarida/Dbas_deepL/documents du 09-09-2021/predictions_7570_77_7030_2200.txt")
Sample=300
Tag="RSDB7030max"
ListSpTarget=c("Cormon","Serser","Phopho","Phooch","Pasdom","Pasmon","Strdec","Apuapu","Delurb","Hirrus","Picpic","Carcar"
         ,"Chlchl")


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
  Spi=sample(ListSpTarget,1)
  Sitei=sample(ListSites,1)
  DataSpi=subset(DataOut,(DataOut$species==Spi)&(DataOut$site==Sitei))
  if(nrow(DataSpi)>0){
  DataSpi=subset(DataSpi,DataSpi$`pourcentage prediction`==max(DataSpi$`pourcentage prediction`))
    Fseli=sample(DataSpi$file,size=1)
  Fsel=c(Fsel,Fseli)
  Sp=c(Sp,Spi)
  
print(length(Fsel))
  }
}
DataSample=data.frame(Fsel,Sp,Tag,Order=c(1:Sample))
DataSample=DataSample[order(DataSample$Fsel),]
DataSample

fwrite(DataSample,paste0("./Tadarida/Dbas_deepL/",Tag,".csv"),sep=";")
