library(xlsx)
library(data.table)

Dir_IdTri="D:/PI_HDL"
SpeciesList=fread("SpeciesList.csv")



SubDir_IdTri=list.dirs(Dir_IdTri)
Dir_txt=subset(SubDir_IdTri,grepl("txt",SubDir_IdTri))

SpeciesShort=subset(SpeciesList,select=c("GroupFR","NomFR","Scientific name","Esp"))
Bat=(SpeciesList$Group=="bat")
SpeciesShort=SpeciesShort[order(-Bat,SpeciesShort$GroupFR),]

for (i in 1:length(Dir_txt))
{
  IdTri=fread(paste0(Dir_txt[i],"/IdTri.csv"))
  IdTri=IdTri[order(IdTri$Group.1,IdTri$Order),]
  
  IdTriShort=subset(IdTri,select=c("Group.1","Order","SpMaxF2","SuccessProb","FreqC","Tstart","Tend","NbCris"))
  names(IdTriShort)=c("Fichier","NumEsp","Espece","Prob","FreqC","Tstart","Tend","NbCris")
  
  write.xlsx2(IdTriShort,paste0(Dir_txt[i],"/Id_Tadarida.xlsx"),sheetName="detail",row.names=F)
  
NbContacts=aggregate(IdTri$Group.1,by=list(IdTri$SpMaxF2),FUN=length)
RisqueMin=aggregate(1-IdTri$SuccessProb,by=list(IdTri$SpMaxF2),FUN=min)$x
FreqMed=round(aggregate(IdTri$FreqC,by=list(IdTri$SpMaxF2),FUN=function(x) quantile(x,0.5))$x)
DureeMoy=round(aggregate(IdTri$Duree,by=list(IdTri$SpMaxF2),FUN=mean)$x,1)
NbCrisMoy=round(aggregate(IdTri$NbCris,by=list(IdTri$SpMaxF2),FUN=mean)$x)

Summary_Tadarida=cbind(NbContacts,RisqueMin,FreqMed,DureeMoy,NbCrisMoy)

Summary_explicit=merge(SpeciesShort,Summary_Tadarida,by.x="Esp",by.y="Group.1")

Bat=(Summary_explicit$GroupFR=="Chauve-souris")
Summary_explicit=Summary_explicit[order(-Bat),]
names(Summary_explicit)=c("Code","Groupe","NomFR","NomSci","NbContacts"
                          ,"RisqueErreur","FrequenceMediane","DureeMoyenne"
                          ,"NbCrisMoyen")

write.xlsx2(Summary_explicit,paste0(Dir_txt[i],"/Id_Tadarida.xlsx")
            ,sheetName="resume",row.names=F,append=T)

print(paste(i,nrow(Summary_explicit)))

}


