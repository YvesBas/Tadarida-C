library(data.table)

DirAddSound="C:/Users/ybas/Documents/mnhn/veolia/TA_LekoMobile"
RSDBlabelled=fread("C:/Users/ybas/Downloads/RSDB_HF_tabase3HF_sansfiltre.csv")
#RSDBlabelled=fread("C:/Users/ybas/Documents/Tadarida/Tadarida-C/RSDBsel_2022-09-08.csv")
SpeciesList=fread("C:/Users/ybas/Documents/Tadarida/Tadarida-C/tadaridaC_src/other_inputs/SpeciesList.csv")

table(RSDBlabelled$Espece)

FileAddSound=list.files(DirAddSound,full.names=T)

FileDataList=list()
for (h in 1:length(FileAddSound)){
  FileDataList[[h]]=fread(FileAddSound[h])
  
  
}
FileData=rbindlist(FileDataList)

FileData$Espece="noise"
FileData$Type="mec"
FileData$Indice=3
FileData$Zone="Seine-Maritime"
FileData$Site="Rouen"
FileData$Commentaire=""
FileData$Materiel="Leko"
FileData$Confidentiel=""
FileData$Date=""
FileData$Auteur="Birdz"
FileData$Etiqueteur="Yves Bas"
FileData$V1=""
FileData$V286=""
FileData$SubNesp=""
#MatchSL=match("noise",SpeciesList$Esp)

RSDBtoadd=merge(FileData,SpeciesList,by.x="Espece",by.y="Esp")
RSDBtoadd$SubNesp=paste(RSDBtoadd$Nesp,RSDBtoadd$Type,sep="_")


RSDBtoadd=subset(RSDBtoadd,select=names(RSDBlabelled))

RSDBextended=rbind(RSDBlabelled,RSDBtoadd)

fwrite(RSDBextended,"RSDBextended.csv",sep=";")

RSDBextendedSample1e5=RSDBextended[sample.int(nrow(RSDBextended),100000,replace=F),]
RSDBextendedSample1e4=RSDBextended[sample.int(nrow(RSDBextended),10000,replace=F),]

fwrite(RSDBextendedSample1e5,"RSDBextendedSample1e5.csv",sep=";")
fwrite(RSDBextendedSample1e4,"RSDBextendedSample1e4.csv",sep=";")
