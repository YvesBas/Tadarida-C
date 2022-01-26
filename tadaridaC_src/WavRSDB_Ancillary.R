library(data.table)

WavDir="./pourMartynCooke"
RSDB=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RSDB_HF_tabase3HF_sansfiltre.csv")
Confiance=c("POSSIBLE","PROBABLE","SUR","SUR","SUR")

WavF=list.files(WavDir,recursive=T)

test=match(basename(WavF),RSDB$Filename)

Auteur=RSDB$Auteur[test]
Lieu=paste0(RSDB$Site[test],", ",RSDB$Zone[test])
DataSR=data.frame(Fichier=WavF,Auteur,Lieu)

DataAR=DataSR
for (i in 1:nrow(DataAR))
{
  Ri=subset(RSDB,RSDB$Filename==basename(DataAR$Fichier[i]))
  Riu=unique(Ri,by=c("Espece","Indice"))
  Riu$Reponse=paste(Riu$Espece,Confiance[Riu$Indice])
  Reponse=Riu$Reponse[1]
  
  if(nrow(Riu)>1)
  {
    #if(nrow(Riu)>1){stop()}
    for (j in 2:nrow(Riu))
    {
      Reponse=paste(Reponse,Riu$Reponse[j],sep=", ")
    }
    
  }
  DataAR$Reponse[i]=Reponse
}
fwrite(DataAR,paste0(WavDir,"/DataAR.csv"),sep=";")
