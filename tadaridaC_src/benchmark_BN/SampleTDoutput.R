library(data.table)

OutBN=fread("./Tadarida/Dbas_deepL/point du 28-10-2021/documents joints/predictions crepurb/predictions_7680_41_7250_jeutest2200.csv")
Sample=200

Tag="TD_crepurb25"
FilterSpecies=c(         "Carcar"       
                ,"Pasdom","Pasmon",       "Delurb","Colmon"
                ,       "Phopho" , "Hirrus"        
                , "Strdec"    
                ,          "Apuapu",              "Picpic"
                ,"Phooch", "Serser","Chlchl"   
)

Out1=OutBN

Out1$ClassC=round(Out1$`pourcentage prediction`/10)
SpeciesL=unique(Out1$`classe predite`)
ClassList=unique(Out1$ClassC)

NbSpF=aggregate(Out1$`classe predite`,by=list(Out1$`fichier wav`),length)
summary(NbSpF$x)

Fsel=vector()
Sp=vector()
Class=vector()
while (length(Fsel)<Sample)
{
  Classi=sample(ClassList,1)
if(is.na(FilterSpecies)){
  Speciesi=sample(SpeciesL,1)
}else{
  Speciesi=sample(FilterSpecies,1)
}
Filei=subset(Out1$`fichier wav`,(Out1$`classe predite`==Speciesi)
             &(Out1$ClassC==Classi))
if(length(Filei)>0)
{
 Fileiw=subset(NbSpF,NbSpF$Group.1 %in% Filei) 
  Files=sample(Fileiw$Group.1,size=1,prob=1/Fileiw$x)
Fsel=c(Fsel,Files)
Sp=c(Sp,Speciesi)
Class=c(Class,Classi)
  }
}

#OutSel=subset(OutBN,OutBN$`Begin File` %in% Fsel)
FileDF=data.frame(Fsel,Sp,Class,Tag,Order=c(1:Sample))
FileDF=FileDF[order(FileDF$Fsel),]
FileDF
table(FileDF$Sp)
#fwrite(FileDF,"FileDF.csv",sep=";")
fwrite(FileDF,paste0("./Tadarida/Dbas_deepL/tests_birdnet/",Tag,".csv"),sep=";")
#fwrite(OutSel,"FileDF_OutSel.csv",sep=";")

