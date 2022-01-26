library(data.table)

OutBN=fread("./Tadarida/Dbas_deepL/resBN_crepurb25p_data.csv")
Sample=200
Out1=subset(OutBN,OutBN$Rank==1)
Tag="BN_crepurb25"
FilterSpecies=c(         "European Goldfinch"       
                ,"House Sparrow",       "Common House-Martin"
                ,       "Common Redstart" , "Barn Swallow"        
                , "Eurasian Collared-Dove"    
                ,          "Common Swift",              "Eurasian Magpie"
                ,"Black Redstart", "European Serin"   
)

Out1$ClassC=round(Out1$Confidence*10)
SpeciesL=unique(Out1$`Common Name`)
ClassList=unique(Out1$ClassC)

NbSpF=aggregate(Out1$`Common Name`,by=list(Out1$`Begin File`),length)
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
Filei=subset(Out1$`Begin File`,(Out1$`Common Name`==Speciesi)
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

