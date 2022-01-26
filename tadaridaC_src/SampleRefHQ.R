library(data.table)
tabase=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RSDB_HF_tabase3HF_sansfiltre.csv")
SpeciesList=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/SpeciesList.csv")
LRSDB=list.files("D:/RSDB_HF",pattern=".wav$",full.names=T,recursive=T)
NewDir="./pourLouis3"
SpSel=c("Urorug","Urobre","Cyrscu"
        ,"Epheph","Ephter","Ephpro"
        ,"Isopyr"
        ,"Barfis"
        ,"Antped","Antsor","Anthis","Antcho","Antbou"
    ,"Rhaann","Pteger","Ptepon","Ptecor","Ptebon"
        ,"Plaaff"
        ,"Confus"
        ,"Roeroe"
        ,"Plasab"
        ,"Eupcha","Euptyr"
        ,"Sepsep"
        ,"Tyllil"
        ,"Phofem"
        ,"Metbra"
        ,"Decalb"
        ,"Plafal"
        ,"Phafal"
        ,"Ptepon"
        ,"Antped"
        ,"Condor"
        ,"Pteger"
        ,"Tetcan"
        ,"Bicbic"
        ,"Antius"
        ,"Thycor"
        ,"Yerbey"
        ,"Lepbos"
        ,"Lepalb"
        
        )
SpSel=c("Myobra","Myobec","Myomyo","Myobly","Myoalc"
        ,"Myocap","Pipnat","Hypsav","Minsch","Eptser","Nycnoc","Vesmur","Eptnil")
SpSel=c("Tadten")
Nfiles=400
FreqMax=10
QualityMin=30 #pourcentage

if(is.na(SpSel[1]))
{
  BC=subset(SpeciesList,SpeciesList$Group=="bat")
  BCF=subset(BC,BC$France=="x")
  
  BCFbase=subset(tabase,tabase$Espece %in% BCF$Esp)
  BCFbase$AmpA=BCFbase$Amp1+BCFbase$Amp2+BCFbase$Amp3+BCFbase$Amp4
  BCFbase=subset(BCFbase,BCFbase$AmpA>quantile(BCFbase$AmpA,0.7))
  BCF_F=unique(BCFbase$Filename)
  
  BCFsample=BCF_F[sample.int(length(BCF_F),100)]
  
  test=match(BCFsample,basename(LRSDB))
  
  Lsel=LRSDB[test]
  
  dir.create(NewDir)
  NewName=paste0(NewDir,"/",basename(Lsel))
  file.copy(from=Lsel,t=NewName)
  
  
}else{
  for (i in 1:length(SpSel))
  {
    BCFbase=subset(tabase,tabase$Espece==SpSel[i])
    BCFbase$AmpA=BCFbase$Amp1+BCFbase$Amp2+BCFbase$Amp3+BCFbase$Amp4
    BCFbase=subset(BCFbase,BCFbase$AmpA>quantile(BCFbase$AmpA,QualityMin/100))
    BCFbase=subset(BCFbase,BCFbase$FreqMP<FreqMax)
    BCF_F=unique(BCFbase$Filename)
    
    BCFsample=BCF_F[sample.int(length(BCF_F),min(Nfiles,length(BCF_F)))]
    
    test=match(BCFsample,basename(LRSDB))
    
    Lsel=LRSDB[test]
    
    dir.create(NewDir)
    dir.create(paste0(NewDir,"/",SpSel[i]))
    NewName=paste0(NewDir,"/",SpSel[i],"/",basename(Lsel))
    file.copy(from=Lsel,t=NewName)
    
    
  }
  
  
}

LWsel=list.files(NewDir,recursive=T)

test=(tabase$Filename %in% basename(LWsel))
RSDBsel=subset(tabase,test)
fwrite(RSDBsel,paste0(NewDir,"/DataRSDB.csv"),sep=";")

