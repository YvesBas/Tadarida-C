library(data.table)
library(archive)
ListTCZ=list.files("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/tcz",full.names=T,pattern=".tar.gz$")
#ListTCZ=list.files("C:/wamp64/www/tcz",full.names=T)
#Part123=fread("ListParValid.csv")
Data=fread("C:/Users/JF/Downloads/DataNE.csv")
FETOT="C:/Users/JF/Downloads/NONEXHAUSTIFS_TOTAL_VRAI_ded.csv"
Exhaustif=F


ETOT=fread(FETOT)
Donnees_E=levels(as.factor(ETOT$donnee))

DonneesManquantes=subset(ETOT,!ETOT$donnee %in% Data$donnee)
PrefDM=substr(DonneesManquantes$donnee,1,15)
#summary(as.factor(PrefDM))
fwrite(DonneesManquantes,"DonneesManquantes.csv")
##ATTENTION NE TRAITE PAS POUR L'INSTANT les validations en surnombre par rapport aux prédictions (= non matchables)

ProbaValid=data.frame()
DataMissing=vector()
TCMissing=vector()
ParDoublons=vector()
TCvide=vector()
for (i in 1:length(Donnees_E))
{
  if(sum(is.na(ProbaValid$Group.1))>0){stop("bug Group.1")}
  Datatemp=subset(Data,Data$donnee %in% Donnees_E[i])
  Partemp=unique(Datatemp$participation)
  if(length(Partemp)>1)
  {
    print(Partemp)
    doublon=Partemp[1]
    for (j in 2:length(Partemp))
    {
      doublon=paste(doublon,Partemp[j])
    }
    ParDoublons=c(ParDoublons,doublon)
    #stop("DOUBLONS !!!")
  }else{
    if(length(Partemp)==0)
    {
      DataMissing=c(DataMissing,Donnees_E[i])
    }else{
      
      TCZmatch=subset(ListTCZ,grepl(Partemp,ListTCZ))
      if(length(TCZmatch)>1){
        print(TCZmatch)
        stop("DOUBLONS !!!")
      }
      
      if(length(TCZmatch)==1){
        
        FI=file.info(TCZmatch)
        
        if(FI$size>100)
        {
          #ProbaValid=data.frame()
          print(paste(i,Sys.time()))
          Validtemp=subset(ETOT,ETOT$donnee %in% Donnees_E[i])
          TCName=paste0(Partemp,"/",Donnees_E[i],".tc")
          x <- archive::archive_read(archive = TCZmatch,file=TCName)
          #x=untar(TCZmatch,file=TCName) 
          Scoretemp=readr::read_csv(x)
          SpSel=subset(Validtemp$SpReMatch,Validtemp$SpReMatch %in% names(Scoretemp))
          SpMissing=subset(Validtemp$SpReMatch,!Validtemp$SpReMatch %in% names(Scoretemp))
          SpMissingI=SpMissing
          ScoreSel=as.matrix(subset(Scoretemp,select=SpSel))
          ScoreDispo=ScoreSel
          ProbaDispo=Scoretemp
          #if((nrow(Validtemp)<(nrow(Scoretemp)))&(nrow(Validtemp)>1)){stop(i)}
          RowSelected=0
          Sp_toRematch=SpSel
          if(length(SpSel)>0)
          {
            for (j in 1:min(nrow(ScoreSel),length(SpSel)))
            {
              if(RowSelected!=0)
              {
                ScoreDispo=ScoreDispo[-RowSelected,]
                if(is.matrix(ScoreDispo))              
                {
                  ScoreDispo=ScoreDispo[,-ColSelected]
                }else{
                  ScoreDispo=ScoreDispo[-ColSelected]
                }
                ProbaDispo=ProbaDispo[-RowSelected,]
                Sp_toRematch=subset(Sp_toRematch
                                    ,Sp_toRematch!=ValidSel$SpReMatch)
                #Sp_toRematch=Sp_toRematch[-ValidSel$SpReMatch]
                
              }
              PosSel=which(ScoreDispo == max(ScoreDispo), arr.ind = TRUE)
              if(is.matrix(PosSel))
              {
                ProbaSel=ProbaDispo[PosSel[1,1],]
                ProbaSel$CaseTracking=1
                ValidSel=subset(Validtemp,Validtemp$SpReMatch==colnames(ScoreDispo)[PosSel[1,2]])
                RowSelected=PosSel[1,1]
                ColSelected=PosSel[1,2]
              }else{
                if(length(Sp_toRematch)==1)
                {
                  ProbaSel=ProbaDispo[PosSel[1],]
                  ProbaSel$CaseTracking=2
                  #if(PosSel>1){stop("test ValidSel")}
                  
                  ValidSel=subset(Validtemp,Validtemp$SpReMatch==Sp_toRematch)
                  RowSelected=PosSel[1]
                  ColSelected=1
                }else{
                  ProbaSel=ProbaDispo[1,]
                  ProbaSel$CaseTracking=3
                  
                  #stop("test ValidSel")
                  ValidSel=subset(Validtemp,Validtemp$SpReMatch==names(ScoreDispo)[PosSel[1]])
                  RowSelected=1
                  ColSelected=PosSel
                  
                }
                
              }
              
              ProbaSel$ValidId=ValidSel$SpReMatch
              ProbaSel$ValidConf=ValidSel$SpConf
              ProbaSel$participation=Partemp
              ProbaValid=rbind(ProbaValid,ProbaSel)
            }
          }
          if((nrow(ProbaDispo)>1))
          {
            if(Exhaustif)
            {
              
              if(RowSelected!=0)
              {
                ProbaDispo=ProbaDispo[-RowSelected,]
              }else{
                ScoreSel=as.matrix(subset(Scoretemp,select="Tetvir"))
              }
              
              ScoreDispo=subset(ScoreSel,Scoretemp$SpMaxF2 %in% ProbaDispo$SpMaxF2)
              Nsupp=nrow(ProbaDispo)
              #if(Nsupp>1){stop("test Nsupp")}
              for (j in 1:Nsupp)
              {
                PosSel=which(ScoreDispo == max(ScoreDispo), arr.ind = TRUE)
                if(is.matrix(PosSel))
                {
                  ProbaSel=ProbaDispo[PosSel[1,1],]
                  ProbaSel$CaseTracking=4
                  if(length(SpMissing)>0)
                  {
                    ValidSel=subset(Validtemp,Validtemp$SpReMatch==SpMissing[1])
                    SpMissing=SpMissing[-1]
                  }else{
                    if(length(SpSel)==0)
                    {
                      ValidSel=subset(Validtemp,Validtemp$SpReMatch==SpMissingI[1])
                    }else{
                      ValidSel=subset(Validtemp,Validtemp$SpReMatch==colnames(ScoreDispo)[PosSel[1,2]])
                    }
                  }
                  RowSelected=PosSel[1,1]
                  
                  ProbaDispo=ProbaDispo[-RowSelected,]
                  ScoreDispo=ScoreDispo[-RowSelected,]
                  
                  
                  
                }else{
                  ProbaSel=ProbaDispo[1,]
                  ProbaSel$CaseTracking=5
                  if(is.matrix(ScoreDispo)){stop("test ScoreDispo")}
                  if(length(SpMissing)>0)
                  {
                    ValidSel=subset(Validtemp,Validtemp$SpReMatch==SpMissing[1])
                    SpMissing=SpMissing[-1]
                  }else{
                    if(length(SpSel)==0){
                      ValidSel=subset(Validtemp,Validtemp$SpReMatch==SpMissingI[1])
                    }else{
                      if(is.null(names(ScoreDispo)))
                      {
                        ValidSel=Validtemp[1,]
                      }else{
                        ValidSel=subset(Validtemp,Validtemp$SpReMatch==names(ScoreDispo)[PosSel[1]])
                      }
                      #if(PosSel>1){stop("test PosSel2")}
                    }
                  }
                  RowSelected=PosSel[1]
                  ProbaDispo=ProbaDispo[-RowSelected,]
                  if(is.matrix(ScoreDispo)){
                    ScoreDispo=ScoreDispo[-RowSelected,]
                  }else{
                    ScoreDispo=ScoreDispo[-RowSelected]
                  }
                }
                print(ScoreDispo)
                ProbaSel$ValidId=ValidSel$SpReMatch
                ProbaSel$ValidConf=ValidSel$SpConf
                ProbaSel$participation=Partemp
                ProbaValid=rbind(ProbaValid,ProbaSel)
              }
            }else{
              if(length(SpMissing)>0)
              {
                if(RowSelected!=0)
                {
                  ProbaDispo=ProbaDispo[-RowSelected,]
                }
                if(length(SpMissing)>nrow(ProbaDispo))
                {
                  SpMissing=SpMissing[1:nrow(ProbaDispo)]
                }
                ProbaSel=ProbaDispo[(1:length(SpMissing)),]
                ValidSel=subset(Validtemp,Validtemp$SpReMatch %in% SpMissing)
                ProbaSel$CaseTracking=6
                
                ProbaSel$ValidId=ValidSel$SpReMatch
                ProbaSel$ValidConf=ValidSel$SpConf
                ProbaSel$participation=Partemp
                ProbaValid=rbind(ProbaValid,ProbaSel)
                
              }
            }
          }
        }else{
          TCvide=c(TCVide,Partemp)
        }
        
        
      }else{
        TCMissing=c(TCMissing,Partemp)
      }
      
      
      
    }
  }
}

fwrite(ProbaValid,paste0(FETOT,"_ProbaTC.csv"))
table(substr(ProbaValid$Group.1,1,3))
table(substr(ProbaValid$participation,1,2))
