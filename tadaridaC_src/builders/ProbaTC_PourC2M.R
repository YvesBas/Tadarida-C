library(data.table) #to handle large numbers of .ta files
GeoFilter="France"
SpeciesList=as.data.frame(fread("C:/Users/JF/Downloads/SpeciesList.csv"))
#SETTINGS (both are intended to balance unvenness in species sampling)
ListTCZ=list.files("C:/Program Files (x86)/EasyPHP-DevServer-14.1VC11/data/localweb/tcz",full.names=T,pattern=".tar.gz$")
#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
IdTot1=fread("C:/Users/JF/Downloads/EXHAUSTIFS_TOTAL_ded_ProbaTC.csv")
IdTot2=fread("C:/Users/JF/Downloads/NONEXHAUSTIFS_TOTAL_VRAI_ded_ProbaTC.csv")
SpToRemove=c("Ortsp.")

IdTot3=rbind(IdTot1,IdTot2)
IdTot3=as.data.frame(IdTot3)


colFilter = match(GeoFilter, colnames(SpeciesList))
SpeciesFilter=subset(SpeciesList,SpeciesList[,(colFilter)]=="x")
test=match(IdTot3$ValidId,SpeciesFilter$Nesp)
IdLost=subset(IdTot3,is.na(test))
table(IdLost$ValidId)
IdTot3=subset(IdTot3,is.na(test)==F)
#filtrer les donn?es point fixe uniquement
#IdTot3=subset(IdTot3,substr(IdTot3$donnee,1,3)=="Car")
IdTot3=subset(IdTot3,!IdTot3$ValidId %in% SpToRemove)

#SpList=levels(as.factor(IdTot3$ValidId))


#test=subset(IdTot3,IdTot3$ValidId=="Ortsp.")

LocaPartI3=substr(IdTot3$Group.1,1,27)
ListLocaPart=levels(as.factor(LocaPartI3))

PourC2=data.frame()
MaxTot=data.frame()
Pardoublons=list()

TCvides=vector()
TCmanquants=vector()
NbCtot=data.frame()
Log=data.frame(participation=0,Time=0,Size=0)
Log=Log[rep(seq(1,length(ListLocaPart))),]
for (i in 1:length(ListLocaPart))
{
  
  print(paste(i,nlevels(as.factor(LocaPartI3))))
  Partemp=unique(subset(IdTot3$participation
                        ,LocaPartI3==ListLocaPart[i]))
  Log$participation[i]=Partemp
  if(length(Partemp)>1){
    Pardoublons=list(Pardoublons,Partemp)
  }else{
    TCZmatch=subset(ListTCZ,grepl(Partemp,ListTCZ))
    if(length(TCZmatch)==0)
    {
      TCmanquants=c(TCmanquants,Partemp)
    }else{
      if(length(TCZmatch)>1){stop("bug doublons tcz")}  
      FI=file.info(TCZmatch)
      
      if(FI$size<100)
      {
        TCvides=c(TCvides,Partemp)
      }else{
        #x <- archive::archive_read(archive = TCZmatch)
        TCdir=gsub(".tar.gz","",TCZmatch)
        if(!dir.exists((TCdir)))
        {
          Sys.time()
          untar(TCZmatch,exdir=dirname(TCZmatch)) 
          Sys.time()
        }
        TClist=list.files(TCdir,full.names=T)
        Log$Size[i]=length(TClist)
        Log$Tie[i]=Sys.time()
        if(i%%50==0){fwrite(Log,"Log.csv")}
        my.data=list()
        Sys.time()
        for (j in 1:length(TClist))
        {
          my.data[[j]]=fread(TClist[j])
        }
        Sys.time()
        TCdata=rbindlist(my.data)
        Sys.time()
        
        Datasub1=subset(TCdata,substr(TCdata$Group.1,1,27)==ListLocaPart[i])  
        Votesub1=subset(IdTot3,LocaPartI3==ListLocaPart[i])
        
        Datasub0=subset(Datasub1,Datasub1$Group.1 %in% Votesub1$Group.1)
        TestProba=(colnames(Datasub0) %in% SpeciesList$Esp)
        ColSel=subset(colnames(Datasub0),TestProba)
        ColOut=subset(colnames(Datasub0),!TestProba)
        DataProba0=subset(Datasub0,select=ColSel)
        colnames(DataProba0)=paste0(colnames(DataProba0),"_max")
        ProbaMax=aggregate(DataProba0,by=list(Datasub0$Group.1),max)
        #plot(ProbaMax$Roeroe_max,ProbaMax$Epheph_max)
        MaxTot=rbind(MaxTot,ProbaMax)
        #tableau comptabilisant le nombre de contacts par esp?ces et par ?v?nement d'?chantillonnage
        nbcT=as.matrix(table(Datasub1$SpMaxF2))
        SpO=match(ColSel,row.names(nbcT))
        nbcTO=nbcT[SpO]
        nbcTO[is.na(nbcTO)]=0
        
        NbCtot=rbind(NbCtot,t(nbcTO))
        #barplot(nbcTO,names.arg=SpList,las=2,cex.names=0.5)
        #boucle qui calcule d'une part les quantiles d'indices de confiance par esp?ces
        #et d'autre part les proportion d'abondance entre esp?ces
        #= donn?es d'entr?e qui serviront ? l'identification
        
        
        table(Votesub1$ValidId)
        Votesub1$SpMaxF2=factor(Votesub1$SpMaxF2,exclude=NULL)
        Sys.time()
        for (j in 1:nlevels(as.factor(Votesub1$SpMaxF2)))
        {
          Datasub2=subset(Datasub1
                          ,Datasub1$SpMaxF2==levels(as.factor(Votesub1$SpMaxF2))[j])
          Votesub2=subset(Votesub1,Votesub1$SpMaxF2==levels(as.factor(Votesub1$SpMaxF2))[j])
          if (nrow(Datasub2)==0)
          {
            stop("bug fichier manquant")
            Datasub2=Datasub1[1,]
            
          }else{
            NCSp=match(levels(as.factor(Votesub1$SpMaxF2))[j],colnames(Datasub2))
            if(is.na(NCSp))
            {
              stop("bug colonne manquante")
              Votesub2$Q25=0
              Votesub2$Q50=0
              Votesub2$Q75=0
              Votesub2$Q90=0
              Votesub2$Q95=0
              Votesub2$Q98=0
              Votesub2$Q100=0
              
            }else{
              
              ProbaSp=as.data.frame(Datasub2[,..NCSp])
              Votesub2$Q25=quantile(ProbaSp[,1],0.25)
              Votesub2$Q50=quantile(ProbaSp[,1],0.50)
              Votesub2$Q75=quantile(ProbaSp[,1],0.75)
              Votesub2$Q90=quantile(ProbaSp[,1],0.90)
              Votesub2$Q95=quantile(ProbaSp[,1],0.95)
              Votesub2$Q98=quantile(ProbaSp[,1],0.98)
              Votesub2$Q100=max(ProbaSp[,1])
              
              RatioContacts=t(matrix(ncol=nrow(Votesub2),nrow=length(nbcTO)
                                     ,data=rep(nbcTO/nrow(Datasub2)
                                               ,nrow(Votesub2))))
              RatioContacts=as.data.frame(RatioContacts)  
              colnames(RatioContacts)=paste0(ColSel,"_ratio")
              Votesub2R=cbind(Votesub2,RatioContacts)
              print(paste(levels(as.factor(Votesub1$SpMaxF2))[j]
                          ,nrow(Votesub2),Votesub2$Q98[1]))
              PourC2=rbind(PourC2,Votesub2R)
            }
          }
        }
        Sys.time()
      }
    }
  }
}

PourC2M=merge(PourC2,MaxTot,by="Group.1")
fwrite(Pourc2M,"PourC2M.csv")
