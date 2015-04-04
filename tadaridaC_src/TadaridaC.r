args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  print("usage: Rscript TadaridaC.r <directory>")
  q()
}

tadir=args[1]

library(randomForest)
library(data.table)

Version=2

obslist=list.files(tadir,pattern=".ta$",full.names=T,recursive=F)

if (length(obslist) == 0) {
  print("no .ta files to process")
  q()
}

# charge classificateur haute fréquence (chiros/orthos)
if (exists("ClassifEsp3")==FALSE) load("ClassifEsp.learner")

my.data <- list()
for (i in 1:length(obslist)){
  my.data[[i]] <- read.csv(obslist[[i]],sep="\t")
}

CTP=as.data.frame(rbindlist(my.data))

#calcule probas par cris
ProbEsp0 <- predict(ClassifEsp3, CTP,type="prob",norm.votes=TRUE)
ProbEsp <-  cbind(CTP[,1:12],ProbEsp0)

#cherche le meilleur score pour 1 cri par espèce et par fichier
MaxparFich<-aggregate(ProbEsp[,13:(ncol(ProbEsp)-1)],by=list(ProbEsp$Filename),FUN=max)

SpMax<-max.col(MaxparFich[,2:ncol(MaxparFich)],ties.method = "first")

SpMax2=cbind(Filename=as.character(MaxparFich[,1]),Ordre="N1",Id=colnames(MaxparFich)[SpMax+1],numsp=SpMax)

ProbEsp2=merge(ProbEsp,SpMax2)

ProbEspDom0=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))

#va chercher la série de probas de la "meilleure espèce"
for (i in 1:(ncol(MaxparFich)-1))
{
  subtemp=subset(ProbEsp2,ProbEsp2$numsp==i)
  Probtemp=cbind(subtemp,subtemp[,(i+12)])
  ProbEspDom0=rbind(ProbEspDom0,Probtemp)
}

#PED = Proba Espece Dominante
colnames(ProbEspDom0)[ncol(ProbEspDom0)]="PED"


#sont considéré former le groupe dominant des cris tous ceux dont la PED>=0.02
ProbEspN1=subset(ProbEspDom0,ProbEspDom0$PED>=0.02)

MaxparFichN1<-aggregate(ProbEspN1[,13:(ncol(ProbEspN1)-4)],by=list(ProbEspN1$Filename),FUN=max)

FreqMed1=aggregate(ProbEspN1$FreqMP,by=list(ProbEspN1$Filename),function(x) floor(quantile(x,0.5)))

TDeb1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) floor(min(x/100))/10)

TFin1=aggregate(ProbEspN1$StTime,by=list(ProbEspN1$Filename),function(x) ceiling(max(x/100))/10)

IdTot=cbind(MaxparFichN1,FreqM=FreqMed1$x,TDeb=TDeb1$x,TFin=TFin1$x,Ordre="N1")


#autres cris = 2ème groupe temporaire (N2t)
ProbEspN2a=subset(ProbEspDom0[,1:(ncol(ProbEspDom0)-4)],ProbEspDom0$PED<0.02)


if (nrow(ProbEspN2a)>0)
{
  
  #On cherche la "meilleur espèce secondaire", et on recommence...
  MaxparFichN2t<-aggregate(ProbEspN2a[,13:(ncol(ProbEspN2a)-1)],by=list(ProbEspN2a$Filename),FUN=max)
  
  SpMaxN2<-max.col(MaxparFichN2t[,2:ncol(MaxparFichN2t)],ties.method = "first")
  
  SpMax2N2=cbind(Filename=as.character(MaxparFichN2t[,1]),Ordre="N2",Id=colnames(MaxparFich)[SpMaxN2+1],numsp=SpMaxN2)
  
  ProbEspN2t=merge(ProbEspN2a,SpMax2N2)
  
  
  ProbEspDom2=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))
  
  
  #va chercher la série de probas de la "meilleure espèce"
  for (i in 1:(ncol(MaxparFichN2t)-1))
  {
    subtemp=subset(ProbEspN2t,ProbEspN2t$numsp==i)
    Probtemp=cbind(subtemp,subtemp[,(i+12)])
    ProbEspDom2=rbind(ProbEspDom2,Probtemp)
  }
  
  
  #PED = Proba Espece Dominante
  colnames(ProbEspDom2)[(ncol(ProbEspDom2)-1):(ncol(ProbEspDom2))]=c("SpMax","PED")
  
  
  #ProbEspN2t=cbind(ProbEspN2t,PED=ProbEspDom2$PED)
  
  #colnames(ProbEspN2t)[(ncol(ProbEspN2t)-1)]="SpMax"
  
  #2ème groupe "définitif"
  ProbEspN2=subset(ProbEspDom2,ProbEspDom2$PED>=0.02)
  
  MaxparFichN2<-aggregate(ProbEspN2[,13:(ncol(ProbEspN2)-4)],by=list(ProbEspN2$Filename),FUN=max)
  
  FreqMed2=aggregate(ProbEspN2$FreqMP,by=list(ProbEspN2$Filename),function(x) floor(quantile(x,0.5)))
  
  TDeb2=aggregate(ProbEspN2$StTime,by=list(ProbEspN2$Filename),function(x) floor(min(x/100))/10)
  
  TFin2=aggregate(ProbEspN2$StTime,by=list(ProbEspN2$Filename),function(x) ceiling(max(x/100))/10)
  
  IdN2=cbind(MaxparFichN2,FreqM=FreqMed2$x,TDeb=TDeb2$x,TFin=TFin2$x,Ordre="N2")
  
  IdTot=rbind(IdTot,IdN2)
  
  
  #éventuel 3ème groupe de cris, et on refait la même procédure une dernière fois
  ProbEspN3a=subset(ProbEspDom2[,1:(ncol(ProbEspDom2)-4)],ProbEspDom2$PED<0.02)
  
  
  if (nrow(ProbEspN3a)>0)
  {
    
    MaxparFichN3t<-aggregate(ProbEspN3a[,13:(ncol(ProbEspN3a)-1)],by=list(ProbEspN3a$Filename),FUN=max)
    
    SpMaxN3<-max.col(MaxparFichN3t[,2:ncol(MaxparFichN3t)],ties.method = "first")
    
    SpMax2N3=cbind(Filename=as.character(MaxparFichN3t[,1]),Ordre="N3",Id=colnames(MaxparFich)[SpMaxN3+1],numsp=SpMaxN3)
    
    ProbEspN3t=merge(ProbEspN3a,SpMax2N3)
    
    ProbEspDom3=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))
    
    
    #va chercher la série de probas de la "meilleure espèce"
    for (i in 1:(ncol(MaxparFichN2)-1))
    {
      subtemp=subset(ProbEspN3t,ProbEspN3t$numsp==i)
      Probtemp=cbind(subtemp,subtemp[,(i+12)])
      ProbEspDom3=rbind(ProbEspDom3,Probtemp)
    }
    
    
    #PED = Proba Espece Dominante
    colnames(ProbEspDom3)[(ncol(ProbEspDom3)-1):(ncol(ProbEspDom3))]=c("SpMax","PED")
    
    ProbEspN3=subset(ProbEspDom3,ProbEspDom3$PED>=0.02)
    
    MaxparFichN3<-aggregate(ProbEspN3[,13:(ncol(ProbEspN3)-4)],by=list(ProbEspN3$Filename),FUN=max)
    
    FreqMed3=aggregate(ProbEspN3$FreqMP,by=list(ProbEspN3$Filename),function(x) floor(quantile(x,0.5)))
    
    TDeb3=aggregate(ProbEspN3$StTime,by=list(ProbEspN3$Filename),function(x) floor(min(x/100))/10)
    
    TFin3=aggregate(ProbEspN3$StTime,by=list(ProbEspN3$Filename),function(x) ceiling(max(x/100))/10)
    
    IdN3=cbind(MaxparFichN3,FreqM=FreqMed3$x,TDeb=TDeb3$x,TFin=TFin3$x,Ordre="N3")
    
    IdTot=rbind(IdTot,IdN3)
    
    
    
    
    ProbEspN4=subset(ProbEspDom3[,1:(ncol(ProbEspDom3)-4)],ProbEspDom3$PED<0.02)
    
    if (nrow(ProbEspN4)>0)
    {
      MaxparFichN4<-aggregate(ProbEspN4[,13:(ncol(ProbEspN4)-1)],by=list(ProbEspN4$Filename),FUN=max)
      
      SpMaxN4<-max.col(MaxparFichN4[,2:ncol(MaxparFichN4)],ties.method = "first")
      
      SpMax2N4=cbind(Filename=as.character(MaxparFichN4[,1]),Ordre="N4",Id=colnames(MaxparFich)[SpMaxN4+1],numsp=SpMaxN4)
      
      
      
      FreqMed4=aggregate(ProbEspN4$FreqMP,by=list(ProbEspN4$Filename),function(x) floor(quantile(x,0.5)))
      
      TDeb4=aggregate(ProbEspN4$StTime,by=list(ProbEspN4$Filename),function(x) floor(min(x/100))/10)
      
      TFin4=aggregate(ProbEspN4$StTime,by=list(ProbEspN4$Filename),function(x) ceiling(max(x/100))/10)
      
      IdN4=cbind(MaxparFichN4,FreqM=FreqMed4$x,TDeb=TDeb4$x,TFin=TFin4$x,Ordre="N4")
      
      IdTot=rbind(IdTot,IdN4)
    }}}


IdTot2=cbind(IdTot,VersionD=CTP$Version[1],VersionC=Version)

#ecriture fichier tc
fichierid=paste(tadir,"output.tc", sep="/")
write.csv(IdTot2,fichierid,row.names=FALSE)

#nettoie tous les objets sauf ClassifEsp3
rm(list=setdiff(ls(), "ClassifEsp3"))
