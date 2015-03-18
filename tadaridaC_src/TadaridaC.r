args <- commandArgs(trailingOnly = TRUE)
if(length(args)==0){
  print("usage: Rscript TadaridaC.r <input.ta>")
  q()
}

fichier=args[1]

library(randomForest)

# charge classificateur haute fréquence (chiros/orthos)
if (exists("ClassifEsp3")==FALSE) load("ClassifEsp.learner")

CTP = read.table(file=fichier, header = TRUE, sep= "\t", dec = ".", fill = TRUE,comment.char = "")

#calcule probas par cris
ProbEsp0 <- predict(ClassifEsp3, CTP,type="prob",norm.votes=TRUE)
ProbEsp <-  cbind(CTP[,1:2],ProbEsp0)

#cherche le meilleur score pour 1 cri par espèce et par fichier
MaxparFich<-aggregate(ProbEsp[,3:(ncol(ProbEsp)-1)],by=list(ProbEsp$Filename),FUN=max)

SpMax<-max.col(MaxparFich[,2:ncol(MaxparFich)],ties.method = "first")

SpMax2=cbind(Filename=as.character(MaxparFich[,1]),Ordre="N1",Id=colnames(MaxparFich)[SpMax+1],numsp=SpMax)

ProbEsp2=merge(ProbEsp,SpMax2)

ProbEspDom0=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))

#va chercher la série de probas de la "meilleure espèce"
for (i in 1:(ncol(MaxparFich)-1))
{
  subtemp=subset(ProbEsp2,ProbEsp2$numsp==i)
  Probtemp=cbind(subtemp,subtemp[,(i+2)])
  ProbEspDom0=rbind(ProbEspDom0,Probtemp)
}

#PED = Proba Espece Dominante
colnames(ProbEspDom0)[ncol(ProbEspDom0)]="PED"


#sont considéré former le groupe dominant des cris tous ceux dont la PED>=0.02
ProbEspN1=subset(ProbEspDom0,ProbEspDom0$PED>=0.02)

MaxparFichN1<-aggregate(ProbEspN1[,3:(ncol(ProbEspN1)-4)],by=list(ProbEspN1$Filename),FUN=max)

#IdN1=cbind(SpMax2,PED=0,AutrEsp="",nbcris=as.numeric(table(ProbEspN1$Filename)))
IdN1=cbind(SpMax2,PED=0,AutrEsp="")

for (i in 1:(nrow(MaxparFichN1)))
{
  IdN1[i,5]=MaxparFichN1[i,(SpMax[i]+1)]
  for (j in 1:(ncol(MaxparFichN1)-1))
  {
    if ((MaxparFichN1[i,(j+1)]>(MaxparFichN1[i,(SpMax[i]+1)]/2))&(j!=SpMax[i]))
    {
      IdN1[i,6]=paste(IdN1[i,6],colnames(MaxparFichN1)[j+1])
    }
  }
}
IdTot=IdN1


#autres cris = 2ème groupe temporaire (N2t)
ProbEspN2a=subset(ProbEspDom0[,1:(ncol(ProbEspDom0)-4)],ProbEspDom0$PED<0.02)


if (nrow(ProbEspN2a)>0)
{

#On cherche la "meilleur espèce secondaire", et on recommence...
MaxparFichN2t<-aggregate(ProbEspN2a[,3:(ncol(ProbEspN2a)-1)],by=list(ProbEspN2a$Filename),FUN=max)

SpMaxN2<-max.col(MaxparFichN2t[,2:ncol(MaxparFichN2t)],ties.method = "first")

SpMax2N2=cbind(Filename=as.character(MaxparFichN2t[,1]),Ordre="N2",Id=colnames(MaxparFich)[SpMaxN2+1],numsp=SpMaxN2)

ProbEspN2t=merge(ProbEspN2a,SpMax2N2)


ProbEspDom2=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))


#va chercher la série de probas de la "meilleure espèce"
for (i in 1:(ncol(MaxparFichN2t)-1))
{
  subtemp=subset(ProbEspN2t,ProbEspN2t$numsp==i)
  Probtemp=cbind(subtemp,subtemp[,(i+2)])
  ProbEspDom2=rbind(ProbEspDom2,Probtemp)
}


#PED = Proba Espece Dominante
colnames(ProbEspDom2)[(ncol(ProbEspDom2)-1):(ncol(ProbEspDom2))]=c("SpMax","PED")


#ProbEspN2t=cbind(ProbEspN2t,PED=ProbEspDom2$PED)

#colnames(ProbEspN2t)[(ncol(ProbEspN2t)-1)]="SpMax"

#2ème groupe "définitif"
ProbEspN2=subset(ProbEspDom2,ProbEspDom2$PED>=0.02)

MaxparFichN2<-aggregate(ProbEspN2[,3:(ncol(ProbEspN2)-4)],by=list(ProbEspN2$Filename),FUN=max)

IdN2=cbind(SpMax2N2,PED=0,AutrEsp="")
for (i in 1:(nrow(MaxparFichN2)))
{
  IdN2[i,5]=MaxparFichN2[i,(SpMaxN2[i]+1)]

  for (j in 1:(ncol(MaxparFichN2)-1))
  {
    if ((MaxparFichN2[i,(j+1)]>(MaxparFichN2[i,(SpMaxN2[i]+1)]/2))&(j!=SpMaxN2[i]))
    {
      IdN2[i,6]=paste(IdN2[i,6],colnames(MaxparFichN2)[j+1])
    }
  }
}
IdTot=rbind(IdTot,IdN2)


#éventuel 3ème groupe de cris, et on refait la même procédure une dernière fois
ProbEspN3a=subset(ProbEspDom2[,1:(ncol(ProbEspDom2)-4)],ProbEspDom2$PED<0.02)


if (nrow(ProbEspN3a)>0)
{

  MaxparFichN3t<-aggregate(ProbEspN3a[,3:(ncol(ProbEspN3a)-1)],by=list(ProbEspN3a$Filename),FUN=max)

  SpMaxN3<-max.col(MaxparFichN3t[,2:ncol(MaxparFichN3t)],ties.method = "first")

  SpMax2N3=cbind(Filename=as.character(MaxparFichN3t[,1]),Ordre="N3",Id=colnames(MaxparFich)[SpMaxN3+1],numsp=SpMaxN3)

  ProbEspN3t=merge(ProbEspN3a,SpMax2N3)

  ProbEspDom3=matrix(nrow=0,ncol=(ncol(MaxparFich)+3))


#va chercher la série de probas de la "meilleure espèce"
for (i in 1:(ncol(MaxparFichN2)-1))
{
  subtemp=subset(ProbEspN3t,ProbEspN3t$numsp==i)
  Probtemp=cbind(subtemp,subtemp[,(i+2)])
  ProbEspDom3=rbind(ProbEspDom3,Probtemp)
}


#PED = Proba Espece Dominante
colnames(ProbEspDom3)[(ncol(ProbEspDom3)-1):(ncol(ProbEspDom3))]=c("SpMax","PED")

ProbEspN3=subset(ProbEspDom3,ProbEspDom3$PED>=0.02)

MaxparFichN3<-aggregate(ProbEspN3[,3:(ncol(ProbEspN3)-4)],by=list(ProbEspN3$Filename),FUN=max)

IdN3=cbind(SpMax2N3,PED=0,AutrEsp="")
for (i in 1:(nrow(MaxparFichN3)))
{
  IdN3[i,5]=MaxparFichN3[i,(SpMaxN3[i]+1)]

  for (j in 1:(ncol(MaxparFichN3)-1))
  {
    if ((MaxparFichN3[i,(j+1)]>(MaxparFichN3[i,(SpMaxN3[i]+1)]/2))&(j!=SpMaxN3[i]))
    {
      IdN3[i,6]=paste(IdN3[i,6],colnames(MaxparFichN3)[j+1])
    }
  }
}
IdTot=rbind(IdTot,IdN3)




ProbEspN4=subset(ProbEspDom3[,1:(ncol(ProbEspDom3)-4)],ProbEspDom3$PED<0.02)

if (nrow(ProbEspN4)>0)
{
  MaxparFichN4<-aggregate(ProbEspN4[,3:(ncol(ProbEspN4)-1)],by=list(ProbEspN4$Filename),FUN=max)

  SpMaxN4<-max.col(MaxparFichN4[,2:ncol(MaxparFichN4)],ties.method = "first")

  SpMax2N4=cbind(Filename=as.character(MaxparFichN4[,1]),Ordre="N4",Id=colnames(MaxparFich)[SpMaxN4+1],numsp=SpMaxN4)


  IdN4=cbind(SpMax2N4,PED=0,AutrEsp="")
  for (i in 1:(nrow(MaxparFichN4)))
  {
    IdN4[i,5]=MaxparFichN4[i,(SpMaxN4[i]+1)]

    for (j in 1:(ncol(MaxparFichN4)-1))
    {
      if ((MaxparFichN4[i,(j+1)]>(MaxparFichN4[i,(SpMaxN4[i]+1)]/2))&(j!=SpMaxN4[i]))
      {
        IdN4[i,6]=paste(IdN4[i,6],colnames(MaxparFichN4)[j+1])
      }
    }
  }
  IdTot=rbind(IdTot,IdN4)
  }}}

#ecriture fichier tac
fichierid=paste(substr(fichier,1,(nchar(fichier)-1)),"c", sep="")
write.csv(IdTot,fichierid,row.names=FALSE)

#nettoie tous les objets sauf ClassifEsp3
rm(list=setdiff(ls(), "ClassifEsp3"))
