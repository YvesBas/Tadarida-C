library(randomForest)
library(data.table) #to handle large numbers of .ta files
#chargement du classifieur
if (exists("ClassifEspC2b")==F){load("ClassifEspC3_2019-03-25.learner")}
#A EDITER - indiquer les données à traiter 
#(=les fichiers pour lesquels vous souhaitez une id)
TCdir="C:/Users/Yves Bas/Downloads/5835dcd8db03c2000fba9dfc.tar/5835dcd8db03c2000fba9dfc/5835dcd8db03c2000fba9dfc"

#


SelTot=fread("C:/wamp64/www/export180227.txt")

SelTot=subset(DataTot,DataTot$participation %in% ListPar[1:20])

#prefixe indiquant l'évenement d'échantillonnage (localité*participation)
LocaS=as.factor(substr((SelTot$donnee),1,27))
LocaPartData=as.factor(substr((DataTot$donnee),1,27))
DataPourC2=subset(DataTot,(LocaPartData %in% levels(as.factor(LocaS))&(DataTot$espece!="")))
LocaPartDC2=as.factor(substr((DataPourC2$donnee),1,27))

#tableau comptabilisant le nombre de contacts par espèces et par événement d'échantillonnage
nbcT=as.matrix(table(LocaPartDC2,DataPourC2$espece))




#boucle qui calcule d'une part les quantiles d'indices de confiance par espèces
#et d'autre part les proportion d'abondance entre espèces
#= données d'entrée qui serviront à l'identification
Q25=vector()
Q50=vector()
Q75=vector()
Q90=vector()
Q95=vector()
Q98=vector()
Q100=vector()
#PropSp=as.data.frame(matrix(nrow=nrow(VotesT2),ncol=ncol(nbcT)))
PropSp=nbcT[0,]
#colnames
compt=0
VoteO=SelTot[0,]
for (i in 1:nlevels(as.factor(LocaPartDC2))) #traite séquentiellement tous les événements d'échantillonnage (localité*participation)
#  for (i in 2760:3000)
  
{
  Datasub1=subset(DataPourC2,(LocaPartDC2==levels(as.factor(LocaPartDC2))[i]))
  Votesub1=subset(SelTot,LocaS==levels(as.factor(LocaPartDC2))[i])
  Votesub1$espece=factor(Votesub1$espece,exclude=NULL)
  for (j in 1:nlevels(as.factor(Votesub1$espece)))
  {
    Datasub2=subset(Datasub1,Datasub1$espece==levels(as.factor(Votesub1$espece))[j])
    Votesub2=subset(Votesub1,Votesub1$espece==levels(as.factor(Votesub1$espece))[j])
    if (nrow(Datasub2)==0) #traite le cas où les identifications différent entre les tableaux "SelTot" et "DataTot" (ex : quand différentes versions de Tadarida ont été utilisées)
    {
      Datasub2=Datasub1[1,]
      Datasub2$probabilite=0.5
    }
    Q25=c(Q25,rep(quantile(Datasub2$probabilite,0.25),nrow(Votesub2)))
    Q50=c(Q50,rep(quantile(Datasub2$probabilite,0.50),nrow(Votesub2)))
    Q75=c(Q75,rep(quantile(Datasub2$probabilite,0.75),nrow(Votesub2)))
    Q90=c(Q90,rep(quantile(Datasub2$probabilite,0.90),nrow(Votesub2)))
    Q95=c(Q95,rep(quantile(Datasub2$probabilite,0.95),nrow(Votesub2)))
    Q98=c(Q98,rep(quantile(Datasub2$probabilite,0.98),nrow(Votesub2)))
    Q100=c(Q100,rep(max(Datasub2$probabilite),nrow(Votesub2)))
    
    Ncont1=max(1,nrow(Datasub2))
    VoteO=rbind(VoteO,Votesub2)
    for (k in 1:nrow(Votesub2))
    {
      PropSp=rbind(PropSp,nbcT[i,]/Ncont1)
    }
    compt=compt+nrow(Votesub2)
    #print(paste(compt,j,i,nlevels(as.factor(LocaS))))
  }
}

Sys.time()
nrow(VoteO)
#colnames(PropSp)=colnames(nbcT)

#aggrégation des données produites (quantiles et proportion d'abondances)
VoteC2=cbind(VoteO,PropSp,Q25,Q50,Q75,Q90,Q95,Q98,Q100)


#édition des titres de colonne pour identifier les variables de type "proportions d'abondances"
for (i in 15:(ncol(VoteC2)-7))
{
  colnames(VoteC2)[i]=paste0(names(VoteC2)[i],"_prop")
}



#rajouter les espèces manquantes
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_prop")
test=match(EspForm,colnames(VoteC2))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(VoteC2),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
VoteC2=cbind(VoteC2,Zeros)



ListDV=levels(as.factor(SelTot$donnee))
DataV=subset(DataPourC2,DataPourC2$donnee %in% ListDV)

#calcule les probabilités max par espèce et par fichier
#(utile pour corriger les erreurs dues à la coexistence de taxons dans le même fichier
#ex: cris sociaux de Pipistrelles identifiées comme autre chose (Noctule, oreillard...))
MaxI=tapply(DataV$probabilite
            ,INDEX=list(c(DataV$donnee),c(DataV$espece))
            ,FUN=max)
MaxI2=as.data.frame(cbind(row.names(MaxI),MaxI))
for (i in 2:ncol(MaxI2))
{
  MaxI2[,i]=as.numeric(as.character(MaxI2[,i]))
}
MaxI2[is.na(MaxI2)]=0

#édition des titres de colonne pour identifier les variables de type "indices max"
for (i in 2:(ncol(MaxI2)))
{
  colnames(MaxI2)[i]=paste0(names(MaxI2)[i],"_maxI")
}


#rajouter les espèces manquantes
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_maxI")
test=match(EspForm,colnames(MaxI2))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(MaxI2),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
MaxI2=cbind(MaxI2,Zeros)




#indice de confiance à l'echelle de l'observation (groupe de cris identifié comme provenant d'une seule espèce par la première couche)
if(exists("IdS3")){rm(IdS3)}
for (i in 1:nlevels(as.factor(SelTot$espece)))
{
  Idsub=subset(SelTot,SelTot$espece==levels(as.factor(SelTot$espece))[i])
  IdS2=cbind(donnee=Idsub$donnee,espece=Idsub$espece,prob=Idsub$probabilite)
  colnames(IdS2)[3]=paste(levels(as.factor(SelTot$espece))[i])
  if(exists("IdS3")){IdS3=merge(IdS3,IdS2,all=T)}else{IdS3=IdS2}
  #print(i)
}

for (i in 3:ncol(IdS3))
{
  IdS3[,i]=as.numeric(as.character(IdS3[,i]))
}

#édition des titres de colonne pour identifier les variables de type "indices de l'observation"
for (i in 3:(ncol(IdS3)))
{
  colnames(IdS3)[i]=paste0(names(IdS3)[i],"_ValI")
}

IdS3[is.na(IdS3)]=0

#rajouter les espèces manquantes
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_ValI")
test=match(EspForm,colnames(IdS3))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(IdS3),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
IdS3=cbind(IdS3,Zeros)



#on merge les prop d'espèces, les quantiles et les indices par fichiers et par observations
VoteC3=merge(VoteC2,MaxI2,by.x="donnee",by.y="V1")
VoteC4=merge(VoteC3,IdS3,by=c("donnee","espece"))
VoteC4$valid.espece=factor(VoteC4$valid.espece,exclude=NULL)
VoteC4$espece=as.factor(VoteC4$espece)
VoteC4$durseq=VoteC4$temps_fin-VoteC4$temps_debut


#application des classificateurs (meilleure espèce et matrice de proba)
#ClassifEspC2b = classificateur qui tente de balancer le poids des espèces (mais attention ne marche peut-être quand même pas pour les espèces ayant un très petit nombre de validations)
ProbEsp_C2b=predict(ClassifEspC2b,VoteC4,type="prob",norm.votes=TRUE)
ProbEsp_C2bs=predict(ClassifEspC2b,VoteC4,type="response",norm.votes=TRUE)


#merge des résultats des 2 classificateurs
t0=table(VoteC4$espece)
t0b=cbind(Espece=row.names(t0),Nb=t0)
t2=table(ProbEsp_C2bs)
t12=cbind(Espece=row.names(t2),t2)
t012=merge(t0b,t12,by="Espece")

Ib=apply(ProbEsp_C2b,MARGIN=1,FUN=max)

IdC2=cbind(VoteC4[,1:13],ProbEsp_C2bs,Ib)
IdDetailB=cbind(VoteC4[,1:13],ProbEsp_C2bs,ProbEsp_C2b)

#IdC2= tableau de synthèse rajoutant l'espèce prédite par chacun des classifieurs et l'indice de confiance associé
#IdDetailB = détail (matrice de probas pour chaque espèce) des identifications des 2 classificateurs
write.csv(IdC2,"IdC2.csv",row.names=F)
write.csv(IdDetailB,"IdDetailB.csv",row.names=F)
Sys.time()

