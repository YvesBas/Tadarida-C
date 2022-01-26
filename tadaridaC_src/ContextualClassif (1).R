args <- commandArgs(trailingOnly = TRUE)
#args=c("./Tadarida/TCtest/5835dcd8db03c2000fba9dfc","SpeciesList.csv","ClassifEspC3_2019-03-25.learner","C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/f_CombineProbas.r","C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/f_Rescale_Probas.r","Referentiel_seuils_ProbEspC3_2019-03-25_G7__D_G.csv")
print(args)
library(randomForest)
library(data.table) #to handle large numbers of .ta files
#chargement du classifieur
if (exists("ClassifEspC2b")==F){load(args[3])}

#A EDITER - indiquer les données à traiter 
#(=les fichiers pour lesquels vous souhaitez une id)
TCdir=args[1]
SpeciesList=fread(args[2])
Weights=c(7,3)
fCP=args[4]
fRP=args[5]
KeepPrev1=F
KeepPrev2=T
OutputTC=T
Version=1904


f2p <- function(x) 
{
  if (is.data.frame(x)) {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 6, nchar(x)-4), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}
source(fCP)
source(fRP)


TClist=list.files(TCdir,full.names=T,pattern=".tc$")
print(length(TClist))
print(head(TClist))
if(KeepPrev2)
{
NameSave=paste0(TClist,"0")
file.copy(from=TClist,to=NameSave)
}
my.data=list()
for (i in 1:length(TClist))
{
  my.data[[i]]=fread(TClist[i])
}
TCdata=rbindlist(my.data)

#prefixe indiquant l'évenement d'échantillonnage (localité*participation)
LocaS=as.factor(substr((TCdata$Group.1),1,27))

ListLocaPart=levels(as.factor(LocaS))

PourC2=data.frame()
MaxTot=data.frame()
Pardoublons=list()
TCvides=vector()
TCmanquants=vector()
NbCtot=data.frame()
for (i in 1:length(ListLocaPart))
{
  
  Datasub1=subset(TCdata,substr(TCdata$Group.1,1,27)==ListLocaPart[i])  
  
  TestProba=(colnames(Datasub1) %in% SpeciesList$Esp)
  ColSel=subset(colnames(Datasub1),TestProba)
  ColOut=subset(colnames(Datasub1),!TestProba)
  DataProba0=subset(Datasub1,select=ColSel)
  colnames(DataProba0)=paste0(colnames(DataProba0),"_max")
  ProbaMax=aggregate(DataProba0,by=list(Datasub1$Group.1),max)
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
  
  
  Datasub1$SpMaxF2=factor(Datasub1$SpMaxF2,exclude=NULL)
  Sys.time()
  for (j in 1:nlevels(as.factor(Datasub1$SpMaxF2)))
  {
    Datasub2=subset(Datasub1
                    ,Datasub1$SpMaxF2==levels(as.factor(Datasub1$SpMaxF2))[j])
    Votesub2=subset(Datasub1,Datasub1$SpMaxF2==levels(as.factor(Datasub1$SpMaxF2))[j])
    if (nrow(Datasub2)==0)
    {
      stop("bug fichier manquant")
      Datasub2=Datasub1[1,]
      
    }else{
      NCSp=match(levels(as.factor(Datasub1$SpMaxF2))[j],colnames(Datasub2))
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
        print(paste(levels(as.factor(Datasub1$SpMaxF2))[j]
                    ,nrow(Votesub2),Votesub2$Q98[1]))
        PourC2=rbind(PourC2,Votesub2R)
      }
    }
  }
  Sys.time()
  
}

PourC2M=merge(PourC2,MaxTot,by="Group.1")

Predictors=row.names(ClassifEspC2b$importance)
PredMissing=subset(Predictors,!(Predictors %in% colnames(PourC2M)))

testCol=!is.na(match(names(PourC2M),SpeciesList$Esp))
RFresults=subset(PourC2M,select=(subset(names(PourC2M),testCol)))
Score=apply(RFresults,MARGIN=1,max)
RFresultsStd=RFresults/Score
names(RFresultsStd)=paste0(names(RFresultsStd),"_std")

PourC2M=cbind(PourC2M,RFresultsStd)
PourC2M$PF=!(substr(PourC2M$Group.1,2,2)=="i")
PourC2M$Pedestre=((substr(PourC2M$Group.1,10,10)=="-")
                  &(!(substr(PourC2M$Group.1,5,5)=="-"))
                  &(!PourC2M$PF))



#ajout de la date
FI=tstrsplit(PourC2M$Group.1,"-")
Passnum=as.numeric(gsub("Pass","",FI[[3]]))
DatePass=pmin(126+Passnum*62,126+62*3)
DateTot=ifelse(PourC2M$PF,yday(f2p(PourC2M$Group.1)),DatePass)
PourC2M$Date=DateTot
PourC2M$Date[is.na(PourC2M$Date)]=mean(subset(PourC2M$Date,!is.na(PourC2M$Date)))



#application des classificateurs (meilleure espèce et matrice de proba)
#ClassifEspC2b = classificateur qui tente de balancer le poids des espèces (mais attention ne marche peut-être quand même pas pour les espèces ayant un très petit nombre de validations)
ProbEsp_C2b=predict(ClassifEspC2b,PourC2M,type="prob",norm.votes=TRUE)
ProbEsp_C2bs=predict(ClassifEspC2b,PourC2M,type="response",norm.votes=TRUE)

#mixing the two classifiers
ColSel=subset(colnames(PourC2M),colnames(PourC2M) %in% SpeciesList$Esp)
ColSel2=subset(colnames(PourC2M),!(colnames(PourC2M) %in% SpeciesList$Esp))
ProbasPrev=subset(PourC2M,select=ColSel)
PourC2MWoProbas=subset(PourC2M,select=ColSel2)
colnames(ProbasPrev)=paste0(colnames(ProbasPrev),"_prev")
ProbasToCombine=cbind(Group.1=PourC2M$Group.1,ProbEsp_C2b,ProbasPrev)

NewProbas=CombineProbas(probas=ProbasToCombine,splist=args[2]
                        ,groupingsp=T,suffixes=c("","_prev")
                        ,weights=Weights,write=F)
NewProbas$OrderInit=c(1:nrow(NewProbas))


ScaledProbas=Rescale_Probas(probas=NewProbas,splist=args[2]
                            ,ref=args[6]
                            ,minp=0.01,maxp=0.99,accuracy=2)

ScaledProbas$Score=apply(ScaledProbas[,4:(ncol(ScaledProbas)-1)],MARGIN=1,max)
ScaledProbas=ScaledProbas[order(ScaledProbas$OrderInit),]


#merge des résultats des 2 classificateurs
t0=table(PourC2M$SpMaxF2)
t0b=cbind(Espece=row.names(t0),Nb=t0)
#t2=table(ProbEsp_C2bs)
t2=table(ScaledProbas$SpMax2)
t2s=aggregate(ScaledProbas$Score,by=list(ScaledProbas$SpMax2),max)
t12=cbind(Espece=row.names(t2),t2,score=t2s$x)
t012=merge(t0b,t12,by="Espece",all=T)
t012$Nb=as.numeric(as.character(t012$Nb))
t012$t2=as.numeric(as.character(t012$t2))
t012$score=as.numeric(as.character(t012$score))

t012[is.na(t012)]=0
t012=subset(t012,(t012$Nb+t012$t2)!=0)
t012=t012[order(as.character(t012$Espece)),]


#t012
PourC2M$SpMax0=ProbEsp_C2bs
PourC2M$SpMax1=ScaledProbas$SpMax1
PourC2M$SpMax2=ScaledProbas$SpMax2
VotesDiverging=subset(PourC2M,PourC2M$SpMaxF2!=ScaledProbas$SpMax2)
fwrite(VotesDiverging,paste0(TCdir,"_diverging.csv"),sep=";")

ColSel2=subset(colnames(PourC2M),!(colnames(PourC2M) %in% SpeciesList$Esp))
PourC2MWoProbas=subset(PourC2M,select=ColSel2)

PourC2Mshort=subset(PourC2MWoProbas,select=-grep("_std",colnames(PourC2MWoProbas)
))
PourC2Mshort=subset(PourC2Mshort,select=-grep("_ratio",colnames(PourC2Mshort)
))
PourC2Mshort=subset(PourC2Mshort,select=-grep("_max",colnames(PourC2Mshort)
))




if(KeepPrev1)
{
  ForTC=cbind(PourC2Mshort,ProbasPrev,ScaledProbas)
}else{
  ForTC=cbind(PourC2Mshort,ScaledProbas)
}

ForTCo=ForTC[order(ForTC$Duree,ForTC$Score,decreasing=T),]
ForTCagg=unique(ForTCo,by=c("Group.1","SpMax2"))  
ForTCagg=ForTCagg[order(ForTCagg$Group.1,ForTCagg$OrderNum),]

ForTCagg$versionC=Version

if(OutputTC)
{
  for (i in 1:nlevels(as.factor(ForTC$Group.1)))
  {
    fichier=levels(as.factor(ForTC$Group.1))[i]
    fichierid=paste(TCdir,'/',substr(fichier,1,(nchar(fichier)-4)),".tc", sep="")
    write.csv(subset(ForTC,ForTC$Group.1==fichier),fichierid,row.names=FALSE)  
  }
  
}else{
  fwrite(ForTC,paste0(TCdir,"_Id.csv"))
}
#t012
#ForTC$Group.1[1]
