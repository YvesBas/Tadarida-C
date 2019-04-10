library(data.table)
library(pROC)
library(ggplot2)
FIdConc="ProbEspHF_C2_PF"
FSp="SpNuit2_Seuil90_DataLP_PF_exportTot"
FGIS="GI_coordWGS84_SpNuit2_Seuil50_DataLP_PF_exportTot_Lat41.45_51.61_Long-5.9_9.73"

CompetingSp="Pipnat"

IdConc=fread(paste0(FIdConc,".csv"))

GIS=fread(paste0("./Vigiechiro/GIS/",FGIS,".csv"))
Particip=fread("C:/wamp64/www/p_export.csv")
SiteLoc=fread("C:/wamp64/www/sites_localites.txt")

GIS_SL=merge(GIS,SiteLoc,by.x=c("Group.1.x","Group.2.x")
             ,by.y=c("longitude","latitude"))
GIS_SLP=merge(GIS_SL,Particip,by="site",allow.cartesian=T)

GIS_SLPu=unique(GIS_SLP,by="participation")


SpNuit=fread(paste0(FSp,".csv"))

if(sum(grepl("IdMan",names(IdConc)))==0)
{
  IdConc$IdMan=IdConc$valid.espece
}

ListSp=levels(as.factor(IdConc$IdMan))

ROClist=list()
AUCtot=vector()

for (i in 1:length(ListSp))
{
  SpNuitC=subset(SpNuit,SpNuit$espece==CompetingSp)
  SpNuitS=subset(SpNuit,SpNuit$espece==ListSp[i])
  PartC=aggregate(SpNuitC$nb_contacts,by=list(SpNuitC$participation),FUN=sum)
  PartS=aggregate(SpNuitS$nb_contacts,by=list(SpNuitS$participation),FUN=sum)
  PartSC=merge(PartS,PartC,by="Group.1",all.x=T,all.y=T)
  PartSC[is.na(PartSC)]=0
  IdConcSp=merge(IdConc,PartSC,by.x="participation",by.y="Group.1")
  
  Label=(IdConcSp$IdMan==ListSp[i])
  testSp=match(ListSp[i],names(IdConcSp))
  ScoreSp=IdConcSp[,..testSp]
  Pos=subset(IdConcSp,as.data.frame(ScoreSp)[,1]==MaxScores)
  FP=(Pos$IdMan!=ListSp[i])
  RCSp=Pos$x.y/(Pos$x.y+Pos$x.x)
  
  qplot(RCSp,as.numeric(FP), geom='smooth',main=ListSp[i])+
    ylab("False Positive rate")+
    xlab(paste0(CompetingSp," / (",ListSp[i],"+",CompetingSp,")"))
  
  
  True=subset(IdConcSp,Label)
  Duree=True$temps_fin-True$temps_debut
  ScoreSp=True[,..testSp]
  AllScores=True[,..ListSp]
  MaxScores=apply(AllScores,MARGIN=1,max)
  FN=(as.data.frame(ScoreSp)[,1]!=MaxScores)
  
  qplot(Duree,as.numeric(FN), geom='smooth',main=ListSp[i])+
    ylab("False Negative rate")+
    xlab("Sequence duration (sec)")
  
True_GIS=merge(True,GIS_SLPu,by="participation")
ScoreSp=True_GIS[,..testSp]
AllScores=True_GIS[,..ListSp]
MaxScores=apply(AllScores,MARGIN=1,max)
FN=(as.data.frame(ScoreSp)[,1]!=MaxScores)

Forest=True_GIS$SpHO31S+True_GIS$SpHO32S
Vegetation=True_GIS$SpHO31S+True_GIS$SpHO32S+
  True_GIS$SpHO41S+True_GIS$SpHO42S

Farmland=True_GIS$SpHO11S+True_GIS$SpHO12S+True_GIS$SpHO211S+True_GIS$SpHO222S
Openfield=Farmland=True_GIS$SpHO11S+True_GIS$SpHO12S

qplot(Forest,as.numeric(FN), geom='smooth',main=ListSp[i])+
  ylab("False Negative rate")+
  xlab("Forest cover (50 m radius)")

qplot(Vegetation,as.numeric(FN), geom='smooth',main=ListSp[i])+
  ylab("False Negative rate")+
  xlab("Vegegtation cover (50 m radius)")


qplot(Farmland,as.numeric(FN), geom='smooth',main=ListSp[i])+
  ylab("False Negative rate")+
  xlab("Farmland proportion (50 m radius)")

