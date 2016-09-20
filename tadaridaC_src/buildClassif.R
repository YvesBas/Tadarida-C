#INPUTS (to be edited according to local path)
#required:
RSDB="C:/Users/yves/Documents/Tadarida/baseref"
MRF="C:/Users/yves/Documents/R/Modified_randomForest.R"
#optional:
SpeciesList=read.csv("C:/Users/yves/Documents/R/SpeciesList.csv",h=T) #to uncomment if a species grouping and/or filtering is necessary
### A TESTER SANS FILTRE##
#GeoFilter="France" #to uncomment and edit if a species filtering is necessary

#SETTINGS (both are intended to balance unvenness in species sampling)
SubSamp=11 #level of minimum subsampling (= X times average number of calls per species)
GradientSamp=-0.1 #gradient strength (must be negative)

#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
library(randomForest)
source(MRF) #Slightly modified randomForest function to allow empty sample strata

###AUTOMATISER IMPORTATION fonction modifiée randomForest

library(data.table) #used to generate features table from labelled sound database

#etidir="C:/Users/yves/Documents/Tadarida/eti" #path where labels (.eti files) are stored 
#pardir="C:/Users/yves/Documents/Tadarida/txt" #path where features (.ta files) are stored 

#setwd("D:/ScanRLogs/R_data")

#listing label files with and without their path

ListDate=list.dirs(RSDB,recursive=F)

if(length(ListDate)==0) {print("RSDB is empty")}

#Initializing and listing label files
etilist1=vector() #for full names
etilist2=vector() #for file names only

for (i in 1:length(ListDate))
{
  etilistemp1=list.files(paste(ListDate[[i]],"/eti",sep=""),pattern=".eti$",full.names=T,recursive=F)
  if(exists("etilist1")==T){etilist1=c(etilist1,etilistemp1)}else{etilist1=etilistemp1}
  etilistemp2=list.files(paste(ListDate[[i]],"/eti",sep=""),pattern=".eti$",full.names=F,recursive=F)
  if(exists("etilist2")==T){etilist2=c(etilist2,etilistemp2)}else{etilist2=etilistemp2}
  
  }


#concatenating labels tables
Sys.time()
my.data <- list() #reading label tables
fichier=vector() #reconstituting wave file names
for (i in 1:length(etilist1)){
  if (file.size(etilist1[[i]])>0)
  {
    my.data[[i]] <- read.csv(etilist1[[i]],sep="\t",h=T,row.names=NULL)
    fichier=c(fichier
              ,rep(paste(substr(etilist2[[i]],1,nchar(etilist2[[i]])-4)
                         ,".wav",sep=""),nrow(my.data[[i]])))
    }
  }

Sys.time()
etitot=as.data.frame(rbindlist(my.data,fill=T))
colnames(etitot)=colnames(
  read.csv(etilist1[[1]],sep="\t",h=T,row.names=1))
etitot2=cbind(fichier,etitot)


#test=subset(etitot2,(etitot2$Cri=="Pippip")&(etitot2$Espece=="social")&()

#concatenating features tables
parlist=vector()

for (i in 1:length(ListDate))
{
  parlistemp1=list.files(paste(ListDate[[i]],"/txt",sep=""),pattern=".ta$",full.names=T,recursive=F)
  if(exists("parlist1")==T){parlist1=c(parlist1,parlistemp1)}else{parlist1=parlistemp1}
}

Sys.time()
my.data <- list()
for (i in 1:length(parlist1)){
  my.data[[i]] <- read.csv(parlist1[[i]],sep="\t")
}
Sys.time()

param3=as.data.frame(rbindlist(my.data))


#merging labels and features
tabase=merge(param3,etitot2,by.x=c("Filename","CallNum"),by.y=c("fichier","Cri"),all.x=T)

#filtering out non-labeled sound events
tabase2=subset(tabase,tabase$Espece!="")


#write.csv(levels(tabase2$Cri),"TtEsp.csv") #to list all species

#Filtering species according to their "country" occurence and grouping undistinguishable species (e.g. Myotis blythii and M. myotis)
if (exists("GeoFilter")==T) 
{
  colFilter = match(GeoFilter, colnames(SpeciesList))
  SpeciesFilter=subset(SpeciesList,SpeciesList[,(colFilter)]=="x")
  tabase3=merge(tabase2,SpeciesFilter,by.x="Espece",by.y="Esp")
  }else{
    if(exists("SpeciesList")==TRUE){
      tabase3=merge(tabase2,SpeciesList,by.x="Espece",by.y="Esp")
    }else{
  tabase3=cbind(tabase2,Nesp=tabase2$Espece)
    }
  }

tabase3$Nesp=factor(tabase3$Nesp,exclude=NULL)
tabase3$Site=factor(tabase3$Site,exclude=NULL)

#converting (rarely) missing features to 0 #A SUPPRIMER
#tabase3[is.na(tabase3)]=0


#creating a formula using all sound features
FormulCrit=colnames(tabase3[5])
for (i in 6:154)
{
  FormulCrit=paste(FormulCrit,colnames(tabase2)[i],sep="+")
}

#average number of sound events per species, used thereafter to balance species weights in the classifier
NbMoyCri=as.numeric(mean(table(tabase3$Nesp)))

#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in 1:50)
{
  
#randomly selecting 63% of sites to build the small forest
SelSiteTemp=cbind(Site=levels(tabase3$Site)
                  ,Sel=sample(0:1,nlevels(tabase3$Site),
                       replace=T,prob=c(0.37,0.63)))
tabase4=merge(tabase3,SelSiteTemp,by="Site")

#designing sampling strata as a combination of species and site
StrataTemp=as.factor(paste(as.character(tabase4$Nesp)
                           ,as.character(tabase4$Sel)))

#maximum sampled sound events per species
SampMax=SubSamp*exp(i*(GradientSamp))*NbMoyCri
#Note that this variable depend on i and thus will vary according to each small random forest
#This is intended to build a large forest mixing a gradient of trees, from:
#- trees using a maximum number of sound events for high performance on common species (beginning of the loop)
#- trees using more and more balanced sound events per species to decrease bias towards common species (end of the loop)

#Defining sampling strata according to both constraints (selected site and maximum number of sound events per species) 
SampTemp=(as.numeric(table(StrataTemp))
          *as.numeric(sapply(levels(StrataTemp)
  ,FUN=function(x) strsplit(as.character(x),split=" ")[[1]][2])))
SampTemp2=sapply(SampTemp,FUN=function(x) min(x,SampMax))

gc()

# building the "10 trees" random forest
ClassifEspTemp=randomForest(as.formula(paste("Nesp ~ ",FormulCrit))
                            ,data=tabase4,replace=F
                            ,strata=StrataTemp
                            ,sampsize=SampTemp2
                            ,importance=F,ntree=10) 

#combine it with previously build small forests
if (exists("ClassifEspA")==TRUE) {ClassifEspA=combine(ClassifEspA,ClassifEspTemp)} else {ClassifEspA=ClassifEspTemp}
}
Sys.time()


save (ClassifEspA,file="ClassifEspHF3.learner") 
Sys.time()


