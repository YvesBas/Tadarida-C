library(data.table) #used to generate features table from labelled sound database
#INPUTS (to be edited according to local path)
#required:
RSDB="F:/RSDB_HF/"
#RSDB="G:/RSDB_LF"

#VarSel=fread("VarSel.csv") #to uncomment to select variables
#optional:
SpeciesList=as.data.frame(fread("C:/Users/croemer01/Documents/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Classificateur/SpeciesListComplete.csv", header=T)) #to uncomment if a species grouping and/or filtering is necessary


### A TESTER SANS FILTRE##
#GeoFilter="France" #to uncomment and edit if a species filtering is necessary


#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
library(randomForest)


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
#for (i in 1:161) # 10/15 version
    
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
fichier2=vector()
for (i in 1:length(etilist1)){ #5e5 labels/min
  if (file.size(etilist1[[i]])>0)
  {
    my.data[[i]] <- read.csv(etilist1[[i]],sep="\t",h=T,row.names=NULL)
    fichier=c(fichier
              ,rep(paste(substr(etilist2[[i]],1,nchar(etilist2[[i]])-4)
                         ,".wav",sep=""),nrow(my.data[[i]])))
    #fichier2=c(fichier2
     #         ,rep(paste(substr(etilist1[[i]],1,nchar(etilist1[[i]])-4)
      #                   ,".wav",sep=""),nrow(my.data[[i]])))
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
for (i in 1:length(parlist1)){ # 3e5 calls/min
#for (i in 1:20300){
  if (length(parlist1[[i]])==1)
  {
  my.data[[i]] <- read.csv(parlist1[[i]],sep="\t")
  }else{
    print(i)
  }
}
Sys.time()

param3=as.data.frame(rbindlist(my.data))
#param3=subset(param3,param3$FreqMP>=8)
if(exists("VarSel")){
param4=subset(param3,select=c("Filename","CallNum",VarSel$VarSel))
}else{
  param4=param3
}

#merging labels and features
tabase=merge(param4,etitot2,by.x=c("Filename","CallNum"),by.y=c("fichier","Cri"),all.x=T)

#filtering out non-labeled sound events
tabase2=subset(tabase,tabase$Espece!="")


write.csv(table(tabase2$Espece),"TtEsp.csv") #to list all species

#Filtering species according to their "country" occurence and grouping undistinguishable species (e.g. Myotis blythii and M. myotis)
if (exists("GeoFilter")==T) 
{
  colFilter = match(GeoFilter, colnames(SpeciesList))
  SpeciesFilter=subset(SpeciesList,SpeciesList[,(colFilter)]=="x")
  tabase3=merge(tabase2,SpeciesFilter,by.x="Espece",by.y="Esp")
  fwrite(tabase3,paste0("tabase3HF_",GeoFilter,".csv"),row.names=F)
  
  }else{
  if(exists("SpeciesList")==TRUE){
    tabase3=merge(tabase2,SpeciesList,by.x="Espece",by.y="Esp")
    }else{
    tabase3=cbind(tabase2,Nesp=tabase2$Espece)
    }
    fwrite(tabase3,paste0(RSDB,"_tabase3HF_sansfiltre.csv"),row.names=F)
    
}


if(!exists("GeoFilter"))
{
SiteEsp=aggregate(tabase3$Filename
                  ,by=c(list(tabase3$Espece),list(tabase3$Site))
                  ,FUN=length)

NbSiteEsp=aggregate(SiteEsp$Group.2
                    ,by=list(SiteEsp$Group.1)
                    ,FUN=length)

NbDataEsp=aggregate(SiteEsp$x
                    ,by=list(SiteEsp$Group.1)
                    ,FUN=sum)

NbSiteEsp$ndata=NbDataEsp$x

fwrite(NbSiteEsp,paste0(RSDB,"_NbSiteEsp.csv"),row.names=F)


}

#converting (rarely) missing features to 0 #A SUPPRIMER
#tabase3[is.na(tabase3)]=0


