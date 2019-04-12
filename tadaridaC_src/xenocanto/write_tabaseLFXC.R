library(data.table)
library(randomForest)
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
source(MRF)


#tabase3=fread("tabase3_LF.csv")
#A remplacer par une vraie lecture la base
RSDB="C:/Users/Yves Bas/Documents/RSDB_LF"
#optional:
#SpeciesList=read.csv("C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/SpeciesList.csv") #to uncomment if a species grouping and/or filtering is necessary
SpeciesList=as.data.frame(fread("SpeciesList_LF.csv")) #to uncomment if a species grouping and/or filtering is necessary

### A TESTER SANS FILTRE##
GeoFilter="France" #to uncomment and edit if a species filtering is necessary


#loading randomForest library to build the classifier then modified randomforest function

###AUTOMATISER IMPORTATION fonction modifiée randomForest


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
fichier2=vector()
for (i in 1:length(etilist1)){
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
parlist1=vector()

for (i in 1:length(ListDate))
{
  parlistemp1=list.files(paste(ListDate[[i]],"/txt",sep=""),pattern=".ta$",full.names=T,recursive=F)
  if(exists("parlist1")==T){parlist1=c(parlist1,parlistemp1)}else{parlist1=parlistemp1}
}

Sys.time()
my.data <- list()
for (i in 1:length(parlist1)){
  #for (i in 1:20300){
  my.data[[i]] <- read.csv(parlist1[[i]],sep="\t")
}
Sys.time()

param3=as.data.frame(rbindlist(my.data))


#merging labels and features
tabase=merge(param3,etitot2,by.x=c("Filename","CallNum"),by.y=c("fichier","Cri"),all.x=T)

#RAJOUTER mes vieilles données
Supp_o=fread("LabelsAudio_YvesBas.csv")
Supp_o=Supp_o[,2:ncol(Supp_o)]
Supp_o$Espece[Supp_o$Espece=="Saxrub"]="Saxrubi"
Supp_o$Espece[Supp_o$Espece=="Saxetr"]="Saxrube"  
Supp_o$Espece[Supp_o$Espece=="Pyrhul"]="Pyrpyr"  


test=match(colnames(tabase),colnames(Supp_o))
summary(test)
head(test)
plot(test)
colnames(tabase)[ncol(tabase)]="NA"
tabase=rbind(tabase,Supp_o)



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
tabase3$Espece=factor(tabase3$Espece,exclude=NULL)
tabase3$Site=factor(tabase3$Site,exclude=NULL)
tabase3[is.na(tabase3)]=0


DirXC="C:/Users/Yves Bas/Documents/XC"
ListXC=list.files(DirXC,pattern="RefXC",full.names=T)
my.data <- list()
for (i in 1:length(ListXC)){
  #for (i in 1:20300){
  my.data[[i]] <- fread(ListXC[[i]])
}
Sys.time()
RefXC=as.data.frame(rbindlist(my.data,fill=T))

#format RefXC and tabase 3 to be compatible
test=match(colnames(RefXC),colnames(tabase3))
testm=subset(colnames(RefXC),is.na(test))
for (i in length(test):1)
{
  if (is.na(test[i]))
  {
    RefXC[,i]=NULL
  }
  print(i)
  #assign(paste0("RefXC$",testm[i]),NULL)
}

test=match(colnames(tabase3),colnames(RefXC))
for (i in length(test):1)
{
  if (is.na(test[i]))
  {
    tabase3[,i]=NULL
  }
  print(i)
  #assign(paste0("RefXC$",testm[i]),NULL)
}

tabase3XC=rbind(tabase3,RefXC)
#fwrite(as.data.table(table(tabase3XC$Nesp)),paste0("tableRef_LF",substr(Sys.time(),1,10)
 #                                   ,".csv"))

tabase3XC$Nesp=factor(tabase3XC$Nesp,exclude=NULL)
summary(tabase3XC$Nesp)
tabase3XC$Site=factor(tabase3XC$Site,exclude=NULL)
tabase3XC[is.na(tabase3XC)]=0

fwrite(tabase3XC,"tabase3_LFXC.csv",row.names=F)
