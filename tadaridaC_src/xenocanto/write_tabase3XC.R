library(data.table)
op <- options(digits.secs = 3)


setwd("C:/Users/Yves Bas/Documents")
tabase3=fread("tabase3_LFXC.csv")
SpeciesList=fread("SpeciesList_LF.csv") #to uncomment if a species grouping and/or filtering is necessary

setwd("C:/Users/Yves Bas/Documents/XC/")

#FAIRE séries de MDXC
ListF=list.files(getwd(),pattern="MDXC")
my.data <- list()
for (i in 1:length(ListF)){
  #for (i in 1:20300){
  my.data[[i]] <- read.csv2(ListF[[i]])
}
Sys.time()
MDXC=as.data.frame(rbindlist(my.data))
table(MDXC$Quality)

ListSplit=dir("E:/XC",pattern="split",full.names=T)
RSDB=paste0(ListSplit[length(ListSplit)],"/txt")
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
#optional:
#SpeciesList=read.csv("C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/SpeciesList.csv") #to uncomment if a species grouping and/or filtering is necessary

### A TESTER SANS FILTRE##
GeoFilter="France" #to uncomment and edit if a species filtering is necessary

#SETTINGS (both are intended to balance unvenness in species sampling)
SubSamp=1 #level of minimum subsampling (= X times average number of calls per species)
GradientSamp=-0.1 #gradient strength (must be negative)

#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
source(MRF) #Slightly modified randomForest function to allow empty sample strata

parlist1=list.files(RSDB,pattern=".ta$",full.names=T,recursive=T)

Sys.time()
my.data <- list()
for (i in 1:length(parlist1)){
  #for (i in 1:20300){
  my.data[[i]] <- read.csv(parlist1[[i]],sep="\t")
}
Sys.time()

param3=as.data.frame(rbindlist(my.data))


#merge with MDXC
param3$Idp=sapply(param3$Filename,FUN=function(x) strsplit(as.character(x),split="-")[[1]][3])
param4=merge(param3,MDXC,by.x="Idp",by.y="Recording_ID")

#merge with SpeciesList (code)
param4$'Scientific name'=paste(param4$Genus,param4$Specific_epithet)
test=match(param4$`Scientific name`,SpeciesList$`Scientific name`)
SpM=subset(param4,is.na(test))
table(SpM$`Scientific name`)
tabaseXC=merge(param4,SpeciesList,by="Scientific name")
table(tabaseXC$'Scientific name')
tabaseXC$Zone=tabaseXC$Country
tabaseXC$Site=tabaseXC$Locality
tabaseXC$Auteur=tabaseXC$Recordist

#format tabaseXC and tabase 3 to be compatible
test=match(colnames(tabaseXC),colnames(tabase3))
testm=subset(colnames(tabaseXC),is.na(test))
for (i in length(test):1)
{
  if (is.na(test[i]))
      {
        tabaseXC[,i]=NULL
  }
  print(i)
  #assign(paste0("tabaseXC$",testm[i]),NULL)
}

test=match(colnames(tabase3),colnames(tabaseXC))
for (i in length(test):1)
{
  if (is.na(test[i]))
  {
    tabase3[,i]=NULL
  }
  print(i)
  #assign(paste0("tabaseXC$",testm[i]),NULL)
}

tabase3XC=rbind(tabase3,tabaseXC)
test=match(tabase3XC$Nesp,levels(as.factor(tabaseXC$Nesp)))
tabase3XC$Nesp2=as.character(tabase3XC$Nesp)
tabase3XC$Nesp2[is.na(test)]="other"

summary(as.factor(tabase3XC$Nesp))

fwrite(tabase3XC,"tabase3XC.csv",row.names=F)
