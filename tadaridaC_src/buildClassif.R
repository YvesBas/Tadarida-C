#loading randomForest library to build the classifier
set.seed(921)
library(randomForest)

library(data.table) #used to generate features table from labelled sound database

etidir="C:/Users/JF/Desktop/Tadarida-C/eti" #path where labels (.eti files) are stored 
pardir="C:/Users/JF/Desktop/Tadarida-C/txt" #path where features (.ta files) are stored 

#setwd("D:/ScanRLogs/R_data")

#listing label files with and without their path
etilist=list.files(etidir,pattern=".eti$",full.names=T,recursive=T)
etilist2=list.files(etidir,pattern=".eti$",full.names=F,recursive=T)

#concatenating labels tables
Sys.time()
my.data <- list()
fichier=vector()
for (i in 1:length(etilist)){
  my.data[[i]] <- read.csv(etilist[[i]],sep="\t",h=T,row.names=NULL)
  fichier=c(fichier
            ,rep(paste(substr(etilist2[[i]],1,nchar(etilist2[[i]])-4)
                       ,".wav",sep=""),nrow(my.data[[i]])))
}

Sys.time()
etitot=as.data.frame(rbindlist(my.data,fill=T))
etitot2=cbind(fichier,etitot)
colnames(etitot2)[2]="NumCri"



#concatenating features tables
parlist=list.files(pardir,pattern=".ta$",full.names=T,recursive=T)

Sys.time()
my.data <- list()
for (i in 1:length(parlist)){
  my.data[[i]] <- read.csv(parlist[[i]],sep="\t")
}
Sys.time()

param3=as.data.frame(rbindlist(my.data))


#merging labels and features
tabase=merge(param3,etitot2,by.x=c("Filename","CallNum"),by.y=c("fichier","NumCri"),all.x=T)

#filtering out non-labeled sound events
tabase2=subset(tabase,tabase$Cri!="")

#summary(tabaseHF$Cri)
#levels(tabaseHF$Cri)
#write.csv(levels(tabase2$Cri),"TtEsp.csv")

#Filtering species according to their "country" occurence and grouping undistinguishable species (e.g. Myotis blythii and M. myotis)
EspVC=read.csv("EspVC.csv",h=T)
EspVC2=subset(EspVC,EspVC$Norfolk=="x") #to be edited according to country
tabaseVC=merge(tabase2,EspVC2,by.x="Cri",by.y="Esp")
tabaseVC$Nesp=factor(tabaseVC$Nesp,exclude=NULL)

#converting (rarely) missing features to 0
tabaseVC[is.na(tabaseVC)]=0

#purge site factor from its missing levels
tabaseVC$Zone=factor(tabaseVC$Zone,exclude=NULL)



#creating a formula using all sound features
FormulCrit=colnames(tabaseVC[5])
for (i in 6:(ncol(tabaseVC)-16))
{
  FormulCrit=paste(FormulCrit,colnames(tabase2)[i],sep="+")
}

#average number of sound events per species, used thereafter to balance species weights in the classifier
NbMoyCri=as.numeric(mean(table(tabaseVC$Nesp)))

#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in 1:50)
{
  
#randomly selecting 63% of sites to build the small forest
SelSiteTemp=cbind(Zone=levels(tabaseVC$Zone)
                  ,Sel=sample(0:1,nlevels(tabaseVC$Zone)
                       ,replace=F,prob=c(37,63)))
tabaseVC2=merge(tabaseVC,SelSiteTemp,by="Zone")

#designing sampling strata as a combination of species and site
StrataTemp=as.factor(paste(as.character(tabaseVC2$Nesp)
                           ,as.character(tabaseVC2$Sel)))

#maximum sampled sound events per species
SampMax=11*exp(i*(-0.1))*NbMoyCri
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
                            ,data=tabaseVC2,replace=F
                            ,strata=StrataTemp
                            ,sampsize=SampTemp2
                            ,importance=F,ntree=10) 

#combine it with previously build small forests
if (exists("ClassifEspA")==TRUE) {ClassifEspA=combine(ClassifEspA,ClassifEspTemp)} else {ClassifEspA=ClassifEspTemp}
}
Sys.time()


save (ClassifEspA,file="ClassifEspHF3.learner") #12 sec
Sys.time()


