library(data.table) #used to generate features table from labelled sound database
#INPUTS (to be edited according to local path)
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
VarSel=fread("VarSel.csv")

### A TESTER SANS FILTRE##
GeoFilter="France" #to uncomment and edit if a species filtering is necessary

#SETTINGS (both are intended to balance unvenness in species sampling)
SubSamp=11 #level of minimum subsampling (= X times average number of calls per species)
GradientSamp=-0.1 #gradient strength (must be negative)

#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
library(randomForest)
source(MRF) #Slightly modified randomForest function to allow empty sample strata


tabase3=fread(paste0("tabase3HF_",GeoFilter,".csv"))

tabase3$Nesp=factor(tabase3$Nesp,exclude=NULL)
tabase3$Site=factor(tabase3$Site,exclude=NULL)

#creating a formula using all sound features
FormulCrit=VarSel$VarSel[1]
for (i in 2:nrow(VarSel))
{
  FormulCrit=paste(FormulCrit,VarSel$VarSel[i],sep="+")
}

#average number of sound events per species, used thereafter to balance species weights in the classifier
NbMoyCri=as.numeric(mean(table(tabase3$Nesp)))

#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in 1:50)
{
  Sys.time()
  print(paste("forest n°",i,Sys.time()))
  
  #randomly selecting 63% of sites to build the small forest
  Sel=vector()
  while(sum(Sel)==0)
  {Sel=sample(0:1,nlevels(tabase3$Site),
              replace=T,prob=c(0.37,0.63))}
  
  SelSiteTemp=cbind(Site=levels(tabase3$Site),Sel)
  
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
  Predictors=tabase4[,5:154]
  
  ClassifEspTemp=randomForest(x=Predictors,y=tabase4$Nesp
                              ,replace=F
                              ,strata=StrataTemp
                              ,sampsize=SampTemp2
                              ,importance=F,ntree=10) 
  
  Sel10=1-as.numeric(sapply(StrataTemp
                            ,FUN=function(x) strsplit(as.character(x),split=" ")[[1]][2]))
  ClassifEspVT=ClassifEspTemp$votes*Sel10
  ClassifEspVT[is.na(ClassifEspVT)]=0
  if (exists("ClassifEspVotes")==TRUE){ClassifEspVotes=ClassifEspVotes+ClassifEspVT}else{ClassifEspVotes=ClassifEspVT}
  #combine it with previously build small forests
  if (exists("ClassifEspA")==TRUE) {ClassifEspA=combine(ClassifEspA,ClassifEspTemp)} else {ClassifEspA=ClassifEspTemp}
  Sys.time()
  save (ClassifEspA,file=paste0("ClassifEsp",GeoFilter
                                ,substr(Sys.time(),1,10),"_",i,".learner")) 
  }
Sys.time()


save (ClassifEspA,file=paste0("ClassifEsp",GeoFilter
                              ,substr(Sys.time(),1,10),".learner")) 
Sys.time()



SumProb=apply(ClassifEspVotes,MARGIN=1,FUN=sum)
ProbEsp0=ClassifEspVotes/SumProb



ProbEsp <-  cbind(tabase4[,1:12],ProbEsp0
                  ,HL=(tabase4$Hup_RFMP!=0),HU=(tabase4$Hlo_PosEn!=9999))

setcolorder(ProbEsp,c(colnames(ProbEsp)[3:12],colnames(ProbEsp)[1:2],colnames(ProbEsp)[13:ncol(ProbEsp)]))

fwrite(ProbEsp,paste0("ProbEspHF",substr(Sys.time(),1,10),".csv"),row.names=F)

