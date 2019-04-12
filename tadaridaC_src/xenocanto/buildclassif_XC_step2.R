#SETTINGS (both are intended to balance unvenness in species sampling)
SubSamp=1 #level of minimum subsampling (= X times average number of calls per species)
GradientSamp=-0.1 #gradient strength (must be negative)
args="tabase3_LFXC"


memory.limit(10 * 10^10)
library(data.table)
library(randomForest)
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
source(MRF)

#tabase3=fread("tabase3_LF.csv")
SpeciesList=as.data.frame(fread("SpeciesList_LF.csv")) #to uncomment if a species grouping and/or filtering is necessary

### A TESTER SANS FILTRE##
GeoFilter="France" #to uncomment and edit if a species filtering is necessary


tabase3XC=fread(paste0(args,"_Reduc.csv"))

ListVar=c(colnames(tabase3XC)[1:12],colnames(tabase3XC)[275:325]
          ,colnames(tabase3XC)[501:505])
tabase3XC=tabase3XC[,..ListVar]
#average number of sound events per species, used thereafter to balance species weights in the classifier
NbMoyCri=as.numeric(mean(table(tabase3XC$Nesp)))

#FormulCrit=colnames(tabase3XC)[4]
#for (i in 5:100)
#{
#  FormulCrit=paste(FormulCrit,colnames(tabase3XC)[i],sep="+")
#}

#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in 1:50)
{
  
  print(paste("forest n°",i,Sys.time()))
  #randomly selecting 63% of sites to build the small forest
  Sel=vector()
  while(sum(Sel)==0)
  {Sel=sample(0:1,nlevels(as.factor(tabase3XC$Site)),
              replace=T,prob=c(0.37,0.63))}
  
  SelSiteTemp=cbind(Site=levels(as.factor(tabase3XC$Site)),Sel)
  
  tabase4=merge(tabase3XC,SelSiteTemp,by="Site")
  
  #designing sampling strata as a combination of species and site
  StrataTemp=as.factor(paste(as.character(tabase4$Nesp)
                             ,as.character(tabase4$Sel)))
  
  #maximum sampled sound events per species
  SampMax=SubSamp*exp(i*(GradientSamp))*NbMoyCri
  print(SampMax)
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
  Sys.time()
  # building the "10 trees" random forest
  Predictors=tabase4[,13:63]
  ClassifEspTemp=randomForest(x=Predictors,y=as.factor(tabase4$Nesp),replace=F
                              ,strata=StrataTemp
                              ,sampsize=SampTemp2
                              ,importance=F,ntree=10) 
  Sys.time()
  
  #Sel10=1-as.numeric(sapply(StrataTemp
   #                         ,FUN=function(x) strsplit(as.character(x),split=" ")[[1]][2]))
  #ClassifEspVT=ClassifEspTemp$votes*Sel10
  #ClassifEspVT[is.na(ClassifEspVT)]=0
  #if (exists("ClassifEspVotes")==TRUE){ClassifEspVotes=ClassifEspVotes+ClassifEspVT}else{ClassifEspVotes=ClassifEspVT}
  #write.csv(ClassifEspVotes,"ClassifEspVotesLFXC.csv",row.names=F)
  #combine it with previously build small forests
  if (exists("ClassifEspA")==TRUE) {ClassifEspA=combine(ClassifEspA,ClassifEspTemp)} else {ClassifEspA=ClassifEspTemp}
}
Sys.time()


save (ClassifEspA,file="ClassifEsp_LF_180320.learner") 
Sys.time()



SumProb=apply(ClassifEspVotes,MARGIN=1,FUN=sum)
ProbEsp0=ClassifEspVotes/SumProb



#Loop init
#this loop intends to detect successively different species within each file if there is sufficient dicrepancy in predicted probabilities
ProbEsp <-  cbind(tabase4[,1:12],ProbEsp0
                  ,HL=(tabase4$Hup_RFMP!=0),HU=(tabase4$Hlo_PosEn!=9999))


fwrite(ProbEsp,"ProbEspLF.csv",row.names=F)

