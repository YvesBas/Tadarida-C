library(data.table) #used to generate features table from labelled sound database
library(dplyr)
library(randomForest)
library(beepr)

setwd("F:/RSDB_HF")

#INPUTS (to be edited according to local path)
MRF="C:/Users/croemer01/Documents/R/Tadarida_GitHub/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
VarSel0=as.data.frame(fread("C:/Users/croemer01/Documents/R/Tadarida_GitHub/Tadarida-C/tadaridaC_src/other_inputs/VarSel.csv"))
SaveTemp=T
Ftabase="_tabase3HF_sansfiltre.csv"
StartForest=1

# If there was a memory problem, it is possible to pursue from where it stopped
# StartForest=45 # indicate the tree number that could not be built
# load("D:/RSDB_HF/ClassifEsp__tabase3HF_sansfiltre_2020-08-13.learner") # Load incomplete learner

Table_TAXREF_Sonotype=fread("C:/Users/croemer01/Documents/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Classificateur/Liste_especes_DOM-TOM.csv")
Table_TAXREF_Sonotype=Table_TAXREF_Sonotype %>% select(LB_NOM, SONOTYPE)
Table_TAXREF_Sonotype=subset(Table_TAXREF_Sonotype, (Table_TAXREF_Sonotype$SONOTYPE!=""))


#SETTINGS (both are intended to balance unvenness in species sampling)
SubSamp=2 #level of minimum subsampling (= X times average number of calls per species)
GradientSamp=-0.1 #gradient strength (must be negative)

#loading randomForest library to build the classifier then modified randomforest function
set.seed(921)
source(MRF) #Slightly modified randomForest function to allow empty sample strata

tabase1=as.data.frame(fread(Ftabase))
tabase1$Nesp=factor(tabase1$Nesp,exclude=NULL)
tabase1$Site=factor(tabase1$Site,exclude=NULL)

# Add a binary feature to help sort out birds from bats (based on if FreqMP>17)
tabase1$FreqSup17=ifelse(tabase1$FreqMP>17, 1, 0)

# Remove any frequency parameter (for Sonotypes)
List_frequency_parameters = c("Fmin", "Fmax", "FreqMP", "FreqPkS", "FreqPkM",
                              "FreqPkS2", "FreqPkM2", 
                              "_HCF", "_FMax", "_FMin", "_FPk", 
                              "_FlF", "_FIF", "_LCF", "_StF", "_EnF",
                              "_CeF", "_5dBBF", "_5dBAF")
tabase2=tabase1 %>% select(!contains(List_frequency_parameters))
VarSel= VarSel0 %>%
  filter(!grepl(paste(List_frequency_parameters, collapse="|"), VarSel))

# Correct some species names
tabase2$`Scientific name`[which(tabase2$`Scientific name`=="Myotis sp. A ('southern' Natterer)")]="Myotis crypticus"

# Include Sonotype and disgard social calls
tabase3a=right_join(tabase2,Table_TAXREF_Sonotype, by=c("Scientific name" = "LB_NOM"))

tabase3=subset(tabase3a, !(tabase3a$Type=="social" & tabase3a$Group=="bat"))

# Replace Sonotype by buzz or social if it is identified as such in Type
tabase3$SONOTYPE[which(tabase3$Type=="buzz")]=tabase3$Type[which(tabase3$Type=="buzz")]
#tabase3$SONOTYPE[which(tabase3$Type=="social")]=tabase3$Type[which(tabase3$Type=="social")]

#creating a formula using all sound features
FormulCrit=VarSel$VarSel[1]
for (i in 2:nrow(VarSel))
{
  FormulCrit=paste(FormulCrit,VarSel$VarSel[i],sep="+")
}

#average number of sound events per Sonotype, used thereafter to balance Sonotype weights in the classifier
NbMoyCri=as.numeric(mean(table(tabase3$SONOTYPE)))

#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in StartForest:50)
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
  StrataTemp=as.factor(paste(as.character(tabase4$SONOTYPE)
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
  Predictors=subset(tabase4,select=VarSel$VarSel)

  
  ClassifEspTemp=randomForest(x=Predictors,y=as.factor(tabase4$SONOTYPE) 
                              ,replace=F
                              ,strata=StrataTemp
                              ,sampsize=SampTemp2
                              ,importance=F,ntree=10) 
  
  if(SaveTemp)
  {
    save (ClassifEspTemp,file=paste0("ClassifEsp"
                                     ,substr(Sys.time(),1,10),"_",i,".learner")) 
  
  }else{
  Sel10=1-as.numeric(sapply(StrataTemp
                            ,FUN=function(x) strsplit(as.character(x),split=" ")[[1]][2]))
  ClassifEspVT=ClassifEspTemp$votes*Sel10
  ClassifEspVT[is.na(ClassifEspVT)]=0
  if (exists("ClassifEspVotes")==TRUE){ClassifEspVotes=ClassifEspVotes+ClassifEspVT}else{ClassifEspVotes=ClassifEspVT}
  #combine it with previously build small forests
  if (exists("ClassifEspA")==TRUE) {ClassifEspA=combine(ClassifEspA,ClassifEspTemp)} else {ClassifEspA=ClassifEspTemp}
  Sys.time()
  }
}
Sys.time()
beep(2)

if(!SaveTemp){
save (ClassifEspA,file=paste0("ClassifEsp_",gsub(".csv","",Ftabase),"_"
                              ,substr(Sys.time(),1,10),".learner")) 
Sys.time()


fwrite(as.data.frame(ClassifEspVotes)
       ,paste0("ClassifEspVotes",substr(Sys.time(),1,10),".csv"))

SumProb=apply(ClassifEspVotes,MARGIN=1,FUN=sum)
ProbEsp0=ClassifEspVotes/SumProb



ProbEsp <-  cbind(tabase4[,1:12],ProbEsp0
                  ,HL=(tabase4$Hup_RFMP!=0),HU=(tabase4$Hlo_PosEn!=9999))

setcolorder(ProbEsp,c(colnames(ProbEsp)[3:12],colnames(ProbEsp)[1:2],colnames(ProbEsp)[13:ncol(ProbEsp)]))

fwrite(ProbEsp,paste0("ProbEspHF",substr(Sys.time(),1,10),".csv"),row.names=F)
}
