library(data.table)
library(randomForest)
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
PourC2M=fread("ForOlivier_PourC2M.csv")
#SpeciesList=fread("SpeciesList.csv")
FactorsToExclude=c("Time" , "Date" ,"datetime",   "daytime"   , "nocturnal" 
                   ,"ID",      "filepath", "Filename","SpeciesPred" 
                   ,"filename"      ,      "Transect"       ,   "present"            
                   , "Species"  ,"Score","rainQ2"             
                   , "rain_min" ,           "RMS"   ,           "Meanspec"           
                   , "CallNum"   ,          "Version"   ,     "FileDur"            
                   , "SampleRate" ,         "StTime"  ,            "Dur"                
                   ,"PrevSt"      ,        "Fmax"  ,              "Fmin"               
                   ,"BW"      ,            "FreqMP" ,         "CM_FIF"             
                   , "Amp1"      ,          "Amp2" ,       "Amp3"               
                   , "Amp4"
                   )
SuffixToExclude=c("_ntot","_RT")
#SuffixToExclude="XXXXX"
SubSamp=20
GradientSamp=-0.1
set.seed(921)
VarToPredict="present"


#f2p <- function(x) 
#{
 # if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
#  op <- options(digits.secs = 3)
  #pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 6, nchar(x)-4), sep = "")
 # strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
#}

source(MRF)

if(exists("ClassifEspC2b")){rm(ClassifEspC2b)}

#ajout de la date
ToPredict=subset(PourC2M,select=VarToPredict)[[1]]
NbMoyCri=as.numeric(mean(table(ToPredict)))

Predictors=subset(names(PourC2M),!names(PourC2M) %in% FactorsToExclude)

for (i in 1:length(SuffixToExclude))
{
Predictors=subset(Predictors,!grepl(SuffixToExclude[i],Predictors))
}

Formul2=Predictors[1]
for (i in 2:length(Predictors))
{
  Formul2=paste(Formul2,Predictors[i],sep="+")
}



#iterative loop building each time a small random forest (10 trees) where sampling vary (see below)
Sys.time()
for (i in 1:50)
{
  print(paste("forest n°",i,Sys.time()))
  
  #randomly selecting 63% of sites to build the small forest
  Sel=vector()
  while(sum(Sel)==0)
  {Sel=sample(0:1,nlevels(as.factor(PourC2M$participation)),
              replace=T,prob=c(0.37,0.63))}
  SelSiteTemp=cbind(participation=levels(as.factor(PourC2M$participation)),Sel)
  
  VoteC5=merge(PourC2M,SelSiteTemp,by="participation")
  
  #designing sampling strata as a combination of species and site
  StrataTemp=as.factor(paste(as.character(VoteC5$ValidId)
                             ,as.character(VoteC5$Sel)))
  
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
  test=apply(VoteC5,MARGIN=2,function(x) sum(is.na(x)))
  
    
  # building the "10 trees" random forest
  ClassifEspTemp=randomForest(as.formula(paste("ValidId ~ ",Formul2))
                              ,data=VoteC5
                              ,replace=F
                              ,strata=StrataTemp
                              ,sampsize=SampTemp2
                              ,mtry=floor(2*(length(Predictors))^0.5)
                              ,importance=T,ntree=10) 
  
  varImpPlot(ClassifEspTemp,cex=0.5)
  Sel10=1-as.numeric(sapply(StrataTemp
                            ,FUN=function(x) strsplit(as.character(x),split=" ")[[1]][2]))
  ClassifEspVT=ClassifEspTemp$votes*Sel10
  ClassifEspVT[is.na(ClassifEspVT)]=0
  if (exists("ClassifEspVotes")==TRUE){ClassifEspVotes=ClassifEspVotes+ClassifEspVT}else{ClassifEspVotes=ClassifEspVT}
  #write.csv(ClassifEspVotes,paste0("ClassifEspVotes_C2B_",substr(Sys.time(),1,10),".csv"),row.names=F)
  #combine it with previously build small forests
  if (exists("ClassifEspC2b")==TRUE) {ClassifEspC2b=combine(ClassifEspC2b,ClassifEspTemp)} else {ClassifEspC2b=ClassifEspTemp}
}
Sys.time()
varImpPlot(ClassifEspC2b,cex=0.5)


save (ClassifEspC2b,file=paste0("ClassifEspC3_",substr(Sys.time(),1,10),".learner"))
Sys.time()
Imp=as.data.frame(ClassifEspC2b$importance)
Imp=cbind(Var=row.names(Imp),Imp)
fwrite(Imp,paste0("ClassifEspC3_",substr(Sys.time(),1,10),"_imp.csv"),sep=";")


#test1=subset(PourC2M,PourC2M$ValidId %in% c("Myomys"))
#test2=subset(PourC2M,PourC2M$ValidId %in% c("Myoalc"))
#test3=subset(PourC2M,PourC2M$ValidId %in% c("Myocap"))
#test4=subset(PourC2M,PourC2M$ValidId %in% c("Myoema"))

#plot(test4$Myoema_ratio,test4$Myomys_ratio,log="xy")
#points(test1$Myoema_ratio,test1$Myomys_ratio,col=2)

SumProb=apply(ClassifEspVotes,MARGIN=1,FUN=sum)
ProbEsp0=ClassifEspVotes/SumProb
testCol=!is.na(match(names(VoteC5),SpeciesList$Esp))
sauvnamesVC5=colnames(VoteC5)
colnames(VoteC5)=mapply(function(x,y) ifelse(y,paste0(x,"_prev"),x)
                        ,colnames(VoteC5),testCol)

ProbEsp <-  cbind(VoteC5,ProbEsp0)

fwrite(ProbEsp,paste0("ProbEspC3_",substr(Sys.time(),1,10),".csv"),row.names=F,sep=";")
