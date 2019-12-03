library(data.table)
library(randomForest)
MRF="C:/Users/Yves Bas/Documents/Tadarida/Tadarida-C/tadaridaC_src/Modified_randomForest.R"
PourC2M=fread("PourC2M_Norfolk.csv")
SpeciesList=fread("SpeciesList.csv")
FactorsToExclude=c("Group.1","participation","ValidConf","ValidId"
                  ,"SuccessProb","VersionC","VersionD","Version"
                  ,"SpMaxF2","Order","LP","CaseTracking")
SuffixToExclude=c("_ntot","_RT")
#SuffixToExclude="XXXXX"
SubSamp=20
GradientSamp=-0.1
set.seed(921)

f2p <- function(x) 
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 22, nchar(x)-8), ".", substr(x, nchar(x) - 6, nchar(x)-4), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}

source(MRF)

if(exists("ClassifEspC2b")){rm(ClassifEspC2b)}
test=match(PourC2M$ValidId,SpeciesList$Esp)

SpMissing=subset(PourC2M,is.na(test))
if(nrow(SpMissing)>0)
  {
  print(table(SpMissing$ValidId))
  stop("MISSING TAXA!!!")
  }
SpGroup=SpeciesList$Nesp[test]
test=(SpGroup==PourC2M$ValidId)
print(table(subset(PourC2M$ValidId,!test)))

PourC2M$ValidId=SpGroup

PourC2M$ValidId=factor(PourC2M$ValidId,exclude=NULL)
PourC2M$SpMaxF2=as.factor(PourC2M$SpMaxF2)
PourC2M$PF=!(substr(PourC2M$Group.1,2,2)=="i")
PourC2M$Pedestre=((substr(PourC2M$Group.1,10,10)=="-")
                  &(!(substr(PourC2M$Group.1,5,5)=="-"))
                  &(!PourC2M$PF))


#création d'indice standardisé pour mieux tenir compte de l'espèce choisie
testCol=!is.na(match(names(PourC2M),SpeciesList$Esp))
RFresults=subset(PourC2M,select=(subset(names(PourC2M),testCol)))
Score=apply(RFresults,MARGIN=1,max)
RFresultsStd=RFresults/Score
names(RFresultsStd)=paste0(names(RFresultsStd),"_std")

PourC2M=cbind(PourC2M,RFresultsStd)

#ajout de la date
FI=tstrsplit(PourC2M$Group.1,"-")
Passnum=as.numeric(gsub("Pass","",FI[[3]]))
DatePass=pmin(126+Passnum*62,126+62*3)
DateTot=ifelse(PourC2M$PF,yday(f2p(PourC2M$Group.1)),DatePass)
PourC2M$Date=DateTot
PourC2M$Date[is.na(PourC2M$Date)]=mean(subset(PourC2M$Date,!is.na(PourC2M$Date)))
#construction du classificateur
#classifieur avec equilibrage des classes
NbMoyCri=as.numeric(mean(table(PourC2M$ValidId)))

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


test1=subset(PourC2M,PourC2M$ValidId %in% c("Myomys"))
test2=subset(PourC2M,PourC2M$ValidId %in% c("Myoalc"))
test3=subset(PourC2M,PourC2M$ValidId %in% c("Myocap"))
test4=subset(PourC2M,PourC2M$ValidId %in% c("Myoema"))

plot(test4$Myoema_ratio,test4$Myomys_ratio,log="xy")
points(test1$Myoema_ratio,test1$Myomys_ratio,col=2)

SumProb=apply(ClassifEspVotes,MARGIN=1,FUN=sum)
ProbEsp0=ClassifEspVotes/SumProb
testCol=!is.na(match(names(VoteC5),SpeciesList$Esp))
sauvnamesVC5=colnames(VoteC5)
colnames(VoteC5)=mapply(function(x,y) ifelse(y,paste0(x,"_prev"),x)
                        ,colnames(VoteC5),testCol)

ProbEsp <-  cbind(VoteC5,ProbEsp0)

fwrite(ProbEsp,paste0("ProbEspC3_",substr(Sys.time(),1,10),".csv"),row.names=F,sep=";")
