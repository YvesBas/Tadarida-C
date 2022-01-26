library(data.table)
library(ggplot2)

tabase=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
SpeciesList=fread("SpeciesList.csv")
Participants=c("Nicolas Fillol")
RSDB="RSDB_HF"
DirQuiz="chiros/rsdb2101"
DirQuiz2="chiros/quiz190611CS"
DirQuiz3="chiros/quiz190611BC"
DirPair="chiros/pourheterodyne2"
SRSel=384000
PairSpecies=c("Pippip","Eptser","Rhihip")


tabase$AmpTot=tabase$Amp1+tabase$Amp2+tabase$Amp3+tabase$Amp4
ggplot(tabase, aes(x=log(Dur), y=log(Amp4) )) +
  geom_bin2d() +
  theme_bw()

tabasePF=subset(tabase,substr(tabase$Filename,1,3)=="Car")
tabasePF=subset(tabasePF,tabasePF$SampleRate==SRSel)


BatSpecies=subset(SpeciesList,SpeciesList$Group=="bat")
BatCalls=subset(tabasePF,tabasePF$Espece %in% BatSpecies$Esp)
ggplot(BatCalls, aes(x=log(Dur), y=log(Amp1) )) +
  geom_bin2d() +
  theme_bw()
BatCallsNonEcho=subset(BatCalls,BatCalls$PrevMP2>20)


CallQuality=aggregate(BatCallsNonEcho$AmpTot,by=list(BatCallsNonEcho$Filename)
                      ,FUN=function(x) quantile(x,0.85))
GoodCQ=subset(CallQuality,CallQuality$x>median(CallQuality$x))
BadCQ=subset(CallQuality,CallQuality$x>median(CallQuality$x))

FileSelect=vector()
FileSelect=c(FileSelect,sample(GoodCQ$Group.1,30))
FileSelect=c(FileSelect,sample(BadCQ$Group.1,10))



CallNb=aggregate(BatCallsNonEcho$AmpTot,by=list(BatCallsNonEcho$Filename)
                      ,FUN=length)
GoodNb=subset(CallNb,CallNb$x>median(CallNb$x))
BadNb=subset(CallNb,CallNb$x>median(CallNb$x))

FileSelect=c(FileSelect,sample(GoodCQ$Group.1,30))
FileSelect=c(FileSelect,sample(BadCQ$Group.1,10))

NoiseSpecies=subset(SpeciesList,SpeciesList$Group=="noise")
NonNoise=subset(tabasePF,!(tabasePF$Espece %in% NoiseSpecies$Esp))
NonNoiseF=levels(as.factor(NonNoise$Filename))
FileSelect=c(FileSelect,sample(NonNoiseF,10))

TotF=levels(as.factor(NonNoise$Filename))
FileSelect=c(FileSelect,sample(TotF,10))

#table(BatCallsNonEcho$Auteur)
CallParticipants=subset(BatCallsNonEcho,BatCallsNonEcho$Auteur %in% Participants)
RecParticipants=levels(as.factor(CallParticipants$Filename))
FileSelect=c(FileSelect,sample(RecParticipants,20,replace=T))

WavList=list.files(RSDB,pattern=".wav$",full.names=T,recursive=T)
WavSelect=subset(WavList,basename(WavList) %in% FileSelect)


dir.create(DirQuiz)
file.copy(from=WavSelect,to=paste0(DirQuiz,"/",basename(WavSelect)))

BatCallsCS=subset(BatCallsNonEcho,BatCallsNonEcho$Type=="social")

CallQuality=aggregate(BatCallsCS$AmpTot,by=list(BatCallsCS$Filename)
                      ,FUN=function(x) quantile(x,0.75))
GoodCQ=subset(CallQuality,CallQuality$x>median(CallQuality$x))

FileSelCS=vector()
FileSelCS=c(FileSelCS,sample(GoodCQ$Group.1,25))

BatCallnbsp=aggregate(BatCallsNonEcho$CallNum
                      ,by=c(list(BatCallsNonEcho$Filename)
                            ,list(BatCallsNonEcho$Espece))
                      ,FUN=length)
BatCallnbsp2=aggregate(BatCallnbsp$Group.1
                      ,by=list(BatCallnbsp$Group.1)
                      ,FUN=length)

BatCallsSevSp=subset(BatCallnbsp2,BatCallnbsp2$x>1)
FileSelCS=c(FileSelCS,sample(BatCallsSevSp$Group.1,20))

WavSelect=subset(WavList,basename(WavList) %in% FileSelCS)

dir.create(DirQuiz2)
file.copy(from=WavSelect,to=paste0(DirQuiz2,"/",basename(WavSelect)))

BCSpecies=subset(SpeciesList,SpeciesList$Group=="bush-cricket")
BCCalls=subset(tabasePF,tabasePF$Espece %in% BCSpecies$Esp)

BCF=levels(as.factor(BCCalls$Filename))

FileSel3=vector()
FileSel3=c(FileSel3,sample(BCF,100))

NBNBSpecies=subset(SpeciesList
                   ,!(SpeciesList$Group %in% c("bush-cricket","bat","noise","bird"
                                               ,"frog","ground-cricket"
                                               ,"grasshopper","cicada"
                                               )))
NBNBCalls=subset(tabasePF,tabasePF$Espece %in% NBNBSpecies$Esp)

NBNBF=levels(as.factor(NBNBCalls$Filename))


FileSel3=c(FileSel3,sample(NBNBF,30))
WavSelect=subset(WavList,basename(WavList) %in% FileSel3)

dir.create(DirQuiz3)
file.copy(from=WavSelect,to=paste0(DirQuiz3,"/",basename(WavSelect)))

DataSp1=subset(tabase,tabase$Espece==PairSpecies[1])
FSp1=levels(as.factor(DataSp1$Filename))
DataSp2=subset(tabase,tabase$Espece==PairSpecies[2])
FSp2=levels(as.factor(DataSp2$Filename))
FPair=subset(FSp1,FSp1 %in% FSp2)

WavSelect=subset(WavList,basename(WavList) %in% FPair)
dir.create(DirPair)
file.copy(from=WavSelect,to=paste0(DirPair,"/",basename(WavSelect)))


