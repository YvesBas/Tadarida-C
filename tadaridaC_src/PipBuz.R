library(randomForest)
library(data.table)

if(length(args)<3) #for local tests
{
  args="C:/wamp64/www/ta3"
args[19]="ClassifBuzzPippip170328.learner"
}

ClassifBuzz=args[19]
TaDir=args[1]

Tafiles=list.files(TaDir,pattern=".ta$",full.names=T)
Talist=list()
for (i in 1:length(Tafiles))
{
  Talist[[i]]=fread(Tafiles[[i]])
}
Tadata=rbindlist(Talist,use.names=T,fill=T)

if(!exists("ClassifEspBuzz")){load(ClassifBuzz)}
ScoreBuzz=predict(ClassifEspBuzz,Tadata,type="prob")
TSB=cbind(Tadata[,1:10],ScoreBuzz)
#fwrite(TSB,"TSB.csv",sep=";")
TSBO=TSB[order(TSB$TRUETRUE,decreasing=T),]
ScoreMax1=aggregate(TSBO$TRUETRUE,by=list(TSBO$Filename),FUN=max)
ScoreMax2=aggregate(TSBO$TRUETRUE,by=list(TSBO$Filename)
                    ,FUN=function(x) ifelse(length(x)>1,x[2],0))
Nhigh=aggregate(TSBO$TRUETRUE,by=list(TSBO$Filename)
                    ,FUN=function(x) (length(subset(x,x>(max(x)/2)))))

PipMax=aggregate(TSBO$FALSETRUE,by=list(TSBO$Filename),FUN=max)

ScoreMax1$S1=ScoreMax1$x
ScoreMax1$S2=ScoreMax2$x
ScoreMax1$NH=Nhigh$x
ScoreMax1$PM=PipMax$x
#plot(ScoreMax1$S1,ScoreMax1$PM)
ScoreMax1$PipBuz=pmax(((ScoreMax1$S1-0.15)/0.1),0)*
  pmax(((ScoreMax1$PM-0.5)/0.2),0)
#plot(ScoreMax1$S1,ScoreMax1$PipBuz)
#fwrite(ScoreMax1,"ScoreMax1.csv",sep=";")
NbBuzz=length(ScoreMax1$PipBuz[ScoreMax1$PipBuz>0.5])
NbBuzzInf=length(ScoreMax1$PipBuz[ScoreMax1$PipBuz>0.975])
NbBuzzSup=length(ScoreMax1$PipBuz[ScoreMax1$PipBuz>0.025])
#ScoreInd=(NbBuzz/10+0.00001)/(NbBuzz/10+1.00001)*20
ScoreInd=(((NbBuzz^0.5/1.5+1))/((NbBuzz^0.5/1.5+1)+1)-0.5)*40
OutputBuz=c(paste0("Nombre de buzz : ",NbBuzz)
            ,paste0("Intervalle de confiance : ",NbBuzzInf,"-",NbBuzzSup)
            ,paste0("Score Indicateur Productivite : ",round(ScoreInd,1))
)
print(OutputBuz)
fwrite(data.frame(OutputBuz),paste0(args[1],"/","IndicateurProductivite.csv")
,sep=";")
