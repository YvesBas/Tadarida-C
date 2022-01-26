library(data.table)

Label=T
Fout="C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point du 21 janvier 2022/a/predictions_11760_96_11820_jeutest10183.csv"
Nspmax=3


Decalage=ifelse(Label,2,0)

DataOut <- fread(Fout)
#DataOut <- fread(file.choose())


DataInfo=tstrsplit(DataOut$`fichier jpg`,split="--")
summary(as.numeric(gsub(".jpg","",DataInfo[[5+Decalage]])))
hist((as.numeric(gsub(".jpg","",DataInfo[[5+Decalage]]))))
plot(log(as.numeric(gsub(".jpg","",DataInfo[[3+Decalage]]))-550),(as.numeric(gsub(".jpg","",DataInfo[[5+Decalage]]))))

DataOut$Amplitude=log(as.numeric(gsub(".jpg","",DataInfo[[3+Decalage]]))-550)
DataOut$Duree=(as.numeric(gsub(".jpg","",DataInfo[[4+Decalage]])))
DataOut$Frequence=(as.numeric(gsub(".jpg","",DataInfo[[5+Decalage]]))/1000)
DataOut$StTime=(as.numeric(gsub(".jpg","",DataInfo[[6+Decalage]]))/1000)

fwrite(DataOut,gsub(".txt","_ancillary.csv",Fout),sep=";")

head(table(DataOut$`classe predite`)[order(table(DataOut$`classe predite`),decreasing=T)],25)
ShortList=names(head(table(DataOut$`classe predite`)[order(table(DataOut$`classe predite`),decreasing=T)],25))
DataShort=subset(DataOut,DataOut$`classe predite` %in% ShortList)
boxplot(DataShort$`pourcentage prediction`~DataShort$`classe predite`,las=2)


DataOutMS=aggregate(DataOut$`pourcentage prediction`,by=c(list(DataOut$`fichier wav`)
                                                          ,list(DataOut$`classe predite`))
                    ,max)

table(DataOutMS$Group.2)[order(table(DataOutMS$Group.2),decreasing=T)]
boxplot(DataOutMS$x~DataOutMS$Group.2,las=2)

DataOutNB=aggregate(DataOut$`pourcentage prediction`,by=c(list(DataOut$`fichier wav`)
                                                          ,list(DataOut$`classe predite`))
                    ,length)
DataOutMS$nbcalls=DataOutNB$x
boxplot(DataOutMS$nbcalls~DataOutMS$Group.2,las=2)

DataOutDur=aggregate(DataOut$Duree,by=c(list(DataOut$`fichier wav`)
                                                          ,list(DataOut$`classe predite`))
                    ,max)
DataOutMS$MaxDur=DataOutDur$x
boxplot(DataOutMS$MaxDur~DataOutMS$Group.2,las=2,ylim=c(0,3000))

DataOutAmp=aggregate(DataOut$Amplitude,by=c(list(DataOut$`fichier wav`)
                                        ,list(DataOut$`classe predite`))
                     ,max)
DataOutMS$Amp=DataOutAmp$x
boxplot(DataOutMS$Amp~DataOutMS$Group.2,las=2)

DataOutFreq=aggregate(DataOut$Frequence,by=c(list(DataOut$`fichier wav`)
                                            ,list(DataOut$`classe predite`))
                     ,median)
DataOutMS$Freq=DataOutFreq$x
boxplot(DataOutMS$Freq~DataOutMS$Group.2,las=2)

DataOutBeg=aggregate(DataOut$StTime,by=c(list(DataOut$`fichier wav`)
                                             ,list(DataOut$`classe predite`))
                      ,min)
DataOutMS$Beg=DataOutBeg$x
boxplot(DataOutMS$Beg~DataOutMS$Group.2,las=2)

DataOutEnd=aggregate((DataOut$StTime+DataOut$Duree/1000),by=c(list(DataOut$`fichier wav`)
                                         ,list(DataOut$`classe predite`))
                     ,max)
DataOutMS$End=DataOutEnd$x
boxplot(DataOutMS$End~DataOutMS$Group.2,las=2)

DataOutMS$DurSeq=DataOutMS$End-DataOutMS$Beg
boxplot(DataOutMS$DurSeq~DataOutMS$Group.2,las=2)

fwrite(DataOutMS,paste0(dirname(Fout),"/Synth_",basename(Fout)),sep=";")


#compute results in 1 comment per file
file=vector()
results=vector()
for (i in 1:length(unique(DataOut$`fichier wav`)))
{
  Datai=subset(DataOut,DataOut$`fichier wav`==unique(DataOut$`fichier wav`)[i])
  Datai=Datai[order(Datai$`pourcentage prediction`,decreasing=T),]
  file=c(file,unique(DataOut$`fichier wav`)[i])
  resultij=""
  if(i%%1000==1){print(paste(i,length(unique(DataOut$`fichier wav`)),Sys.time()))}
  for (j in 1:length(unique(Datai$`classe predite`)))
  {
    Dataij=subset(Datai,Datai$`classe predite`==unique(Datai$`classe predite`)[j]) 
    #if(nrow(Dataij)>1){stop()}
    resultij=paste0(resultij,unique(Datai$`classe predite`)[j])
    resultij=paste(resultij,max(round(Dataij$`pourcentage prediction`)))
    Freqij=round(median(Dataij$Frequence),1)
    resultij=paste0(resultij," F",Freqij)
    Tminij=round(min(Dataij$StTime),1)
    Tmaxij=round(max(Dataij$StTime+Dataij$Duree/1000),1)
    resultij=paste0(resultij," N",nrow(Dataij))
    resultij=paste0(resultij," T",Tminij,"_",Tmaxij," - ")
  }
  results=c(results,resultij)
}

DataRes=data.table(file,results)

fwrite(DataRes,gsub(".csv","_1perfile.txt",Fout),sep=";")

