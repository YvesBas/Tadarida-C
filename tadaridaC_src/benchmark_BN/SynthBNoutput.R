library(data.table)

FOutBN="./www/res_sisteron_data.csv"

OutBN=fread(FOutBN)

Out1=subset(OutBN,OutBN$Rank==1)

#compute results in 1 comment per file
file=vector()
results=vector()
for (i in 1:length(unique(OutBN$`Begin File`)))
{
  Datai=subset(OutBN,OutBN$`Begin File`==unique(OutBN$`Begin File`)[i])
  Datai=Datai[order(Datai$`Confidence`,decreasing=T),]
  file=c(file,unique(OutBN$`Begin File`)[i])
  resultij=""
  if(i%%1000==1){print(paste(i,length(unique(OutBN$`Begin File`)),Sys.time()))}
  for (j in 1:length(unique(Datai$`Common Name`)))
  {
    Dataij=subset(Datai,Datai$`Common Name`==unique(Datai$`Common Name`)[j]) 
    #if(nrow(Dataij)>1){stop()}
    resultij=paste0(resultij,unique(Datai$`Common Name`)[j])
    resultij=paste(resultij,max(round(Dataij$`Confidence`*100)))
    #Freqij=round(median(Dataij$Frequence),1)
    #resultij=paste0(resultij," F",Freqij)
    Tminij=round(min(Dataij$`Begin Time (s)`),1)
    Tmaxij=round(max(Dataij$'End Time (s)'),1)
    resultij=paste0(resultij," N",nrow(Dataij))
    resultij=paste0(resultij," T",Tminij,"_",Tmaxij," - ")
  }
  results=c(results,resultij)
}

DataRes=data.table(file,results)

fwrite(DataRes,gsub(".csv","_1perfile.csv",FOutBN),sep=";")

