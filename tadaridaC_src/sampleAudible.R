library(data.table)

ListF=list.files("./Tadarida/echantillon_audible",full.names=T)

dataF=list()
for (i in 1:length(ListF))
{
  dataF[[i]]=fread(ListF[i])
}
data=rbindlist(dataF)

fwrite(data,"data_audible.csv",sep=";")

ListSp=unique(data$SpMaxF2)
data=subset(data,data$FreqP<10)
data$ConfianceClass=round(data$Ind*10)
data$ConfianceFreq=round(log(data$FreqP))
table(data$ConfianceFreq)
data=data[order(sample.int(nrow(data))),]

dataS=unique(data,by=c("SpMaxF2","ConfianceClass","ConfianceFreq"))

dataS=dataS[order(dataS$Filename),]
fwrite(dataS,"data_Saudible.csv",sep=";")
