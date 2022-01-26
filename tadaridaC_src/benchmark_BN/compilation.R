library(data.table)

DirOut="./www/res_sisteron"

Lout=list.files(DirOut,full.names=T)

ListOut=list()
for (i in 1:length(Lout))
{
  ListOut[[i]]=fread(Lout[i])
}
DataOut=rbindlist(ListOut)

Data1=subset(DataOut,DataOut$Rank==1)
head(table(Data1$`Common Name`)[order(table(Data1$`Common Name`),decreasing = T)],30)
ShortList=names(head(table(Data1$`Common Name`)[order(table(Data1$`Common Name`),decreasing = T)],30))

DataShort=subset(Data1,Data1$`Common Name` %in% ShortList)
boxplot(DataShort$Confidence~DataShort$`Common Name`,las=2)

fwrite(DataOut,paste0(DirOut,"_data.csv"),sep=";")
