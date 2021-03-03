library(data.table)

MaxDSEs=200

if(!exists("args"))
{
  Test=T
}else{
  if(length(args)<3)
  {
    Test=T
  }else{
    Test=F
  }
}
print(paste0("Test:",Test))

if(Test)
{
  args="D:/ta"
}  


ListTA=list.files(args[1],full.names=T)

for (i in 1:length(ListTA))
{
  if(i%%1000==1){print(paste("Resize",i,"/",length(ListTA),Sys.time()))}
  datai=fread(ListTA[i])
  if(nrow(datai)>MaxDSEs){
   Index=datai$Dur+datai$BW/10
  datai=subset(datai
                      ,Index>quantile(Index,1-MaxDSEs/nrow(datai)))
  fwrite(datai,ListTA[i],sep="\t")
  }
}

