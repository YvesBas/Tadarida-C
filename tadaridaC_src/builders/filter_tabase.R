library(data.table)

tabasetot=fread("RSDB_HF_tabase3HF_sansfiltre.csv")
GeoFilter="France"
Prefix="Cir"
SpeciesList=fread("SpeciesList.csv")

if(!is.null(Prefix))
{
  testPrefix=substr(tabasetot$Filename,1,nchar(Prefix))
  tabase=subset(tabasetot,testPrefix==Prefix)
}else{
  tabasetot=tabase
}

if (!is.null(GeoFilter)) 
{
  colFilter = subset(SpeciesList,select=GeoFilter)
  SpeciesFilter=subset(SpeciesList,as.data.frame(colFilter)[,1]=="x")
  tabase3=subset(tabase,tabase$Nesp %in% SpeciesFilter$Esp)
  FilteredOut=subset(tabase$Esp,!(tabase$Esp %in% SpeciesFilter$Esp))
  print(table(FilteredOut))
  fwrite(tabase3,paste0("tabase3HF_",GeoFilter,"_",Prefix,".csv"),row.names=F)
}else{
  fwrite(tabase,paste0("tabase3HF_",Prefix,".csv"),row.names=F)
}
