library(data.table)


Dir="../mnt"
Pattern="RSDB_HF_R"

RSDBdirs=list.dirs(Dir,recursive=F)

for (i in 1:length(RSDBdirs)){
  print(RSDBdirs[i])
  #ex1
   Talist=list.files(paste0(RSDBdirs[i],"/ex1/txt"),full.names=T
                    ,pattern=".ta$")
  DataTa=list()
  for (j in 1:length(Talist))
  {
    DataTa[[j]]=fread(Talist[j])
  }
  Data1=rbindlist(DataTA)
  
  print("ex1 done!")
  #ex10
  Talist=list.files(paste0(RSDBdirs[i],"/ex10/txt"),full.names=T
                    ,pattern=".ta$")
  DataTa=list()
  for (j in 1:length(Talist))
  {
    DataTa[[j]]=fread(Talist[j])
  }
  Data10=rbindlist(DataTA)
  print("ex10 done!")
  DataAll=rbind(Data1,Data10)
  fwrite(DataAll,paste0(RSDBdirs[i],"/FeaturesAll.csv"),sep=";")
}
