library(data.table)
ListTa=list.files("mnt/XC",pattern=".ta$",full.names=T,recursive=T)
print("L3")
CoefX10=fread("XC/CoeftableX10.csv")
print("L5")
MaxI=1e7
Output="dataTAtotF.csv"

print(ListTa[8444:8445])

datalist=list()
for (i in 1:min(MaxI,length(ListTa)))
{
if(i%%1000==1){print(paste(i,Sys.time()))}
#print(ListTa[i])
temp=try(read.csv(ListTa[i],sep="\t"))
  if(class(temp)!="try-error"){
  if(nrow(temp)>0){
  datalist[[i]]=temp
  datalist[[i]]$FileTA=ListTa[i]
  }
  }
}
dataTA=rbindlist(datalist)
dataTA$X10=as.numeric(grepl("-x10.wav",dataTA$Filename))
dataX1=subset(dataTA,X10==0)
dataX10=subset(dataTA,X10==1)
dataX10=as.data.frame(dataX10)
print(ncol(dataX10))
dataX10[is.na(dataX10)]=0
print("L31")
for (i in 3:(ncol(dataX10)-2))
{
print(i)
 test=match(names(dataX10)[i],CoefX10$param) 
 Coef=CoefX10$coef[test]
 dataX10[,i]=dataX10[,i]*Coef
}

dataTA=rbind(dataX1,dataX10)
#File1=substr(dataTA$Filename,1,1)
#File1U=unique(File1)

#for (i in 1:length(File1U)
#{
#	datai=File1U
#}

fwrite(dataTA,Output,sep=";")

