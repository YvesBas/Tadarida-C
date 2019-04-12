library(warbleR)
listSp=c("Fringilla coelebs","Cyanistes caeruleus","Turdus merula"
         ,"Emberiza cirlus","Sylvia melanocephala"
         ,"Turdus pilaris","Corvus cornix","Falco tinnunculus"
         ,"Spinus spinus","Phylloscopus collybita")


if(exists("MDXC")){rm(MDXC)}
i=1
setwd("E:/XC")

for (i in 1:length(listSp))
{
test=querxc(listSp[i])
if(exists("MDXC")){MDXC=rbind(MDXC,test)}else{MDXC=test}

print(paste(listSp[i],nrow(test)))
Sys.time()
querxc(X=test[sample(1:nrow(test),50),])
Sys.time()
testS=grepl("song",test$Vocalization_type)
testC=subset(test,testS==F)
querxc(X=testC[sample(1:nrow(testC),50),])
Sys.time()
print(paste(i,nrow(testC)))
}
write.csv2(MDXC,paste0("C:/Users/Yves Bas/Documents/XC/MDXC",substr(Sys.time(),1,10),".csv"),row.names=F)


ListW=list.files(getwd(),pattern=".wav$")
ListM=list.files(getwd(),pattern=".mp3$")

while(length(ListW)<length(ListM)) #to handle quite frequent crashes in mp32wav function
{
Sys.time()
try(mp32wav())
Sys.time()
ListW=list.files(getwd(),pattern=".wav$")
ListM=list.files(getwd(),pattern=".mp3$")
print(paste(length(ListW),length(ListM),sep="/"))
}

#ListWF=list.files(getwd(),pattern=".wav$",full.names=T)

SplitDir=paste0(getwd(),"/split",substr(Sys.time(),1,10),"/")
dir.create(SplitDir)


InfoW=file.info(ListW)
TimeW=Sys.time()-InfoW$ctime
ListWrecent=subset(ListW,TimeW<15000)


#j=1
for (j in 1:length(ListWrecent))

  {
  Dur=0
  if((file.size(ListWrecent[j]))>50000)
  {
tempW=readWave(ListWrecent[j])
Dur=duration(tempW)
if(Dur>0)
 {
for(k in 1:ceiling(Dur/5))
{
  tempCL=cutw(channel(tempW,which="left"),from=(k-1)*5,to=min(Dur,k*5),output="Wave")
  tempCL=normalize(tempCL,level=0.3)
  savewav(tempCL,filename=paste0(SplitDir,substr(ListWrecent[j],1,nchar(ListWrecent[j])-4),"-L",k,".wav"))
  if(length(tempW@right)>0){
  tempCR=cutw(channel(tempW,which="right"),from=(k-1)*5,to=min(Dur,k*5),output="Wave",normalize= "16")
  tempCR=normalize(tempCR,level=0.3)
  savewav(tempCR,filename=paste0(SplitDir,substr(ListWrecent[j],1,nchar(ListWrecent[j])-4),"-R",k,".wav"))
  }
}
}}
print(paste(j,ListWrecent[j],Dur))

}
ListS=list.files(SplitDir,pattern=".wav$",full.names=T)
SelDir=paste0(getwd(),"/sel",substr(Sys.time(),1,10),"/")
dir.create(SelDir)
SelS=sample(ListS,10)
file.copy(from=SelS,to=SelDir)
setwd("C:/Users/Yves Bas/Documents")
