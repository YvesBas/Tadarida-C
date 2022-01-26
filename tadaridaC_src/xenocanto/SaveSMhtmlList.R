#library(xml2)
library(data.table)
Year=c(2014:2022)
Site="https://sonotheque.mnhn.fr/sounds/mnhn/so"
#DownloadPath="https://sonotheque.mnhn.fr/download/sounds/"
OutDir="C:/Users/yvesb/Documents/test"
OutF="HtmlList_test.Rdata"
dir.create(OutDir)

HtmlList=vector()
HtmlData=list()
for (h in 1:length(Year))
{
  for (i in 1:20000)
  {
    if(i%%100==1){
      print(paste(i,Sys.time()))
    }
    #Htmli=read_html(paste0(Site,"/",Year[h],"-",i))
    Adressi=paste0(Site,"/",Year[h],"-",i)
    Htmli=try(read.table(Adressi,sep="$")) #dirty
    if(class(Htmli)=="try-error")
    {
      print("retry")
      Sys.sleep(60)
      Htmli=try(read.table(Adressi,sep="$")) #dirty
      print(Adressi)
      print(class(Htmli))
    }
    if(class(Htmli)=="data.frame")
    {
      testW=max(grepl("block",Htmli$V1))
      if(testW==1)
      {
        HtmlList=c(HtmlList,Adressi)
        HtmlData[[i]]=Htmli
      }
      
    }
    
  }
}
fwrite(data.frame(listeH=HtmlList),"HtmlList_test.csv",sep=";")
save(HtmlData,file=OutF)

