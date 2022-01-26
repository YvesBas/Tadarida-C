library(data.table)

htmlDir="C:/Users/yvesb/Downloads/htmlsonomnhntest"
WavDir="C:/Users/yvesb/Downloads/WAVsonomnhntest"


dir.create(WavDir)

htmlF=list.files(htmlDir,full.names=T)

Link=vector()
ErrorD=vector()
License=vector()
Author=vector()
Duration=vector()
Format=vector()
SampleRate=vector()
Species=vector()
Date=vector()
Location=vector()
Type=vector()
for (i in 1:length(htmlF))
{
  print(basename(htmlF[i]))
  htmlData=read.table(htmlF[i],sep="$",quote="")
  #htmlData=read.table(htmlF[i],sep="$")
  htmlDownload=subset(htmlData,grepl("https://sonotheque.mnhn.fr/download/sounds/",htmlData$V1))
  htmlDownload=subset(htmlData,grepl("button raised",htmlData$V1))
  
    htmlDownload2=tstrsplit(htmlDownload[1],split="href=")
    htmlDownload2b=unlist(htmlDownload2)
    htmlDownload2c=subset(htmlDownload2b,grepl("https://sonotheque.mnhn.fr/download/sounds/",htmlDownload2b))
  htmlDownload3=tstrsplit(htmlDownload2c[1],split=">")[[1]]
  htmlDownload3=gsub("\\\\","",htmlDownload3)
  htmlDownload3=gsub("\"","",htmlDownload3)
  
  Link=c(Link,htmlDownload3)
  NameWave=gsub(htmlDir,WavDir,htmlF[i])
  NameWave=gsub(".html",".wav",NameWave)
  
  test=try(download.file(htmlDownload3,destfile=NameWave))
  if(test!=0){ErrorD=c(ErrorD,htmlDownload3)}
  hLicense=subset(htmlData
                  ,grepl("https://sonotheque.mnhn.fr/img/license/"
                         ,htmlData$V1))
  hLicense2=tstrsplit(hLicense$V1[1],split="https://sonotheque.mnhn.fr/img/license/")[[2]]  
  hLicense3=tstrsplit(hLicense2,split=".png")[[1]]
  License=c(License,hLicense3)
  
  Author1=subset(htmlData,grepl("authors",htmlData$V1))
  Author1=subset(Author1,grepl("class=",Author1$V1))
  Author2=htmlData$V1[as.numeric(row.names(Author1))+3]
  Author3=gsub("\t","",Author2)
  Author=c(Author,Author3)
  
  
  Dur1=subset(htmlData,grepl("Duration:",htmlData$V1))
  Dur2=tstrsplit(Dur1,split="Duration: ")[[2]]            
  Dur3=gsub("</li>","",Dur2)
  Duration=c(Duration,Dur3)
  
  Format1=subset(htmlData,grepl("div class=",htmlData$V1))
  Format1=subset(Format1,grepl("value",Format1$V1))
  FormatRows=as.numeric(row.names(Format1))
  FormatRows2=subset(FormatRows,FormatRows<as.numeric(row.names(Dur1[1])))
  FormatRange=c((FormatRows2[length(FormatRows2)]+1):
                   (as.numeric(row.names(Dur1[1]))-1))
  Format2=htmlData[FormatRange,]
  Format2=gsub("\t","",Format2)
  Format2=gsub("<li>","",Format2)
  Format2=gsub("<ul>","",Format2)
  Format2=gsub("</li>","",Format2)
  Format3=subset(Format2,Format2!="")
  Format4=Format3[1]
  if(length(Format3)>1){
    for (j in 2:length(Format3))
    {
      Format4=paste0(Format4,Format3[j])
    }
  }
  Format=c(Format,Format4)
  
  SR1=subset(htmlData,grepl("Fichier son: ",htmlData$V1))
  SR2=tstrsplit(SR1$V1,split="Fichier son: ")[[2]]
  SR3=gsub("</li>","",SR2)
  SampleRate=c(SampleRate,SR3)
  
  Species1=subset(htmlData,grepl("taxonomy",htmlData$V1))
  Species1=subset(Species1,grepl("class=",Species1$V1))
    if(nrow(Species1)==0){
   SpeciesM1=subset(htmlData,grepl("h3>Liste des esp",htmlData$V1))
   SpeciesM2=subset(htmlData,grepl("<h3>Recording</h3>",htmlData$V1))
     SpeciesRange=c(as.numeric(row.names(SpeciesM1)[1]):as.numeric(row.names(SpeciesM2)[1]))
     SpeciesData=htmlData[SpeciesRange,]
     SpeciesData2=subset(SpeciesData,grepl("<li>",SpeciesData))
     SpeciesData2=gsub("\t","",SpeciesData2)
     SpeciesData2=gsub("</li>","",SpeciesData2)
     SpeciesData2=gsub("<li>","",SpeciesData2)
     
          SpeciesData3=SpeciesData2[1]
     if(length(SpeciesData2)>1){
       for (j in 2:length(SpeciesData2))
       {
         SpeciesData3=paste0(SpeciesData3,", ",SpeciesData2[j])
       }
     }
          Species=c(Species,SpeciesData3)    
  }else{
  
  Species2=tstrsplit(Species1,split="class=\"treeview-child\"><li><span>")
  Species3=Species2[[length(Species2)]]
  Species4=tstrsplit(Species3,split="</span>")[[1]]
Species=c(Species,Species4)
  }

Date1=subset(htmlData,grepl("Date of recording",htmlData$V1))
Date2=htmlData[as.numeric(row.names(Date1))+2,]
Date3=gsub("\t","",Date2)
Date=c(Date,Date3)

Location1=subset(htmlData,grepl("class=\"value\" id=\"location",htmlData$V1))
Location2=tstrsplit(Location1,split="class=\"treeview-child\"><li><span>")
Location3=""
if(length(Location2)>1){
for (k in length(Location2):2)
{
  Locak=tstrsplit(Location2[[k]],split="</span>")[[1]]
  Location3=paste0(Location3,Locak,", ")
}
}
Location4=tstrsplit(Location2[[1]],split="class=\"treeview\"><li><span>")[[2]]
Location5=gsub("</span><ul ","",Location4)
Location3=paste0(Location3,Location5)
Location=c(Location,Location3)

Type1=subset(htmlData,grepl("<span class=\"subtitle capitalized",htmlData$V1))
Type2=htmlData[as.numeric(row.names(Type1)[1])+3,]
if(gsub("\t","",Type2)==""){
  Type2=htmlData[as.numeric(row.names(Type1)[1])+2,]
}
Type3=gsub("\t","",Type2)
Type=c(Type,Type3)
  }

Table=data.frame(File=gsub(".html","",basename(htmlF)),Species,Type,Location,Date,License,Author,Duration,Format
                 ,SampleRate)
fwrite(Table,"TableSonoMNHN.csv",sep=";")
