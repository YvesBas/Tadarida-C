library(warbleR)
library(data.table)
#setInternet2(TRUE)
listSp=c("Streptopelia decaocto","Apus apus","Delichon urbicum"
         ,"Hirundo rustica","Phoenicurus ochruros"
         ,"Phoenicurus phoenicurus","Coloeus monedula","Pica pica"
         ,"Carduelis carduelis","Serinus serinus","Passer domesticus"
         ,"Passer montanus")
listSp=c("Parus major"
         ,"Fringilla coelebs"
         ,"Corvus corone"
         ,"Columba palumbus"
         ,"Phylloscopus collybita"
         ,"Cyanistes caeruleus"
         ,"Erithacus rubecula"
         ,"Troglodytes troglodytes"
         ,"Sturnus vulgaris"
         ,"Turdus philomelos"
         ,"Cuculus canorus"
         ,"Picus viridis"
         ,"Garrulus glandarius"
         ,"Dendrocopos major"
         ,"Motacilla alba"
         ,"Sylvia communis"
         ,"Buteo buteo"
         ,"Alauda arvensis"
         ,"Certhia brachydactyla"
         ,"Streptopelia turtur"
         ,"Luscinia megarhynchos"
         ,"Emberiza citrinella"
         ,"Prunella modularis"
         ,"Phasianus colchicus"
         ,"Saxicola rubicola"
         ,"Falco tinnunculus"
         ,"Hippolais polyglotta"
         ,"Sitta europaea"
         ,"Linaria cannabina"
         ,"Aegithalos caudatus"
         ,"Turdus viscivorus"
         ,"Oriolus oriolus"
         ,"Sylvia borin"
         ,"Emberiza cirlus"
         ,"Anthus trivialis"
         ,"Emberiza calandra"
         ,"Anas platyrhynchos"
         ,"Lanius collurio"
         ,"Lullula arborea"
         ,"Poecile palustris"
         ,"Regulus ignicapilla"
         ,"Upupa epops"
          ,"Ardea cinerea"
         ,"Coturnix coturnix"
         ,"Gallinula chloropus"
         ,"Corvus frugilegus"
         ,"Columba livia"
         ,"Dryocopus martius"         
,"Regulus regulus"
         ,"Motacilla flava"
         ,"Alectoris rufa"
         ,"Milvus migrans"
         ,"Dryobates minor"
         ,"Pyrrhula pyrrhula"
         ,"Perdix perdix"
         ,"Lophophanes cristatus"
         ,"Phylloscopus trochilus"
         ,"Muscicapa striata"
         ,"Periparus ater"
         ,"Jynx torquilla"
         ,"Phylloscopus bonelli"
         ,"Coccothraustes coccothraustes"
         ,"Locustella naevia"
         ,"Accipiter nisus"
         ,"Columba oenas"
         ,"Phylloscopus sibilatrix"
         ,"Motacilla cinerea"
         ,"Sylvia curruca"
         ,"Acrocephalus scirpaceus"
         ,"Vanellus vanellus"
         ,"Fulica atra"
         ,"Cettia cetti"
         ,"Circus cyaneus"
         ,"Strix aluco"
         ,"Burhinus oedicnemus"
         ,"Cisticola juncidis"
         ,"Dendrocoptes medius"
         ,"Poecile montanus"
         ,"Emberiza schoeniclus"
         ,"Certhia familiaris"
         ,"Alcedo atthis"
         ,"Corvus corax"
         ,"Chroicocephalus ridibundus"
         ,"Turdus pilaris"
         ,"Anthus pratensis"
         ,"Athene noctua"
         ,"Falco subbuteo"
         ,"Tachybaptus ruficollis"
         ,"Saxicola rubetra"
         ,"Circus pygargus"
         ,"Phalacrocorax carbo"
         ,"Larus argentatus"
         ,"Podiceps cristatus"
         ,"Circus aeruginosus"
         ,"Cygnus olor"
         ,"Acrocephalus palustris"
         ,"Egretta garzetta"
         ,"Galerida cristata"
         ,"Sylvia melanocephala"
         ,"Milvus milvus"
         ,"Sylvia cantillans"
         ,"Ficedula hypoleuca"
         ,"Acrocephalus schoenobaenus"
         ,"Oenanthe oenanthe"
         ,"Actitis hypoleucos"
         ,"Bubulcus ibis"
         ,"Larus fuscus"
         ,"Larus michahellis"
         ,"Pernis apivorus"
         ,"Lanius senator"
         ,"Merops apiaster"
         ,"Tadorna tadorna"
         ,"Numenius arquata"
         ,"Loxia curvirostra"
         ,"Emberiza hortulana"
         ,"Turdus torquatus"
         ,"Anthus campestris"
         ,"Rallus aquaticus"
         ,"Luscinia svecica"
         ,"Ardea alba"
         ,"Charadrius dubius"
         ,"Spinus spinus"
         ,"Ciconia ciconia"
         ,"Emberiza cia"
         ,"Tringa ochropus"
         ,"Cinclus cinclus"
         ,"Picus canus"
         ,"Circaetus gallicus"
         ,"Asio otus"
         ,"Sterna hirundo"
         ,"Petronia petronia"
         ,"Sylvia undata"
         ,"Himantopus himantopus"
         ,"Acrocephalus arundinaceus"
         ,"Clamator glandarius"
         ,"Nycticorax nycticorax"
         ,"Falco peregrinus"
         ,"Numenius phaeopus"
         ,"Hippolais icterina"
         ,"Tringa totanus"
         ,"Sylvia hortensis"
         ,"Ardea purpurea"
         ,"Coracias garrulus"
         ,"Riparia riparia"
         ,"Accipiter gentilis"
         ,"Spatula clypeata"
         ,"Ichthyaetus melanocephalus"
         ,"Tetrax tetrax"
         ,"Branta canadensis"
         ,"Lanius excubitor"
         ,"Aythya fuligula"
         ,"Aythya ferina"
         ,"Ptyonoprogne rupestris"
         ,"Otus scops"
         ,"Tringa nebularia"
         ,"Anthus spinoletta"
         ,"Fringilla montifringilla"
         ,"Turdus iliacus"
         ,"Larus marinus"
         ,"Nucifraga caryocatactes"
         ,"Lanius meridionalis"
         ,"Anas strepera"
         ,"Alopochen aegyptiaca"
         ,"Sylvia conspicillata"
         ,"Recurvirostra avosetta"
         ,"Caprimulgus europaeus"
         ,"Serinus citrinella"
         ,"Netta rufina"
         ,"Acanthis flammea"
         ,"Gallinago gallinago"
         ,"Anas querquedula"
         ,"Falco vespertinus"
         ,"Monticola saxatilis")

Nfiles=40
Nfiles="all"
output="I:/"


if(exists("MDXC")){rm(MDXC)}
i=1
setwd(output)

for (i in 1:length(listSp))
{
  test=querxc(listSp[i])
  if(exists("MDXC")){MDXC=rbindlist(list(MDXC,test),use.names=T,fill=T)
  }else{MDXC=test}
  
  print(paste(listSp[i],nrow(test)))
  Sys.time()
  if(Nfiles!="all")
  {
    querxc(X=test[sample(1:nrow(test),Nfiles),])
    
    Sys.time()
    testS=(grepl("song",test$Vocalization_type)|
             grepl("Song",test$Vocalization_type))
    testC=subset(test,testS==F)
    querxc(X=testC[sample(1:nrow(testC),Nfiles),])
  }else{
    init=T
    while(init)
    {
      TryDownload=try(querxc(X=test))
      if(class(TryDownload)!="try-error"){init=F}
      if(init){Sys.sleep(300)}
    }
  }
  
  print(Sys.time())
  print(paste(i,length(listSp)))
}
write.csv2(MDXC,paste0(output,"/MDXC_",substr(Sys.time(),1,10),".csv"),row.names=F)


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
SelS=sample(ListS,Nfiles)
file.copy(from=SelS,to=SelDir)
setwd("C:/Users/Yves Bas/Documents")
