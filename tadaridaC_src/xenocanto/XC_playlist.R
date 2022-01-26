library(warbleR)
library(data.table)
Nfiles=2000
#Nfiles="all"
output="./XCpl"


wd0=getwd()
setwd(output)


  test=querxc('cnt:"=France"')
  test=subset(test,test$Latitude>30)
  test=subset(test,test$Longitude>-10)
  test=subset(test,test$Longitude<15)
  
  NbPerSp=aggregate(test$Recording_ID,by=list(test$English_name),length)
  NbPerSp$Weights=1/NbPerSp$x
  test2=merge(test,NbPerSp,by.x="English_name",by.y="Group.1")
  
  SamplePL=test2[sample(1:nrow(test2),round(Nfiles/2),prob=test2$Weights),]
  
  testS=(grepl("song",test$Vocalization_type)|
           grepl("Song",test$Vocalization_type)|grepl("Drum",test$Vocalization_type)
         |grepl("Drum",test$Vocalization_type))
  summary(testS)
  testC=subset(test,testS==F)
  
  
  NbPerSp=aggregate(testC$Recording_ID,by=list(testC$English_name),length)
  NbPerSp$Weights=1/NbPerSp$x
  hist(NbPerSp$Weights)
  test2=merge(testC,NbPerSp,by.x="English_name",by.y="Group.1")
  
  SamplePL2=test2[sample(1:nrow(test2),round(Nfiles/2),prob=test2$Weights),]
  
  table(SamplePL2$English_name)
  querxc(X=SamplePL)
  querxc(X=SamplePL2)  
  setwd(wd0)
