library(data.table)

idtest=fread("RSDBsel_2021-02-16test.csv")
results=fread("ProbDetail2021-02-16.csv")

idSimple=subset(idtest,select=c("Filename","CallNum","Espece"))

IdConc=merge(idSimple,results)
IdConc$IdMan=IdConc$Espece

fwrite(IdConc,paste0("ProbIdConc_",Sys.Date(),".csv"),sep=";")
