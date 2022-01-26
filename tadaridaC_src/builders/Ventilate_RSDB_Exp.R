library(data.table)


Cut=F
ExpData=fread("C:/Users/yvesb/Documents/RSDB_HF_ExpData.csv")
RSDB="C:/Users/yvesb/Documents/RSDB_HF"

LW=list.files(RSDB,pattern=".wav$",recursive=T,full.names = T)

Exp1=subset(ExpData,ExpData$EF==1)

test1=match(basename(LW),basename(Exp1$File))

LW1=subset(LW,!is.na(test1))
LWremain=subset(LW,is.na(test1))

Exp10=subset(ExpData,ExpData$EF==10)

test10=match(basename(LWremain),basename(Exp10$File))
LW10=subset(LWremain,!is.na(test10))

dir.create(paste0(RSDB,"/ex1"))

test=file.copy(from=LW1,to=paste0(RSDB,"/ex1/",basename(LW1)))

if(Cut&min(test)==1){
  file.remove(LW1)
}

dir.create(paste0(RSDB,"/ex10"))

test=file.copy(from=LW10,to=paste0(RSDB,"/ex10/",basename(LW10)))

if(Cut&min(test)==1){
  file.remove(LW10)
}
