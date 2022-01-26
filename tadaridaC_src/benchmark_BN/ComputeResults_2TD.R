library(data.table)

DirWav="C:/Users/yvesb/Documents/www/wav_sample_TadariDeep220121"
ResTD=fread("C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point du 21 janvier 2022/a/predictions_11760_96_11820_jeutest10183_1perfile.txt")
ResBN=fread("C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/point du 21 janvier 2022/a/predictions_11765_96_11820_jeutest10183_1perfile.txt")
SelW=F


if(SelW){  
LW=list.files(DirWav)

test1=match(LW,paste0(ResTD$file,".wav"))
test2=match(LW,ResBN$file)


ResultTD=ResTD$results[test1]
ResultBN=ResBN$results[test2]

}else{
  if(nrow(ResTD)==nrow(ResBN)){
    LW=ResTD$file
  ResultTD=ResTD$results
  ResultBN=ResBN$results
  
  }else{
    stop("different number of rows from the 2 tables")
  }
}

MSTD=as.numeric(tstrsplit(ResultTD,split=" ")[[2]])
hist(MSTD)
MSBN1=as.numeric(tstrsplit(ResultBN,split=" ")[[2]])
MSBN2=as.numeric(tstrsplit(ResultBN,split=" ")[[3]])
MSBN=mapply(function(x,y) ifelse(is.na(y),x,y),MSBN1,MSBN2)
hist(MSBN)

FreqTD=as.numeric(gsub("F","",tstrsplit(ResultTD,split=" ")[[3]]))
hist(FreqTD)


TableRes=data.table(LW,ResultTD,MSTD,ResultBN,MSBN,FreqTD)

fwrite(TableRes,paste0(DirWav,"TR.csv"),sep=";")
print(paste0(DirWav,"TR.csv"))

