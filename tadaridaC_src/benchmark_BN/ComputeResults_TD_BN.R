library(data.table)

DirWav="C:/Users/yvesb/Documents/www/wav_sample_crepurb25e"
ResTD=fread("C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/pointdu28-10-2021/documents joints/predictions crepurb/predictions_7680_41_7250_jeutest2200_1perfile.txt")
ResBN=fread("C:/Users/yvesb/Documents/Tadarida/Dbas_deepL/resBN_crepurb25p_data_1perfile.csv")

  
LW=list.files(DirWav)

test1=match(LW,paste0(ResTD$file,".wav"))
test2=match(LW,ResBN$file)


ResultTD=ResTD$results[test1]
ResultBN=ResBN$results[test2]

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

