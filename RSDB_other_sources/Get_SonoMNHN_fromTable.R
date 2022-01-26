library(readxl)
library(rdownload)

OutDir="C:/Users/yvesb/Downloads/SonoMNHN/"

TabSmnhn=read_xlsx("C:/Users/yvesb/Downloads/export_sonotheque(2) (1).xlsx")

dir.create(OutDir)

for (i in 2:nrow(TabSmnhn))
{
  if(i%%100==1){print(paste(i,Sys.time()))}
  NameFile=paste0(OutDir,"/",TabSmnhn$institutionCode[i],"-"
                  ,TabSmnhn$collectionCode[i]
                  ,"-",TabSmnhn$catalogNumber[i],".wav")
  
download.file(TabSmnhn$download[i],NameFile, quiet = TRUE, mode = "wb")
}
