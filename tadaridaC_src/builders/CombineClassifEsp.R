
library(randomForest)
library(beepr)
setwd("D:/RSDB_HF")

Name_Date = "2020-08-18"
Name_Learner = paste("ClassifEsp",Name_Date, sep="")
Ftabase="_tabase3HF_sansfiltre.csv"

#Load
ls1 <- list.files("D:/RSDB_HF",include.dirs = FALSE, recursive=FALSE,
                  pattern=paste("^ClassifEsp", Name_Date, sep=""))
ld <- lapply(ls1, function(x) get(load(x)))

#Combine
ClassifEspA <- do.call("combine", ld)


#Save
save (ClassifEspA,file=paste0("ClassifEsp_",gsub(".csv","",Ftabase),"_"
                              ,Name_Date,".learner")) 
beep(2)