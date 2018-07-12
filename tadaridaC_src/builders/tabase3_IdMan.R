library(data.table)

args="tabase3HF_France.csv"

tabase3=fread(args[1])

IdMan=aggregate(tabase3$Indice,by=c(list(tabase3$Filename),list(tabase3$Espece)),FUN=max)

fwrite(IdMan,paste0(substr(args,1,nchar(args)-4),"_IdMan.csv"))
