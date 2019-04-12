library(data.table)
library(MASS)
setwd("C:/Users/Yves Bas/Documents")
args="tabase3_LFXC"


#pour afficher les milisecondes
op <- options(digits.secs=3)

tabase3XC=fread(paste0(args,".csv"))

#tabase3XC=tabase3XC2
#tabase3XC=tabase3XC2[sample(c(1:nrow(tabase3XC2)),100000),]
#table(tabase3XC$Nesp)

Sys.time()
tabaseR=apply(tabase3XC[,4:274],MARGIN=2,FUN=function(x) frank(x,ties.method="min")) # 2 min
Sys.time()

Sys.time()
Reduc1=lda(x=tabaseR,grouping=tabase3XC$Nesp) # 0.0003 sec/ données / 2 heures pour 11 espèces
saveRDS(Reduc1,file=paste0(args,"_Reduc1.rds"))
#Reduc1=lda(x=tabaseR,grouping=tabase3XC$Nesp,tol=1.0e-6) # 0.0003 sec/ données / 2 heures pour 11 espèces
Sys.time()
Reduc2=predict(Reduc1,tabaseR) # 3 min
Sys.time()

tabase3XCsup=as.data.frame(tabase3XC)[,(4:274)]

tabase3XC=cbind(tabase3XC[,1:3],tabase3XCsup
                ,as.data.frame(Reduc2$x),tabase3XC[,275:ncol(tabase3XC)])

test=as.vector(tabase3XC[,4])
test=test[order(test[,1]),]
cuts=c(-Inf,as.vector(test [[1]]),Inf)
test2=findInterval(as.vector(tabase3XC[,4])[[1]],cuts,left.open=T)
plot(test2,tabaseR[,1])

fwrite(tabase3XC,paste0(args,"_Reduc.csv"),row.names=F)
names(Reduc1)
fwrite(as.data.table(cbind(row.names(Reduc1$scaling),Reduc1$scaling))
       ,paste0(args,"_ReducXCscaling.csv"))
