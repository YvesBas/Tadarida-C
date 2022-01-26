library(data.table)
library(raster)

find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] >= x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}



ColInd="Filename"
FeaturesAll=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/FeaturesAll.csv")
SpeciesList=fread("C:/Users/yvesb/Documents/SpeciesList.csv")
tabase=fread("C:/Users/yvesb/Documents/vrac_md_dell2021/RSDB_HF_tabase3HF_sansfiltre.csv")
Communes=shapefile("C:/Users/yvesb/Documents/SIG/Limite_administrative/Communes_L93_centroides.shp")

CommunesWGS84=spTransform(Communes,CRS("+init=epsg:4326"))


ListSp=unique(FeaturesAll$Espece)

Ncalls=vector()
Fc_mean=vector()
Fc_sd=vector()
Fmin_mean=vector()
Fmin_sd=vector()
Fmax_mean=vector()
Fmax_sd=vector()
Fme_mean=vector()
Fme_sd=vector()
Slope_mean=vector()
Slope_sd=vector()
Dur_mean=vector()
Dur_sd=vector()
IPI_mean=vector()
IPI_sd=vector()
for (i in 1:length(ListSp))
{
  print(ListSp[i])
  DataSp=subset(FeaturesAll,FeaturesAll$Espece==ListSp[i])
  #DataNonEcho=subset(DataSp,DataSp$PrevMP2>20)
  
  Fc=ifelse(is.na(DataSp$fc_T_2048)
            ,ifelse(is.na(DataSp$fc_T_1024)
                    ,ifelse(is.na(DataSp$fc_T_512)
                            ,ifelse(is.na(DataSp$fc_T_256)
                                    ,ifelse(DataSp$CM_FIF==0,DataSp$FreqMP*1000
                                            ,DataSp$CM_FIF*1000)
                                    ,DataSp$fc_T_256),DataSp$fc_T_512),DataSp$fc_T_1024)
            ,DataSp$fc_T_2048)
  
  
  
  if(length(Fc)>1){
    plot(density(Fc),main=ListSp[i])
    modesFC=density(Fc)$x[find_modes(density(Fc)$y)]
    mainmode=density(Fc)$x[which.max(density(Fc)$y)]
    mainmode2=which(modesFC==mainmode)
    
    if(mainmode2>1)
    {
      ModeInf=Fc>(modesFC[mainmode2-1]+modesFC[mainmode2])/2
    }else{
      ModeInf=rep(T,nrow(DataSp))
    }
    
    
    if(mainmode2<length(modesFC))
    {
      ModeSup=Fc<(modesFC[mainmode2+1]+modesFC[mainmode2])/2
    }else{
      ModeSup=rep(T,nrow(DataSp))
    }
    DataMainMode=subset(DataSp,ModeInf&ModeSup)
    Fc=subset(Fc,ModeInf&ModeSup)
  }
  
  Ncalls=c(Ncalls,max(nrow(DataMainMode),1))
  
  Fc_mean=c(Fc_mean,mean(Fc,na.rm=T))
  #Fc_median=c(Fc_median,median(Fc,na.rm=T))
  Fc_sd=c(Fc_sd,sd(Fc,na.rm=T))
  
  Slope=ifelse(is.na(DataMainMode$slope_T_256)
               ,ifelse(is.na(DataMainMode$slope_T_512)
                       ,ifelse(is.na(DataMainMode$slope_T_1024)
                               ,ifelse(is.na(DataMainMode$slope_T_2048)
                                       ,ifelse(DataMainMode$CO2_Slope==9999,NA,DataMainMode$CO2_Slope*1000)
                                       ,DataMainMode$slope_T_2048),DataMainMode$slope_T_1024)
                       ,DataMainMode$slope_T_512)
               ,DataMainMode$slope_T_256)
  
  
  Slope_mean=c(Slope_mean,mean(Slope,na.rm=T))
  #Slope_median=c(Slope_median,median(Slope,na.rm=T))
  Slope_sd=c(Slope_sd,sd(Slope,na.rm=T))
  
  Fmin=ifelse(is.na(DataMainMode$freq_min_T_2048)
              ,ifelse(is.na(DataMainMode$freq_min_T_1024)
                      ,ifelse(is.na(DataMainMode$freq_min_T_512)
                              ,ifelse(is.na(DataMainMode$freq_min_T_256)
                                      ,ifelse(DataMainMode$CM_Fmin==0,NA
                                              ,DataMainMode$CM_Fmin*1000)
                                      ,DataMainMode$freq_min_T_256),DataMainMode$freq_min_T_512),DataMainMode$freq_min_T_1024)
              ,DataMainMode$freq_min_T_2048)
  
  Fmax=ifelse(is.na(DataMainMode$freq_max_T_2048)
              ,ifelse(is.na(DataMainMode$freq_max_T_1024)
                      ,ifelse(is.na(DataMainMode$freq_max_T_512)
                              ,ifelse(is.na(DataMainMode$freq_max_T_256)
                                      ,ifelse(DataMainMode$CM_Fmax==0,NA
                                              ,DataMainMode$CM_Fmax*1000)
                                      ,DataMainMode$freq_max_T_256),DataMainMode$freq_max_T_512),DataMainMode$freq_max_T_1024)
              ,DataMainMode$freq_max_T_2048)
  
  
  Fme=ifelse(is.na(DataMainMode$freq_max_amp_T_2048)
             ,ifelse(is.na(DataMainMode$freq_max_amp_T_1024)
                     ,ifelse(is.na(DataMainMode$freq_max_amp_T_512)
                             ,ifelse(is.na(DataMainMode$freq_max_amp_T_256)
                                     ,DataMainMode$FreqMP*1000
                                     ,DataMainMode$freq_max_amp_T_256),DataMainMode$freq_max_amp_T_512),DataMainMode$freq_max_amp_T_1024)
             ,DataMainMode$freq_max_amp_T_2048)
  
  Fmin_mean=c(Fmin_mean,mean(Fmin,na.rm=T))
  Fmin_sd=c(Fmin_sd,sd(Fmin,na.rm=T))
  Fmax_mean=c(Fmax_mean,mean(Fmax,na.rm=T))
  Fmax_sd=c(Fmax_sd,sd(Fmax,na.rm=T))
  Fme_mean=c(Fme_mean,mean(Fme,na.rm=T))
  Fme_sd=c(Fme_sd,sd(Fme,na.rm=T))
  
  
  Dur=ifelse(is.na(DataMainMode$duration_T_256),ifelse(is.na(DataMainMode$duration_B_256)
                                                       ,ifelse(is.na(DataMainMode$duration_T_512)
                                                               ,ifelse(is.na(DataMainMode$duration_B_512)
                                                                       ,ifelse(is.na(DataMainMode$duration_T_1024)
                                                                               ,ifelse(is.na(DataMainMode$duration_B_1024)
                                                                                       ,ifelse(is.na(DataMainMode$duration_T_2048)
                                                                                               ,ifelse(is.na(DataMainMode$duration_B_2048)
                                                                                                       ,DataMainMode$CO2_Dur,DataMainMode$duration_B_2048)
                                                                                               ,DataMainMode$duration_T_2048)
                                                                                       ,DataMainMode$duration_B_1024)
                                                                               ,DataMainMode$duration_T_1024)
                                                                       ,DataMainMode$duration_B_512)
                                                               ,DataMainMode$duration_T_512)
                                                       ,DataMainMode$duration_B_256)
             ,DataMainMode$duration_T_256)
  
  Dur_mean=c(Dur_mean,mean(Dur,na.rm=T))
  Dur_sd=c(Dur_sd,sd(Dur,na.rm=T))
  
  
  DataNonEcho=subset(DataMainMode,DataMainMode$PrevMP2>20)
  
  if(nrow(DataNonEcho)>1){
    plot(density(DataNonEcho$PrevMP2,n=2048),xlim=c(0,400),main=ListSp[i])
    modesIPI=density(DataNonEcho$PrevMP2,n=2048)$x[find_modes(density(DataNonEcho$PrevMP2,n=2048)$y)]
    #discard echoes
    EchoProblem1=(length(modesIPI)>1)
    EchoProblem2=(modesIPI[2]>3*modesIPI[1])
    test=match(modesIPI,density(DataNonEcho$PrevMP2,n=2048)$x)
    EchoProblem3=(density(DataNonEcho$PrevMP2,n=2048)$y[test[2]]>density(DataNonEcho$PrevMP2,n=2048)$y[test[1]]*0.5)
    if(EchoProblem1*EchoProblem2*EchoProblem3==1){ #meanse modesIPI[1] is echo-biased
        DataSave=DataNonEcho
        DataNonEcho=subset(DataNonEcho,DataNonEcho$PrevMP2>mean(c(modesIPI[1],modesIPI[2])))
        if(nrow(DataNonEcho)>1){
          plot(density(DataNonEcho$PrevMP2,n=2048),xlim=c(0,400),main=ListSp[i])
          modesIPI=density(DataNonEcho$PrevMP2,n=2048)$x[find_modes(density(DataNonEcho$PrevMP2,n=2048)$y)]
        }else{
          DataNonEcho=DataSave
        }
      }
    
    mainmode=density(DataNonEcho$PrevMP2,n=2048)$x[which.max(density(DataNonEcho$PrevMP2,n=2048)$y)]
    mainmode2=which(modesIPI==mainmode)
    
    if(mainmode2>1)
    {
      ModeInf=DataNonEcho$PrevMP2>(modesIPI[mainmode2-1]+modesIPI[mainmode2])/2
    }else{
      ModeInf=rep(T,nrow(DataNonEcho))
    }
    
    
    if(mainmode2<length(modesIPI))
    {
      ModeSup=DataNonEcho$PrevMP2<(modesIPI[mainmode2+1]+modesIPI[mainmode2])/2
    }else{
      ModeSup=rep(T,nrow(DataNonEcho))
    }
    DataMainModeIPI=subset(DataNonEcho,ModeInf&ModeSup)
  }else{
    DataMainModeIPI=DataNonEcho
    
  }
  IPI_mean=c(IPI_mean,mean(DataMainModeIPI$PrevMP2,na.rm=T))
  IPI_sd=c(IPI_sd,sd(DataMainModeIPI$PrevMP2,na.rm=T))
}

TableSp=data.frame(Species=ListSp,Ncalls,
                   Fc_mean,
                   Fc_sd,
                   Fmin_mean,
                   Fmin_sd,
                   Fmax_mean,
                   Fmax_sd,
                   Fme_mean,
                   Fme_sd,
                   Slope_mean,
                   Slope_sd,
                   Dur_mean,
                   Dur_sd,
                   IPI_mean,
                   IPI_sd)

BatList=subset(SpeciesList,SpeciesList$Group=="bat")
SpeciesShort=subset(BatList,select=c("Esp","Scientific name"))
TableSp=merge(TableSp,SpeciesShort,by.x="Species",by.y="Esp")

TableSp=TableSp[order(TableSp$Species),]


DataInd=subset(FeaturesAll,select=ColInd)
names(DataInd)="Ind"
listInd=unique(DataInd$Ind)
listInd=subset(listInd,listInd!="")


Species=vector()
Site=vector()
Ncalls=vector()
Fc_mean=vector()
Fc_sd=vector()
Fmin_mean=vector()
Fmin_sd=vector()
Fmax_mean=vector()
Fmax_sd=vector()
Fme_mean=vector()
Fme_sd=vector()
Slope_mean=vector()
Slope_sd=vector()
Dur_mean=vector()
Dur_sd=vector()
IPI_mean=vector()
IPI_sd=vector()
for (i in 1:length(listInd))
{
  DataSp=subset(FeaturesAll,DataInd$Ind==listInd[i])
  Species=c(Species,DataSp$Espece[1])
  Site=c(Site,DataSp$Site[1])
  
  print(paste(listInd[i],DataSp$Espece[1]))
  
  DataNonEcho=subset(DataSp,DataSp$PrevMP2>20)
  
  Fc=ifelse(is.na(DataSp$fc_T_2048)
            ,ifelse(is.na(DataSp$fc_T_1024)
                    ,ifelse(is.na(DataSp$fc_T_512)
                            ,ifelse(is.na(DataSp$fc_T_256)
                                    ,ifelse(DataSp$CM_FIF==0,DataSp$FreqMP*1000
                                            ,DataSp$CM_FIF*1000)
                                    ,DataSp$fc_T_256),DataSp$fc_T_512),DataSp$fc_T_1024)
            ,DataSp$fc_T_2048)
  
  
  if(length(Fc)>1)
  {
    plot(density(Fc),main=ListSp[i])
    
    if(find_modes(density(Fc)$y)[1]!="This is a monotonic distribution")
    {
      modesFC=density(Fc)$x[find_modes(density(Fc)$y)]
      mainmode=density(Fc)$x[which.max(density(Fc)$y)]
      mainmode2=which(modesFC==mainmode)
      
      if(mainmode2>1)
      {
        ModeInf=Fc>(modesFC[mainmode2-1]+modesFC[mainmode2])/2
      }else{
        ModeInf=rep(T,nrow(DataSp))
      }
      
      
      if(mainmode2<length(modesFC))
      {
        ModeSup=Fc<(modesFC[mainmode2+1]+modesFC[mainmode2])/2
      }else{
        ModeSup=rep(T,nrow(DataSp))
      }
      DataMainMode=subset(DataSp,ModeInf&ModeSup)
    }else{
      DataMainMode=DataSp
    }
  }else{
    DataMainMode=DataSp
  }
  Fc=subset(Fc,ModeInf&ModeSup)
  
  Ncalls=c(Ncalls,max(nrow(DataMainMode),1))
  
  Fc_mean=c(Fc_mean,mean(Fc,na.rm=T))
  #Fc_median=c(Fc_median,median(Fc,na.rm=T))
  Fc_sd=c(Fc_sd,sd(Fc,na.rm=T))
  
  Slope=ifelse(is.na(DataMainMode$slope_T_256)
               ,ifelse(is.na(DataMainMode$slope_T_512)
                       ,ifelse(is.na(DataMainMode$slope_T_1024)
                               ,ifelse(is.na(DataMainMode$slope_T_2048)
                                       ,ifelse(DataMainMode$CO2_Slope==9999,NA,DataMainMode$CO2_Slope*1000)
                                       ,DataMainMode$slope_T_2048),DataMainMode$slope_T_1024)
                       ,DataMainMode$slope_T_512)
               ,DataMainMode$slope_T_256)
  
  
  Slope_mean=c(Slope_mean,mean(Slope,na.rm=T))
  #Slope_median=c(Slope_median,median(Slope,na.rm=T))
  Slope_sd=c(Slope_sd,sd(Slope,na.rm=T))
  
  Fmin=ifelse(is.na(DataMainMode$freq_min_T_2048)
              ,ifelse(is.na(DataMainMode$freq_min_T_1024)
                      ,ifelse(is.na(DataMainMode$freq_min_T_512)
                              ,ifelse(is.na(DataMainMode$freq_min_T_256)
                                      ,ifelse(DataMainMode$CM_Fmin==0,NA
                                              ,DataMainMode$CM_Fmin*1000)
                                      ,DataMainMode$freq_min_T_256),DataMainMode$freq_min_T_512),DataMainMode$freq_min_T_1024)
              ,DataMainMode$freq_min_T_2048)
  
  Fmax=ifelse(is.na(DataMainMode$freq_max_T_2048)
              ,ifelse(is.na(DataMainMode$freq_max_T_1024)
                      ,ifelse(is.na(DataMainMode$freq_max_T_512)
                              ,ifelse(is.na(DataMainMode$freq_max_T_256)
                                      ,ifelse(DataMainMode$CM_Fmax==0,NA
                                              ,DataMainMode$CM_Fmax*1000)
                                      ,DataMainMode$freq_max_T_256),DataMainMode$freq_max_T_512),DataMainMode$freq_max_T_1024)
              ,DataMainMode$freq_max_T_2048)
  
  
  Fme=ifelse(is.na(DataMainMode$freq_max_amp_T_2048)
             ,ifelse(is.na(DataMainMode$freq_max_amp_T_1024)
                     ,ifelse(is.na(DataMainMode$freq_max_amp_T_512)
                             ,ifelse(is.na(DataMainMode$freq_max_amp_T_256)
                                     ,DataMainMode$FreqMP*1000
                                     ,DataMainMode$freq_max_amp_T_256),DataMainMode$freq_max_amp_T_512),DataMainMode$freq_max_amp_T_1024)
             ,DataMainMode$freq_max_amp_T_2048)
  
  Fmin_mean=c(Fmin_mean,mean(Fmin,na.rm=T))
  Fmin_sd=c(Fmin_sd,sd(Fmin,na.rm=T))
  Fmax_mean=c(Fmax_mean,mean(Fmax,na.rm=T))
  Fmax_sd=c(Fmax_sd,sd(Fmax,na.rm=T))
  Fme_mean=c(Fme_mean,mean(Fme,na.rm=T))
  Fme_sd=c(Fme_sd,sd(Fme,na.rm=T))
  
  
  Dur=ifelse(is.na(DataMainMode$duration_T_256),ifelse(is.na(DataMainMode$duration_B_256)
                                                       ,ifelse(is.na(DataMainMode$duration_T_512)
                                                               ,ifelse(is.na(DataMainMode$duration_B_512)
                                                                       ,ifelse(is.na(DataMainMode$duration_T_1024)
                                                                               ,ifelse(is.na(DataMainMode$duration_B_1024)
                                                                                       ,ifelse(is.na(DataMainMode$duration_T_2048)
                                                                                               ,ifelse(is.na(DataMainMode$duration_B_2048)
                                                                                                       ,DataMainMode$CO2_Dur,DataMainMode$duration_B_2048)
                                                                                               ,DataMainMode$duration_T_2048)
                                                                                       ,DataMainMode$duration_B_1024)
                                                                               ,DataMainMode$duration_T_1024)
                                                                       ,DataMainMode$duration_B_512)
                                                               ,DataMainMode$duration_T_512)
                                                       ,DataMainMode$duration_B_256)
             ,DataMainMode$duration_T_256)
  
  Dur_mean=c(Dur_mean,mean(Dur,na.rm=T))
  Dur_sd=c(Dur_sd,sd(Dur,na.rm=T))
  DataNonEcho=subset(DataMainMode,DataMainMode$PrevMP2>20)
  
  if(nrow(DataNonEcho)>1){
    plot(density(DataNonEcho$PrevMP2,n=2048),xlim=c(0,400),main=ListSp[i])
    modesIPI=density(DataNonEcho$PrevMP2,n=2048)$x[find_modes(density(DataNonEcho$PrevMP2,n=2048)$y)]
    #discard echoes
    if(length(modesIPI)>1){
    EchoProblem2=(modesIPI[2]>3*modesIPI[1])
    test=match(modesIPI,density(DataNonEcho$PrevMP2,n=2048)$x)
    EchoProblem3=(density(DataNonEcho$PrevMP2,n=2048)$y[test[2]]>density(DataNonEcho$PrevMP2,n=2048)$y[test[1]]*0.5)
    if(EchoProblem2*EchoProblem3==1){ #meanse modesIPI[1] is echo-biased
      DataSave=DataNonEcho
      DataNonEcho=subset(DataNonEcho,DataNonEcho$PrevMP2>mean(c(modesIPI[1],modesIPI[2])))
      if(nrow(DataNonEcho)>1){
        plot(density(DataNonEcho$PrevMP2,n=2048),xlim=c(0,400),main=ListSp[i])
        modesIPI=density(DataNonEcho$PrevMP2,n=2048)$x[find_modes(density(DataNonEcho$PrevMP2,n=2048)$y)]
      }else{
        DataNonEcho=DataSave
      }
    }
    }
    mainmode=density(DataNonEcho$PrevMP2,n=2048)$x[which.max(density(DataNonEcho$PrevMP2,n=2048)$y)]
    mainmode2=which(modesIPI==mainmode)
    
    if(mainmode2>1)
    {
      ModeInf=DataNonEcho$PrevMP2>(modesIPI[mainmode2-1]+modesIPI[mainmode2])/2
    }else{
      ModeInf=rep(T,nrow(DataNonEcho))
    }
    
    
    if(mainmode2<length(modesIPI))
    {
      ModeSup=DataNonEcho$PrevMP2<(modesIPI[mainmode2+1]+modesIPI[mainmode2])/2
    }else{
      ModeSup=rep(T,nrow(DataNonEcho))
    }
    DataMainModeIPI=subset(DataNonEcho,ModeInf&ModeSup)
  }else{
    DataMainModeIPI=DataNonEcho
    
  }
  IPI_mean=c(IPI_mean,mean(DataMainModeIPI$PrevMP2,na.rm=T))
  IPI_sd=c(IPI_sd,sd(DataMainModeIPI$PrevMP2,na.rm=T))
}

TableInd=data.frame(Individual=listInd,Species,Site,Ncalls,
                    Fc_mean,
                    Fc_sd,
                    Fmin_mean,
                    Fmin_sd,
                    Fmax_mean,
                    Fmax_sd,
                    Fme_mean,
                    Fme_sd,
                    Slope_mean,
                    Slope_sd,
                    Dur_mean,
                    Dur_sd,
                    IPI_mean,
                    IPI_sd)


tabLabelled=subset(tabase,tabase$Site!="")

test=match(TableInd$Individual,tabLabelled$Filename)
TableInd$Zone=tabLabelled$Zone[test]
TableInd$Site=tabLabelled$Site[test]
TableInd$Recordist=tabLabelled$Auteur[test]
TableInd$Labeller=tabLabelled$Etiqueteur[test]
TableInd$Site=tabLabelled$Site[test]

DepCommune=paste(TableInd$Zone,TableInd$Site)
DepCommune2=paste(CommunesWGS84$DÃ.pARTEM0,CommunesWGS84$COMMUNE0)
ListZone=unique(TableInd$Zone)
ListDep=unique(CommunesWGS84$DÃ.pARTEM0)
test2=adist(ListZone,ListDep,ignore.case=T)

ListZD=vector()
for (i in 1:length(ListZone))
{
  test=which.min(test2[i,])
  ListZD[i]=ListDep[test]
}
CorrZ=data.frame(ListZone,ListZD)
testZ=match(TableInd$Zone,CorrZ$ListZone)
TableInd$Dep=CorrZ$ListZD[testZ]

CommunesT=vector()
CommunesSIG=vector()
Long_WGS84=vector()
Lat_WGS84=vector()
DepForC=vector()
for (i in 1:length(unique(TableInd$Dep)))
{
  print(unique(TableInd$Dep)[i])
  Sitei=subset(TableInd$Site,TableInd$Dep==unique(TableInd$Dep)[i])
  Commi=subset(CommunesWGS84,CommunesWGS84$DÃ.pARTEM0==unique(TableInd$Dep)[i])
  CommTi=unique(Sitei)
  test=adist(CommTi,Commi$COMMUNE0,ignore.case = T)
  
  ListCsig=vector()
  for (j in 1:length(CommTi))
  {
    test0=which.min(test[j,])
    ListCsig[j]=Commi$COMMUNE0[test0]
  }
  DepForC=c(DepForC,rep(as.character(unique(TableInd$Dep)[i]),length(CommTi)))
  CommunesT=c(CommunesT,CommTi)
  CommunesSIG=c(CommunesSIG,ListCsig)
  testC=match(ListCsig,Commi$COMMUNE0)
  CommSIGcorr=Commi[testC,]
  Long_WGS84=c(Long_WGS84,coordinates(CommSIGcorr)[,1])
  Lat_WGS84=c(Lat_WGS84,coordinates(CommSIGcorr)[,2])
  
}
CorrC=data.frame(DepForC,CommunesT,CommunesSIG,Long_WGS84,Lat_WGS84)

testC2=match(paste(TableInd$Dep,TableInd$Site)
             ,paste(CorrC$DepForC,CorrC$CommunesT))
TableInd$Commune=CorrC$CommunesSIG[testC2]
TableInd$Long_WGS84=CorrC$Long_WGS84[testC2]
TableInd$Lat_WGS84=CorrC$Lat_WGS84[testC2]

Abroad=(TableInd$Zone %in% c("Espagne","Angleterre","Surrey","Andorre"
                             ,"Turquie","Cumbria","Pologne","Lituanie"
                             ,"Lithuania","Limon"))

TableInd$Dep=ifelse(Abroad,NA,TableInd$Dep)
TableInd$Commune=ifelse(Abroad,NA,TableInd$Commune)
TableInd$Long_WGS84=ifelse(Abroad,NA,TableInd$Long_WGS84)
TableInd$Lat_WGS84=ifelse(Abroad,NA,TableInd$Lat_WGS84)

TableInd=TableInd[order(TableInd$Individual),]



#correction of aliasing for Hipposideros
for (a in 1:nrow(TableSp))
{
  if(substr(TableSp$Species[a],1,3)=="Hip"){
    AliasedProb=(TableSp$Fmax_mean[a]-TableSp$Fc_mean[a])/(125000-TableSp$Fc_mean[a])
    print(paste(TableSp$Individual[a],TableSp$Species[a],AliasedProb))
    if(AliasedProb>0.5){
      if(TableSp$Fmin_mean[a]>TableSp$Fc_mean[a]-3000)
      {
        TableSp$Fmin_mean[a]=250000-TableSp$Fmax_mean[a]
      }
      TableSp$Fc_mean[a]=250000-TableSp$Fc_mean[a]
      TableSp$Fme_mean[a]=250000-TableSp$Fme_mean[a]
      TableSp$Fmax_mean[a]=250000-TableSp$Fc_mean[a]    
      TableSp$Slope_mean[a]=-abs(TableSp$Slope_mean[a])
    }
    
  }
}


for (a in 1:nrow(TableInd))
{
  if(substr(TableInd$Species[a],1,3)=="Hip"){
    AliasedProb=(TableInd$Fmax_mean[a]-TableInd$Fc_mean[a])/(125000-TableInd$Fc_mean[a])
    print(paste(TableInd$Individual[a],TableInd$Species[a],AliasedProb))
    if(AliasedProb>0.5){
      if(TableInd$Fmin_mean[a]>TableInd$Fc_mean[a]-3000)
      {
        TableInd$Fmin_mean[a]=250000-TableInd$Fmax_mean[a]
      }
      TableInd$Fc_mean[a]=250000-TableInd$Fc_mean[a]
      TableInd$Fme_mean[a]=250000-TableInd$Fme_mean[a]
      TableInd$Fmax_mean[a]=250000-TableInd$Fc_mean[a]    
      TableInd$Slope_mean[a]=-abs(TableInd$Slope_mean[a])
    }
    
  }
}

fwrite(TableSp,"TableSp.csv",sep=";")
fwrite(TableInd,"TableInd.csv",sep=";")

