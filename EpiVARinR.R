  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(frequency)
  library(tidyquant)  
  library(ggpubr)
  library(ISOweek)
  library(readr)
  library(zoo)
  library(smooth)
  library(viridis)
  library(readr)
  library(reshape2)
  library(vars)
  library(rmgarch)
  library(forecast)
  library(RColorBrewer)
  library(timetk)
  library(sweep)
  library(scales)
  library(lubridate)
  library(rstudioapi)
  
  current_path = rstudioapi::getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
  exit <- function() { invokeRestart("abort") }    
  
  ####################################################
  # Telechargement et préparation des données
  ####################################################
  
  # General data from SPF
  url <- "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"
  dest <- "./spf_donneesensemble.csv"
  spf<- download.file(url,dest)
  dbspf<- read_csv("spf_donneesensemble.csv")
  LastObs<-(max(dbspf$date)-3) 
  
  # Correction jours fériés SPF
  download.file("https://www.data.gouv.fr/fr/datasets/r/6637991e-c4d8-4cd6-854e-ce33c5ab49d5",
                "joursferies.csv")
  joursferies <- read_csv("joursferies.csv")
  joursferies$ferie<-c(rep(1))
  dbspf<-full_join(dbspf,joursferies) 
  dbspf$ferie <- replace_na(dbspf$ferie,0) 
  dbspf$postferie <- case_when(
    lag(dbspf$ferie,1)==1 ~ 1,
    TRUE ~ 0 
  )
  
  dbspf <- dbspf %>%
    subset(select = -c(annee,zone,nom_jour_ferie) ) 
  dbspf$JourSemaine<-lubridate::wday(dbspf$date, week_start = 1)
  dbspf$Poids<-case_when(
    dbspf$JourSemaine==1 ~ 0.188,
    dbspf$JourSemaine==2 ~ 0.177,
    dbspf$JourSemaine==3 ~ 0.164,
    dbspf$JourSemaine==4 ~ 0.175,
    dbspf$JourSemaine==5 ~ 0.179,
    dbspf$JourSemaine==6 ~ 0.094,
    dbspf$JourSemaine==7 ~ 0.022
  )
  
  dbspf$PoidsAdj<-case_when(
    dbspf$ferie==1 ~ 0.022,
    dbspf$ferie==0 ~ dbspf$Poids,
  )
  
  dbspf$PoidsAdj<-case_when(
    dbspf$postferie==1 & dbspf$JourSemaine<6  ~ 0.188,
    TRUE ~ dbspf$PoidsAdj,
  )
  
  dbspf$PoidsMoyen<-rollapply(dbspf$PoidsAdj,7,sum,align="right",fill=NA)
  dbspf$pos7jAdj<-dbspf$pos_7j/dbspf$PoidsMoyen/7
  dbspf<-filter(dbspf,dbspf$date>"2020-01-01" & dbspf$date<=LastObs )
  
  gCorrectionJoursFeries<-ggplot(data=dbspf) +
    geom_line(aes(x=date,y=pos_7j/7,color="Brut")) +
    geom_line(aes(x=date,y=pos7jAdj,color="Corrigé des jours fériés")) +
    theme_bw() + 
    scale_color_manual(name="",values=c("Brut"="blue",
                                        "Corrigé des jours fériés"="red")) +  
    labs(y="R",
         x=NULL,
         title = "Nombre de cas positif sur 7 jours glissants",
         caption="Notes : correction des jours fériés d'après la méthode SPF. \nSource: Santé Publique France. \n Calculs : P. Aldama @paldama") 
  print(gCorrectionJoursFeries)
  ggsave("./gCorrectionJoursFeries.png", plot = gCorrectionJoursFeries, bg = "white", width = 7, height = 4)
  
  
  # Prolongation du Reffectif
  dbspf <- dbspf %>%
    mutate(ReBrut = pos_7j/lag(pos_7j,7)) %>%
    mutate(Re = rollapply(ReBrut,3,mean,align="right",fill=NA))
  
  library(EpiEstim)
  incid<-dbspf%>%
    subset(select = c(date,pos7jAdj) )%>%
    rename(I = pos7jAdj)%>%
    rename(dates = date) %>%
    na.omit()
  incid$index<-1:nrow(incid)
  SmoothIncid<-loess(I ~ index, data = incid,span = 0.05)
  incid$I<-SmoothIncid$fitted
  
  Restim<-estimate_R(incid,
             method = "parametric_si",
             config = make_config(
               list(
                 mean_si = 5.5, 
                 std_si = 1.5)
               )
  )
  RestimData<-Restim$R
  RestimData$date<-seq(from = as.Date(min(incid$dates)+7),
                       to = as.Date(max(incid$dates)),
                       by = 'day')
  dbspf<-full_join(RestimData,dbspf)
  dbspf$REpiEstim<-dbspf$`Mean(R)`
  
  
  gRcompare<-ggplot(data=filter(dbspf,dbspf$date>"2020-07-01")) +
    geom_line(aes(x=date,y=REpiEstim,color="R: EpiEstim"),size=1) +
    geom_line(aes(x=date,y=R, color="R: SPF"),size=1) +
    geom_hline(yintercept = 1, size=0.3)+
    scale_color_manual(name="",values=c("R: EpiEstim"="blue",
                                        "R: SPF"="black")) +
    labs(y="R",
         x=NULL,
         caption="Notes : calcul du R effectif à partir du package EpiEstim (mean_si = 5.5, std_si = 1.5). \nSource: Santé Publique France. \n Calculs : P. Aldama @paldama") + 
    theme_bw() + theme(legend.position = "right")
  print(gRcompare)
  ggsave("./gRcompare.png", plot = gRcompare, bg = "white", width = 7, height = 4)
    
  
  
  db<-dbspf%>%
    filter(date>="2020-07-01" & date<=LastObs)  %>%
    na.pass()
  
  # Lissage
  db$index<-1:nrow(db)
  
  db$cas<-I(db$pos)
  db$cas_sm<-rollapply(db$cas,7,mean,align="right",fill=NA)/db$PoidsMoyen
  cas_sm_model<-loess(cas_sm ~ index,data = db ,span=0.05)
  db$cas_sm<-predict(cas_sm_model,newdata = db,na.action = na.exclude)
  ggplot(data=db) + 
    geom_point(aes(x=date,y=cas),color = "black") + 
    geom_line(aes(x=date,y=cas_sm),color = "red") 
  
  hosp_sm_model<-loess(hosp ~ index,data = db ,span=0.05)
  db$hosp_sm<-predict(hosp_sm_model,newdata = db, na.action = na.exclude)
  ggplot(data=db) + 
    geom_point(aes(x=date,y=hosp),color = "black") + 
    geom_line(aes(x=date,y=hosp_sm),color = "red")
  
  rea_sm_model<-loess(rea ~ index,data = db ,span=0.05)
  db$rea_sm<-predict(rea_sm_model,newdata = db, na.action = na.exclude)
  ggplot(data=db) + 
    geom_point(aes(x=date,y=rea),color = "black") + 
    geom_line(aes(x=date,y=rea_sm),color = "red")
  
  db$dc <- db$incid_dchosp
  db$incid_dchosp_sm<-rollapply(db$incid_dchosp,7,mean,align="right",fill=NA)
  dc_sm_model<-loess(incid_dchosp_sm ~ index,data = db ,span=0.05)
  db$dc_sm<-predict(dc_sm_model,newdata = db, na.action = na.exclude)
  ggplot(data=db) + 
    geom_point(aes(x=date,y=dc),color = "black") + 
    geom_line(aes(x=date,y=dc_sm),color = "red")
  
  
  ####################################################
  # Estimation et forecast full-sample
  ####################################################
  
  # Preparation du dataset
  k<-0
  OutofSample<-7*k
  HorizonForecast<-7*2+k
  LengthGraph <- 3*30 # longueur des graphiques
  ConfidenceLevel <- 0.95
  
  dateFcst<-seq(from = as.Date(LastObs-OutofSample+1), to = as.Date(LastObs-OutofSample+HorizonForecast), by = 'day')
  debFcst<-LastObs-OutofSample
  FirstDayFcst<-debFcst+1
  
  DataEpiVAR <- db %>%
    mutate(R = log(REpiEstim/lag(REpiEstim,1))) %>%
    mutate(cas = log(cas_sm/lag(cas_sm,1))) %>%
    mutate(hosp = log(hosp_sm/lag(hosp_sm,1))) %>%
    mutate(rea = log(rea_sm/lag(rea_sm,1))) %>%
    mutate(dc = log(dc_sm/lag(dc_sm,1))) %>%
    filter(date < LastObs-OutofSample) %>%
    subset(select = c(R,cas,hosp,rea,dc)) %>%
    na.omit %>%
    ts()
  
  # Estimation VAR
  EpiVAR<-VAR(DataEpiVAR,
              p=21,
              type="trend")
  summary(EpiVAR)
  
  # Test de stabilité du VAR
  if ( max(roots(EpiVAR))<1 ) {
    print("EpiVAR is stationary")
    print(paste("Max root =", round(max(roots(EpiVAR)) , 2) ))
  } else {
    print("EpiVAR is non stationary!")
    print(paste("Max root =", round(max(roots(EpiVAR)) , 2) ))
    exit()
  }
  
  # checkresiduals(EpiVAR$varresult$R)
  # checkresiduals(EpiVAR$varresult$cas)
  # checkresiduals(EpiVAR$varresult$hosp)
  # checkresiduals(EpiVAR$varresult$rea)
  # checkresiduals(EpiVAR$varresult$dc)
  
  # Forecasts
  Forecast<-predict(EpiVAR, 
                    n.ahead = HorizonForecast, 
                    pi = ConfidenceLevel, 
                    dumvar = NULL)
  
  # Plots forecasts
  fcstR <-exp(cumsum(as.data.frame(Forecast$fcst$R)))
  fcstR$date<- dateFcst
  fcstRBind<-full_join(db,fcstR)
  fcstRBind$fcst<-fcstRBind$fcst*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind$low<-fcstRBind$lower*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind$up<-fcstRBind$upper*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind<-filter(fcstRBind,fcstRBind$date>=as.Date(LastObs-LengthGraph))
  
  gR<-ggplot(data=fcstRBind) +
    geom_line(aes(x=date, y = REpiEstim, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) + 
    geom_ribbon(aes(x=date, ymin = low, ymax=up, fill="Intervalle de prévision"), alpha = 0.2)  +
    geom_hline(yintercept = 1, size = 0.2) +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL , title = "Taux de reproduction effectif (Reff)")
  
  fcstcas <-exp(cumsum(as.data.frame(Forecast$fcst$cas)))
  fcstcas$date<-dateFcst
  fcstcasBind<-full_join(db,fcstcas)
  fcstcasBind$fcst<-fcstcasBind$fcst*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind$low<-fcstcasBind$lower*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind$up<-fcstcasBind$upper*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind<-filter(fcstcasBind,fcstcasBind$date>=as.Date(LastObs-LengthGraph))
  
  gcas<-ggplot(data=fcstcasBind) +
    geom_col(aes(x=date, y = cas), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = cas_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) + 
    geom_ribbon(aes(x=date, ymin = low, ymax=up, fill="Intervalle de prévision"), alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3) ) + 
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL , title = "Cas confirmés (date de prélèvement)")
    
  fcsthosp <-exp(cumsum(as.data.frame(Forecast$fcst$hosp)))
  fcsthosp$date<-dateFcst
  fcsthospBind<-full_join(db,fcsthosp)
  fcsthospBind$fcst<-fcsthospBind$fcst*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind$low<-fcsthospBind$lower*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind$up<-fcsthospBind$upper*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind<-filter(fcsthospBind,fcsthospBind$date>=as.Date(LastObs-LengthGraph))
  
  ghosp<-ggplot(data=fcsthospBind) +
    geom_col(aes(x=date, y = hosp), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = hosp_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL,  title = "Lits en hospitalisation conventionnelle")
  
  
  fcstrea <-exp(cumsum(as.data.frame(Forecast$fcst$rea)))
  fcstrea$date<-dateFcst
  fcstreaBind<-full_join(db,fcstrea)
  fcstreaBind$fcst<-fcstreaBind$fcst*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind$low<-fcstreaBind$lower*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind$up<-fcstreaBind$upper*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind<-filter(fcstreaBind,fcstreaBind$date>=as.Date(LastObs-LengthGraph))
  
  grea<-ggplot(data=fcstreaBind) +
    geom_col(aes(x=date, y = rea), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = rea_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL,y = NULL , title = "Lits en soins critiques")
    
  
  fcstdc <-exp(cumsum(as.data.frame(Forecast$fcst$dc)))
  fcstdc$date<-dateFcst
  fcstdcBind<-full_join(db,fcstdc)
  fcstdcBind$fcst<-fcstdcBind$fcst*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind$low<-fcstdcBind$lower*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind$up<-fcstdcBind$upper*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind<-filter(fcstdcBind,fcstdcBind$date>=as.Date(LastObs-LengthGraph))
  
  gdc<-ggplot(data=fcstdcBind) +
    geom_col(aes(x=date, y = dc), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = dc_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL, title = "Décès hospitaliers")
  
  
  TitleGraph = paste("Projection EpiVAR à partir du ",FirstDayFcst," (Dernier point observé : ",LastObs,")\n",sep="")
    
  gEpiVAR<-ggarrange(gR,gcas,ghosp,grea,gdc,
                     ncol=2,
                     nrow=3,
                     legend = "bottom",
                     common.legend = TRUE,
                     labels = "auto",
                     align="hv") %>%
    annotate_figure( top = text_grob(TitleGraph,
                                              just = "top",
                                              face = "bold", 
                                              size = 12),
                              bottom = text_grob("Source: Santé Publique France. \nModèle et calculs : P. Aldama @paldama.",
                                                 hjust = 1, 
                                                 x = 1, 
                                                 face = "italic", 
                                                 size = 10))
  ggsave("./gEpiVAR.png", plot=gEpiVAR, bg="white", width = 10, height = 8)
  
  print(gEpiVAR)
  
  ####################################################
  # Estimation et forecast out-of-sample
  ####################################################
  
  # Preparation du dataset
  k<-2
  OutofSample<-7*k
  HorizonForecast<-7*2+k
  LengthGraph <- 1*30 # longueur des graphiques
  ConfidenceLevel <- 0.95
  
  dateFcst<-seq(from = as.Date(LastObs-OutofSample+1), to = as.Date(LastObs-OutofSample+HorizonForecast), by = 'day')
  debFcst<-LastObs-OutofSample
  FirstDayFcst<-debFcst+1
  
  DataEpiVAR <- db %>%
    mutate(R = log(REpiEstim/lag(REpiEstim,1))) %>%
    mutate(cas = log(cas_sm/lag(cas_sm,1))) %>%
    mutate(hosp = log(hosp_sm/lag(hosp_sm,1))) %>%
    mutate(rea = log(rea_sm/lag(rea_sm,1))) %>%
    mutate(dc = log(dc_sm/lag(dc_sm,1))) %>%
    filter(date < LastObs-OutofSample) %>%
    subset(select = c(R,cas,hosp,rea,dc)) %>%
    na.omit %>%
    ts()
  
  # Estimation VAR
  EpiVAR<-VAR(DataEpiVAR,
              p=21,
              type="trend")
  summary(EpiVAR)
  
  # Test de stabilité du VAR
  if ( max(roots(EpiVAR))<1 ) {
    print("EpiVAR is stationary")
    print(paste("Max root =", round(max(roots(EpiVAR)) , 2) ))
  } else {
    print("EpiVAR is non stationary!")
    print(paste("Max root =", round(max(roots(EpiVAR)) , 2) ))
    exit()
  }
  
  # checkresiduals(EpiVAR$varresult$R)
  # checkresiduals(EpiVAR$varresult$cas)
  # checkresiduals(EpiVAR$varresult$hosp)
  # checkresiduals(EpiVAR$varresult$rea)
  # checkresiduals(EpiVAR$varresult$dc)
  
  # Forecasts
  Forecast<-predict(EpiVAR, 
                    n.ahead = HorizonForecast, 
                    pi = ConfidenceLevel, 
                    dumvar = NULL)
  
  # Plots forecasts
  fcstR <-exp(cumsum(as.data.frame(Forecast$fcst$R)))
  fcstR$date<- dateFcst
  fcstRBind<-full_join(db,fcstR)
  fcstRBind$fcst<-fcstRBind$fcst*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind$low<-fcstRBind$lower*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind$up<-fcstRBind$upper*fcstRBind$REpiEstim[fcstRBind$date==debFcst]
  fcstRBind<-filter(fcstRBind,fcstRBind$date>=as.Date(LastObs-LengthGraph))
  
  gR<-ggplot(data=fcstRBind) +
    geom_line(aes(x=date, y = REpiEstim, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) + 
    geom_ribbon(aes(x=date, ymin = low, ymax=up, fill="Intervalle de prévision"), alpha = 0.2)  +
    geom_hline(yintercept = 1, size = 0.2) +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL , title = "Taux de reproduction effectif (Reff)")
  
  fcstcas <-exp(cumsum(as.data.frame(Forecast$fcst$cas)))
  fcstcas$date<-dateFcst
  fcstcasBind<-full_join(db,fcstcas)
  fcstcasBind$fcst<-fcstcasBind$fcst*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind$low<-fcstcasBind$lower*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind$up<-fcstcasBind$upper*fcstcasBind$cas_sm[db$date==debFcst]
  fcstcasBind<-filter(fcstcasBind,fcstcasBind$date>=as.Date(LastObs-LengthGraph))
  
  gcas<-ggplot(data=fcstcasBind) +
    geom_col(aes(x=date, y = cas), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = cas_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) + 
    geom_ribbon(aes(x=date, ymin = low, ymax=up, fill="Intervalle de prévision"), alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3) ) + 
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL , title = "Cas confirmés (date de prélèvement)")
  
  fcsthosp <-exp(cumsum(as.data.frame(Forecast$fcst$hosp)))
  fcsthosp$date<-dateFcst
  fcsthospBind<-full_join(db,fcsthosp)
  fcsthospBind$fcst<-fcsthospBind$fcst*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind$low<-fcsthospBind$lower*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind$up<-fcsthospBind$upper*fcsthospBind$hosp_sm[db$date==debFcst]
  fcsthospBind<-filter(fcsthospBind,fcsthospBind$date>=as.Date(LastObs-LengthGraph))
  
  ghosp<-ggplot(data=fcsthospBind) +
    geom_col(aes(x=date, y = hosp), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = hosp_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL,  title = "Lits en hospitalisation conventionnelle")
  
  
  fcstrea <-exp(cumsum(as.data.frame(Forecast$fcst$rea)))
  fcstrea$date<-dateFcst
  fcstreaBind<-full_join(db,fcstrea)
  fcstreaBind$fcst<-fcstreaBind$fcst*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind$low<-fcstreaBind$lower*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind$up<-fcstreaBind$upper*fcstreaBind$rea_sm[db$date==debFcst]
  fcstreaBind<-filter(fcstreaBind,fcstreaBind$date>=as.Date(LastObs-LengthGraph))
  
  grea<-ggplot(data=fcstreaBind) +
    geom_col(aes(x=date, y = rea), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = rea_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL,y = NULL , title = "Lits en soins critiques")
  
  
  fcstdc <-exp(cumsum(as.data.frame(Forecast$fcst$dc)))
  fcstdc$date<-dateFcst
  fcstdcBind<-full_join(db,fcstdc)
  fcstdcBind$fcst<-fcstdcBind$fcst*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind$low<-fcstdcBind$lower*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind$up<-fcstdcBind$upper*fcstdcBind$dc_sm[db$date==debFcst]
  fcstdcBind<-filter(fcstdcBind,fcstdcBind$date>=as.Date(LastObs-LengthGraph))
  
  gdc<-ggplot(data=fcstdcBind) +
    geom_col(aes(x=date, y = dc), fill="grey",alpha = 0.4) +
    geom_line(aes(x=date, y = dc_sm, color = "Observé")) +
    geom_line(aes(x=date, y = fcst, color = "Projection")) +
    geom_ribbon(aes(x=date, ymin = low, ymax=up), fill="blue", alpha = 0.2)  +
    scale_color_manual(name  ="", values = c("Observé" = "black", "Projection" = "blue")) +
    scale_fill_manual(name = "", values = c("Intervalle de prévision" = "blue" )) +
    theme_bw() + theme(plot.title = element_text(size = 11)) +
    labs(x = NULL, y = NULL, title = "Décès hospitaliers")
  
  
  TitleGraph = paste("Projection EpiVAR à partir du ",FirstDayFcst," (Dernier point observé : ",LastObs,")\n",sep="")
  
  gEpiVAR<-ggarrange(gR,gcas,ghosp,grea,gdc,
                     ncol=2,
                     nrow=3,
                     legend = "bottom",
                     common.legend = TRUE,
                     labels = "auto",
                     align="hv") %>%
    annotate_figure( top = text_grob(TitleGraph,
                                     just = "top",
                                     face = "bold", 
                                     size = 12),
                     bottom = text_grob("Source: Santé Publique France. \nModèle et calculs : P. Aldama @paldama.",
                                        hjust = 1, 
                                        x = 1, 
                                        face = "italic", 
                                        size = 10))
  ggsave("./gEpiVAROutOfSample.png", plot=gEpiVAR, bg="white", width = 10, height = 8)
  
  print(gEpiVAR)
   
  ######################################
  # IRF
  ######################################
  
  library(devtools)
  source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R")
  
  
  irf<-irf(EpiVAR,
             impulse = c("R","cas","hosp","rea","dc"),
             response = NULL,
             n.ahead = 60,
             ortho = TRUE,
             cumulative = TRUE,
             boot = TRUE,
             ci = 0.90,
             runs = 1000 )
  
  
  multiple_varirf <- extract_varirf(irf) %>%
    pivot_longer(
      cols = !period,
      names_to = c("type","impulse","response"),
      names_pattern = "(.*)_(.*)_(.*)",
      values_to = "values",
      values_drop_na = TRUE
    ) %>%
    pivot_wider(names_from = type, values_from = values)

  gIRF<-multiple_varirf %>%
    ggplot(aes(x=period, y=irf*100, ymin=lower*100, ymax=upper*100)) +
    geom_hline(yintercept = 0, color="black") +
    geom_ribbon(aes(fill=impulse), alpha=.1) +
    geom_line(aes(color = impulse), size = 0.8) +
    theme_bw() + scale_color_viridis(discrete=TRUE) + scale_fill_viridis(discrete=TRUE) +
    ggtitle("Fonctions impulsion-réponse orthogonalisées du modèle EpiVAR")+
    facet_grid(factor(impulse, levels = c("r","cas","hosp","rea","dc"))~factor(response, levels = c("r","cas","hosp","rea","dc")),scale="free")+
    scale_x_continuous(sec.axis = sec_axis(~ . , name = "Réponses des variables", breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Impulsions", breaks = NULL, labels = NULL)) +
        theme(plot.title = element_text(size = 11,face="bold", hjust=0.5),
          axis.title.y = element_text(size=11)) +
    labs(x = "jours",
         y = "écart en %",
         fill = "Chocs",
         color = "Chocs",
         caption = "Les intervalles de confiance sont les 5ème et 95ème percentiles de la distribution obtenue par boostrapp. \nSource: Santé Publique France. \nModèle et calculs : P. Aldama @paldama.")
  ggsave("./gIRF.png",plot=gIRF,bg="white",width=8,height = 8)
  print(gIRF)
  
  #################################################
  # Forecast Error Variance Decomposition
  #################################################
  
  fevd <- fevd(EpiVAR,
               n.ahead = 60)
  fevd_df <- as.data.frame(lapply(fevd, unlist)) %>%
    mutate(period = c(1:60) ) %>%
    pivot_longer(
      cols = !period,
      names_to = c("response","impulse"),
      names_pattern = "(.*)\\.(.*)",
      values_to = "values",
      values_drop_na = TRUE
    )
  
  gFEVD <- fevd_df %>%
    ggplot(aes(x=period,y=values,fill=impulse)) +
    geom_area(stat = "identity",
             position = "stack") +
    theme_bw() + scale_fill_viridis(discrete = TRUE) +
    ggtitle("Décomposition de la variance de l'erreur de prévision selon les chocs dans le modèle EpiVAR")+
    facet_wrap(factor(response, levels = c("R","cas","hosp","rea","dc"))~.)+
    theme(plot.title = element_text(size = 11,face="bold", hjust=0.5),
          axis.title.y = element_text(size=11)) +
    labs(x = "jours",
         y = "% de la variance expliquée", 
         fill = "Chocs",
         caption = "Source: Santé Publique France. \nModèle et calculs : P. Aldama @paldama.")
  print(gFEVD) 
  
  ggsave("./gFEVD.png",plot=gFEVD,bg="white",width=8,height = 8)
  
