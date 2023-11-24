library(tidyverse)
library(dplyr)
library(ggplot2)
library(frequency)
library(tidyquant)  
library(ggpubr)
library(ISOweek)
library(ggthemes)
library(patchwork)
library(grid)

temp <- dirname(rstudioapi::getSourceEditorContext()$path)
if (getwd()!=temp){setwd(temp)}

##############################################################################################
# Telecharge les donnees

# Insee data on mortality 1968/2019
url<-"https://www.insee.fr/fr/statistiques/fichier/4771989/T79JDEC.csv" 
dest<-"./HistoricalDataMortality.csv"
data<-download.file(url, dest)

# data from SPF
url<-"https://www.data.gouv.fr/fr/datasets/r/d3a98a30-893f-47f7-96c5-2f4bcaaa0d71"
dest<-'./SPF.csv'
spf<-download.file(url, dest)

# Data from Sentinelles
url<-"https://www.sentiweb.fr/datasets/incidence-PAY-3.csv"
dest<-'./SentinellesIncidenceGrippe.csv'
Sentinelles<-download.file(url, dest)

# Data from SI-VIC 
url <- "https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"
dest <- "./sivic_donneeshospit.csv"
sivic<- download.file(url,dest)

# Insee data on mortality for 2020/2021/2022
url<-"https://www.insee.fr/fr/statistiques/fichier/4487988/2023-10-27_detail.zip"
dest<-"./dataMortality.zip"
download.file(url, dest)
unzip(dest)

##############################################################################################
# Importe les données historiques des décés quotidiens et détaillées pour 2018-2021


## Import historical data from Insee: 
db<-read.csv("./HistoricalDataMortality.csv",sep=";")
db <- filter(db, db$JOUR!="MM" & db$DECES!="NA")
db$Date<-as.Date(paste(db$JOUR, db$MOIS2,db$ANNEE_1968_2019, sep = "/"),"%d/%m/%Y")
db <- subset(db, select = -c(JOUR,MOIS2,ANNEE_1968_2019) ) # delete useless columns 
DecesFM <- db %>%
   filter(db$CHAMP=="FM") %>% 
   subset(select = -c(CHAMP) )
DecesFE <- db %>%
   filter(db$CHAMP=="FE") %>% 
   subset(select = -c(CHAMP) )   

# Importe les données sur les décés individuels depuis 2018 diffusés par l'Insee
source <- c("./DC_2018_det.csv", "./DC_2019_det.csv", "./DC_2020_det.csv","./DC_2021_det.csv","./DC_20222023_det.csv")
dataframes <- list()

for (x in source) {
  dataframe <- read_csv2(x)
  dataframe$Date <- as.Date(paste(dataframe$JDEC, dataframe$MDEC,dataframe$ADEC, sep = "/"),"%d/%m/%Y")
  dataframe$CHAMP <- case_when(
    dataframe$DEPDEC=="2A" ~ "FM",
    dataframe$DEPDEC=="2B" ~ "FM",
    as.numeric(dataframe$DEPDEC)<900 ~"FM" ,
    TRUE ~ "OM")
  dataframe$age<-dataframe$ADEC-dataframe$ANAIS
  dataframes[[x]] <- dataframe
}
db <- reduce(dataframes, full_join)

# Prepare the data
db$AnneeDeces<-as.character(db$ADEC)
db$MoisDeces<-format(as.Date(db$Date, format="%d/%m/%Y"),"(%m) %B")
db$ADEC<-as.character(db$ADEC)
db$SEXE<-fct_recode(db$SEXE,
                        "Femme" = "F",
                        "Homme" = "M")

##############################################################################################
# Redressement des series Insee

# Charge les données pour le redressement : ces données représentent le coefficient de redressement moyen
# observé sur plusieurs publications des données de décès par l'Insee
# inspiré par B. Coulmont : https://coulmont.com/blog/2020/11/13/une-deuxieme-vague/
dbRedress <- read.csv("./RedressementDistance.csv",sep=";",dec=",")

# Normalise le coefficient moyen et la variable de distance
dbRedress$dist<-dbRedress$dist-10
dbRedress$coefNorm <- dbRedress$coef - c(rep(1))

# Estime un modéle polynomial inverse
model.Redressement <- lm(coefNorm ~ I((dist)^-1) + I((dist)^-2) + I((dist)^-3) + + I((dist)^-4) -1,
                         data = dbRedress)
summary(model.Redressement)
dbRedress$coefsmooth<-predict(model.Redressement)

# Plot les données et le modéle
ggplot(data = dbRedress) +
   geom_point(aes(x=dist,y=coefNorm)) +
   geom_smooth(aes(x=dist,y=coefNorm),
               se = FALSE, 
               method = lm,
               formula = y ~ I((x)^-1) + I((x)^-2) + I((x)^-3) + I((x)^-4) -1  ) 

# Sauvegarde les coefficients
coef1<-model.Redressement$coefficients[[ 1 ]] # 1/(dist-10)
coef2<-model.Redressement$coefficients[[ 2 ]] # 1/(dist-10)^2
coef3<-model.Redressement$coefficients[[ 3 ]] # 1/(dist-10)^3
coef4<-model.Redressement$coefficients[[ 4 ]] # 1/(dist-10)^4

##############################################################################################
# Complète les données historiques sur les décés quotidiens avec les données 
# individuelles diffusées par l'Insee

DecesFM_add <-  db %>%
  filter(db$CHAMP=="FM")%>%
  group_by(Date) %>%
  summarise(DECES=n())

DecesFE_add <-  db %>%
  group_by(Date) %>%
  summarise(DECES=n())

# Fusionne les données récentes avec les données historiques
DecesFE<-bind_rows(DecesFE,DecesFE_add)
DecesFM<-bind_rows(DecesFM,DecesFM_add)

DecesFE$Annee<-as.character(format(as.Date(DecesFE$Date, format="%d/%m/%Y"),"%Y"))
DecesFE$Mois<-as.character(format(as.Date(DecesFE$Date, format="%d/%m/%Y"),"%m"))
DecesFE$MoisJour<-as.character(format(as.Date(DecesFE$Date, format="%d/%m/%Y"),"%m/%d"))

DecesFM$Annee<-as.character(format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%Y"))
DecesFM$Mois<-as.character(format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%m"))
DecesFM$MoisJour<-as.character(format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%m/%d"))

# Redressement
# crée la variable de distance
DecesFE$distadj <- c(nrow(DecesFE):1)
DecesFM$distadj <- c(nrow(DecesFM):1)
# crée des vecteurs avec les coefficients
DecesFE$coef1 <- c(rep(coef1))
DecesFE$coef2 <- c(rep(coef2))
DecesFE$coef3 <- c(rep(coef3))
DecesFE$coef4 <- c(rep(coef4))
DecesFM$coef1 <- c(rep(coef1))
DecesFM$coef2 <- c(rep(coef2))
DecesFM$coef3 <- c(rep(coef3))
DecesFM$coef4 <- c(rep(coef4))
# crée le coefficient de redressement
DecesFE$coefredress <- 1 + DecesFE$coef1/DecesFE$distadj + DecesFE$coef2/(DecesFE$distadj)^2  + DecesFE$coef3/(DecesFE$distadj)^3   + DecesFE$coef4/(DecesFE$distadj)^4 
DecesFM$coefredress <- 1 + DecesFM$coef1/DecesFM$distadj + DecesFM$coef2/(DecesFM$distadj)^2  + DecesFM$coef3/(DecesFM$distadj)^3   + DecesFM$coef4/(DecesFM$distadj)^4 
# correction des séries de décés
DecesFE$DECES<- DecesFE$DECES*DecesFE$coefredress 
DecesFM$DECES<- DecesFM$DECES*DecesFM$coefredress 

##############################################################################################
# Aggregation hebdo et estimation de la tendance hors-épidémie

# Importation des données de surveillance épidémique (Grippe, réseau Sentinelles)
Sentinelles<- read_csv("SentinellesIncidenceGrippe.csv", 
                                                    skip = 1)
Sentinelles$weekTemp<-as.character(Sentinelles$week-1)
Sentinelles$weekTemp<-as.character(
   paste(
      substr(Sentinelles$week,1,4),
      paste("W",substr(Sentinelles$week,5,6),sep=""),
      "7",
      sep="-")
)
Sentinelles$iso.date <- ISOweek2date(Sentinelles$weekTemp)
Sentinelles$ISO.week <- date2ISOweek(Sentinelles$iso.date)
Sentinelles <- Sentinelles %>%
   arrange(ISO.week)
Sentinelles$inc100 <- as.numeric(Sentinelles$inc100)

SentinellesMerge<-Sentinelles%>%
   subset(select = c(ISO.week,inc100))

# Donnees d'ensemble SPF
url <- "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"
dest <- "./spf_donneesensemble.csv"
spf<- download.file(url,dest)
dbspf<- read_csv("spf_donneesensemble.csv")
dbspfhebdo <- dbspf %>%
   tq_transmute(select     = incid_dchosp,
                mutate_fun = apply.weekly,
                FUN        = mean)
dbspfhebdo$incid_dchosp<-round(dbspfhebdo$incid_dchosp*7) #utilise la moyenne pour éviter le biais de semaine incompléte
dbspfhebdo$ISO.week<-date2ISOweek(dbspfhebdo$date)
dbspfhebdo<-subset( dbspfhebdo, select = -date )


# Aggregation hebdo de la série de décés quotidiens
DecesFEhebdo<-DecesFE %>%
   tq_transmute(select     = DECES,
                mutate_fun = apply.weekly,
                FUN        = mean)
DecesFEhebdo$DECES<-DecesFEhebdo$DECES*7 #utilise la moyenne pour éviter le biais de semaine incompléte
DecesFEhebdo$ISO.week<-date2ISOweek(DecesFEhebdo$Date)
DecesFEhebdo$DECES<-round(DecesFEhebdo$DECES)

# Merge des datasets
dbMerge<-left_join(DecesFEhebdo,SentinellesMerge,by = "ISO.week")
dbMerge<-full_join(dbMerge,dbspfhebdo,keep=FALSE,by="ISO.week")
dbMerge$incid_dchosp<-replace_na(dbMerge$incid_dchosp, 0)
dbMergeNAs <- dbMerge[rowSums(is.na(dbMerge)) > 0,]
dbMerge<-drop_na(dbMerge)
dbMerge$Annee<-format(as.Date(dbMerge$Date, format="%d/%m/%Y"),"%Y")


# List of target years for which we want to create the dummy
target_years <- unique(dbMerge$Annee)  # Adjust as needed

# Initialize an empty list to store the results
seasonal_dummies_list <- list()

# Iterate through each target year and create the seasonal dummy
for (year in target_years) {
  yearPrev<-as.character(as.numeric(year)-1)
  # Create a seasonal dummy from October to March overlapping two years
  seasonal_dummy <- case_when(
    year(dbMerge$Date)==yearPrev & month(dbMerge$Date) == c(10) ~ 1,
    year(dbMerge$Date)==yearPrev & month(dbMerge$Date) == c(11) ~ 1,
    year(dbMerge$Date)==yearPrev & month(dbMerge$Date) == c(12) ~ 1,
    year(dbMerge$Date)==year & month(dbMerge$Date) == c(1) ~ 1 ,
    year(dbMerge$Date)==year & month(dbMerge$Date) == c(2) ~ 1 ,
    year(dbMerge$Date)==year & month(dbMerge$Date) == c(3) ~ 1 ,
    TRUE ~ 0)
  # Append the seasonal dummy to the list
  seasonal_dummies_list[[as.character(year)]] <- seasonal_dummy
}

# Iterate through each year and merge the seasonal dummy
for (year in target_years) {
  seasonal_dummy <- seasonal_dummies_list[[as.character(year)]]
  col_name <- paste("SeasonalDummy_", year, sep = "")
  dbMerge <- cbind(dbMerge, seasonal_dummy)
  names(dbMerge)[ncol(dbMerge)] <- col_name  # Rename the last column
}

# Estime le modéle quasi-Poisson de 2014 à 2019
dbMerge$t<-c(1:nrow(dbMerge))
dbMerge$cose<-cos(2*pi*dbMerge$t/52)
dbMerge$sine<-sin(2*pi*dbMerge$t/52)
ModelPoisson<-glm( 
   DECES ~ t + cose + sine + lead(incid_dchosp,1) + inc100 + SeasonalDummy_2014*inc100 +  SeasonalDummy_2015*inc100 +  SeasonalDummy_2016*inc100 +
     SeasonalDummy_2017*inc100 + SeasonalDummy_2018*inc100 +  SeasonalDummy_2019*inc100 +  SeasonalDummy_2020*inc100 +
     SeasonalDummy_2021*inc100 +  SeasonalDummy_2022*inc100 +  SeasonalDummy_2023*inc100 ,
   data = filter(dbMerge,dbMerge$Annee >= 2014 & dbMerge$Annee <= 2023),
   family = quasipoisson(link="log"),
   control = list(maxit = 500))

summary(ModelPoisson)
pseudoR2<- 1-(ModelPoisson$deviance/ModelPoisson$null.deviance)
pseudoR2
phiPoisson<-summary(ModelPoisson)$deviance / summary(ModelPoisson)$df.residual

# Prediction et écart-type ajusté de la surdispersion
# Sauvegarde les coefficients
coefIntercept<-ModelPoisson$coefficients[[ 1 ]]
coefTrend<-ModelPoisson$coefficients[[ 2 ]]
coefCos<-ModelPoisson$coefficients[[ 3 ]] 
coefSin<-ModelPoisson$coefficients[[ 4 ]] 
coefIncidDecesCovid<-ModelPoisson$coefficients[[ 5 ]] 

dbMerge$DecesAttendus <- exp((coefIntercept + coefTrend*dbMerge$t + coefCos*dbMerge$cose + coefSin*dbMerge$sine))
dbMerge$DecesCovid <- dbMerge$DecesAttendus+lead(dbMerge$incid_dchosp, 1)

dbMerge$DecesAttendusSE <- sqrt(phiPoisson*dbMerge$DecesAttendus )
DecesFit<-predict(ModelPoisson,newdata=dbMerge,type = c("response"))
dbMerge<-cbind(dbMerge,DecesFit)
dbMerge$Resid<-dbMerge$DECES-dbMerge$DecesFit


## Defines a function to create a zoom plot:
gg_zoom <- function(.plot, zoom_cmd, draw_box = TRUE, box_nudge = 1, to_label = FALSE, label) {
  
  ## use enquo for tidyeval syntax
  zoom_cmd <- dplyr::enquo(zoom_cmd)
  
  ## subset data to zoom in on
  zoom_data <-
    .plot$data %>%
    dplyr::filter(!!zoom_cmd) 
  
  ## build the "zoom plot" based on the original ggplot object
  zoom_plot <- .plot
  ## coerce the data element to be the filtered data
  zoom_plot$data <- zoom_data
  
  ## if label  arg then add a repel text label
  if(to_label) {
    
    ## tidyeval syntax allows for a bare column name to be supplied
    label <- dplyr::enquo(label)
    
    zoom_plot <-
      zoom_plot +
      ggrepel::geom_text_repel(aes(label = !!label))
  }
  
  ## if draw box then add a box around data used in zoom
  if(draw_box) {
    
    ## need to get x and y variables (stored as quosures) from ggplot2 object
    x <- .plot$mapping$x
    y <- .plot$mapping$y
    
    ## create min/max x and y values for box around full plot
    box_data <- 
      zoom_data %>%
      dplyr::summarise(
        xmin = min(!!x, na.rm = TRUE),
        xmax = max(!!x, na.rm = TRUE),
        ymin = min(!!y, na.rm = TRUE),
        ymax = max(!!y, na.rm = TRUE)) %>%
      ## add to min and max so that the box sits just outside point
      dplyr::mutate(
        xmin = xmin -  box_nudge,
        xmax = xmax + box_nudge,
        ymin = ymin -  box_nudge,
        ymax = ymax +  box_nudge)
    
    ## use geom_rect to add to the plot
    .plot <-
      .plot +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = box_data$xmin, 
                     xmax = box_data$xmax, 
                     ymin = box_data$ymin, 
                     ymax = box_data$ymax), 
        fill = NA, 
        col = "grey", 
        lty = "dotted")
    
  }
  
  ## return in a basic patchwork layout
  .plot + zoom_plot
  
}


# Plot les données en time series
gTimeSeriesPoisson<-ggplot(data=filter(dbMerge,dbMerge$Annee>=2014)) +
   geom_line(aes(x=Date, y=DECES, color = "obs"),size=0.5) +
   geom_line(aes(x=Date, y=DecesAttendus, color = "attendu"),size=1) +
    geom_line(aes(x=Date, y=DecesFit, color = "fit"),size=0.4) +
   scale_color_manual(name =" Nb. de décés ",
                      labels = c("attendus en abs. d'épidémie","prédits","observés"),
                      values = c("obs" = "black", "attendu" = "blue","fit" = "green"))+
   geom_ribbon(aes(x=Date, 
                   ymin = DecesAttendus , 
                   ymax = DecesCovid, 
                   fill="Décés hosp. du COVID"), alpha=0.5) +
   geom_ribbon(aes(x=Date, 
                   ymin = DecesAttendus - 1.96*DecesAttendusSE, 
                   ymax = DecesAttendus + 1.96*DecesAttendusSE, 
                   fill="Sur/sous-mortalité normale"), alpha=0.1) +
   scale_fill_manual(c("",""),values=c("purple","blue")) +
  theme_minimal()+
  theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 10),
         plot.caption = element_text(size = 10, face = "italic"),
         legend.position = "top") +
   labs(x = NULL,
        y = NULL,
        title = "Mortalité hebdomadaire en France",
        subtitle = "Nombre de décés observés, prédits par la régression de Poisson et attendus en absence d'épidémie (grippale ou Covid19)")
ggsave("gTimeSeriesPoisson.png",plot=gTimeSeriesPoisson, bg="white", height = 7, width =10)
print(gTimeSeriesPoisson)

# Plot les données en time series: zoom 2020-2023
gTimeSeriesPoissonZoom<-ggplot(data=filter(dbMerge,dbMerge$Annee>=2020)) +
  geom_line(aes(x=Date, y=DECES, color = "obs"),size=0.5,color = "black", show.legend = FALSE) +
  geom_line(aes(x=Date, y=DecesAttendus, color = "attendu"),size=1,color = "blue", show.legend = FALSE) +
  geom_line(aes(x=Date, y=DecesFit, color = "fit"),size=0.4,color='green',show.legend = FALSE) +
  geom_ribbon(aes(x=Date, 
                  ymin = DecesAttendus , 
                  ymax = DecesCovid, 
                  fill="Décés hospitaliers du COVID"),fill="purple",show.legend = FALSE, alpha=0.5) +
  geom_ribbon(aes(x=Date, 
                  ymin = DecesAttendus - 1.96*DecesAttendusSE, 
                  ymax = DecesAttendus + 1.96*DecesAttendusSE, 
                  fill="Sur/sous-mortalité normale"), alpha=0.1,fill="blue", show.legend = FALSE) +
  theme_minimal()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, face = "italic"),
        legend.position = "top") +
  labs(x = NULL,
       y = NULL)
ggsave("gTimeSeriesPoissonZoom.png",plot=gTimeSeriesPoissonZoom, bg="white", height = 7, width =10)
print(gTimeSeriesPoissonZoom)

dbMerge$WeekNum<-substr(ISOweek(dbMerge$Date), 6, 8) 
dbMerge$Annee<-substr(ISOweek(dbMerge$Date), 1, 4)
dbMerge$AnneeNum<-as.numeric(dbMerge$Annee)
dbMergeBis<- dbMerge %>%
   mutate(ExcesMortalite=DECES-DecesAttendus) %>%
   filter(AnneeNum>=2014) %>%
   group_by(Annee) %>%
   mutate(ExcesMortaliteCumsum=cumsum(ExcesMortalite)) %>%
   ungroup() %>%
   subset(select = c(Date,ISO.week,ExcesMortaliteCumsum,WeekNum,Annee,AnneeNum))

graph.ExcesMortalite<-ggplot() +
   geom_line(data = filter(dbMergeBis, dbMergeBis$AnneeNum>2019),
             aes(x=WeekNum, y=ExcesMortaliteCumsum, group = Annee, color = Annee),
             show.legend = TRUE,
             na.rm=TRUE,
             size=2)  +
   geom_line(data = filter(dbMergeBis, dbMergeBis$AnneeNum<=2019),
             aes(x=WeekNum, y=ExcesMortaliteCumsum, group = Annee ),
             show.legend = FALSE,
             na.rm=TRUE,
             size = 0.5,
             color = "grey")  +
   geom_smooth(data = filter(dbMergeBis, dbMergeBis$AnneeNum<=2019),
               aes(x=WeekNum, y=ExcesMortaliteCumsum, group = 1),
               show.legend = FALSE,
               na.rm=TRUE,
               color = "lightblue",
               size = 1.25,
               span=0.1)  +
    scale_color_viridis_d() +
   scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000,60000))+
   scale_x_discrete(breaks = c("W05","W10","W15","W20","W25","W30", "W35","W40","W45","W50") )+
   geom_hline( yintercept = 0)  +
  theme_minimal()+
  theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 10),
         plot.caption = element_text(size = 10, face = "italic"),
         legend.position = "top") +
   labs(x = NULL,
        y = NULL,
        color = NULL,
        title = "Excés de mortalité en France de 2020 à 2023",
        subtitle = "La courbe en bleu-ciel représente l'excés de mortalité moyen sur la période 2014-2019, principlement lié aux épidémies grippales",
        caption = "Sources : Santé Publique France, Insee et Réseau Sentinelles.\nCalculs et erreurs : P. Aldama / @paldama")



print(graph.ExcesMortalite)
ggsave("grExcesMortalite.png",plot = graph.ExcesMortalite, bg = "white", width=12)

gMortalite<-ggarrange(gTimeSeriesPoisson, gTimeSeriesPoissonZoom, graph.ExcesMortalite,nrow=3)
ggsave("gMortalite.png",plot=gMortalite,bg="white",height=12, width=9)
