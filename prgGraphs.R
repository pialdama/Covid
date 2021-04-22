library(tidyverse)
library(dplyr)
library(ggplot2)
library(frequency)
library(tidyquant)  


##############################################################################################
##############################################################################################
# Importe les données historiques des décès quotidiens et détaillées pour 2018-2021
##############################################################################################
##############################################################################################

## Import historical data from Insee: 
db<-read.csv("~/Documents/Covid/HistoricalDataMortality.csv",sep=";")
db <- filter(db, db$JOUR!="MM" & db$DECES!="NA")
db$Date<-as.Date(paste(db$JOUR, db$MOIS2,db$ANNEE_1968_2019, sep = "/"),"%d/%m/%Y")
db <- subset(db, select = -c(JOUR,MOIS2,ANNEE_1968_2019) ) # delete useless columns 
DecesFM <- db %>%
   filter(db$CHAMP=="FM") %>% 
   subset(select = -c(CHAMP) )
DecesFE <- db %>%
   filter(db$CHAMP=="FE") %>% 
   subset(select = -c(CHAMP) )   

# Import data for 2018 and 2019 from Insee
db2018 <- read.csv("~/Documents/Covid/DC_2018_det.csv",sep=";")
db2018$Date <- as.Date(paste(db2018$JDEC, db2018$MDEC,db2018$ADEC, sep = "/"),"%d/%m/%Y")
db2018$CHAMP <- case_when(
   db2018$DEPDEC=="2A" ~ "FM",
   db2018$DEPDEC=="2B" ~ "FM",
   as.numeric(db2018$DEPDEC)<900 ~"FM" ,
   TRUE ~ "OM")
db2018$age<-db2018$ADEC-db2018$ANAIS

db2019 <- read.csv("~/Documents/Covid/DC_2019_det.csv",sep=";")
db2019$Date <- as.Date(paste(db2019$JDEC, db2019$MDEC,db2019$ADEC, sep = "/"),"%d/%m/%Y")
db2019$CHAMP <- case_when(
   db2019$DEPDEC=="2A" ~ "FM",
   db2019$DEPDEC=="2B" ~ "FM",
   as.numeric(db2019$DEPDEC)<900 ~"FM" ,
   TRUE ~ "OM")
db2019$age<-db2019$ADEC-db2019$ANAIS

# Import data for 2020-2021 from Insee
db20202021 <- read.csv("~/Documents/Covid/DC_20202021_det.csv",sep=";")
db20202021$Date <- as.Date(paste(db20202021$JDEC, db20202021$MDEC,db20202021$ADEC, sep = "/"),"%d/%m/%Y")
db20202021$CHAMP <- case_when(
   db20202021$DEPDEC=="2A" ~ "FM",
   db20202021$DEPDEC=="2B" ~ "FM",
   as.numeric(db20202021$DEPDEC)<900 ~"FM" ,
   TRUE ~ "OM")
db20202021$age<-db20202021$ADEC-db20202021$ANAIS

# Sépare les données 2020 et 2021
db2020 <- filter(db20202021,db20202021$ADEC==2020)
db2021 <- filter(db20202021,db20202021$ADEC==2021)

# Bind un dataframe complet pour 2018-2020
db1820<-rbind.data.frame(db2018,db2019,db2020)
db1820$AnneeDeces<-as.character(db1820$ADEC)
db1820$MoisDeces<-format(as.Date(db1820$Date, format="%d/%m/%Y"),"(%m) %B")
db1820$ADEC<-as.character(db1820$ADEC)
db1820$SEXE<-fct_recode(db1820$SEXE,
                "Femme" = "F",
                "Homme" = "M")

##############################################################################################
##############################################################################################
# Redressement des series Insee
##############################################################################################
##############################################################################################

# Charge les données pour le redressement
dbRedress <- read.csv("~/Documents/Covid/RedressementDistance.csv",sep=";",dec=",")
# Normalise le coefficient moyen et la variable de distance
dbRedress$dist<-dbRedress$dist-10
dbRedress$coefNorm <- dbRedress$coef - c(rep(1))
# Estime un modèle polynomial inverse
model.Redressement <- lm(coefNorm ~ I((dist)^-1) + I((dist)^-2) + I((dist)^-3) + + I((dist)^-4) -1,
                         data = dbRedress)
summary(model.Redressement)
dbRedress$coefsmooth<-predict(model.Redressement)
# plot les données et le modèle
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
##############################################################################################
# Merge les données quotidiennes historiques avec les fichiers 2020/21
##############################################################################################
##############################################################################################

# Creer des datafram temporaires
DecesFM2021 <- db20202021 %>%
   filter(db20202021$CHAMP=="FM") %>%
   group_by(Date) %>%
   summarise(DECES = n())
DecesFE2021 <- db20202021 %>% 
   group_by(Date) %>%
   summarise(DECES = n())

# Merge historical dataset with data for 2020 and 2021 and remove intermediate data.frame
DecesFE<-bind_rows(DecesFE,DecesFE2021)
DecesFM<-bind_rows(DecesFM,DecesFM2021)
rm(DecesFE2021,DecesFM2021)

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
# correction des séries de décès
DecesFE$DECES<- DecesFE$DECES*DecesFE$coefredress 
DecesFM$DECES<- DecesFM$DECES*DecesFM$coefredress 


##############################################################################################
##############################################################################################
# Graphiques de répartition
##############################################################################################
##############################################################################################

gRepartitionAge<-ggplot(data=db1820,aes(x=age,y=after_stat(count),fill=SEXE)) + 
   geom_histogram(data=filter(db1820,db1820$ADEC==2020),
                  aes(x=age),
                  binwidth = 1,
                  size=1.2)+
   geom_freqpoly(data=filter(db1820,db1820$ADEC!=2020 & db1820$ADEC!=2021),
                 aes(y = after_stat(count) / (n_distinct(db1820$ADEC)-1),linetype=SEXE, group=SEXE),
                 position = "stack",
                 binwidth = 1,
                 size=0.5)+
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   labs(x = "Age",
     y = "Nombre de décès par âge",
     title = "Répartition des décès par âge et par sexe en 2020",
     subtitle = "En noir, la moyenne 2018-2019 du nombre de décès par âge et par sexe.",
     caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gRepartitionAge.png", plot=gRepartitionAge, height = 5 , width = 12)


gRepartitionAgeSexe<-ggplot(data=db1820,aes(x=age,after_stat(count))) +  
   geom_histogram(data=filter(db1820,db1820$ADEC==2020),aes(x=age,fill=MoisDeces), binwidth = 1,na.rm=TRUE,size=1.2)+
   geom_freqpoly(data=filter(db1820,db1820$ADEC!=2020),
                 aes(y = ..count.. / (n_distinct(db1820$ADEC)-1), linetype=MoisDeces, group=MoisDeces), 
                 position  = "stack", 
                 binwidth = 1,
                 size=0.5)+   
   facet_grid(rows=vars(SEXE))+
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   labs(x = "Age",
        y = "Effectifs",
        title = "Répartition des décès par âge et selon le sexe en 2020",
        subtitle = "En noir, la moyenne 2018-2019.",
        caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gRepartitionAgeSexe.png", plot=gRepartitionAgeSexe, height = 5 , width = 10)


gRepartitionAgeSexeMois<-ggplot(data=db1820,aes(x=age,after_stat(count))) +  
   geom_histogram(data=filter(db1820,db1820$ADEC==2020),
                  aes(x=age, color=MoisDeces), 
                  binwidth = 1,
                  na.rm=TRUE,
                  size=1.2)+
   geom_freqpoly(data=filter(db1820,db1820$ADEC!=2020),
                 aes(y = ..count.. / (n_distinct(db1820$ADEC)-1)), 
                 color = "black", 
                 binwidth = 1,
                 size=0.5,
                 show.legend = NA)+   
   facet_grid(MoisDeces~SEXE)+
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   labs(x = "Age",
        y = "Effectifs",
        title = "Répartition des décès par âge et selon le sexe et le mois en 2020",
        subtitle = "En noir, la moyenne 2018-2019.",
        caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gRepartitionAgeSexeMois.png", plot=gRepartitionAgeSexeMois, height = 12 , width = 10)


############################################################################################################
############################################################################################################
# Graphiques en times series pour 2018-2021 par classe d'âge
############################################################################################################
############################################################################################################

db1820<-rbind.data.frame(db2018,db2019,db2020,db2021)
db1820$JourMois<-format(as.Date(db1820$Date, format="%d/%m/%Y"),"%m/%d")
db1820$JourMois<-format(as.Date(db1820$Date, format="%d/%m/%Y"),"%m/%d")
db1820$JourMoisBis<-format(as.Date(db1820$Date, format="%d/%m/%Y"),"%b %d")
db1820$Annee<-format(as.Date(db1820$Date, format="%d/%m/%Y"),"%Y")

db1820$ClasseAge <- case_when(
   db1820$age >= 0  & db1820$age < 20  ~ "0 à 19 ans",
   db1820$age >= 19 & db1820$age < 40  ~ "20 à 39 ans",
   db1820$age >= 40 & db1820$age < 60  ~ "40 à 59 ans",
   db1820$age >= 60 & db1820$age < 65  ~ "60 à 64 ans",
   db1820$age >= 65 & db1820$age < 70  ~ "65 à 69 ans",
   db1820$age >= 70 & db1820$age < 75  ~ "70 à 75 ans",
   db1820$age >= 75 & db1820$age < 80  ~ "75 à 79 ans",
   db1820$age >= 80 & db1820$age < 90  ~ "80 à 89 ans",
   TRUE ~ "Plus de 90 ans")

db1820ClasseAge<-db1820 %>%
   group_by(Date,Annee,JourMois,ClasseAge) %>%
   summarise(DECES=n())

gClasseAge<-ggplot(data=db1820ClasseAge,aes(x=JourMois, y=DECES)) +
   geom_ma(aes(color = Annee, group = Annee), ma_fun = SMA, n = 7,size = 0.6,linetype = "solid") +
   scale_colour_manual(values = c("darkgray", "lightgray", "blue","red")) +
   stat_smooth(data=subset(db1820ClasseAge,Annee<2020),
               aes(x=JourMois, y=DECES, group=ClasseAge),
               method="glm",
               formula = y~splines::bs(x,6),
               size = 0.6,
               color="black") +
   facet_grid(ClasseAge~.,scales = "free") +
   theme_minimal() +
   theme(plot.title = element_text(size = 16, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   scale_x_discrete("Date",
                    breaks = c("01/01","02/01","03/01","04/01","05/01","06/01","07/01","08/01","09/01","10/01","11/01","12/01"),
                    labels = c("Jan","Fév","Mar","Avr","Mai","Jui","Jul","Aoû","Sep","Oct","Nov","Déc")) +
   labs( y = "Effectifs",
         title = "Décès quotidiens par classes d'âge, de 2018 à 2021",
         subtitle = "Moyenne glissante sur 7 jours. Tendance 2018-2019 en noir. France métropolitaine.",
         caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gClasseAge.png", plot=gClasseAge, height = 10 , width = 1.2*10)


db1820ClasseAgeSexe<-db1820 %>%
   group_by(Date,JourMois,Annee,ClasseAge,SEXE) %>%
   summarise(DECES=n())

gClasseAgeSexe<-ggplot(data=db1820ClasseAgeSexe,aes(x=JourMois, y=DECES)) +
   geom_ma(aes(color = Annee, group = Annee), ma_fun = SMA, n = 7,size = 0.6,linetype = "solid") +
   scale_colour_manual(values = c("darkgray", "lightgray", "blue","red"))  +
   stat_smooth(data=subset(db1820ClasseAgeSexe,Annee<2020),
               aes(x=JourMois, y=DECES, group = ClasseAge),
               method="glm",
               formula = y~splines::bs(x,6),
               size = 0.6,
               color="black") +
   facet_grid(ClasseAge~SEXE,scales = "free") +
   theme_minimal() +
   theme(plot.title = element_text(size = 16, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   scale_x_discrete("Date",
                    breaks = c("01/01","02/01","03/01","04/01","05/01","06/01","07/01","08/01","09/01","10/01","11/01","12/01"),
                    labels = c("Jan","Fév","Mar","Avr","Mai","Jui","Jul","Aoû","Sep","Oct","Nov","Déc")) +
   labs( y = "Effectifs",
         title = "Décès quotidiens par classes d'âge et selon le sexe, de 2018 à 2021",
         subtitle = "Moyenne glissante sur 7 jours. Tendance 2018-2019 en noir. France métropolitaine.",
         caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gClasseAgeSexe.png",plot=gClasseAgeSexe, height = 10 , width = 1.2*10)

gTimeSeriesClasseAgeSexe<-ggplot(data=db1820ClasseAgeSexe) +
   geom_ma(aes(x=Date, y=DECES), ma_fun = SMA,n=7 ,size = 0.6,color="blue", linetype="solid") +
   # stat_smooth(data=subset(db1820ClasseAgeSexe,Annee<2020),
   #             aes(x=Date, y=DECES),
   #             method="glm",
   #             method.args = list(family = "poisson"(link="log")),
   #             formula = y~x+cos(2*x*pi/365.25)+sin(2*x*pi/365.25),
   #             size = 0.6,
   #             color="black") +
   scale_color_brewer("Annee", palette = "Set2")+
   facet_grid(ClasseAge~SEXE,scales = "free") +
   theme_minimal() +
   theme(plot.title = element_text(size = 16, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   labs( y = "Effectifs",
         title = "Décès quotidiens par classes d'âge, de 2018 à 2021",
         subtitle = "Moyenne glissante sur 7 jours. Tendance 2018-2019 en noir. France métropolitaine.",
         caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gTimeSeriesClasseAgeSexe.png",plot=gTimeSeriesClasseAgeSexe, height = 10 , width = 1.2*10)


############################################################################################################
############################################################################################################
# Graphique simple : comparaison à la tendance 2014/2019 
############################################################################################################
############################################################################################################


DecesFM$JourMois<-format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%m/%d")
DecesFM$JourMoisBis<-format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%b %d")
DecesFM$Annee<-format(as.Date(DecesFM$Date, format="%d/%m/%Y"),"%Y")

gTimeSeriesTransversal<-ggplot(data=filter(DecesFM,DecesFM$Annee>=2000),aes(x=JourMois, y=DECES)) +
      geom_ma(data=filter(DecesFM,DecesFM$Annee>=2000 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
           aes(group=Annee),
           ma_fun = SMA, 
           n = 7,
           linetype = "solid",
           color="gray",
           size=0.2) +
   geom_ma(data=filter(DecesFM,DecesFM$Annee>=2020),
           aes(color = Annee, group = Annee),
           ma_fun = SMA,
           n = 7,
           linetype = "solid",
           size = 1) +
   scale_colour_manual(values = c("blue","red"))  +
   geom_smooth(aes(group=1),
               method="lm",
               formula = y ~ splines::bs(x,6),
               color="black",
               se=FALSE,
               data=filter(DecesFM,DecesFM$Annee>=2016 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
               show.legend = FALSE)   +
   geom_smooth(aes(group=1),
               method="lm",
               formula = y ~ splines::bs(x,6),
               color="black",
               linetype = "dashed",
               se=FALSE,
               data=filter(DecesFM,DecesFM$Annee>=2010 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
               show.legend = FALSE)   +
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   scale_x_discrete(breaks = c("01/01","02/01","03/01","04/01","05/01","06/01","07/01","08/01","09/01","10/01","11/01","12/01"),
                    labels = c("Jan","Fév","Mar","Avr","Mai","Jui","Jul","Aoû","Sep","Oct","Nov","Déc")) +
   labs(y=NULL, x= NULL,
        colour= "Année",
        title = "Décès quotidiens en 2020 et 2021 en France métropolitaine",
        subtitle = "En noir, la tendance 2016–2019 et en pointillé, la tendance décennale 2010–2019. Moyenne mobile 7 jours.",
        caption = " Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gTimeSeriesTransversal.png",plot=gTimeSeriesTransversal, height = 4 , width =8)


############################################################################################################
############################################################################################################
# Reproduction graphiques avec décès attendus en fonction d'une quasi-Poisson
############################################################################################################
############################################################################################################

# Estime le modèle quasi-Poisson de 2014 à 2019
DecesFM$t<-c(1:nrow(DecesFM))
DecesFM$cose<-cos(2*pi*DecesFM$t/365.25)
DecesFM$sine<-sin(2*pi*DecesFM$t/365.25)
ModelPoisson<-glm( 
   DECES ~ t  + cose + sine ,
   data = filter(DecesFM,DecesFM$Annee>=2014 & DecesFM$Annee < 2020),
   family = quasipoisson(link="log"))
summary(ModelPoisson)
pseudoR2<- 1-(ModelPoisson$deviance/ModelPoisson$null.deviance)
pseudoR2
phiPoisson<-summary(ModelPoisson)$deviance / summary(ModelPoisson)$df.residual

# Prediction et écart-type ajusté de la surdispersion
DecesFM$DecesAttendus<-predict.glm(object=ModelPoisson,
                                   newdata=DecesFM,
                                   type="response" )
DecesFM$se<-sqrt(phiPoisson*DecesFM$DecesAttendus)


# Plot les données en time series
gTimeSeriesPoisson<-ggplot(data=filter(DecesFM,DecesFM$Annee>=2014)) +
   geom_ribbon(aes(x=Date, ymin = DecesAttendus - 1.96*se, ymax = DecesAttendus + 1.96*se), fill = "blue", alpha=0.1) +
   geom_ma(aes(x=Date, y=DECES), ma_fun = SMA, n = 7, size=0.5, linetype = "solid", color = "black") +
   geom_line(aes(x=Date, y=DecesAttendus),colour="blue",size=0.5) +
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   labs(x = NULL,
        y = NULL,
        title = "Décès quotidiens de 2014 à 2021 en France métropolitaine",
        subtitle = "En bleu, la tendance estimée de 2014 à 2019, par un modèle Quasi-Poisson. Moyenne glissante sur 7 jours.",
        caption = "Source : Insee, fichier des décès individuels. Calculs : @paldama.")
ggsave("gTimeSeriesPoisson.png",plot=gTimeSeriesPoisson, height = 4, width =14)
print(gTimeSeriesPoisson)
