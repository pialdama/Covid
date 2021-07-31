library(tidyverse)
library(dplyr)
library(ggplot2)
library(frequency)
library(tidyquant)  
library(ggpubr)
library(ISOweek)

setwd("~/Documents/Covid")

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

gRepartitionAge2018<-print(ggplot(data=db1820,aes(x=age,y=after_stat(count),fill=SEXE)) + 
  geom_histogram(data=filter(db1820,db1820$ADEC==2018),
                 aes(x=age),
                 binwidth = 1,
                 size=1.2)+
  geom_freqpoly(data=filter(db1820,db1820$ADEC!=2020 & db1820$ADEC!=2021),
                aes(y = after_stat(count) / (n_distinct(db1820$ADEC)-1),linetype=SEXE, group=SEXE),
                position = "stack",
                binwidth = 1,
                size=0.5)+
  theme_minimal() +
    theme(plot.title = element_text(size = 11)) +  labs(x = NULL, y = NULL) + 
  labs(title = "En 2018 et moyenne 2018-2019 en noir"))


gRepartitionAge2019<-print(ggplot(data=db1820,aes(x=age,y=after_stat(count),fill=SEXE)) + 
  geom_histogram(data=filter(db1820,db1820$ADEC==2019),
                 aes(x=age),
                 binwidth = 1,
                 size=1.2)+
  geom_freqpoly(data=filter(db1820,db1820$ADEC!=2020 & db1820$ADEC!=2021),
                aes(y = after_stat(count) / (n_distinct(db1820$ADEC)-1),linetype=SEXE, group=SEXE),
                position = "stack",
                binwidth = 1,
                size=0.5)+
  theme_minimal() +
    theme(plot.title = element_text(size = 11)) +  labs(x = NULL, y = NULL) + 
  labs(title = "En 2019 et moyenne 2018-2019 en noir"))

gRepartitionAge2020<-print(ggplot(data=db1820,aes(x=age,y=after_stat(count),fill=SEXE)) + 
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
   theme(plot.title = element_text(size = 11)) +  labs(x = NULL, y = NULL) + 
     labs(title = "En 2020 et moyenne 2018-2019 en noir"))

figRepartitionAge<-ggarrange(gRepartitionAge2018, gRepartitionAge2019, gRepartitionAge2020 
                             + rremove("x.text") + font("x.text", size = 10),
                             common.legend = TRUE, legend="bottom", ncol = 1, nrow = 3) %>%
  annotate_figure(figRepartitionAge,
                top = text_grob("Répartition des décès par âge et selon le sexe en France",
                                hjust = 0.7,
                                vjust = 0.5,
                                face = "bold", 
                                size = 14),
                bottom = text_grob("Source: Insee, état civil, fichiers des décès individuels. Graphique : P. Aldama @paldama.",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Nombre de décès par âge", color = "black", rot = 90) )
ggsave("gFigRepartitionAgeSexe.png", plot=figRepartitionAge, height = 10, width = 8)


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
               se=FALSE,
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
   # stat_summary(data=subset(db1820ClasseAgeSexe,Annee<2020),
   #              aes(x=JourMois, y=DECES, group = ClasseAge), 
   #              fun=mean, colour="black", 
   #              geom="line",
   #              group=1) +
   stat_smooth(data=subset(db1820ClasseAgeSexe,Annee<2020),
               aes(x=JourMois, y=DECES, group = ClasseAge),
               method="glm",
               formula = y~splines::bs(x,6),
               se=FALSE,
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
   # geom_ribbon(data=filter(DecesFM,DecesFM$Annee>=2000 & DecesFM$Annee!=2003  & DecesFM$Annee!=2020 & DecesFM$Annee!=2021 ),
   #             aes(ymin = min(DECES), ymax = max(DECES)), 
   #             alpha=.5, fill="gray") +
   geom_line(data=filter(DecesFM,DecesFM$Annee>=2000 & DecesFM$Annee!=2003  & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
           aes(group=Annee),
           linetype = "solid",
           color="gray",
           size=0.2) +
   geom_line(data=filter(DecesFM,DecesFM$Annee==2003),
             aes(color=Annee,group = Annee),
             linetype = "solid",
             size=0.5) +
   geom_line(data=filter(DecesFM,DecesFM$Annee>=2020),
           aes(color = Annee, group = Annee),
           linetype = "solid",
           size = 1) +
   scale_colour_manual(values = c("orange","blue","red"))  +
   stat_summary(data=filter(DecesFM,DecesFM$Annee>=2016 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
                aes(y = DECES,group=1), 
                fun=mean, colour="black", 
                geom="line",
                group=1) +
   # stat_summary(data=filter(DecesFM,DecesFM$Annee>=2010 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
   #              aes(y = DECES,group=1), 
   #              fun=mean, colour="black", 
   #              geom="line",
   #              linetype="dashed",
   #              group=1) +
   # geom_smooth(aes(group=1),
   #             method="lm",
   #             formula = y ~ splines::bs(x,6),
   #             color="black",
   #             se=FALSE,
   #             data=filter(DecesFM,DecesFM$Annee>=2016 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
   #             show.legend = FALSE)   +
   # geom_smooth(aes(group=1),
   #             method="lm",
   #             formula = y ~ splines::bs(x,6),
   #             color="black",
   #             linetype = "dashed",
   #             se=FALSE,
   #             data=filter(DecesFM,DecesFM$Annee>=2010 & DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
   #             show.legend = FALSE)   +
   coord_polar() +
   scale_y_continuous(limits = c(0,3655),
                      breaks = c(0,500,1000,1500,2000,2500),
                      minor_breaks = NULL,
                      expand=expansion(add=c(0,-1000)))+
   theme_minimal() +
   theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 9)) +
   scale_x_discrete(breaks = c("01/01","02/01","03/01","04/01","05/01","06/01","07/01","08/01","09/01","10/01","11/01","12/01"),
                    labels = c("Jan","Fév","Mar","Avr","Mai","Jui","Jul","Aoû","Sep","Oct","Nov","Déc"),na.translate = FALSE) +
   labs(y=NULL, x= NULL,
        colour= "Année",
        title = "Décès quotidiens depuis 2000 en France métropolitaine",
        subtitle = "En noir, la moyenne 2016–2019.",
        caption = " Source : Insee, fichier des décès individuels. Graphique : @paldama, inspiré de B. Coulmont.")
ggsave("gTimeSeriesTransversal.png",plot=gTimeSeriesTransversal, height = 7 , width = 7)


# Graphique depuis 1968

# Détrending de la série de décès
DecesFM$t<-c(1:nrow(DecesFM))
DecesFM$cose<-cos(2*pi*DecesFM$t/365.25)
DecesFM$sine<-sin(2*pi*DecesFM$t/365.25)
ModelOLS<-lm(
   I(log(DECES)) ~ t + I(t^2) + I(t^3)  + cose + sine ,
   data = filter(DecesFM, DecesFM$Annee < 2020))
summary(ModelOLS)
DecesFM$Fit<-exp(predict.lm(object = ModelOLS,
                        newdata = DecesFM,
                        type = "response" ))
DecesFM$ExcesDeces<-(DecesFM$DECES/DecesFM$Fit-1)*100
DecesFM$ExcesDecesAbs<-(DecesFM$DECES-DecesFM$Fit)
qplot(DecesFM$Date,DecesFM$ExcesDeces) + geom_line() 

gTimeSeriesLongTerme<-ggplot(data=filter(DecesFM,DecesFM$Annee>=1968),aes(x=JourMois, y=ExcesDeces)) +
   geom_line(data=filter(DecesFM, DecesFM$Annee!=2020 & DecesFM$Annee!=2021),
           aes(group=Annee),
           linetype = "solid",
           color="gray",
           size=0.2) +
   geom_line(data=filter(DecesFM, DecesFM$Annee>=1969 & DecesFM$Annee<=1970),
             aes(color = Annee, group = Annee),
             linetype = "solid",
             size = 0.5) +
   geom_line(data=filter(DecesFM, DecesFM$Annee==2003),
             aes(color = Annee, group = Annee),
             linetype = "solid",
             size = 0.5) +
   geom_line(data=filter(DecesFM, DecesFM$Annee>=2020 ),
           aes(color = Annee, group = Annee),
           linetype = "solid",
           size = 0.7) +
   scale_colour_manual(values = c("brown","orange2","green2","blue","red"))  +
    geom_hline(yintercept = 0) +
   theme_minimal() +
   theme(plot.title = element_text(size = 12, face = "bold"),
         plot.subtitle = element_text(size = 7)) +
   scale_x_discrete(breaks = c("01/01","02/01","03/01","04/01","05/01","06/01","07/01","08/01","09/01","10/01","11/01","12/01"),
                    labels = c("Jan","Fév","Mar","Avr","Mai","Jui","Jul","Aoû","Sep","Oct","Nov","Déc")) +
   #coord_polar("x") +
   labs(y=NULL, x= NULL,
        colour= "Année",
        title = "Excès de mortalité en France métropolitaine depuis 1968",
        subtitle = "En pourcentage de la mortalité attendue, calculée à partir d'une régression linéaire avec tendance polynomiale et composante cyclique.",
        caption = " Source : Insee, fichier des décès individuels. Calculs : @paldama")
ggsave("gTimeSeriesLongTerme.png",plot=gTimeSeriesLongTerme, height = 4 , width =8)


##############################################################################################
##############################################################################################
# Aggregation hebdo et estimation de la tendance hors-épidémie
##############################################################################################
##############################################################################################

# Importation des données de surveillance épidémique (Grippe, réseau Sentinelles)
Sentinelles<-read.csv("~/Documents/covid/SentinellesIncidenceGrippe.csv",skip = 1)
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
view(Sentinelles)

SentinellesMerge<-Sentinelles%>%
  select(ISO.week,inc100)


# Aggregation hebdo de la série de décès quotidiens
DecesFEhebdo<-DecesFE %>%
   tq_transmute(select     = DECES,
                mutate_fun = apply.weekly,
                FUN        = mean)
DecesFEhebdo$DECES<-DecesFEhebdo$DECES*7 #utilise la moyenne pour éviter le biais de semaine incomplète
DecesFEhebdo$ISO.week<-date2ISOweek(DecesFEhebdo$Date)
DecesFEhebdo$DECES<-round(DecesFEhebdo$DECES)


# Merge des datasets
dbMerge<-left_join(DecesFEhebdo,SentinellesMerge,by = "ISO.week")
dbMergeNAs <- dbMerge[rowSums(is.na(dbMerge)) > 0,]
dbMerge<-drop_na(dbMerge)
dbMerge$Annee<-format(as.Date(dbMerge$Date, format="%d/%m/%Y"),"%Y")


# Estime le modèle quasi-Poisson de 2014 à 2019
dbMerge$t<-c(1:nrow(dbMerge))
dbMerge$cose<-cos(2*pi*dbMerge$t/52)
dbMerge$sine<-sin(2*pi*dbMerge$t/52)
ModelPoissonBis<-glm( 
  DECES ~ t + cose + sine + inc100:Annee,
  data = filter(dbMerge,dbMerge$Annee>=2014 & dbMerge$Annee=<2019),
  family = quasipoisson(link="log"),
  control = list(maxit = 500))

summary(ModelPoissonBis)
pseudoR2<- 1-(ModelPoissonBis$deviance/ModelPoissonBis$null.deviance)
pseudoR2
phiPoisson<-summary(ModelPoissonBis)$deviance / summary(ModelPoissonBis)$df.residual

# Prediction et écart-type ajusté de la surdispersion
# Sauvegarde les coefficients
coefIntercept<-ModelPoissonBis$coefficients[[ 1 ]]
coefTrend<-ModelPoissonBis$coefficients[[ 2 ]]
coefCos<-ModelPoissonBis$coefficients[[ 3 ]] 
coefSin<-ModelPoissonBis$coefficients[[ 4 ]] 
dbMerge$DecesAttendus <- exp((coefIntercept + coefTrend*dbMerge$t + coefCos*dbMerge$cose + coefSin*dbMerge$sine))
dbMerge$DecesAttendusSE <- sqrt(phiPoisson*dbMerge$DecesAttendus )


# Plot les données en time series
gTimeSeriesPoisson<-ggplot(data=filter(dbMerge,dbMerge$Annee>=2014)) +
  geom_line(aes(x=Date, y=DECES, color = "obs"),size=0.5) +
  geom_line(aes(x=Date, y=DecesAttendus, color = "fit"),size=1) +
  scale_color_manual(name =" Nombre de décès ",
                     labels = c("attendus","observés"),
                     values = c("obs" = "black", "fit" = "blue"))+
  geom_ribbon(aes(x=Date, 
                  ymin = DecesAttendus - 1.96*DecesAttendusSE, 
                  ymax = DecesAttendus + 1.96*DecesAttendusSE, 
                  fill="Sur/sous-mortalité normale"), alpha=0.1) +
  scale_fill_manual("",values="blue") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10, face = "italic"),
        legend.position = "top") +
  labs(x = NULL,
        y = NULL,
        title = "Mortalité hebdomadaires en France",
        subtitle = "Nombre de décès observés et attendus en absence d'épidémie (grippale ou Covid19)",
        caption = "Sources : Insee, fichier des décès individuels et Réseau Sentinelles pour l'incidence de syndrômes grippaux. Calculs et erreurs : P. Aldama / @paldama")

ggsave("gTimeSeriesPoisson.png",plot=gTimeSeriesPoisson, height = 7, width =10)
print(gTimeSeriesPoisson)
