
library(tidyverse)
library(dplyr)
library(ggplot2)
library(frequency)
library(tidyquant)  
library(ggpubr)
library(ISOweek)
library(readr)
library(zoo)

setwd("~/Documents/Covid")

############################################################################################################################################
# Importation et preparation des donnees
############################################################################################################################################

# Data from SI-VIC 
url <- "https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"
dest <- "~/Documents/covid/sivic_donneeshospit.csv"
sivic<- download.file(url,dest)

# Data from SI-DEP
url <- "https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c"
dest <- "~/Documents/covid/sidep_donneesincidence.csv"
sivic<- download.file(url,dest)


# Charge les donnees hospitalieres par classe d'age
sivic_donneeshospit <- read_delim("~/Documents/Covid/sivic_donneeshospit.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
DataHospit<- sivic_donneeshospit %>%
  as.data.frame() %>%
  subset(select = -c(reg,HospConv,SSR_USLD,autres)) %>% # supprime les colonnes inutiles
  group_by(cl_age90,jour) %>% # groupe le dataframe par classe age et date
  summarise(across(everything(), sum)) # somme par région (en ligne)

# Charge les donnees incidence
sidep_donneesincidence <- read_delim("~/Documents/Covid/sidep_donneesincidence.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)
DataIncidence<-sidep_donneesincidence %>%
  as.data.frame() %>%
  group_by(cl_age90,jour) # groupe le dataframe par classe age et date

# Croise et fusionne les deux bases de donnees
db <- merge(DataHospit,DataIncidence,by=c("jour","cl_age90"))
db$TxPos<- db$P/db$T*100

# Recode la serie de classe d'age
db$cl_age90<-as.numeric(db$cl_age90)
db$classe_age <- case_when(
  db$cl_age90 == 9  ~ "0-9 ans",
  db$cl_age90 == 19  ~ "10-19 ans",
  db$cl_age90 == 29  ~ "20-29 ans",
  db$cl_age90 == 39  ~ "30-39 ans",
  db$cl_age90 == 49  ~ "40-49 ans",
  db$cl_age90 == 59  ~ "50-59 ans",
  db$cl_age90 == 69  ~ "60-69 ans",
  db$cl_age90 == 79  ~ "70-79 ans",
  db$cl_age90 == 89  ~ "80-89 ans",
  db$cl_age90 == 0  ~ "Total",
  TRUE ~ "Plus de 90 ans" )

# Calcul taux d'incidence
db<-arrange(db,classe_age,jour) %>%
mutate(TauxIncidence=round(rollapply(P,7,sum,align='right',fill=NA)/pop*100000))%>%
mutate(Reffectif= TauxIncidence/lag(TauxIncidence,7))


db<-db %>%
  filter(cl_age90 != 0)  # filtre les données France entière

DateDebutGraphique <- as.Date("2020-07-01")

# Graphique
graph.CasCovid<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=P) ) +
  geom_col(colour = "light grey") +
  geom_smooth( span = 0.1 , se = FALSE)+
  facet_wrap(.~classe_age, nrow = 5, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Nombre de cas positif au Covid19 par classes d'âge",
        subtitle = "Tendance LOESS en bleu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.CasCovid)
ggsave("gCasCovid.png", plot=graph.CasCovid, height = 8 , width = 12)

# Graphique
graph.incidence<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                       aes(x=jour, y=TauxIncidence) ) +
  geom_point(aes(group = classe_age, color = classe_age),
             size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = 0.1 ,
              se = FALSE)+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Taux d'incidence du Covid19 par classes d'âge",
        subtitle = "Nombre de cas positifs sur 7 jours glissants pour 100 000 habitants",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.incidence)
ggsave("gIncidence.png", plot=graph.incidence, height = 8 , width = 12)

# Graphique
graph.Reffectif<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=Reffectif) ) +
  geom_point(aes(group = classe_age, color = classe_age),
              size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = 0.1 ,
              se = FALSE)+
  geom_hline(yintercept =1, colour = "black")+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Taux de reproduction effectif par classe d'âge",
        subtitle = "Ratio du nombre de cas sur 7 jours entre j et j-7",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.Reffectif)
ggsave("gReffectif.png", plot=graph.Reffectif, height = 8 , width = 12)

# Graphique
graph.TauxPosSmooth<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=TxPos) ) +
  geom_point(size = 0.1) +
  geom_smooth( span = 0.1 , se = TRUE)+
  facet_wrap(.~classe_age, nrow = 5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Taux de positivité au Covid19 par classe d'âge",
        subtitle = "En %, tendance LOESS en bleu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.TauxPosSmooth)
ggsave("gTauxPos.png", plot=graph.TauxPosSmooth, height = 8 , width = 12)


# Graphique
graph.HospitLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=hosp) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = 0.25, se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
  labs( y = "Effectis",
        x = NULL ,
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.HospitLog)
ggsave("gHospitLog.png", plot=graph.HospitLog, height = 8 , width = 12)

graph.Hospit<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=hosp, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.Hospit)
ggsave("gHospit.png", plot=graph.Hospit, height = 8 , width = 12)


graph.ReaLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                     aes(x=jour, y=rea) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = 0.25, se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
  labs( y = "Effectis",
        x = NULL ,
        title = "Nombre de personnes en soins critiques pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.ReaLog)
ggsave("gReaLog.png", plot=graph.ReaLog, height = 8 , width = 12)

graph.rea<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                     aes(x=jour, y=rea, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.rea)
ggsave("gRea.png", plot=graph.rea, height = 8 , width = 12)
