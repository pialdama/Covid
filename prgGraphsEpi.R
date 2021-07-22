
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

# Data from Our World in Data
url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
dest <- "~/Documents/covid/owid.csv"
owid <- download.file(url,dest)

# Data from SI-VIC 
url <- "https://www.data.gouv.fr/fr/datasets/r/08c18e08-6780-452d-9b8c-ae244ad529b3"
dest <- "~/Documents/covid/sivic_donneeshospit.csv"
sivic<- download.file(url,dest)

# Data from SI-DEP
url <- "https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c"
dest <- "~/Documents/covid/sidep_donneesincidence.csv"
sidep<- download.file(url,dest)

url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
dest <- "~/Documents/covid/sidep_donneesincidence_dep.csv"
sidep_dep<- download.file(url,dest)

# General data from SPF
url <- "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"
dest <- "~/Documents/covid/spf_donneesensemble.csv"
sivic<- download.file(url,dest)

# Data on vaccination from SPF
url <- "https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd"
dest <-   "~/Documents/covid/spf_donneesvaccins.csv"
vaccin <-download.file(url,dest)
 
# Charge les donnees SPF d'ensemble
dbspf <- read_csv("spf_donneesensemble.csv")

# Charge les donnees SPF sur les vaccins 
dbvaccin <- read_delim("spf_donneesvaccins.csv",
                       ";",
                       escape_double = FALSE,
                       trim_ws = TRUE)
dbvaccin$clage_vacsi<-as.numeric(dbvaccin$clage_vacsi)
dbvaccin$clage_vacsi <- case_when(
  dbvaccin$clage_vacsi == 4  ~ "0-4 ans",
  dbvaccin$clage_vacsi == 9  ~ "5-9 ans",
  dbvaccin$clage_vacsi == 11  ~ "10-11 ans",
  dbvaccin$clage_vacsi == 17  ~ "12-17 ans",
  dbvaccin$clage_vacsi == 24  ~ "18-24 ans",
  dbvaccin$clage_vacsi == 29  ~ "25-29 ans",
  dbvaccin$clage_vacsi == 39  ~ "30-39 ans",
  dbvaccin$clage_vacsi == 49  ~ "40-49 ans",
  dbvaccin$clage_vacsi == 59  ~ "50-59 ans",
  dbvaccin$clage_vacsi == 69  ~ "60-69 ans",
  dbvaccin$clage_vacsi == 74  ~ "70-74 ans",
  dbvaccin$clage_vacsi == 79  ~ "75-79 ans",
  dbvaccin$clage_vacsi == 80  ~ "80 ans et plus",
  TRUE ~ "Total")

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

#DateDebutGraphique <- as.Date("2020-07-01")
#SpanParam<-0.1
DateDebutGraphique <- as.Date("2021-03-01")
SpanParam<-0.3


# Graphique
graph.CasCovid<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=P) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth( span = SpanParam , se = FALSE)+
  facet_wrap(.~classe_age, nrow = 5, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de cas positif au Covid19 par classes d'âge",
        subtitle = "Tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.CasCovid)
ggsave("gCasCovid.png", plot=graph.CasCovid, height = 6, width = 10)

# Graphique
graph.incidence<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                       aes(x=jour, y=TauxIncidence) ) +
  geom_point(aes(group = classe_age, color = classe_age),
             size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = SpanParam ,
              se = FALSE)+
  geom_smooth(data = filter(dbspf,dbspf$date>DateDebutGraphique),
              aes(x=date, y=tx_incid),
              size = 1.5,
              span = SpanParam ,
              se = FALSE,
              color = "black")+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux d'incidence du Covid19 par classes d'âge",
        subtitle = "Nombre de cas positifs sur 7 jours glissants pour 100 000 habitants. En noir, le taux d'incidence pour la France entière.",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.incidence)
ggsave("gIncidence.png", plot=graph.incidence, height = 6, width = 10)

# Graphique
graph.Reffectif<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=Reffectif) ) +
  geom_point(aes(group = classe_age, color = classe_age),
              size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = SpanParam ,
              se = FALSE)+
  geom_smooth(data = filter(dbspf,dbspf$date>DateDebutGraphique),
              aes(x=date, y=R),
              size = 1.5,
              span = SpanParam ,
              se = FALSE,
              color = "black")+
  geom_hline(yintercept =1, colour = "black")+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux de reproduction effectif par classe d'âge",
        subtitle = "Ratio du nombre de cas sur 7 jours entre j et j-7. En noir, le R pour la France entière.",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.Reffectif)
ggsave("gReffectif.png", plot=graph.Reffectif, height = 6, width = 10)

# Graphique
graph.TauxPosSmooth<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=TxPos, group = classe_age, color = classe_age) ) +
  geom_point(size = 0.1) +
  geom_smooth( span = SpanParam , se = FALSE)+
  #facet_wrap(.~classe_age, nrow = 5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux de positivité au Covid19 par classe d'âge",
        subtitle = "En %, tendance LOESS en trait continu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.TauxPosSmooth)
ggsave("gTauxPos.png", plot=graph.TauxPosSmooth, height = 6, width = 10)



# Graphique
graph.HospitLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=hosp) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = SpanParam, se = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.HospitLog)
ggsave("gHospitLog.png", plot=graph.HospitLog, height = 6, width = 10)

# Graphique
graph.Hospit<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                        aes(x=jour, y=hosp, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        fill = "Classes d'âge",
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.Hospit)
ggsave("gHospit.png", plot=graph.Hospit, height = 6, width = 10)

# Graphique
graph.HospitRepart<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                     aes(x = jour, y = hosp, fill = classe_age) ) +
  geom_col(alpha=0.6, position ="fill", width = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_continuous(labels = scales::percent) +
  labs( y = NULL,
        x = NULL ,
        fill = "Classes d'âge",
        title = "Personnes hospitalisées pour Covid19",
        subtitle = "Répartition en % par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.HospitRepart)
ggsave("gHospitRepart.png", plot=graph.HospitRepart, height = 6, width = 10)

# Graphique
graph.ReaLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                     aes(x=jour, y=rea) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = SpanParam, se = TRUE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
  labs( y = "Pourcentage",
        x = NULL ,
        color = "Classes d'âge",
        title = "Nombre de personnes en soins critiques pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS en trait continu",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.ReaLog)
ggsave("gReaLog.png", plot=graph.ReaLog, height = 6, width = 10)

# Graphique
graph.rea<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                     aes(x=jour, y=rea, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        fill = "Classe d'âge",
        title = "Nombre de personnes en soins critiques pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.rea)
ggsave("gRea.png", plot=graph.rea, height = 6, width = 10)

# Graphique
graph.ReaRepart<-ggplot(data=filter(db,db$jour>DateDebutGraphique),
                           aes(x = jour, y = rea, fill = classe_age) ) +
  geom_col(alpha=0.6, position ="fill", width = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_continuous(labels = scales::percent) +
  labs( y = NULL,
        x = NULL ,
        fill = "Classes d'âge",
        title = "Personnes en soins critiques pour Covid19",
        subtitle = "Répartition en % par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.ReaRepart)
ggsave("gReaRepart.png", plot=graph.ReaRepart, height = 6, width = 10)



# Graphique
graph.NCas<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                    aes(x=date, y=pos) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nombre de nouveaux cas de Covid19",
        subtitle = "Par date de prélèvement, tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.NCas)
ggsave("gNCas.png", plot=graph.NCas, height = 6, width = 10)

# Graphique
graph.Nhosp<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                  aes(x=date, y=incid_hosp) ) +
  geom_col(fill = "light blue" , alpha = 0.5,  width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouvelles hospitalisations pour Covid19",
        subtitle = "Tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.Nhosp)
ggsave("gNhosp.png", plot=graph.Nhosp, height = 6, width = 10)

# Graphique
graph.Nrea<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                    aes(x=date, y=incid_rea) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouvelles entrées en soins critiques pour Covid19",
        subtitle = "Tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.Nrea)
ggsave("gNrea.png", plot=graph.Nrea, height = 6, width = 10)

# Graphique
graph.Ndeces<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                   aes(x=date, y=incid_dchosp ) )+
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouveaux décès lié au Covid19 en millieu hospitalier",
        subtitle = "Tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.Ndeces)
ggsave("gNdeces.png", plot=graph.Ndeces, height = 6, width = 10)

# Graphique
graph.Vaccins<-ggplot(data=filter(dbvaccin,
                                  dbvaccin$jour>as.Date.character("2020-12-1") & dbvaccin$clage_vacsi!="Total" & dbvaccin$clage_vacsi!="0-4 ans"  & dbvaccin$clage_vacsi!="5-9 ans" & dbvaccin$clage_vacsi!="10-11 ans") )+
  geom_smooth(aes(y = couv_dose1 , x = jour, group = clage_vacsi, colour = clage_vacsi  ),
              span = SpanParam, size=.6,se = FALSE, linetype = "dashed") +
  geom_smooth(aes(y = couv_complet , x = jour, group = clage_vacsi, colour = clage_vacsi),
              span = SpanParam, size=.9,se = FALSE, linetype = "solid") +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed", size = 0.7)  +
  geom_hline(yintercept = 80, colour = "black", linetype = "solid", size = 0.7 )  +
  #facet_wrap(.~clage_vacsi)+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  annotate("text", x=as.Date.character("2020-12-27"), y=50+2, label = "50%", color = "black") +
  annotate("text", x=as.Date.character("2020-12-27"), y=80+2, label = "80%", color = "black") +
  scale_y_continuous( limits = c(0,100)) +
  labs( y = "En %",
        x = NULL ,
        color = NULL,
        title = "Taux de couverture vaccinale par classes d'âge",
        subtitle = "Taux de primo-vaccinés (en trait pointillé) et de totalement vaccinés (en trait continu)",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.Vaccins)
ggsave("gVaccin.png", plot=graph.Vaccins, height = 6, width = 10)


#####################################################################################################################################
# Donnees Our world in data
#####################################################################################################################################

owid <- read_csv("owid.csv")
as.data.frame(owid)
panel <- c("France","United Kingdom", "Spain", "Germany", "Italy", "Netherlands", "United States", "Israel")
owid<-owid %>%
  filter(location %in% panel   )


# Graphique
graph.NcasOWID<-ggplot(data=filter(owid,owid$date>DateDebutGraphique),
                        aes(x=date, y=new_cases_smoothed_per_million) ) +
  geom_point(aes(group = location, color = location),
             size = 0.5)+
  geom_smooth(aes(group = location, color = location),
              size = 0.8,
              span = SpanParam ,
              se = FALSE)+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de nouveaux cas de Covid19",
        subtitle = "Par million d'habitants, tendances LOESS",
        caption = "Source : Our World in Data. Graphique : P. Aldama @paldama.")
print(graph.NcasOWID)
ggsave("gNCasOwid.png", plot=graph.NcasOWID, height = 6, width = 10)


# Graphique
graph.NdecesOWID<-ggplot(data=filter(owid,owid$date>DateDebutGraphique),
                       aes(x=date, y=new_deaths_smoothed_per_million) ) +
  geom_point(aes(group = location, color = location),
             size = 0.5)+
  geom_smooth(aes(group = location, color = location),
              size = 0.8,
              span = SpanParam ,
              se = FALSE)+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de nouveaux déces lié au Covid19",
        subtitle = "Par million d'habitants, tendances LOESS",
        caption = "Source : Our World in Data. Graphique : P. Aldama @paldama.")
print(graph.NdecesOWID)
ggsave("gNdecesOwid.png", plot=graph.NdecesOWID, height = 6, width = 10)

# Graphique
graph.ReffOWID<-ggplot(data=filter(owid,owid$date>DateDebutGraphique),
                       aes(x=date, y=reproduction_rate) ) +
  geom_point(aes(group = location, color = location),
             size = 0.5)+
  geom_smooth(aes(group = location, color = location),
              size = 0.8,
              span = SpanParam ,
              se = FALSE) +
  geom_hline(yintercept = 1, colour = "black")+
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        title = "Taux de reproduction effectif du Covid19",
        color = NULL,
        subtitle = "Tendance LOESS",
        caption = "Source : Our World in Data. Graphique : P. Aldama @paldama.")
print(graph.ReffOWID)
ggsave("gReffOwid.png", plot=graph.ReffOWID, height = 6, width = 10)


##################################################################################################################################
# Graphs et cartes par départements
##################################################################################################################################

 
dbsidep_dep <- read_delim("sidep_donneesincidence_dep.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)
dbsidep_dep <- dbsidep_dep %>%
  as.data.frame() %>%
  arrange(dep,cl_age90,jour) %>%
  mutate(TauxIncidence=round(rollapply(P,7,sum,align='right',fill=NA)/pop*100000))

dbsidep_dep$cl_age90<-as.numeric(dbsidep_dep$cl_age90)
dbsidep_dep$classe_age <- case_when(
  dbsidep_dep$cl_age90 == 9  ~ "0-9 ans",
  dbsidep_dep$cl_age90 == 19  ~ "10-19 ans",
  dbsidep_dep$cl_age90 == 29  ~ "20-29 ans",
  dbsidep_dep$cl_age90 == 39  ~ "30-39 ans",
  dbsidep_dep$cl_age90 == 49  ~ "40-49 ans",
  dbsidep_dep$cl_age90 == 59  ~ "50-59 ans",
  dbsidep_dep$cl_age90 == 69  ~ "60-69 ans",
  dbsidep_dep$cl_age90 == 79  ~ "70-79 ans",
  dbsidep_dep$cl_age90 == 89  ~ "80-89 ans",
  dbsidep_dep$cl_age90 == 0  ~ "Total",
  TRUE ~ "Plus de 90 ans" )


# Graphique
graph.TauxIncidence64<-ggplot(
  data=filter(dbsidep_dep,dbsidep_dep$jour>DateDebutGraphique & dbsidep_dep$dep == 64 & dbsidep_dep$classe_age!="Total"),
                       aes(x=jour, y=TauxIncidence) ) +
  geom_point(aes(group = classe_age, color = classe_age),
             size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = SpanParam ,
              se = FALSE) +
  geom_smooth(data= filter(dbsidep_dep,dbsidep_dep$jour>DateDebutGraphique & dbsidep_dep$dep == 64 & dbsidep_dep$classe_age=="Total"),
              aes(x=jour, y=TauxIncidence),
              method = "loess",
              size = 1.5,
              span = SpanParam ,
              se = FALSE,
              colour = "black") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux d'incidence du Covid19 dans les Pyrénées-Atlantiques (64)",
        subtitle = "Nombre de cas sur 7 jours, pour 100 000 habitants, tendance LOESS en trait continu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.TauxIncidence64)
ggsave("gTauxIncidence64.png", plot=graph.TauxIncidence64, height = 7, width = 9)




