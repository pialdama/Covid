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
sidep<- download.file(url,dest)

url <- "https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675"
dest <- "~/Documents/covid/sidep_donneesincidence_dep.csv"
sidep_dep<- download.file(url,dest)

# General data from SPF
url <- "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"
dest <- "~/Documents/covid/spf_donneesensemble.csv"
spf<- download.file(url,dest)
url<-"https://www.data.gouv.fr/fr/datasets/r/5c4e1452-3850-4b59-b11c-3dd51d7fb8b5"
dest <- "~/Documents/covid/spf_donneesensemble_dep.csv"
spf_dep<- download.file(url,dest)

# Data on vaccination from SPF
url <- "https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd"
dest <-   "~/Documents/covid/spf_donneesvaccins.csv"
vaccin <-download.file(url,dest)
url <- "https://www.data.gouv.fr/fr/datasets/r/4f39ec91-80d7-4602-befb-4b522804c0af"
dest <-   "~/Documents/covid/spf_donneesvaccins_dep.csv"
vaccin <-download.file(url,dest)


url <- "https://www.data.gouv.fr/fr/datasets/r/70cef74f-70b1-495a-8500-c089229c0254"
dest <- "~/Documents/covid/departementFr.csv"
vaccin <- download.file(url,dest)
departementFr <- read.csv("~/Documents/covid/departementFr.csv")




DateDebutGraphique <- as.Date("2020-07-01")
SpanParam<-0.1
# # DateDebutGraphique <- as.Date("2021-03-01")
# # SpanParam<-0.125
# DateDebutGraphique <- as.Date("2021-09-01")
# SpanParam<-0.3


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

# Charge les donnees SPF d'ensemble
dbspf <- read_csv("spf_donneesensemble.csv")
dbspfDep <- read_csv("~/Documents/covid/spf_donneesensemble_dep.csv")
dbspfDep <- dbspfDep %>%
  mutate(incid_hosp_7j=rollapply(incid_hosp, 7, sum, align = "right", fill = NA))%>%
  mutate(incid_rea_7j=rollapply(incid_rea, 7, sum, align = "right", fill = NA)) %>%
  mutate(incid_dc_7j=rollapply(incid_dchosp, 7, sum, align = "right", fill = NA)) %>%
  mutate(RatioHospIncidence=incid_hosp_7j/lag(pos_7j,7)*1000) %>%
  mutate(RatioReaIncidence=incid_rea_7j/lag(pos_7j,7)*1000) %>%
  mutate(RatioDcIncidence=incid_dc_7j/lag(pos_7j,20)*1000)

dbspfDep$jour <- dbspfDep$date  


# Charge les donnees SPF sur les vaccins 
dbvaccinDep <- read_delim("spf_donneesvaccins_dep.csv",
                       ";",
                       escape_double = FALSE,
                       trim_ws = TRUE)
dbDep<-right_join(dbvaccinDep,dbspfDep,by=c("jour","dep"))
dbDep<-subset(dbDep, dbDep$jour >= as.character("2020-12-27"))



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
mutate(ReffectifCas= TauxIncidence/lag(TauxIncidence,7)) %>%
mutate(ReffectifHosp = hosp/lag(hosp,7)) %>%
mutate(ReffectifRea = rea/lag(rea,7)) 

# db<-db %>%
#   filter(cl_age90 != 0)  # filtre les données France entière

# ggplot(data = dbspfDep) +
#   geom_col(aes(x=date,y=TO,fill=TO)) +
#   geom_hline(yintercept = 1, color = "red")+
#   facet_wrap(.~lib_reg) + theme_minimal() +
#   scale_fill_viridis(name = "Taux d'occupation des lits en soins critiques",labels = scales::percent) +
#   scale_y_continuous(labels = scales::percent) +labs(y=NULL,x=NULL)


library(RColorBrewer)
colourCount = length(unique(db$lib_reg))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Graphique
graph.CasCovid<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                        aes(x=jour, y=P) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth( span = SpanParam , se = FALSE)+
  facet_wrap(.~classe_age, nrow = 5, scales = "free") +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de cas positif au Covid19 par classes d'âge",
        subtitle = "Tendance LOESS en trait continu bleu",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.CasCovid)
ggsave("gCasCovid.png", plot=graph.CasCovid,bg="white", height = 10, width = 15)

# Graphique
graph.incidence<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
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
  theme_minimal() +  labs_pubr() +
  scale_color_brewer(type = "qualitative", palette = "Set3") +
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux d'incidence du Covid19 par classes d'âge",
        subtitle = "Nombre de cas positifs sur 7 jours glissants pour 100 000 habitants. En noir, le taux d'incidence pour la France entière.")
print(graph.incidence)
ggsave("gIncidence.png", plot=graph.incidence,bg="white", height = 6, width = 10)


graph.HeatMapIncidenceClasseAge <- ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0)) + 
  geom_tile( aes(jour, classe_age, fill=TauxIncidence) ) +
  theme_minimal() + labs_pubr() +
  scale_fill_viridis("Taux d'incidence",
                     trans = scales::pseudo_log_trans(sigma = 50),
                     option = "C") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  labs( y = "Classe d'âge",
        x = NULL ,
        fill = "Taux d'incidence",
        title = "Taux d'incidence par classes d'âge",
        subtitle = "Nombre de cas sur 7 jours, pour 100 000 habitants")
print(graph.HeatMapIncidenceClasseAge)
ggsave('grHeatMapIncidenceClasseAge.png', plot=graph.HeatMapIncidenceClasseAge,bg="white",width=10)

# Graphique
graph.Reffectif<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                        aes(x=jour, y=ReffectifCas) ) +
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
  scale_color_brewer(type = "qualitative", palette = "Set3") +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux de reproduction effectif par classe d'âge",
        subtitle = "Ratio du nombre de cas sur 7 jours entre j et j-7. En noir, le R pour la France entière.")
print(graph.Reffectif)
ggsave("gReffectif.png", plot=graph.Reffectif,bg="white", height = 6, width = 10)

# Graphique
SpanParamTemp<- 0.3
graph.ReffectifHosp<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                        aes(x=jour, y=ReffectifHosp) ) +
  geom_point(aes(group = classe_age,  color = classe_age),
             size = 0.5, alpha = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),
              size = 0.8,
              span = SpanParamTemp ,
              se = FALSE)+
  geom_smooth(data = filter(db,db$jour>DateDebutGraphique & cl_age90 == 0),
              aes(x=jour, y=ReffectifHosp),
              size = 1.5,
              span = SpanParam ,
              se = FALSE,
              color = "black")+
  geom_hline(yintercept =1, colour = "black")+
  scale_color_brewer(type = "qualitative", palette = "Set3") +
  #facet_grid(.~classe_age) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux de reproduction effectif par classe d'âge à partir des hospitalisations",
        subtitle = "Ratio du nombre d'hospitalisations sur 7 jours entre j et j-7. En noir, le R pour la France entière.",
        caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
print(graph.ReffectifHosp)
ggsave("gReffectifHosp.png", plot=graph.ReffectifHosp,bg="white", height = 6, width = 10)


# Graphique
graph.TauxPosSmooth<-ggplot( ) +
  geom_point(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
             aes(x=jour, y=TxPos, group = classe_age, color = classe_age),
             size = 0.5) +
  geom_smooth(data=filter(db,db$jour>DateDebutGraphique),
              aes(x=jour, y=TxPos, group = classe_age, color = classe_age),
              size = 0.8,
              span = SpanParam ,
              se = FALSE) +
  geom_smooth(data = filter(dbspf,dbspf$date>DateDebutGraphique),
              aes(x=date, y=tx_pos),
              size = 1.5,
              span = SpanParam ,
              se = FALSE,
              color = "black") +
  theme_minimal() +  labs_pubr() +
  scale_color_brewer(type = "qualitative", palette = "Set3") +
  theme(plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Taux de positivité au Covid19 par classe d'âge",
        subtitle = "En %, tendance LOESS en trait continu. En noir, le taux de positivité pour la France entière.")
print(graph.TauxPosSmooth)
ggsave("gTauxPos.png", plot=graph.TauxPosSmooth,bg="white", height = 6, width = 10)

gCasSynthese<-ggarrange(graph.incidence,graph.Reffectif,graph.TauxPosSmooth,
          ncol=1,
          nrow=3,
          legend = "bottom",
          common.legend = TRUE,
          labels = "auto",
          align="hv")
ggsave("gCasSynthese.png",plot=gCasSynthese,bg="white",height=10,width=10)

# Graphique
graph.HospitLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                        aes(x=jour, y=hosp) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = SpanParam, se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
    scale_color_brewer(type = "qualitative", palette = "Set3") +
  labs( y = "Effectis",
        x = NULL ,
        color = NULL,
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.HospitLog)
ggsave("gHospitLog.png", plot=graph.HospitLog,bg="white", height = 6, width = 10)

# Graphique
graph.Hospit<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                        aes(x=jour, y=hosp, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +  labs_pubr() +
  scale_fill_brewer(type = "qualitative", palette = "Set3") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        fill = "Classes d'âge",
        title = "Nombre de personnes hospitalisées pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.Hospit)
ggsave("gHospit.png", plot=graph.Hospit,bg="white", height = 6, width = 10)

# Graphique
graph.HospitRepart<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                     aes(x = jour, y = hosp, fill = classe_age) ) +
  geom_col(alpha=0.6, position ="fill", width = 1) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(type = "qualitative", palette = "Set3") +
  labs( y = NULL,
        x = NULL ,
        fill = "Classes d'âge",
        title = "Personnes hospitalisées pour Covid19",
        subtitle = "Répartition en % par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.") 
print(graph.HospitRepart)
ggsave("gHospitRepart.png", plot=graph.HospitRepart,bg="white", height = 6, width = 10)

# Graphique
graph.ReaLog<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                     aes(x=jour, y=rea) ) +
  geom_point(aes(group = classe_age, color = classe_age), size = 0.5)+
  geom_smooth(aes(group = classe_age, color = classe_age),span = SpanParam, se = FALSE) +
  theme_minimal() +  labs_pubr() +
    scale_color_brewer(type = "qualitative", palette = "Set3") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_y_log10() +
  labs( y = NULL,
        x = NULL ,
        color = "Classes d'âge",
        title = "Nombre de personnes en soins critiques pour Covid19",
        subtitle = "Répartition par classes d'âge, échelle log en base 10, tendance LOESS en trait continu",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.ReaLog)
ggsave("gReaLog.png", plot=graph.ReaLog,bg="white", height = 6, width = 10)

# Graphique
graph.rea<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                     aes(x=jour, y=rea, fill = classe_age) ) +
  geom_area(alpha=0.6 , size=.5, colour="white") +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  scale_fill_brewer(type = "qualitative", palette = "Set3") +
  labs( y = "Effectis",
        x = NULL ,
        fill = "Classe d'âge",
        title = "Nombre de personnes en soins critiques pour Covid19",
        subtitle = "Répartition par classes d'âge",
        caption = "Source : Santé Publique France, SI-VIC. Graphique : P. Aldama @paldama.")
print(graph.rea)
ggsave("gRea.png", plot=graph.rea,bg="white", height = 6, width = 10)

# Graphique
graph.ReaRepart<-ggplot(data=filter(db,db$jour>DateDebutGraphique & cl_age90 != 0),
                           aes(x = jour, y = rea, fill = classe_age) ) +
  geom_col(alpha=0.6, position ="fill", width = 1) +
  theme_minimal() +  labs_pubr() +
  scale_fill_brewer(type = "qualitative", palette = "Set3") +
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
ggsave("gReaRepart.png", plot=graph.ReaRepart,bg="white", height = 6, width = 10)


# Graphique
graph.NCasRemontee<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                   aes(x=date, y=conf_j1) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Cas confirmés par date de remontée")
print(graph.NCasRemontee)
#ggsave("gNCas.png", plot=graph.NCas,bg="white", height = 6, width = 10)


# Graphique
graph.NCas<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                    aes(x=date, y=pos) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Cas confirmés par date de prélèvement")
print(graph.NCas)
#ggsave("gNCas.png", plot=graph.NCas,bg="white", height = 6, width = 10)

# Graphique
graph.Nhosp<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                  aes(x=date, y=incid_hosp) ) +
  geom_col(fill = "light blue" , alpha = 0.5,  width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouvelles hospitalisations")
print(graph.Nhosp)
#ggsave("gNhosp.png", plot=graph.Nhosp,bg="white", height = 6, width = 10)

# Graphique
graph.Nrea<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                    aes(x=date, y=incid_rea) ) +
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouvelles entrées en soins critiques")
print(graph.Nrea)
#ggsave("gNrea.png", plot=graph.Nrea,bg="white", height = 6, width = 10)

# Graphique
graph.Ndeces<-ggplot(data=filter(dbspf,dbspf$date>DateDebutGraphique),
                   aes(x=date, y=incid_dchosp ) )+
  geom_col(fill = "light blue" , alpha = 0.5, width = 1) +
  geom_smooth(span = SpanParam, size=.5, colour = "blue",se = FALSE) +
  theme_minimal() +  labs_pubr() +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  labs( y = NULL,
        x = NULL ,
        color = NULL,
        title = "Nouveaux décès en millieu hospitalier")
print(graph.Ndeces)
#ggsave("gNdeces.png", plot=graph.Ndeces,bg="white", height = 6, width = 10)

gEpiFrance<-ggarrange(graph.NCasRemontee, graph.NCas, graph.Nhosp, graph.Nrea, graph.Ndeces,
                      ncol=2, nrow = 3)
title <- expression(atop(bold("Epidémie de Covid19 en France")))
gEpiFrance<-annotate_figure(gEpiFrance,
                            top = text_grob(title,
                                just = "top",
                                face = "bold", 
                                size = 14),
                            bottom = text_grob("Source: Santé Publique France. Graphique : P. Aldama @paldama.",
                                   hjust = 1, 
                                   x = 1, 
                                   face = "italic", 
                                   size = 10))
ggsave("gEpiFrance.png", plot=gEpiFrance,bg="white", height = 7, width = 10)
print(gEpiFrance)

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
  theme_minimal() + labs_pubr() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 9)) +
  annotate("text", x=as.Date.character("2020-12-27"), y=50+2, label = "50%", color = "black") +
  annotate("text", x=as.Date.character("2020-12-27"), y=80+2, label = "80%", color = "black") +
  scale_y_continuous( limits = c(0,100)) +
    scale_color_brewer(type = "qualitative", palette = "Set3") +
  labs( y = "En %",
        x = NULL ,
        color = NULL,
        title = "Taux de couverture vaccinale par classes d'âge",
        subtitle = "Taux de primo-vaccinés (en trait pointillé) et de totalement vaccinés (en trait continu)",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
print(graph.Vaccins)
ggsave("gVaccin.png", plot=graph.Vaccins,bg="white", height = 6, width = 10)


################################################################################################################################################################################################
# Graphs efficacité vaccinale
################################################################################################################################################################################################

library(RColorBrewer)
colourCount = length(unique(dbDep$lib_reg))
getPalette = colorRampPalette(brewer.pal(9, "Set2"))

seuilTxIncid <- 10

modelWOLS <-  lm( RatioHospIncidence ~ couv_dose1,
                  data = filter(dbDep,dbDep$incid_hosp_7j>0 & dbDep$tx_incid > seuilTxIncid),
                  weights = tx_incid)
summary(modelWOLS)
CoefIntercept<- round(modelWOLS$coefficients[1], digits = 0)
CoefSlope<- round(modelWOLS$coefficients[2], digits = 2)
Rsquared<-round(summary(modelWOLS)$r.squared, digits = 2)
labelWOLS <- paste("y = ",CoefIntercept,CoefSlope,"x,"," R2 = ", Rsquared, sep = "") 

graph.VaccinationIncidenceHosp <- ggplot(data = filter(dbDep,dbDep$incid_hosp_7j>0 & dbDep$tx_incid > seuilTxIncid), 
                                         aes(x=couv_dose1, y=RatioHospIncidence)) +
  geom_point(aes(color = lib_reg, alpha = incid_hosp_7j , size = pos_7j)) + 
  geom_smooth(aes(group = 1,weight =  pos_7j) , method = lm,  color = "blue", se = TRUE ) +
  geom_density2d(aes(group = 1), linetype = "dashed", color = "blue", show.legend = FALSE) + 
  scale_y_log10(n.breaks = 20)+
  # stat_regline_equation(
  #   aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #   formula = I(10^y) ~ x ,
  #   color = "blue") +
  annotate("text", x = 10, y = 500,
           label = labelWOLS , color="blue",
           size=4, fontface="bold") +
  theme_minimal() + labs_pubr() +
  scale_color_manual(values = getPalette(colourCount)) +
    theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  labs( y =  NULL,
        x = "Taux de vaccination (au moins une dose)" ,
        color = "Régions",
        alpha = "Hospitalisations sur 7 jours",
        size = "Cas positifs sur 7 jours",
        title = "Hospitalisations pour 1000 cas positifs détectés 7 jours plus tôt",
        subtitle = "Chaque point représente le nombre d'hospitalisations par jour et par région pour 1000 cas positifs détectés 7 jours plus tôt \nen fonction du taux de vaccination (au moins une dose) et lorsque le taux d'incidence dépasse 10 pour 100k habitants.",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama."
        )
ggsave('grVaccinationIncidenceHosp.png', plot=graph.VaccinationIncidenceHosp, bg="white", height = 12, width = 10)
print(graph.VaccinationIncidenceHosp)

modelWOLS <-  lm( RatioHospIncidence ~ couv_dose1,
              data = filter(dbDep,dbDep$incid_hosp_7j>0 & dbDep$tx_incid > seuilTxIncid),
              weights = tx_incid)
summary(modelWOLS)
CoefIntercept<- round(modelWOLS$coefficients[1], digits = 0)
CoefSlope<- round(modelWOLS$coefficients[2], digits = 2)
Rsquared<-round(summary(modelWOLS)$r.squared, digits = 2)
labelWOLS <- paste("y = ",CoefIntercept,CoefSlope,"x,"," R2 = ", Rsquared, sep = "") 
#plot(modelWOLS)

modelOLS <-  lm( RatioHospIncidence ~ couv_dose1,
              data = filter(dbDep,dbDep$incid_hosp_7j>0 & dbDep$tx_incid > seuilTxIncid))
summary(modelOLS)

graph.VaccinationIncidenceHospBis <- ggplot(data = filter(dbDep,dbDep$incid_hosp_7j>0 & dbDep$tx_incid > seuilTxIncid), 
                                         aes(x=couv_dose1, y=RatioHospIncidence)) +
  geom_point(aes(colour = tx_incid, alpha = incid_hosp_7j)) + 
  geom_smooth(aes(group = 1, weight = tx_incid) , method = lm,  color = "blue", se = TRUE ) +
  geom_density2d(aes(group = 1), linetype = "dashed", color = "blue", show.legend = FALSE) + 
  scale_y_log10(n.breaks = 20)+
  annotate("text", x = 10, y = 500,
           label = labelWOLS , color="blue",
           size=4, fontface="bold") +
  theme_minimal() + labs_pubr() +
  scale_colour_viridis_c(trans = scales::pseudo_log_trans(sigma = 200),
                         option = "C", direction = -1 ) +
  #scale_color_manual(values = getPalette(colourCount)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  labs( y =  NULL,
        x = "Taux de vaccination (au moins une dose)" ,
        color = "Taux d'incidence",
        alpha = "Hospitalisations sur 7 jours",
        title = "Hospitalisations pour 1000 cas positifs détectés 7 jours plus tôt",
        subtitle = "Chaque point représente le nombre d'hospitalisations par jour et par régions pour 1000 cas positifs détectés 7 jours plus tôt \nen fonction du taux de vaccination (au moins une dose) et lorsque le taux d'incidence dépasse 10 pour 100k habitants.",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama."
  )
ggsave('grVaccinationIncidenceHospBis.png', plot=graph.VaccinationIncidenceHospBis, bg="white", height = 12, width = 10)
print(graph.VaccinationIncidenceHospBis)



graph.VaccinationIncidenceRea <- ggplot(data = filter(dbDep,dbDep$incid_rea_7j>0 & dbDep$tx_incid > seuilTxIncid), 
                                        aes(x=couv_dose1, y=RatioReaIncidence)) +
  geom_point(aes(color = lib_reg, alpha = incid_rea_7j , size = pos_7j)) + 
  geom_smooth(aes(group = 1,weight =  pos_7j) , method = lm,  color = "blue", se = TRUE ) +
  geom_density2d(aes(group = 1), linetype = "dashed", color = "blue", show.legend = FALSE) +
  # stat_regline_equation(
  #   aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #   formula = I(10^y) ~ x ,
  #   color = "blue") +
  theme_minimal() +  labs_pubr() +
  scale_color_manual(values = getPalette(colourCount))+
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_y_log10(n.breaks = 20)+
  labs( y =  NULL,
        x = "Taux de vaccination (au moins une dose)" ,
        color = "Régions",
        alpha = "Entrées en soins critiques sur 7 jours",
        size = "Cas positifs sur 7 jours",
        title = "Entrées en soins critiques pour 1000 cas positifs détectés 7 jours plus tôt",
        subtitle = "Chaque point représente le nombre d'entrées en soins critiaues par jour et par région pour 1000 cas positifs détectés 7 jours \nplus tôt en fonction du taux de vaccination (au moins une dose) et lorsque le taux d'incidence dépasse 10 pour 100k habitants.",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
ggsave('grVaccinationIncidenceRea.png', plot=graph.VaccinationIncidenceRea, bg="white", height = 12, width = 10)


graph.VaccinationIncidenceDeces <- ggplot(data = filter(dbDep,dbDep$incid_dc_7j>0 & dbDep$tx_incid > seuilTxIncid ), 
                                          aes(x=couv_dose1, y=RatioDcIncidence)) +
  geom_point(aes(color = lib_reg, alpha = incid_dc_7j , size = pos_7j)) + 
  geom_smooth(aes(group = 1, weight =  pos_7j) , method = lm,  color = "blue", se = TRUE ) +
  geom_density2d(aes(group = 1), linetype = "dashed", color = "blue", show.legend = FALSE) + 
  # stat_regline_equation(
  #   aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
  #   formula = I(10^y) ~ x ,
  #   color = "blue") +
  theme_minimal() + labs_pubr() +
  scale_color_manual(values = getPalette(colourCount))+
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  scale_y_log10(n.breaks = 20)+
  labs( y =  NULL,
        x = "Taux de vaccination (au moins une dose)" ,
        color = "Régions",
        alpha = "Décès hospitaliers sur 7 jours",
        size = "Cas positifs sur 7 jours",
        title = "Décès hospitaliers pour 1000 cas positifs détectés 20 jours plus tôt",
        subtitle = "Chaque point représente le nombre de décès hospitaliers par jour et par région pour 1000 cas positifs détectés 7 jours plus tôt \nen fonction du taux de vaccination (au moins une dose) et lorsque le taux d'incidence dépasse 10 pour 100k habitants.",
        caption = "Source : Santé Publique France. Graphique : P. Aldama @paldama.")
ggsave('grVaccinationIncidenceDeces.png', plot=graph.VaccinationIncidenceDeces,bg="white", height = 12, width = 10)

# ##################################################################################################################################
# # Graphs et cartes par départements
# ##################################################################################################################################
# 
# dbsidep_dep <- read_delim("sidep_donneesincidence_dep.csv", 
#                                          ";", escape_double = FALSE, trim_ws = TRUE)
# dbsidep_dep <- dbsidep_dep %>%
#   as.data.frame() %>%
#   arrange(dep,cl_age90,jour) %>%
#   mutate(TauxIncidence=round(rollapply(P,7,sum,align='right',fill=NA)/pop*100000)) %>%
#   mutate(CroissanceHebdo=P/lag(P,7)) %>%
#   mutate(CroissanceHebdoMoy3j= rollapply(CroissanceHebdo,3,mean,align='right',fill=NA))
#   
#   
# 
# dbsidep_dep$cl_age90<-as.numeric(dbsidep_dep$cl_age90)
# dbsidep_dep$classe_age <- case_when(
#   dbsidep_dep$cl_age90 == 9  ~ "0-9 ans",
#   dbsidep_dep$cl_age90 == 19  ~ "10-19 ans",
#   dbsidep_dep$cl_age90 == 29  ~ "20-29 ans",
#   dbsidep_dep$cl_age90 == 39  ~ "30-39 ans",
#   dbsidep_dep$cl_age90 == 49  ~ "40-49 ans",
#   dbsidep_dep$cl_age90 == 59  ~ "50-59 ans",
#   dbsidep_dep$cl_age90 == 69  ~ "60-69 ans",
#   dbsidep_dep$cl_age90 == 79  ~ "70-79 ans",
#   dbsidep_dep$cl_age90 == 89  ~ "80-89 ans",
#   dbsidep_dep$cl_age90 == 0  ~ "Total",
#   TRUE ~ "Plus de 90 ans" )
# 
# # Graphique
# graph.HeatMapIncidence <- ggplot(data=filter(dbsidep_dep,dbsidep_dep$jour>as.character("2020-05-01") & dbsidep_dep$classe_age=="Total" & dbsidep_dep$dep<=95)) + 
#   geom_tile( aes(jour, dep, fill=TauxIncidence) ) +
#   theme_minimal() +  labs_pubr() +
#   scale_fill_viridis("Taux d'incidence",
#                      trans = scales::pseudo_log_trans(sigma = 50),
#                      option = "C") +
#   theme(plot.title = element_text(size = 18, face = "bold"),
#         plot.subtitle = element_text(size = 12),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank())+
#   labs( y = "Départements",
#         x = NULL ,
#         color = NULL,
#         title = "Taux d'incidence par département en France métropolitaine",
#         subtitle = "Nombre de cas sur 7 jours, pour 100 000 habitants",
#         caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
# print(graph.HeatMapIncidence)
# ggsave('grHeatMapIncidence.png', plot=graph.HeatMapIncidence,bg="white", height = 20, width = 15)
# 
# 
# # Graphique
# for (Dep in departementFr$code_departement){
#   #Dep<-"32"
#   NomDepartement = departementFr$nom_departement[departementFr$code_departement==Dep]
#   Legende <- paste("Taux d'incidence du Covid19 dans le département",Dep,"(",NomDepartement,")")
# 
#   graph.TauxIncidenceDep<-ggplot(
#     data=filter(dbsidep_dep,dbsidep_dep$jour>DateDebutGraphique & dbsidep_dep$dep == Dep & dbsidep_dep$classe_age!="Total"),
#     aes(x=jour, y=TauxIncidence) ) +
#     geom_point(aes(group = classe_age, color = classe_age),
#                size = 0.5)+
#     geom_smooth(aes(group = classe_age, color = classe_age),
#                 size = 0.8,
#                 span = SpanParam ,
#                 se = FALSE) +
#     geom_smooth(data= filter(dbsidep_dep,dbsidep_dep$jour>DateDebutGraphique & dbsidep_dep$dep == Dep & dbsidep_dep$classe_age=="Total"),
#                 aes(x=jour, y=TauxIncidence),
#                 method = "loess",
#                 size = 1.5,
#                 span = SpanParam ,
#                 se = FALSE,
#                 colour = "black") +
#     theme_minimal() +  labs_pubr() +
#       scale_color_brewer(type = "qualitative", palette = "Set3") +
#     theme(plot.title = element_text(size = 14, face = "bold"),
#           plot.subtitle = element_text(size = 9)) +
#     labs( y = NULL,
#           x = NULL ,
#           color = NULL,
#           title = Legende,
#           subtitle = "Nombre de cas sur 7 jours, pour 100 000 habitants, tendance LOESS en trait continu",
#           caption = "Source : Santé Publique France, SI-DEP. Graphique : P. Aldama @paldama.")
# 
#   #print(graph.TauxIncidenceDep)
# 
#   GraphOutput <- paste("gTauxIncidenceDep",Dep,".png")
# 
#   ggsave(GraphOutput, plot=graph.TauxIncidenceDep,bg="white", height = 7, width = 9)
# 
# }
# 




