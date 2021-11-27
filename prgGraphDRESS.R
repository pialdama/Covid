

#######
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

# Data from DRESS 
url <-"https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-par-age-issus-des-appariements-entre-si-vic-si-dep-et-vac-si/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
dest <- "~/Documents/covid/dataDRESS.csv"
download.file(url,dest)

url <-"https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-issus-des-appariements-entre-si-vic-si-dep-et-vac-si/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
dest <- "~/Documents/covid/dataDRESSNational.csv"
download.file(url,dest)


seuilEffectif<-5000


dataDRESS <- read_delim("dataDRESS.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

dataDRESS$vac_statut_bis <- case_when(
  dataDRESS$vac_statut == "Primo dose récente"  ~ "Primo dose récente",
  dataDRESS$vac_statut == "Primo dose efficace"  ~ "Primo dose efficace",
  dataDRESS$vac_statut == "Complet de 6 mois et plus - avec rappel"  ~ "Complet avec rappel",
  dataDRESS$vac_statut == "Complet de 6 mois et plus - sans rappel"  ~ "Complet sans rappel",
  dataDRESS$vac_statut == "Complet de moins de 3 mois - avec rappel"  ~ "Complet avec rappel",
  dataDRESS$vac_statut == "Complet de moins de 3 mois - sans rappel"  ~ "Complet sans rappel",
  dataDRESS$vac_statut == "Complet entre 3 mois et 6 mois - avec rappel"  ~ "Complet avec rappel",
  dataDRESS$vac_statut == "Complet entre 3 mois et 6 mois - sans rappel"  ~ "Complet sans rappel",
  dataDRESS$vac_statut == "Non-vaccinés" ~ "Non-vaccinés")

dbDRESS<-dataDRESS %>%
  subset(select = -c(vac_statut)) %>%
  group_by(date,vac_statut_bis,age) %>%
  summarise(across(everything(), sum)) %>%
  filter(effectif>seuilEffectif)

dbDRESS<-dbDRESS %>%
  mutate(PCR_100k = 10^5*nb_PCR/effectif) %>%
  mutate(PCRpos_100k = 10^5*`nb_PCR+`/effectif) %>%
  mutate(HC_100k =  10^5*HC/effectif) %>%
  mutate(HCpos_100k = 10^5*`HC_PCR+`/effectif) %>%
  mutate(SC_100k=  10^5*SC/effectif) %>%
  mutate(SCpos_100k = 10^5*`SC_PCR+`/effectif) 

dbDRESS<-dbDRESS %>%
  as.data.frame() %>%
  arrange(vac_statut_bis, age, date)%>%
  mutate(PCR_100k = rollapply(PCR_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(PCRpos_100k = rollapply(PCRpos_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(HC_100k = rollapply(HC_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(HCpos_100k = rollapply(HCpos_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(SC_100k=  rollapply(SC_100k,7,sum,align="right",fill=NA) )  %>%
  mutate(SCpos_100k = rollapply(SCpos_100k,7,sum,align="right",fill=NA) )  

dbDRESS<-melt(dbDRESS,id.vars=1:3, measure.vars=11:16)

dbDRESS$age <- case_when(
  dbDRESS$age == "[0,19]"  ~ "0-19 ans",
  dbDRESS$age == "[20,39]"  ~ "20-39 ans",
  dbDRESS$age == "[40,59]"  ~ "40-59 ans",
  dbDRESS$age == "[60,79]"  ~ "60-79 ans",
  TRUE ~ "80 ans et plus")

dbDRESS$VariableName <- case_when(
  dbDRESS$variable == "PCRpos_100k"  ~ "Cas Covid+",
  dbDRESS$variable == "PCR_100k"  ~ "PCR",
  dbDRESS$variable == "HC_100k"  ~ "Hospitalisations",
  dbDRESS$variable == "HCpos_100k"  ~ "Hospitalisation PCR+",
  dbDRESS$variable == "SC_100k"  ~ "Soins critiques",
  dbDRESS$variable == "SCpos_100k"  ~ "Soins critiques avec PCR+")


graph.vaccinationDress<-ggplot(data = filter(dbDRESS,dbDRESS$date>=as.Date("2021-05-1") &  dbDRESS$age!="0-19 ans" &
                                               dbDRESS$variable!="PCR_100k" & dbDRESS$variable!="HCpos_100k" & dbDRESS$variable!="SCpos_100k"  ),
                               aes(x=date,  y=value, group = vac_statut_bis, colour = vac_statut_bis )) +
  geom_point(size = 0.6)+
  geom_smooth(aes(fill = vac_statut_bis),
              alpha = 0.2,
              span = 0.3,
              se=TRUE,
              inherit.aes = TRUE) +
  guides(fill = "none")  +
  facet_wrap(VariableName~age, scales = "free", ncol = 4) +
  theme_minimal() + theme(legend.position="top") +
  theme(strip.text.x = element_text( face = "bold", size = 10)) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  labs( y = "Incidence hebdomadaire pour 100 000",
        x = NULL ,
        colour = "Statut vaccinal",
        title = "Nombre de cas COVID+, d'hospitalisations et de soins critiques selon le statut vaccinal",
        subtitle = "Echelles spécifiques par classes d'âge, lissage de l'incidence observée par une régression LOESS.",
        caption = "Source : DRESS à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")
size<-8
ggsave("gr_vaccinationDressAge.png", plot=graph.vaccinationDress,bg="white", height = size, width = 1/0.7*size )

#######
dataDRESSNational <- read_delim("dataDRESSNational.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

dataDRESSNational$vac_statut_bis <- case_when(
  dataDRESSNational$vac_statut == "Primo dose récente"  ~ "Primo dose récente",
  dataDRESSNational$vac_statut == "Primo dose efficace"  ~ "Primo dose efficace",
  dataDRESSNational$vac_statut == "Complet de 6 mois et plus - avec rappel"  ~ "Complet avec rappel",
  dataDRESSNational$vac_statut == "Complet de 6 mois et plus - sans rappel"  ~ "Complet sans rappel",
  dataDRESSNational$vac_statut == "Complet de moins de 3 mois - avec rappel"  ~ "Complet avec rappel",
  dataDRESSNational$vac_statut == "Complet de moins de 3 mois - sans rappel"  ~ "Complet sans rappel",
  dataDRESSNational$vac_statut == "Complet entre 3 mois et 6 mois - avec rappel"  ~ "Complet avec rappel",
  dataDRESSNational$vac_statut == "Complet entre 3 mois et 6 mois - sans rappel"  ~ "Complet sans rappel",
  dataDRESSNational$vac_statut == "Non-vaccinés" ~ "Non-vaccinés")


dbDRESSNational<-dataDRESSNational %>%
  subset(select = -c(vac_statut)) %>%
  group_by(date,vac_statut_bis) %>%
  summarise(across(everything(), sum)) %>%
  filter(effectif>10000)


dbDRESSNational<-dbDRESSNational %>%
  mutate(PCR_100k = 10^5*nb_PCR/effectif) %>%
  mutate(PCRpos_100k = 10^5*`nb_PCR+`/effectif) %>%
  mutate(HC_100k =  10^5*HC/effectif) %>%
  mutate(HCpos_100k = 10^5*`HC_PCR+`/effectif) %>%
  mutate(SC_100k=  10^5*SC/effectif) %>%
  mutate(SCpos_100k = 10^5*`SC_PCR+`/effectif) 

dbDRESSNational<-dbDRESSNational %>%
  as.data.frame() %>%
  arrange(vac_statut_bis, date)%>%
  mutate(PCR_100k = rollapply(PCR_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(PCRpos_100k = rollapply(PCRpos_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(HC_100k = rollapply(HC_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(HCpos_100k = rollapply(HCpos_100k,7,sum,align="right",fill=NA) ) %>%
  mutate(SC_100k=  rollapply(SC_100k,7,sum,align="right",fill=NA) )  %>%
  mutate(SCpos_100k = rollapply(SCpos_100k,7,sum,align="right",fill=NA) )  


dbDRESSNational<-melt(dbDRESSNational,id.vars=1:2, measure.vars=14:19)

dbDRESSNational$VariableName <- case_when(
  dbDRESSNational$variable == "PCRpos_100k"  ~ "Cas Covid+",
  dbDRESSNational$variable == "PCR_100k"  ~ "PCR",
  dbDRESSNational$variable == "HC_100k"  ~ "Hospitalisations",
  dbDRESSNational$variable == "HCpos_100k"  ~ "Hospitalisation PCR+",
  dbDRESSNational$variable == "SC_100k"  ~ "Soins critiques",
  dbDRESSNational$variable == "SCpos_100k"  ~ "Soins critiques avec PCR+")



graph.vaccinationDressNational<-ggplot(data = filter(dbDRESSNational,dbDRESSNational$date>=as.Date("2021-05-1")   & 
                                                       dbDRESSNational$variable!="PCR_100k" & dbDRESSNational$variable!="HCpos_100k" & dbDRESSNational$variable!="SCpos_100k"  ),
                                       aes(x=date,  y=value, group = vac_statut_bis, colour = vac_statut_bis )) +
  geom_point(size = 0.6) +
  geom_smooth(aes(fill = vac_statut_bis),
              alpha = 0.2,
              span = 0.3,
              se=TRUE,
              inherit.aes = TRUE) +
  guides(fill = "none")  +
  facet_wrap(~VariableName,scales = "free") +
  theme_minimal() + theme(legend.position="top") +
  theme(strip.text.x = element_text( face = "bold", size = 11)) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  #scale_y_log10(n.breaks = 10)+
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  labs( y = "Incidence hebdomadaire pour 100 000",
        x = NULL ,
        colour = "Statut vaccinal",
        title = "Nombre de cas COVID+, d'hospitalisations et de soins critiques selon le statut vaccinal",
        subtitle = "Lissage de l'incidence observée par une régression LOESS.",
        caption = "Source : DRESS à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")
size<-8
ggsave("gr_vaccinationDressNational.png", plot=graph.vaccinationDressNational,bg="white", height = size, width = 1/0.7*size )
