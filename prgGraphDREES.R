setwd("~/Documents/covid/")



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




# Data from DREES 
url <-"https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-par-age-issus-des-appariements-entre-si-vic-si-dep-et-vac-si/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
dest <- "~/Documents/covid/dataDREES.csv"
download.file(url,dest)

url <-"https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-regionaux-issus-des-appariements-entre-si-vic-si-dep-et-vac-s/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
dest <- "~/Documents/covid/dataDREESRegions.csv"
download.file(url,dest)

url <-"https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-issus-des-appariements-entre-si-vic-si-dep-et-vac-si/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
dest <- "~/Documents/covid/dataDREESNational.csv"
download.file(url,dest)

seuilEffectif<-0
Moyenne<-7


dataDREES <- read_delim("dataDREES.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

dataDREES$vac_statut_bis2 <- case_when(
  dataDREES$vac_statut == "Primo dose récente"  ~ "Vaccinés : Primo dose récente",
  dataDREES$vac_statut == "Primo dose efficace"  ~ "Vaccinés : Primo dose efficace",
  dataDREES$vac_statut == "Complet de 6 mois et plus - avec rappel"  ~ "Vaccinés : Complet avec rappel",
  dataDREES$vac_statut == "Complet de 6 mois et plus - sans rappel"  ~ "Vaccinés : Complet > 6 mois sans rappel",
  dataDREES$vac_statut == "Complet de moins de 3 mois - avec rappel"  ~ "Vaccinés : Complet avec rappel",
  dataDREES$vac_statut == "Complet de moins de 3 mois - sans rappel"  ~ "Vaccinés : Complet < 6 mois sans rappel",
  dataDREES$vac_statut == "Complet entre 3 mois et 6 mois - avec rappel"  ~ "Vaccinés : Complet avec rappel",
  dataDREES$vac_statut == "Complet entre 3 mois et 6 mois - sans rappel"  ~ "Vaccinés : Complet < 6 mois sans rappel",
  dataDREES$vac_statut == "Non-vaccinés" ~ "Non-vaccinés")

# dataDREES$vac_statut_bis2 <- case_when(
#   dataDREES$vac_statut == "Primo dose récente"  ~ "Non-vaccinés et primo dose récente",
#   dataDREES$vac_statut == "Primo dose efficace"  ~ "Vaccinés : Primo dose efficace",
#   dataDREES$vac_statut == "Complet de 6 mois et plus - avec rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Complet de 6 mois et plus - sans rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Complet de moins de 3 mois - avec rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Complet de moins de 3 mois - sans rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Complet entre 3 mois et 6 mois - avec rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Complet entre 3 mois et 6 mois - sans rappel"  ~ "Vaccinés : Complet",
#   dataDREES$vac_statut == "Non-vaccinés" ~ "Non-vaccinés et primo dose récente")


dbDREES<-dataDREES %>%
  subset(select = -c(vac_statut)) %>%
  group_by(date,vac_statut_bis2,age) %>%
  summarise(across(everything(), sum)) %>%
  filter(effectif>seuilEffectif)



dbDREES<-dbDREES %>%
  mutate(PCR_100k = 10^5*nb_PCR/effectif) %>%
  mutate(PCRpos_100k = 10^5*`nb_PCR+`/effectif) %>%
  mutate(HC_100k =  10^5*HC/effectif) %>%
  mutate(HCpos_100k = 10^5*`HC_PCR+`/effectif) %>%
  mutate(SC_1m=  10^6*SC/effectif) %>%
  mutate(SCpos_1m = 10^6*`SC_PCR+`/effectif) 

dbDREES<-dbDREES %>%
  as.data.frame() %>%
  arrange(vac_statut_bis2, age, date)%>%
  mutate(PCR_100k = rollapply(PCR_100k,Moyenne,mean,align="right",fill=NA) ) %>%
  mutate(PCRpos_100k = rollapply(PCRpos_100k,Moyenne,mean,align="right",fill=NA) ) %>%
  mutate(HC_100k = rollapply(HC_100k,Moyenne,mean,align="right",fill=NA) ) %>%
  mutate(HCpos_100k = rollapply(HCpos_100k,Moyenne,mean,align="right",fill=NA) ) %>%
  mutate(SC_1m=  rollapply(SC_1m,Moyenne,mean,align="right",fill=NA) )  %>%
  mutate(SCpos_1m = rollapply(SCpos_1m,Moyenne,mean,align="right",fill=NA) )  

dbDREES<-melt(dbDREES,id.vars=1:3, measure.vars=11:16)

dbDREES$age <- case_when(
  dbDREES$age == "[0,19]"  ~ "0-19 ans",
  dbDREES$age == "[20,39]"  ~ "20-39 ans",
  dbDREES$age == "[40,59]"  ~ "40-59 ans",
  dbDREES$age == "[60,79]"  ~ "60-79 ans",
  TRUE ~ "80 ans et plus")

dbDREES$VariableName <- case_when(
  dbDREES$variable == "PCRpos_100k"  ~ "Cas Covid+",
  dbDREES$variable == "PCR_100k"  ~ "PCR",
  dbDREES$variable == "HC_100k"  ~ "Hospitalisations",
  dbDREES$variable == "HCpos_100k"  ~ "Hospitalisation PCR+",
  dbDREES$variable == "SC_1m"  ~ "Soins critiques",
  dbDREES$variable == "SCpos_1m"  ~ "Soins critiques avec PCR+")


graph.vaccinationDREES<-ggplot(data = filter(dbDREES,dbDREES$date>=as.Date("2021-08-1") &  dbDREES$age!="0-19 ans" &
                                               dbDREES$vac_statut_bis2!="Vaccinés : Primo dose récente" & dbDREES$vac_statut_bis2!="Vaccinés : Primo dose efficace" & 
                                               dbDREES$variable!="PCR_100k" & dbDREES$variable!="HCpos_100k" & dbDREES$variable!="SCpos_1m"  ),
                               aes(x=date,  y=value, group = vac_statut_bis2, colour = vac_statut_bis2 )) +
  #geom_point(size = 0.6)+
  geom_smooth(aes(fill = vac_statut_bis2),
              alpha = 0.2,
              span = 0.5,
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
  labs( y = "Incidence hebdomadaire pour 100 000 personnes (ou 1 million pour les soins critiques)",
        x = NULL ,
        colour = "Statut vaccinal",
        title = "Nombre de cas COVID+, d'hospitalisations et de soins critiques selon le statut vaccinal",
        subtitle = "Echelles spécifiques par classes d'âge, lissage de l'incidence observée par une régression LOESS.",
        caption = "Source : DREES à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")
size<-8
ggsave("gr_vaccinationDREESAge.png", plot=graph.vaccinationDREES,bg="white", height = size, width = 1/0.7*size )


grBoxCasCovid<-ggplot(data = filter(dbDREES,dbDREES$date>=as.Date("2021-05-1") &  dbDREES$age!="0-19 ans" &  dbDREES$variable=="PCRpos_100k" ),
       aes(x=vac_statut_bis2,  y=value, fill = vac_statut_bis2)) +
  geom_boxplot(show.legend = TRUE) +
  scale_fill_brewer(palette = "Set1",
                    breaks=rev(c("Vaccinés : Complet avec rappel",
                             "Vaccinés : Complet > 6 mois sans rappel",
                             "Vaccinés : Complet < 6 mois sans rappel",
                             "Vaccinés : Primo dose efficace",
                             "Vaccinés : Primo dose récente",
                             "Non-vaccinés"))  )+
  coord_flip() +  theme_minimal() + labs_pubr() +
  facet_wrap(.~age,scales = "free",ncol=1)+
  theme(plot.title = element_text(size = 13, face = "bold"), legend.position="left") +
  scale_x_discrete(limits=c("Vaccinés : Complet avec rappel",
                            "Vaccinés : Complet > 6 mois sans rappel",
                            "Vaccinés : Complet < 6 mois sans rappel",
                            "Vaccinés : Primo dose efficace",
                            "Vaccinés : Primo dose récente",
                            "Non-vaccinés"),
                   labels = NULL,
                   breaks = NULL) + labs(x = "") +
  labs( y = NULL,
      x = NULL ,
      fill = "Statut vaccinal",
      title = "Nombre de cas de Covid selon le statut vaccinal, pour 100 000 personnes",
      caption = "Source : DREES à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")

ggsave("grBoxCasCovid.png",grBoxCasCovid,bg="white",width=12)

grBoxHosp<-ggplot(data = filter(dbDREES,dbDREES$date>=as.Date("2021-05-1") &  dbDREES$age!="0-19 ans" &  dbDREES$variable=="HC_100k" ),
       aes(x=vac_statut_bis2,  y=value, fill = vac_statut_bis2)) +
  geom_boxplot(show.legend = TRUE) +
  scale_fill_brewer(palette = "Set1",
                    breaks=rev(c("Vaccinés : Complet avec rappel",
                             "Vaccinés : Complet > 6 mois sans rappel",
                             "Vaccinés : Complet < 6 mois sans rappel",
                             "Vaccinés : Primo dose efficace",
                             "Vaccinés : Primo dose récente",
                             "Non-vaccinés")) ) +
  coord_flip() +  theme_minimal() + labs_pubr() +
  facet_wrap(.~age,scales = "free",ncol=1)+
  theme(plot.title = element_text(size = 13, face = "bold"), legend.position="left") +
  scale_x_discrete(limits=c("Vaccinés : Complet avec rappel",
                            "Vaccinés : Complet > 6 mois sans rappel",
                            "Vaccinés : Complet < 6 mois sans rappel",
                            "Vaccinés : Primo dose efficace",
                            "Vaccinés : Primo dose récente",
                            "Non-vaccinés"),
                   labels = NULL,
                   breaks = NULL) + labs(x = "") +
  labs( y = NULL,
        x = NULL ,
        fill = "Statut vaccinal",
        title = "Admissions à l'hôpital pour Covid19 selon le statut vaccinal, pour 100 000 personnes",
        caption = "Source : DREES à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")

ggsave("grBoxHosp.png",grBoxHosp,bg="white",width=12)


grBoxRea<-ggplot(data = filter(dbDREES,dbDREES$date>=as.Date("2021-05-1") &  dbDREES$age!="0-19 ans" &  dbDREES$variable=="SC_1m" ),
       aes(x=vac_statut_bis2,  y=value, fill = vac_statut_bis2)) +
  geom_boxplot(show.legend = TRUE) +
  scale_fill_brewer(palette = "Set1",
                    breaks=rev(c("Vaccinés : Complet avec rappel",
                             "Vaccinés : Complet > 6 mois sans rappel",
                             "Vaccinés : Complet < 6 mois sans rappel",
                             "Vaccinés : Primo dose efficace",
                             "Vaccinés : Primo dose récente",
                             "Non-vaccinés")) ) +
  coord_flip() +  theme_minimal() + labs_pubr() +
  facet_wrap(.~age,scales = "free",ncol=1)+
  theme(plot.title = element_text(size = 13, face = "bold"), legend.position="left") +
  scale_x_discrete(limits=c("Vaccinés : Complet avec rappel",
                            "Vaccinés : Complet > 6 mois sans rappel",
                            "Vaccinés : Complet < 6 mois sans rappel",
                            "Vaccinés : Primo dose efficace",
                            "Vaccinés : Primo dose récente",
                            "Non-vaccinés"),
                   labels = NULL,
                   breaks = NULL) + labs(x = "") +
  labs( y = NULL,
        x = NULL ,
        fill = "Statut vaccinal",
        title = "Admissions en soins critiques pour Covid19 selon le statut vaccinal, pour 1 million de personnes",
        caption = "Source : DREES à partir des bases de données SI-DEP, SI-VIC et VAC-SI. Graphique : P. Aldama @paldama.")
ggsave("grBoxRea.png",grBoxRea,bg="white",width=12)
