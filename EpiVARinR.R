library(tidyverse)
library(tidyquant)
library(ggpubr)
library(viridis)
library(vars)
library(scales)
library(forecast)
library(rmarkdown)
library(rstudioapi)

temp <- dirname(rstudioapi::getSourceEditorContext()$path)
if (getwd()!=temp){setwd(temp)}

exit <- function() {
  invokeRestart("abort")
}

VARFirstDiff <- 1 
ComputeIRFandFEVD <- 1 # produire les graphiques d'IRF et de FEVD
lissageLOESS <- 1 # si 0 pas de lissage LOESS des series

####################################################
# Telechargement et préparation des données
####################################################

# General data from SPF
url <-
  "https://www.data.gouv.fr/fr/datasets/r/f335f9ea-86e3-4ffa-9684-93c009d5e617"
dest <- "./spf_donneesensemble.csv"
spf <- download.file(url, dest)
dbspf <- read_csv("spf_donneesensemble.csv")
LastObs <- (max(dbspf$date))
LastObsCas <- LastObs - 3

# Correction jours fériés SPF
download.file(
  "https://www.data.gouv.fr/fr/datasets/r/6637991e-c4d8-4cd6-854e-ce33c5ab49d5",
  "joursferies.csv"
)
joursferies <- read_csv("joursferies.csv")
joursferies$ferie <- c(rep(1))
dbspf <- full_join(dbspf, joursferies)
dbspf$ferie <- replace_na(dbspf$ferie, 0)
dbspf$postferie <- case_when(lag(dbspf$ferie, 1) == 1 ~ 1,
                             TRUE ~ 0)

dbspf <- dbspf %>%
  subset(select = -c(annee, zone, nom_jour_ferie))
dbspf$JourSemaine <- lubridate::wday(dbspf$date, week_start = 1)
dbspf$Poids <- case_when(
  dbspf$JourSemaine == 1 ~ 0.188,
  dbspf$JourSemaine == 2 ~ 0.177,
  dbspf$JourSemaine == 3 ~ 0.164,
  dbspf$JourSemaine == 4 ~ 0.175,
  dbspf$JourSemaine == 5 ~ 0.179,
  dbspf$JourSemaine == 6 ~ 0.094,
  dbspf$JourSemaine == 7 ~ 0.022
)

dbspf$PoidsAdj <- case_when(dbspf$ferie == 1 ~ 0.022,
                            dbspf$ferie == 0 ~ dbspf$Poids,)

dbspf$PoidsAdj <- case_when(dbspf$postferie == 1 &
                              dbspf$JourSemaine < 6  ~ 0.188,
                            TRUE ~ dbspf$PoidsAdj,)

dbspf$PoidsMoyen <-
  rollapply(dbspf$PoidsAdj, 7, sum, align = "right", fill = NA)
dbspf$posAdj <- dbspf$pos_7j / dbspf$PoidsMoyen / 7
dbspf <-
  filter(dbspf, dbspf$date > "2020-06-01" & dbspf$date <= LastObs)

gCorrectionJoursFeries <- ggplot(data = dbspf) +
  geom_line(aes(x = date, y = pos_7j / 7, color = "Brut"), size=0.4) +
  geom_line(aes(x = date, y = posAdj, color = "Corrigé des jours fériés"), size=.8) +
  scale_y_log10(labels = label_number(suffix = " k", scale = 1e-3)) +
  scale_x_date(date_label = "%Y-%m") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "",
                     values = c("Brut" = "blue",
                                "Corrigé des jours fériés" = "red")) +
  labs(
    y = NULL,
    x = NULL,
    title = "Nombre de cas positifs, moyenne sur 7 jours glissants (échelle log en base 10)",
    caption = "Notes : correction des jours fériés d'après la méthode SPF. \nSource: Santé Publique France. \n Calculs : P. Aldama @paldama"
  )
print(gCorrectionJoursFeries)
ggsave(
  "./gCorrectionJoursFeries.png",
  plot = gCorrectionJoursFeries,
  bg = "white",
  width = 7,
  height = 4
)


# Prolongation du Reffectif
dbspf <- dbspf %>%
  mutate(ReBrut = posAdj / lag(posAdj, 7)) %>%
  mutate(Re = rollapply(ReBrut, 14, mean, align = "center", fill = NA))

library(EpiEstim)
incid <- dbspf %>%
  subset(select = c(date, posAdj)) %>%
  rename(I = posAdj) %>%
  rename(dates = date) %>%
  na.omit()
incid$index <- 1:nrow(incid)
SmoothIncid <- loess(I ~ index, data = incid, span = 0.05)
incid$I <- SmoothIncid$fitted

# Source des estimatons d'interval seriel et ecart-type estimé
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7448781/
mu_si <- 5.19
sd_si <- (6.02-5.19)/1.96

Restim <- estimate_R(incid,
                     method = "parametric_si",
                     config = make_config(list(mean_si = mu_si,
                                               std_si = sd_si)))
RestimData <- Restim$R
RestimData$date <- seq(from = as.Date(min(incid$dates) + 7),
                       to = as.Date(max(incid$dates)),
                       by = 'day')
dbspf <- full_join(RestimData, dbspf)
dbspf$R_sm <- dbspf$`Mean(R)`


gRcompare <- ggplot(data = filter(dbspf, dbspf$date > "2020-07-01")) +
  geom_line(aes(x = date, y = R_sm, color = "R: EpiEstim"), size =1) +
  geom_point(aes(x = date, y = R, color = "R: SPF"), linetype="solid", size = 0.7) +
  geom_line(aes(x = date, y = Re, color = "R: Moy. mobile de la croissance hebdomadaire"),size = 0.3) +
  geom_hline(yintercept = 1, size = 0.3) +
  scale_x_date(date_label = "%Y-%m") +
  scale_color_manual(
    name = "",
    values = c(
      "R: EpiEstim" = "blue",
      "R: SPF" = "black",
      "R: Moy. mobile de la croissance hebdomadaire" =
        "red"
    )
  ) +
  labs(
    y = "R",
    x = NULL,
    title = "Estimation du taux de reproduction effectif et comparaison à la mesure SPF",
    caption = "Notes : calcul du R effectif à partir du package EpiEstim (mean_si = 5.19, std_si = 0.42). \nSource: Santé Publique France. \n Calculs : P. Aldama @paldama"
  ) +
  theme_minimal(base_size = 8) + theme(legend.position = "bottom")


print(gRcompare)
ggsave(
  "./gRcompare.png",
  plot = gRcompare,
  bg = "white",
  width = 7,
  height = 4
)


## Construction de la base de donnees

db <- dbspf %>%
  subset(
    select = c(
      date,
      Re,
      R,
      R_sm,
      pos,
      posAdj,
      hosp,
      rea,
      incid_hosp,
      incid_rea,
      incid_dchosp
    )
  ) %>%
  filter(date >= "2020-07-01")  %>%
  na.pass()



# Lissage
db$index <- 1:nrow(db)

paramAlign <- "right"

  db <- db %>%
    mutate(dc_mean = rollapply(incid_dchosp, 7, sum, align = paramAlign, fill =NA) / 7) %>%
    mutate(cas = pos) %>%
    mutate(cas_mean = posAdj) %>%
    mutate(dc = incid_dchosp) 

# Lissage LOESS
if (lissageLOESS==1){
  cas_sm_model <- loess(cas_mean ~ index, data = db , span = 0.05)
  db$cas_sm <-predict(cas_sm_model, newdata = db, na.action = na.exclude)
  
  hosp_sm_model <- loess(hosp ~ index, data = db , span = 0.05)
  db$hosp_sm <- predict(hosp_sm_model, newdata = db, na.action = na.exclude)
  
  rea_sm_model <- loess(rea ~ index, data = db , span = 0.05)
  db$rea_sm <-predict(rea_sm_model, newdata = db, na.action = na.exclude)
  
  dc_sm_model <- loess(dc ~ index, data = db , span = 0.05)
  db$dc_sm <-predict(dc_sm_model, newdata = db, na.action = na.exclude)
} else {
  db$cas_sm <- db$cas_mean
  db$hosp_sm <- db$hosp_mean
  db$rea_sm <-db$rea_mean
  db$dc_sm <-db$dc_mean
} 

# Graphs
gCas<-ggplot(data = db) +
  geom_point(aes(x = date, y = cas, color = "Brut")) +
  geom_line(aes(x = date, y = cas_sm, color = "Tendance")) +
  scale_color_manual(name = "",
                     values = c("Brut" = "black",
                                "Tendance" = "red")) + 
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  theme_minimal(base_size = 8) +
  labs(title = "Cas positifs (corrigés des jours fériés)",y=NULL,x=NULL)

gHosp<-ggplot(data = db) +
  geom_point(aes(x = date, y = hosp, color ="Brut")) +
  geom_line(aes(x = date, y = hosp_sm, color = "Tendance")) +
  scale_color_manual(name = "",
                     values = c("Brut" = "black",
                                "Tendance" = "red")) + 
  scale_y_continuous(labels = label_number(suffix = " k", scale = 1e-3)) +
  theme_minimal(base_size = 8) +
  labs(title = "Lits en hospitalisation conventionnelle",y=NULL,x=NULL)

gRea<-ggplot(data = db) +
  geom_point(aes(x = date, y = rea, color ="Brut")) +
  geom_line(aes(x = date, y = rea_sm, color = "Tendance")) +
  scale_color_manual(name = "",
                     values = c("Brut" = "black",
                                "Tendance" = "red")) + 
  theme_minimal(base_size = 8) +
  labs(title = "Lits en soins critiques",y=NULL,x=NULL)


gDeces<-ggplot(data = db) +
  geom_point(aes(x = date, y = dc, color = "Brut")) +
  geom_line(aes(x = date, y = dc_sm, color = "Tendance")) +
  scale_color_manual(
    name = "",
    values = c(
      "Brut" = "black",
      "Tendance" = "red")
  ) + 
  theme_minimal(base_size = 8) +
  labs(title = "Décès hospitaliers",y=NULL,x=NULL)

gDonnees<-ggarrange(gCas, gHosp, gRea, gDeces, 
                    ncol = 1,
                    nrow = 4,
                    legend = "bottom",
                    common.legend = TRUE,
                    labels = "auto",
                    align = "hv")
ggsave(
  "./gDonnees.png",
  plot = gDonnees,
  bg = "white",
  width = 7,
  height = 10)

####################################################
# Estimation et forecast full-sample
####################################################

ConfidenceLevel <- 0.9

# Preparation du dataset
OutofSample <- 0
HorizonForecast <- 7 * (2)

debFcst <- LastObsCas - OutofSample
dateFcst <-
  seq(
    from = as.Date(debFcst - OutofSample + 1),
    to = as.Date(debFcst - OutofSample + HorizonForecast),
    by = 'day'
  )
FirstDayFcst <- debFcst + 1
 if (VARFirstDiff==1){
   DataEpiVAR <- db %>%
   mutate(R =  log(R_sm/lag(R_sm, 1)))  %>%
   mutate(cas = log(cas_sm / lag(cas_sm, 1))) %>%
   mutate(hosp = log(hosp_sm / lag(hosp_sm, 1))) %>%
   mutate(rea = log(rea_sm / lag(rea_sm, 1))) %>%
   mutate(dc = log(dc_sm / lag(dc_sm, 1))) %>%
   filter(date < LastObs - OutofSample) %>%
   subset(select = c(R, cas, hosp, rea, dc)) %>%
   na.omit %>%
   ts()
   } else {
     DataEpiVAR <- db %>%
       mutate(R =  log(R_sm))  %>%
       mutate(cas = log(cas_sm )) %>%
       mutate(hosp = log(hosp_sm)) %>%
       mutate(rea = log(rea_sm )) %>%
       mutate(dc = log(dc_sm )) %>%
       filter(date < LastObs - OutofSample) %>%
       subset(select = c(R, cas, hosp, rea, dc)) %>%
       na.omit %>%
       ts()
   }

LengthGraph <- length(DataEpiVAR) # longueur des graphiques


# Estimation VAR
OptimalLag<-VARselect(DataEpiVAR,lag.max = 50, type=c("none"))
nlags<-max(OptimalLag$selection)

EpiVAR <- VAR(DataEpiVAR,
              p = nlags,
              type = "none")

# Test de stabilité du VAR
if (max(roots(EpiVAR)) < 1) {
  print("EpiVAR is stationary")
  print(paste("Max root =", round(max(roots(
    EpiVAR
  )) , 2)))
} else {
  print("EpiVAR is non stationary!")
  print(paste("Max root =", round(max(roots(
    EpiVAR
  )) , 2)))
  exit()
}

 checkresiduals(EpiVAR$varresult$R)
 checkresiduals(EpiVAR$varresult$cas)
 checkresiduals(EpiVAR$varresult$hosp)
 checkresiduals(EpiVAR$varresult$rea)
 checkresiduals(EpiVAR$varresult$dc)

# Forecasts
Forecast <- predict(EpiVAR,
                    n.ahead = HorizonForecast,
                    pi = ConfidenceLevel,
                    dumvar = NULL)

Forecast_df <- as.data.frame(lapply(Forecast$fcst, unlist)) %>%
  subset(select = -c(R.CI, cas.CI, hosp.CI, rea.CI, dc.CI)) %>%
  mutate_all(function(x)
    exp(cumsum(x))) %>%
  mutate(date = dateFcst) %>%
  full_join(db, Forecast_df, by = "date") %>%
  mutate(R.fcst = R.fcst * R_sm[date == debFcst]) %>%
  mutate(R.fcstUp = R.upper * R_sm[date == debFcst]) %>%
  mutate(R.fcstLow = R.lower * R_sm[date == debFcst]) %>%
  mutate(cas.fcst = cas.fcst * cas_sm[date == debFcst]) %>%
  mutate(cas.fcstUp = cas.upper * cas_sm[date == debFcst]) %>%
  mutate(cas.fcstLow = cas.lower * cas_sm[date == debFcst]) %>%
  mutate(hosp.fcst = hosp.fcst * hosp_sm[date == debFcst]) %>%
  mutate(hosp.fcstUp = hosp.upper * hosp_sm[date == debFcst]) %>%
  mutate(hosp.fcstLow = hosp.lower * hosp_sm[date == debFcst]) %>%
  mutate(rea.fcst = rea.fcst * rea_sm[date == debFcst]) %>%
  mutate(rea.fcstUp = rea.upper * rea_sm[date == debFcst]) %>%
  mutate(rea.fcstLow = rea.lower * rea_sm[date == debFcst]) %>%
  mutate(dc.fcst = dc.fcst * dc_sm[date == debFcst]) %>%
  mutate(dc.fcstUp = dc.upper * dc_sm[date == debFcst]) %>%
  mutate(dc.fcstLow = dc.lower * dc_sm[date == debFcst]) %>%
  arrange(desc(date)) %>%
  filter(date >= as.Date(LastObs - LengthGraph))


####################################################
# Estimation et forecast out-of-sample
####################################################

# Preparation du dataset
k <- 2
OutofSample <- 7 * k
HorizonForecast <- 7 * k + 3

debFcst <- LastObsCas - OutofSample
dateFcst <-
  seq(
    from = as.Date(LastObsCas - OutofSample + 1),
    to = as.Date(LastObsCas - OutofSample + HorizonForecast),
    by = 'day'
  )

FirstDayFcst <- debFcst + 1

if (VARFirstDiff==1){
  DataEpiVAR <- db %>%
    mutate(R =  log(R_sm/lag(R_sm, 1)))  %>%
    mutate(cas = log(cas_sm / lag(cas_sm, 1))) %>%
    mutate(hosp = log(hosp_sm / lag(hosp_sm, 1))) %>%
    mutate(rea = log(rea_sm / lag(rea_sm, 1))) %>%
    mutate(dc = log(dc_sm / lag(dc_sm, 1))) %>%
    filter(date < LastObs - OutofSample) %>%
    subset(select = c(R, cas, hosp, rea, dc)) %>%
    na.omit %>%
    ts()
} else {
  DataEpiVAR <- db %>%
    mutate(R =  log(R_sm))  %>%
    mutate(cas = log(cas_sm )) %>%
    mutate(hosp = log(hosp_sm)) %>%
    mutate(rea = log(rea_sm )) %>%
    mutate(dc = log(dc_sm )) %>%
    filter(date < LastObs - OutofSample) %>%
    subset(select = c(R, cas, hosp, rea, dc)) %>%
    na.omit %>%
    ts()
}

# Estimation VAR
EpiVAR <- VAR(DataEpiVAR,
              p = nlags,
              type = "none")
summary(EpiVAR)

# Test de stabilité du VAR
if (max(roots(EpiVAR)) < 1) {
  print("EpiVAR is stationary")
  print(paste("Max root =", round(max(roots(
    EpiVAR
  )) , 2)))
} else {
  print("EpiVAR is non stationary!")
  print(paste("Max root =", round(max(roots(
    EpiVAR
  )) , 2)))
  exit()
}

# Forecasts
ForecastOutOFSample <- predict(EpiVAR,
                               n.ahead = HorizonForecast,
                               pi = ConfidenceLevel,
                               dumvar = NULL)

ForecastOutOFSample_df <-
  as.data.frame(lapply(ForecastOutOFSample$fcst, unlist)) %>%
  subset(select = -c(R.CI, cas.CI, hosp.CI, rea.CI, dc.CI)) %>%
  mutate_all(function(x)
    exp(cumsum(x))) %>%
  mutate(date = dateFcst) %>%
  full_join(db, ForecastOutOFSample_df, by = "date") %>%
  mutate(R.fcst = R.fcst * R_sm[date == debFcst]) %>%
  mutate(R.fcstUp = R.upper * R_sm[date == debFcst]) %>%
  mutate(R.fcstLow = R.lower * R_sm[date == debFcst]) %>%
  mutate(cas.fcst = cas.fcst * cas_sm[date == debFcst]) %>%
  mutate(cas.fcstUp = cas.upper * cas_sm[date == debFcst]) %>%
  mutate(cas.fcstLow = cas.lower * cas_sm[date == debFcst]) %>%
  mutate(hosp.fcst = hosp.fcst * hosp_sm[date == debFcst]) %>%
  mutate(hosp.fcstUp = hosp.upper * hosp_sm[date == debFcst]) %>%
  mutate(hosp.fcstLow = hosp.lower * hosp_sm[date == debFcst]) %>%
  mutate(rea.fcst = rea.fcst * rea_sm[date == debFcst]) %>%
  mutate(rea.fcstUp = rea.upper * rea_sm[date == debFcst]) %>%
  mutate(rea.fcstLow = rea.lower * rea_sm[date == debFcst]) %>%
  mutate(dc.fcst = dc.fcst * dc_sm[date == debFcst]) %>%
  mutate(dc.fcstUp = dc.upper * dc_sm[date == debFcst]) %>%
  mutate(dc.fcstLow = dc.lower * dc_sm[date == debFcst]) %>%
  arrange(desc(date)) %>%
  filter(date >= as.Date(LastObs - LengthGraph))

######################################
# Plots
######################################

# Function to create the common structure of the plots
PlotEpiVar <- function(data, y, y_sm, y_fcst, y_fcstLow, y_fcstUp, title) {
  ggplot(data = data) +
    geom_point(aes(x = date, y = .data[[y]]), color = "grey") +
    geom_line(aes(x = date, y = .data[[y_sm]], color = "Tendance")) +
    geom_line(aes(x = date, y = .data[[y_fcst]], color = "Projection")) +
    geom_line(data = ForecastOutOFSample_df, aes(x = date, y = .data[[y_fcst]], color = "Projection out-of-sample")) +
    geom_ribbon(aes(x = date, ymin = .data[[y_fcstLow]], ymax = .data[[y_fcstUp]], fill = "full sample"), alpha = 0.2) +
    geom_ribbon(data = ForecastOutOFSample_df, aes(x = date, ymin = .data[[y_fcstLow]], ymax = .data[[y_fcstUp]], fill = "out-of-sample"), alpha = 0.1) +
    scale_color_manual(name = "", values = c("Tendance" = "black", "Projection" = "blue", "Projection out-of-sample" = "red")) +
    scale_fill_manual(name = "Intervalles de prévisions", values = c("full sample" = "blue", "out-of-sample" = "red")) +
    scale_x_date(date_label = "%Y-%m") +
    theme_minimal(base_size = 8) +
    theme(plot.title = element_text(size = 11), legend.position="topleft") +
    labs(x = NULL, y = NULL, title = title, fill = "Intervalles de prévisions")
}

# Create the plots using the function
gR <- PlotEpiVar(Forecast_df, "R", "R_sm", "R.fcst", "R.fcstLow", "R.fcstUp", "Taux de reproduction effectif (Reff)")
gcas <- PlotEpiVar(Forecast_df, "cas", "cas_sm", "cas.fcst", "cas.fcstLow", "cas.fcstUp", "Cas confirmés (date de prélèvement)")
ghosp <- PlotEpiVar(Forecast_df, "hosp", "hosp_sm", "hosp.fcst", "hosp.fcstLow", "hosp.fcstUp", "Lits en hospitalisation conventionnelle")
grea <- PlotEpiVar(Forecast_df, "rea", "rea_sm", "rea.fcst", "rea.fcstLow", "rea.fcstUp", "Lits en soins critiques")
gdc <- PlotEpiVar(Forecast_df, "dc", "dc_sm", "dc.fcst", "dc.fcstLow", "dc.fcstUp", "Décès hospitaliers")

TitleGraph = paste("Projection EpiVAR ")

gEpiVAR <- ggarrange(
  gR,
  gcas,
  ghosp,
  grea,
  gdc,
  ncol = 1,
  nrow = 5,
  legend = "bottom",
  common.legend = TRUE,
  labels = "auto",
  align = "hv"
) %>%
  annotate_figure(
    top = text_grob(
      TitleGraph,
      just = "top",
      face = "bold",
      size = 12
    ),
    bottom = text_grob(
      "Source: Santé Publique France. \nModèle et calculs : P. Aldama @paldama.",
      hjust = 1,
      x = 1,
      face = "italic",
      size = 10
    )
  )
ggsave(
  "./gEpiVAR.png",
  plot = gEpiVAR,
  bg = "white",
  width = 8,
  height = 10
)

print(gEpiVAR)

if (ComputeIRFandFEVD==1) {

######################################
# IRF
######################################

horizonIRFFEVD<-60  

if (file.exists("./extract_varirf.R")){ } else 
  {
    download.file(
      "https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R",
      "./extract_varirf.R"
    )
    }  

source("./extract_varirf.R")

irf <- irf(
  EpiVAR,
  impulse = c("R", "cas", "hosp", "rea", "dc"),
  response = NULL,
  n.ahead = horizonIRFFEVD,
  ortho = TRUE,
  cumulative = TRUE,
  boot = TRUE,
  ci = 0.95,
  runs = 100
)

multiple_varirf <- extract_varirf(irf) %>%
  pivot_longer(
    cols = !period,
    names_to = c("type", "impulse", "response"),
    names_pattern = "(.*)_(.*)_(.*)",
    values_to = "values",
    values_drop_na = TRUE
  ) %>%
  pivot_wider(names_from = type, values_from = values)

gIRF <- multiple_varirf %>%
  ggplot(aes(
    x = period,
    y = irf * 100,
    ymin = lower * 100,
    ymax = upper * 100
  )) +
  geom_hline(yintercept = 0, color = "black") +
  geom_ribbon(aes(fill = impulse), alpha = .1) +
  geom_line(aes(color = impulse), size = 0.8) +
  theme_minimal(base_size = 8) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Fonctions impulsion-réponse orthogonalisées du modèle EpiVAR") +
  facet_wrap(. ~factor(response, levels = c("r", "cas", "hosp", "rea", "dc")),
             scale = "free") +
  scale_x_continuous(sec.axis = sec_axis(
    ~ . ,
    name = "Réponses des variables",
    breaks = NULL,
    labels = NULL
  )) +
  scale_y_continuous(sec.axis = sec_axis(
    ~ . ,
    name = "Impulsions",
    breaks = NULL,
    labels = NULL
  )) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5) ) +
  labs(
    x = "Jours après le choc",
    y = "Ecart à la moyenne en points de %",
    fill = "Chocs",
    color = "Chocs",
    caption = "Les intervalles de confiance sont les 5ème et 95ème percentiles de la distribution obtenue par boostrapp. \nSource: Santé Publique France. \nModèle et calculs : P. Aldama @paldama."
  )
ggsave(
  "./gIRF.png",
  plot = gIRF,
  bg = "white",
  width = 8,
  height = 8
)
print(gIRF)

#################################################
# Forecast Error Variance Decomposition
#################################################

fevd <- fevd(EpiVAR,
             n.ahead = horizonIRFFEVD)
fevd_df <- as.data.frame(lapply(fevd, unlist)) %>%
  mutate(period = c(1:horizonIRFFEVD)) %>%
  pivot_longer(
    cols = !period,
    names_to = c("response", "impulse"),
    names_pattern = "(.*)\\.(.*)",
    values_to = "values",
    values_drop_na = TRUE
  )

gFEVD <- fevd_df %>%
  ggplot(aes(x = period, y = values, fill = impulse)) +
  geom_area(stat = "identity",
            position = "stack") +
  theme_minimal(base_size = 8) + scale_fill_viridis(discrete = TRUE) +
  ggtitle(
    "Décomposition de la variance de l'erreur de prévision selon les chocs dans le modèle EpiVAR"
  ) +
  facet_grid(.~factor(response, levels = c("R", "cas", "hosp", "rea", "dc")) ) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 11)
  ) +
  labs(
    x = "jours",
    y = "% de la variance expliquée",
    fill = "Chocs",
    caption = "Source: Santé Publique France. \nModèle et calculs : P. Aldama @paldama."
  )
print(gFEVD)

ggsave(
  "./gFEVD.png",
  plot = gFEVD,
  bg = "white",
  width = 8,
  height = 4
)
}

rmarkdown::render(input = "OutputEpiVAR.Rmd", output_format = "html_document")
rmarkdown::render(input = "OutputEpiVAR.Rmd", output_format = "pdf_document")
