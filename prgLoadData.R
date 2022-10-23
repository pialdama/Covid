CurrentWorkingDir<-getwd()
setwd(CurrentWorkingDir)

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
url<-"http://www.insee.fr/fr/statistiques/fichier/4487988/2022-09-30_detail.zip"
dest<-"./dataMortality.zip"
download.file(url, dest)

