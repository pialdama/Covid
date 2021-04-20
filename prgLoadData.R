# Insee data on mortality 1968/2019
url<-"https://www.insee.fr/fr/statistiques/fichier/4771989/T79JDEC.csv" 
dest<-"~/Documents/Covid/HistoricalDataMortality.csv"
data<-download.file(url, dest)

# Insee data on mortality for 2020/2021
url<-"https://www.insee.fr/fr/statistiques/fichier/4487988/2021-04-16_detail.zip"
dest<-"~/Documents/Covid/dataZ.zip"
download.file(url, dest)
unzip("~/Documents/Covid/dataZ.zip")

# data from SPF
url<-"https://www.data.gouv.fr/fr/datasets/r/d3a98a30-893f-47f7-96c5-2f4bcaaa0d71"
dest<-'~/Documents/covid/SPF.csv'
spf<-download.file(url, dest)

# Data from Sentinelles
url<-"https://www.sentiweb.fr/datasets/incidence-PAY-3.csv"
dest<-'~/Documents/covid/SentinellesIncidenceGrippe.csv'
Sentinelles<-download.file(url, dest)

