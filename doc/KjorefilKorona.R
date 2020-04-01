#Kjørefil for Rapporteket-Pandemi
library(korona)
RegData <- KoronaPreprosesser(KoronaDataSQL())

tidsenhet='dag'
erMann=9
bekr=9
skjemastatus=9
dodSh=9
valgtEnhet='Alle'
tilgangsNivaa <- 'SC'

table(RegData$RHF)
reshID = 102090 #enhetsnivå

antallTidEnhTab(RegData, tidsenhet=tidsenhet, erMann=9, tilgangsNivaa=tilgangsNivaa,
                bekr=1, skjemastatus=0, dodSh=9, valgtEnhet='Sør-Øst')



library(knitr)
library(korona)
library(tidyverse)
setwd('C:/ResultattjenesteGIT/korona/inst')
knitr::knit('KoronaManuell.Rnw', encoding = 'UTF-8')
tools::texi2pdf(file='KoronaManuell.tex')
knitr::knit2pdf('KoronaManuell.Rnw') #, encoding = 'UTF-8')


unique(PandemiInn[,c("UnitId","HealthUnitShortName",'HF')])
PandemiInn %>% dplyr::group_by(RHF, HF) %>% dplyr::summarise(Antall = length(unique(HealthUnitShortName)))
PandemiInn %>% dplyr::group_by(RHF) %>% dplyr::summarise(Antall = length(unique(HF)))


PandemiInn <- read.table('A:/Pandemi/Pandemiskjema2020-04-01.csv', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
PandemiInn$InnDato <- as.Date(PandemiInn$FormDate, tz= 'UTC', format="%d.%m.%Y") #DateAdmittedIntensive
PandemiInn$Dag <- format(PandemiInn$InnDato, '%d.%B')


# Antall uferdige registreringer (skjemastatus = 1?) - se på data

# Antall med fristbrudd (24t) - også ferdigstilte? Kun fra 1.april? Spør








Pandemiskjema:
Utskrivningsdato
TimerSidenRelevantDato
RelevantDato
Innleggelse (dato)
FormDate

Utskriving:
Utskrivningsdato
TimerSidenRelevantDato
RelevantDato
FormDate



