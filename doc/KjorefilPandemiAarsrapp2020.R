#  FIGURER OG TABELLER TIL ÅRSRAPPORT, Norsk Intensivregister

rm(list=ls())
setwd('/home/rstudio/korona/Aarsrapport')
library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-01-01' #Vi har pandemi fra 1.mars..
datoTil <- '2020-12-31'	#
datoFra1aar <- '2020-01-01'

#Klargjøre data
#Analyser baseres på opphold. Kun de med Covid som hovedårsak til innleggelse. Alle er nå ferdigstilte
KoroData <- KoronaDataSQL(datoTil = datoTil)
KoroData <- KoronaPreprosesser(RegData = KoroData[KoroData$ArsakInnleggelse==1, ], aggPers = 0)
IntData <- NIRRegDataSQL(datoFra=datoFra, datoTil=datoTil)

#OverfortAnnetSykehusInnleggelse:
#Ble pasienten overført fra et annet sykehus til dette sykehuset ved innleggelse?
#1,2,3: Ja, Nei, Ukjent
table(KoroData$OverfortAnnetSykehusInnleggelse)
table(KoroData$OverfortAnnetSykehusUtskrivning)

#Alle koronapasienter pr HF :
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

TabFerdig <- FerdigeRegTab(RegData=KoroData)
                           # valgtEnhet=enh,
                           # enhetsNivaa = 'HF')





#--------------------------------------- Andeler ----------------------------------

variabler <- c('SAPSII', 'nyreBeh', 'nyreBehTid','spesTiltak')
for (valgtVar in variabler) {
   outfile <- paste0(valgtVar, '_Ford.pdf')
   NIRFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra1aar, datoTil=datoTil,
                 outfile=outfile)
}


