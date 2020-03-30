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
