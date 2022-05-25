#  FIGURER OG TABELLER TIL ÅRSRAPPORT, NiPar, Pandemi-data


#--------Klargjøre data-----------
#Analyser baseres på opphold. Kun de med Covid som hovedårsak til innleggelse.

setwd('../speil/aarsrapp/pandemi')

library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2021-12-31'	#
datoFra1aar <- '2021-01-01'

KoroDataRaa <- KoronaDataSQL(datoTil = datoTil)
KoroDataPre <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0)
#KoroDataPers <- KoronaPreprosesser(RegData = KoroDataRaa[KoroDataRaa$ArsakInnleggelse==1, ], aggPers = 1)
#Legger til re-/nyinnleggelser osv. NB: Hvis vi kun ser på Covid som hovedårsak, mister vi noen reinnleggelser/overføringer.
#RegData <- KoroDataPre[ ,c('InnTidspunkt', 'UtTidspunkt', 'ReshId', 'PasientID', 'OverfortAnnetSykehusInnleggelse', 'OverfortAnnetSykehusUtskrivning')]
KoroDataPre <- korona::LeggTilNyInnOverf(RegData=KoroDataPre, PasientID='PasientID')
KoroDataPre1aar <- KoroDataPre[KoroDataPre$InnDato >= as.Date(datoFra1aar), ]
KoroData <- KoroDataPre[KoroDataPre$ArsakInnleggelse==1, ]
KoroData1aar <- KoroData[KoroData$InnDato >= as.Date(datoFra1aar), ]

BeredDataRaa <- NIRberedskDataSQL(datoTil = datoTil)
BeredData <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 0)
BeredDataPers <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 1)

#Finn pandemipasienter som har vært på intensiv
KoroDataAllePers <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 1)
KoroDataAllePers$BeredPas <-  ifelse(!is.na(match(KoroDataAllePers$PersonId, BeredData$PersonId)), 1, 0)
KoroDataPers <- KoroDataAllePers[KoroDataAllePers$CovidJaFinnes == 3, ] #Minst ett med Covid som hovedårsak



# #Sjekker...
#   persBered <- unique(BeredDataRaa$PersonId)
#   persPandemi <- unique(KoroDataRaa$PersonId)
#   persBeredUpandemi <- setdiff(persBered, persPandemi) #Bare registerert på intensiv
#   test <- BeredDataRaa$PersonId[BeredDataRaa$PersonId %in% persBeredUpandemi]
# #For perioden fram til 31.desember 2021:
#   # 42 pasienter er kun registrert i beredskapsskjema og aldri i pandemi.

# beredRegUpan <- BeredDataRaa[which(BeredDataRaa$PersonId %in% persBeredUpandemi),
#                              c("PersonId", "SkjemaGUID", "ShNavn", "RHF")]
#write.table(beredRegUpan, file = 'BeredRegUpan.csv', row.names = F, sep = ';')
#FEIL!!! Kan bare brukes for personaggregerte data. Må også ta hensyn til innleggelsestidspunkt.
# KoroData <- merge(KoroDataPre, BeredData, all.x = T, all.y = F,
#                   suffixes = c("", "Bered"), by = 'PersonId')

#Hvilke pandemiskjema har beredskapsskjema?
# #Koble personaggregerte data for sjekk
# KoblPers <- merge(KoroDataAllePers[which(KoroDataAllePers$Nir_beredskapsskjema_CoV2==1) ,c('PasientID', 'PersonId', 'Nir_beredskapsskjema_CoV2')],
#                   BeredDataPers[ ,c('PasientID', 'PersonId')],
#               all.x = T, all.y = F, by = 'PersonId', suffixes = c("", "Bered"))
# KoblPers <- merge(KoroDataAllePers[ ,c('PasientID', 'PersonId')], BeredDataPers[ ,c('PasientID', 'PersonId')],
#                   all.x = F, all.y = T, by = 'PersonId', suffixes = c("", "Bered"))
# sum(!is.na(KoblPers$PasientID))


# #Kobler pandemi og beredskap, opphold: - VIL IKKE GJØRE DETTE FOR ÅRSRAPP 2021 SIDEN DET IKKE ER NOEN ENTYDIG KOBLING ML PANDEMI OG BEREDSKAP PÅ OPPHOLDSNIVÅ
#   KoroDataPre$SkjemaGUIDBered <- as.character(BeredData$SkjemaGUID[match(KoroDataPre$PersonId, BeredData$PersonId)])
#   KoroDataPre$BeredPas <- !is.na(KoroDataPre$SkjemaGUIDBered)
#   table(table(KoroDataPre$SkjemaGUIDBered))
#   sum(!is.na(KoroDataPre$SkjemaGUIDBered))
#   length(unique(KoroDataPre$PersonId[!is.na(KoroDataPre$SkjemaGUIDBered)]))
#
# #Kan bare koble på personid. Flere personer har mer enn ett skjema. Får derfor ikke entydig kobling av
# #hvilket koronaopphold som har intensivopphold.
#   Kobl <- merge(KoroDataPre[,c("PersonId","SkjemaGUID")],
#               BeredData[,c("PersonId", "SkjemaGUID")],
#               all.x = T, all.y = F, # For å få med alle pandemiskjema
#               suffixes = c("", "Bered"), by = 'PersonId')
#   sum(!is.na(unique(Kobl$SkjemaGUIDBered)))
#   KoblRed <- Kobl %>% group_by(SkjemaGUID) %>%
#     summarise(SkjemaGUID = SkjemaGUID[1],
#             AntBered = sum(!is.na(SkjemaGUIDBered)),
#       BeredPas = ifelse(sum(!is.na(SkjemaGUIDBered))>0 ,1 ,0)
#       )
#   sort(table(KoblRed$AntBered))
#
# KoroData <- merge(KoroDataPre, KoblRed, by = 'SkjemaGUID')

# #Får vi nå et intensivopphold for alle skjema som tilhører en pasient som har hatt intensivopphold?
#     persInt <- length(unique(BeredData$PersonId))
#     pers <- unique(KoroData$PersonId[which(KoroData$BeredPas==1)])
#     length(pers)
#     test <- KoroData[which(KoroData$PersonId %in% pers), c("PersonId", "SkjemaGUID", "BeredPas")] #Har alle disse beredskapsopphold på hvert skjema
#     table(test$BeredPas)
#     table(KoroData$BeredPas)
# #Skjekk ved å sjekke antall beredskapsskjema som er knyttet opp. TEll også hvor mange beredskapsskjema hver pasient har og om

# skjemaReinn <- KoroData$SkjemaGUID[KoroData$Reinn==1]
# skjemaOverf <- KoroData$SkjemaGUID[KoroData$Overf==1]
# colSums(KoroData[ ,c('Reinn', 'Overf')])
# table(KoroData$Reinn)
# table(KoroData$Overf)
#   table(KoroData$OverfortAnnetSykehusInnleggelse)
#   table(KoroData$OverfortAnnetSykehusUtskrivning)
#   persReinn <- KoroData$PersonId[KoroData$Reinn==1]
#   KoroData <- KoroData[KoroData$PersonId %in% persReinn, ]
#   test <- KoroData[KoroData$Overf==1 | KoroData$OverfortAnnetSykehusInnleggelse==1 | KoroData$OverfortAnnetSykehusUtskrivning==1,
#            c('Overf', 'OverfortAnnetSykehusInnleggelse', 'OverfortAnnetSykehusUtskrivning',
#            'PersonId', "InnTidspunkt", "UtTidspunkt",'Reinn')]
#write.table(test)


#------------Nøkkeltall pr HF---------
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

#RegData <- KoroData
Nokkeltall <- FerdigeRegTab(RegData=KoroData1aar)
tab <- Nokkeltall$Tab[-7, ]
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opph.', 'Del opph.')
intBeh <- round(100*prop.table(table(KoroDataPers$BeredPas))[2],1)

print(xtable::xtable(tab, align=c('l','r','r','c','r','r'),
                     label = 'tab:pan_tot',
                     caption=paste0('Nøkkeltal for pandemipasientar. Dei ', Nokkeltall$N,
                                    ' opphalda gjeld ', Nokkeltall$AntPas, ' pasientar. ',
                              intBeh, '\\% av pasientane er intensivbehandla.')
))

HFer <- unique(KoroData$HF)
for (enh in HFer) {
Nokkeltall <- FerdigeRegTab(RegData=KoroData,
                            valgtEnhet=enh,
                            enhetsNivaa = 'HF')
colnames(Nokkeltall$Tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opph.', 'Del opph.')

tabInn <- table(KoroDataPers$BeredPas[KoroDataPers$HF == enh])
intBehInn <- round(100*prop.table(tabInn)[2],1)
tabUt <- table(KoroDataPers$BeredPas[KoroDataPers$HFut == enh])
intBehUt <- round(100*prop.table(tabUt)[2],1)


print(xtable::xtable(Nokkeltall$Tab, align=c('l','r','r','c','r','r'),
               #digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
               caption=paste0('Nøkkeltal for ', enh,'. De ', Nokkeltall$N,
                              ' opphalda gjeld ', Nokkeltall$AntPas, ' pasientar. ',
                              intBehInn, '\\% av dei ', sum(tabInn), ' pasientane som vart
                              innlagde på ', enh,' er intensivbehandla medan ',
                              intBehUt, '\\% av dei ', sum(tabUt), ' pasientane utskrivne frå ', enh,' er intensivbehandla')
                              ))
}

#-------------Div-------------
#Alder- og kjønnsfigur
AlderKjFig(RegData=KoroData, datoFra = datoFra1aar, outfile='KoronaAlderKj.pdf')

#Covid-19 hovedårsak til innleggeslse?
Aarsak <- paste0(sprintf('%.1f', prop.table(table(KoroDataPre$ArsakInnleggelse[KoroDataPre$InnDato > as.Date(datoFra1aar)]))*100),' %')
names(Aarsak) <- c('Ja', 'Nei', 'Ukjent')

#----------------Alle fordelingsfigurer, basert på opphold-----------------
variabler <- c( 'demografi', 'liggetid', 'risikoInn',
                'antibiotikaInn', 'antibiotikaUt', 'regForsinkelseInn', 'regForsinkelseUt',
                'respSviktInn', 'respSviktUt', 'sirkSviktInn', 'sirkSviktUt', 'tilstandInn')
#Ikke brukt -20: 'alder',

for (valgtVar in variabler) {
  KoronaFigAndeler(RegData=KoroData, datoFra=datoFra1aar, valgtVar=valgtVar, outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}


#----------------Alle figurer, tidsuvikling, basert på opphold-----------------
variabler <- c('alder_u18', 'alder_u40', 'alder_o60', 'alder_o80', 'isolertInn',  'dodSh')
#valgtVar <- 'alder_o80'
for (valgtVar in variabler) {
KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar, tidsenhet = 'Mnd',
                  outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}

#Denne må baseres på personnivå:
KoronaFigAndelTid(RegData=KoroDataPers, valgtVar='beredPas', tidsenhet = 'Mnd', outfile = 'KoronaUtvTid_beredPas.pdf')

#--------------------Testing-----------------------------
test <- KoroData[,c('OverfortAnnetSykehusInnleggelse', 'OverfortAnnetSykehusUtskrivning', "Overf")]
# test <- RegDataSort[sort(unique(c(indPasFlereOpph, indPasFlereOpph-1))),
#                     c("PasientID", "OpphNr","Reinn","NyInn", "TidUtInn","InnTidspunkt", "UtTidspunkt", "ReshId")]
#OverfortAnnetSykehusInnleggelse:
#Ble pasienten overført fra et annet sykehus til dette sykehuset ved innleggelse?#1,2,3: Ja, Nei, Ukjent
table(KoroData$OverfortAnnetSykehusInnleggelse)
table(KoroData$OverfortAnnetSykehusUtskrivning)
table(KoroData$Overf)

 test <- KoroData[KoroData$OpphNr>1,
                  c("PasientID", "OpphNr","Reinn","Overf", 'OverfortAnnetSykehusInnleggelse', "TidUtInn","InnTidspunkt", "UtTidspunkt", "ReshId")]
