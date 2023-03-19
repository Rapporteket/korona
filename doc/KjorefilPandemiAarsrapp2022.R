#  FIGURER OG TABELLER TIL ÅRSRAPPORT, NiPar, Pandemi-data
# Henter alle data, aggregerer til personnivå, men tar høyde for flere smitteforløp (3 forløp: 6pers, 4 forl:1pers)
#Filtrerer på smitteforløp hvor covid er hovedårsak til første innleggelse i smitteforløpet

#--------Klargjøre data-----------
#Legger til re-/nyinnleggelser osv. NB: Hvis vi kun ser på Covid som hovedårsak, mister vi noen reinnleggelser/overføringer.
#2021: valgt å beregne overføringer/nye innleggelser først og så filtrere på hovedårsak covid.
#Vi får dermed flere overføringer sammenlignet med 2020.
#2022: Vi skal ha personaggregerte data hvor vi teller flere smitteforløp. Smitteforløp med minst ett opphold hvor Covid hovedårsak.

setwd('AarsRappFig/')
library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2022-12-31'	#
datoFra1aar <- '2022-01-01'

KoroDataRaa <- KoronaDataSQL(datoTil = datoTil)
KoroDataOpph <- KoronaPreprosesser(RegData=KoroDataRaa, aggPers=0, kobleBered=1)
KoroDataPers <- KoronaPreprosesser(RegData=KoroDataRaa, aggPers=1, kobleBered=1, tellFlereForlop=1)
#ELLER: (pass på at staging oppdatert)
KoroDataPers <- rapbase::loadStagingData("korona", "KoroData")

# sjekk <- KoroDataOpph[KoroDataOpph$Nir_beredskapsskjema_CoV2==1 & KoroDataOpph$BeredReg==0 &
#                          KoroDataOpph$Aar==2022 & KoroDataOpph$ArsakInnleggelse==1,
#                       c("SkjemaGUID", "InnDato", 'Aar')]
# table(KoroDataOpph[ ,c('Nir_beredskapsskjema_CoV2', 'Aar')])

#Bare smitteforløp hvor alle opph har Covid som hovedårsak
KoroData <- KoronaUtvalg(RegData = KoroDataPers, aarsakInn = 1, datoTil = datoTil)$RegData
KoroData1aar <- KoroData[KoroData$InnDato >= as.Date(datoFra1aar), ]

#-----------------------TABELLER--------------------------------

#----Nøkkeltall pr HF---
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

KoroData1aar$BeredPasTest <- ifelse(KoroData1aar$Nir_beredskapsskjema_CoV2==1, 1, 0)
#Antall med beredskapsskjema stemmer ikke med antall som vi får koblet til.
#For 2022 kobles 536 smitteforløp til beredskapsforløp, mens det er 601 forløp som skal ha intensivopphold,
#basert på "Nir_beredskapsskjema_CoV2", altså 65 forløp hvor man ikke finner det aktuelle beredskapsskjemaet.
#                         BeredPas
# Nir_beredskapsskjema_CoV2     0     1
#                          1    65   536
#                          2 11202     0


Nokkeltall <- FerdigeRegTab(RegData=KoroData1aar)
tab <- Nokkeltall$Tab[-3, ]
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opphald', 'Del opphald')
intBehPas <- round(100*prop.table(table(KoroData$BeredReg))[2],1)

print(xtable::xtable(tab, align=c('l','r','r','c','r','r'),
                     label = 'tab:pan_tot',
                     caption=paste0('Nøkkeltal for pandemipasientar. Dei ', Nokkeltall$N,
                                    ' opphalda gjeld ', Nokkeltall$AntPas, ' pasientar. '
                              ,intBehPas, '\\% av pasientane er intensivbehandla.')
))

HFer <- unique(KoroData$HF)
for (enh in HFer) {
Nokkeltall <- FerdigeRegTab(RegData=KoroData1aar,
                            valgtEnhet=enh,
                            enhetsNivaa = 'HF')
colnames(Nokkeltall$Tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opphald', 'Del opphald')

print(xtable::xtable(Nokkeltall$Tab[-3, ], align=c('l','r','r','c','r','r'),
               #digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
               caption=paste0('Nøkkeltal for ', enh,'. De ', Nokkeltall$N,
                              ' opphalda gjeld ', Nokkeltall$AntPas, ' pasientar. ')
                              # ,intBehInn, '\\% av dei ', sum(tabInn), ' pasientane som vart
                              # innlagde på ', enh,' er intensivbehandla medan ',
                              # intBehUt, '\\% av dei ', sum(tabUt), ' pasientane utskrivne frå ', enh,' er intensivbehandla')
                              ))
}

#-------------FIGURER-------------
#Innleggelser
AntTab <- antallTidEnhTab(RegData=KoroDataOpph, datoTil = as.Date(datoTil), #tilgangsNivaa=rolle,valgtEnhet= egenEnhet,
                          tidsenhet= 'maaned'
                          )
korona::FigTidEnhet(AntTab, outfile='KoronaInnleggelserMnd.pdf')

#Alder- og kjønnsfigur
#Annen visning
dum <- AlderKjFig(RegData=KoroData1aar, outfile='KoronaAlderKj.pdf')

#Covid-19 hovedårsak til innleggeslse?
Aarsak <- paste0(sprintf('%.1f', prop.table(table(KoroData$ArsakInnleggelse[KoroData$InnDato > as.Date(datoFra1aar)]))*100),' %')
names(Aarsak) <- c('Ja', 'Nei', 'Ukjent')

#----------------Alle fordelingsfigurer, basert på smitteforløp-----------------
# 'liggetid',
variabler <- c('regForsinkelseInn', 'regForsinkelseUt')
for (valgtVar in variabler) {
  KoronaFigAndeler(RegData=KoroData, datoFra=datoFra1aar, valgtVar=valgtVar, aarsakInn=1,
                   outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

#Kun for 2020 - 1.kvartal 2022
variabler <- c('respSviktInn', 'respSviktUt', 'risikoInn', 'sirkSviktInn', 'sirkSviktUt', 'tilstandInn')
for (valgtVar in variabler) {
   KoronaFigAndeler(RegData=KoroData, valgtVar=valgtVar, aarsakInn=1,
                    datoTil='2022-04-11', outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

#Kun for 2020 - 1.kvartal 2022
variabler <- c('antibiotikaInn', 'antibiotikaUt')
for (valgtVar in variabler) {
   KoronaFigAndeler(RegData=KoroData, valgtVar=valgtVar, aarsakInn=1,
                    datoTil='2022-04-11', outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

#----------------Alle figurer, tidsuvikling, basert på smitteforløp-----------------
variabler <- c('alder_u18', 'alder_u40', 'alder_o60', 'alder_o80', 'beredPas', 'dodSh',
               'isolertInn')
for (valgtVar in variabler) {
   KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar,
                     tidsenhet = 'Kvartal', aarsakInn=1,
                     outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}

KoronaFigAndelTid(RegData=KoroData, valgtVar='risikoInn', datoTil = '2022-03-31',
                  tidsenhet = 'Kvartal', aarsakInn=1,
                  outfile = paste0('KoronaUtvTid_risikoInn.pdf'))


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



 #-----------Datakvalitet. Kompletthet-----------------------
 # NB: Bare de med Covid som hovedårsak til innleggelse
 # 7403 inn-skjema
 # 7187 ut-skjema

 manglerUt <- innManglerUt(KoroData1aar) #Kun ett skjema

 KoroInnDataRaa <- korona::KoronaDataSQL(datoFra = datoFra1aar, datoTil = datoTil,
                                      skjema = 1, koble = 0)
 varNavnInn <- intersect(names(KoroInnDataRaa), names(KoroData1aar))
 KoroInnData <- KoroData1aar[KoroData1aar$ArsakInnleggelse == 1, varNavnInn]


 Nkoro <- dim(KoroData1aar)[1]
 antNA <- colSums(is.na(KoroInnData)) + colSums(KoroInnData==-1, na.rm = T)
#Ukjente.
 #AkuttSirkulasjonsvikt og  AkuttRespirasjonsvikt har ukjent kodet 999
 #RontgenThorax har ukjent kodet 5
  antUkj <- colSums(KoroInnData ==3, na.rm = T)
 antUkj[which(names(antUkj) == 'AkuttSirkulasjonsvikt')] <- sum(KoroInnData$AkuttSirkulasjonsvikt == 999)
 antUkj[which(names(antUkj) == 'AkuttRespirasjonsvikt')] <- sum(KoroInnData$AkuttRespirasjonsvikt == 999)
 antUkj[which(names(antUkj) == 'RontgenThorax')] <- sum(KoroInnData$RontgenThorax == 5)

 antUkj[which(names(antUkj) == 'Antibiotika')] <- sum(KoroInnData$AntibiotikaUkjent)
 antUkj[which(names(antUkj) == 'Bilirubin')] <- sum(KoroInnData$BilirubinUkjent)
 antUkj[which(names(antUkj) == 'Ddimer')] <- sum(KoroInnData$DdimerUkjent)
 antUkj[which(names(antUkj) == 'DiastoliskBlodtrykk')] <- sum(KoroInnData$DiastoliskBlodtrykkUkjent)
 antUkj[which(names(antUkj) == 'Hjertefrekvens')] <- sum(KoroInnData$HjertefrekvensUkjent)
 antUkj[which(names(antUkj) == 'Hoyde')] <- sum(KoroInnData$HoydeUkjent)
 antUkj[which(names(antUkj) == 'Kreatinin')] <- sum(KoroInnData$SkreatininUkjent)
 antUkj[which(names(antUkj) == 'Leukocytter')] <- sum(KoroInnData$LeukocytterUkjent)
 antUkj[which(names(antUkj) == 'Oksygenmetning')] <- sum(KoroInnData$OkysgenmetningUkjent)
 antUkj[which(names(antUkj) == 'Respirasjonsfrekvens')] <- sum(KoroInnData$RespirasjonsfrekvensUkjent)
 antUkj[which(names(antUkj) == 'SystoliskBlodtrykk')] <- sum(KoroInnData$SystoliskBlodtrykkUkjent)
 antUkj[which(names(antUkj) == 'Temp')] <- sum(KoroInnData$TempUkjent)
 antUkj[which(names(antUkj) == 'Trombocytter')] <- sum(KoroInnData$TrombocytterUkjent)
 antUkj[which(names(antUkj) == 'Vekt')] <- sum(KoroInnData$VektUkjent)

 fjernVarInn <- c('AntibiotikaUkjent', 'BilirubinUkjent', 'DdimerUkjent', 'DiastoliskBlodtrykkUkjent',
   'HjertefrekvensUkjent', 'HoydeUkjent', 'LeukocytterUkjent', 'OkysgenmetningUkjent',
   'RespirasjonsfrekvensUkjent', 'SystoliskBlodtrykkUkjent', 'SkreatininUkjent', 'TempUkjent', 'TrombocytterUkjent', 'VektUkjent',
   'FormDate', 'Helseenhet', 'HelseenhetID', 'HelseenhetKortNavn', 'HF', 'PersonId', 'SkjemaGUID',
   'RHF', 'Sykehus', 'CurrentMunicipalNumber', 'DistrictCode', 'MunicipalNumber', 'Municipal', 'PersonIdBC19Hash',
   'Importert', 'FormStatus', 'FormTypeId', 'CreationDate', 'Innleggelse',
   'ArsakInnleggelse')

tabInn <- cbind(
  'Tal tomme' = antNA,
  'Tal ukjend' = antUkj,
  'Del tomme' = paste0(sprintf('%.1f', 100*antNA/Nkoro), '%'),
  'Del ukjend' = paste0(sprintf('%.1f', 100*antUkj/Nkoro), '%')
)


tabInn <- tabInn[-which(names(antUkj) %in% fjernVarInn), ]


 xtable::xtable(tabInn,
                digits = 0,
                align = c('l','r','r','r','r'),
        caption = 'Komplettheit for variablar registrert ved innlegging i pandemidelen av registeret.',
        label = 'tab:pan_kompl_inn'
 )


#--------ut
 varNavn <- c('Antifungalbehandling'  ,'AntiviralBehandling','UtsAkuttNyresvikt','UtsAkuttRespirasjonsvikt'
 ,'UtsAkuttSirkulasjonsvikt','UtsAminoglykosid','UtsAndreGencefalosporin','UtsAntibiotika'
 ,'UtsAntibiotikaAnnet' ,'UtsAntibiotikaUkjent','UtsAntifungalbehandling','UtsAntiviralBehandling'
 ,'FirstTimeClosedUt','ImportertUt','UtsKarbapenem','UtsKinolon','UtsMakrolid','UtsPenicillin'
 ,'UtsPenicillinEnzymhemmer','OverfortAnnetSykehusUtskrivning'
 ,'UtsTredjeGencefalosporin','StatusVedUtskriving','Utskrivningsdato')

 KoroUtData <- KoroData1aar[, varNavn]

 Nkoro <- dim(KoroData1aar)[1]
 antNA <- colSums(is.na(KoroUtData)) + colSums(KoroUtData==-1, na.rm = T)
 #Ukjente.
 #AkuttSirkulasjonsvikt og  AkuttRespirasjonsvikt har ukjent kodet 999
 #RontgenThorax har ukjent kodet 5
 antUkj <- colSums(KoroUtData ==3, na.rm = T)
 antUkj[which(names(antUkj) == 'UtsAkuttSirkulasjonsvikt')] <- sum(KoroUtData$UtsAkuttSirkulasjonsvikt == 999, na.rm = T)
 antUkj[which(names(antUkj) == 'UtsAkuttRespirasjonsvikt')] <- sum(KoroUtData$UtsAkuttRespirasjonsvikt == 999, na.rm = T)

 tabUt <- cbind(
   'Tal tomme' = antNA,
   'Tal ukjend' = antUkj,
   'Del tomme' = paste0(sprintf('%.1f', 100*antNA/Nkoro), '%'),
   'Del ukjend' = paste0(sprintf('%.1f', 100*antUkj/Nkoro), '%')
 )

 tabUt <- tabUt[-which(names(antUkj) %in% 'UtsAntibiotikaUkjent'), ]

 xtable::xtable(tabUt,
              digits = 0,
        caption = 'Komplettheit for variablar registrert ved utskriving i pandemidelen av registeret.',
        label = 'tab:pan_kompl_ut'
 )
