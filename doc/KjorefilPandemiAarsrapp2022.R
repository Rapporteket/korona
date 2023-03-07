#  FIGURER OG TABELLER TIL ÅRSRAPPORT, NiPar, Pandemi-data
# Henter alle data, aggregerer til personnivå, men tar høyde for flere sykdomsforløp (3 forløp: 6pers, 4 forl:1pers)
#Filtrerer på sykdomsforløp hvor covid er hovedårsak til første innleggelse i sykdomsforløpet

#--------Klargjøre data-----------
library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2022-12-31'	#
datoFra1aar <- '2022-01-01'

KoroDataRaa <- KoronaDataSQL(datoFra = '2021-01-01', datoTil = '2022-12-31')
KoroDataRaa <- KoronaDataSQL(datoTil = datoTil)
#KoroDataPre <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0)
KoroDataPre <- KoronaPreprosesser(RegData=KoroDataRaa, aggPers=1, kobleBered=1, tellFlereForlop=1)
#Legger til re-/nyinnleggelser osv. NB: Hvis vi kun ser på Covid som hovedårsak, mister vi noen reinnleggelser/overføringer.
#2021: valgt å beregne overføringer/nye innleggelser først og så filtrere på hovedårsak covid.
#Vi får dermed flere overføringer sammenlignet med 2020.
#2022: Vi skal ha personaggregerte data hvor vi teller flere sykdomsforløp. Velger inn-årsak covid i første innleggelse

#KoroDataPre <- korona::LeggTilNyInnOverf(RegData=KoroDataPre, PasientID='PasientID') Dette er inkludert i preprosess
KoroData <- KoroDataPre[KoroDataPre$ArsakInnleggelse==1, ] #Bare innleggelser pga Covid
KoroData1aar <- KoroData[KoroData$InnDato >= as.Date(datoFra1aar), ]

BeredDataRaa <- NIRberedskDataSQL(datoTil = datoTil)
BeredData <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 0)
BeredData1aar <- NIRPreprosessBeredsk(RegData=NIRberedskDataSQL(datoFra = datoFra1aar, datoTil = datoTil), aggPers = 0)
#BeredDataPers <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 1)

# Personnivå
#6.mars 2023: Usikker på om trenger disse.
# KoroDataPersAlle <- KoronaPreprosesser(RegData = KoronaDataSQL(), aggPers = 1)
# KoroDataPersAlle$BeredPas <-  ifelse(!is.na(match(KoroDataPersAlle$PersonId, BeredData$PersonId)), 1, 0)
# KoroDataPersAlle$BeredPasMRS <- ifelse(KoroDataPersAlle$Nir_beredskapsskjema_CoV2==1, 1,0) #Variabelen finner alle personer med intensivopphold
# KoroDataPersAlle$Diff <- KoroDataPersAlle$BeredPas - KoroDataPersAlle$BeredPasMRS
# KoroDataPers <- KoroDataPers[KoroDataPers$InnDato < '2022-01-01', ]
# test <- KoroDataPersAlle[which(KoroDataPers$Diff != 0), ]
# table(KoroDataPersAlle$BeredPas)
# KoroDataPers <- KoroDataPers[KoroDataPers$ArsakInnleggelse == 1,]


#-----------------------TABELLER--------------------------------

#------------Nøkkeltall pr HF---------
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

KoroData1aar$BeredPas <- ifelse(KoroData1aar$Nir_beredskapsskjema_CoV2==1, 1, 0)

Nokkeltall <- FerdigeRegTab(RegData=KoroData1aar)
tab <- Nokkeltall$Tab[-3, ]
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opphald', 'Del opphald')
intBehPas <- round(100*prop.table(table(KoroDataPers$BeredPas))[2],1)

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

#tabInn <- table(KoroDataPers$BeredPas[KoroDataPers$HF == enh])
#intBehInn <- round(100*prop.table(tabInn)[2],1)
#tabUt <- table(KoroDataPers$BeredPas[KoroDataPers$HFut == enh])
#intBehUt <- round(100*prop.table(tabUt)[2],1)


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
for (valgtVar in variabler) {
KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar, tidsenhet = 'Kvartal',
                  outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}

KoroData$BeredPas <- ifelse(KoroData$Nir_beredskapsskjema_CoV2==1, 1, 0)
KoronaFigAndelTid(RegData=KoroData, valgtVar='beredPas', tidsenhet = 'Kvartal', outfile = 'KoronaUtvTid_beredOpph.pdf')

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
