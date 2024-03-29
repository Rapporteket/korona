#  FIGURER OG TABELLER TIL ÅRSRAPPORT, NiPar, Pandemi-data


#--------Klargjøre data-----------
#Analyser baseres på opphold. Kun de med Covid som hovedårsak til innleggelse.
#Alle pandemidata for 2020 er nå ferdigstilte

#Inklusjon <- read.table('A:/Pandemi/InklusjonSkjemaDataContract2021-04-05', sep=';',
#                                    stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# KoroDataInn <- read.table('I:/korona/InklusjonSkjemaDataContract2020-06-11 09-29-30.txt', sep=';',
#                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# KoroDataInn <- KoroDataInn %>% select(-Utskrivningsdato)
# KoroDataUt <- read.table('I:/korona/UtskrivningSkjemaDataContract2020-06-11 09-29-30.txt', sep=';',
#                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# names(KoroDataUt)[names(KoroDataUt) == "HelseenhetKortNavn"] <- "ShNavnUt"
# BeredData <-  read.table('I:/nir/ReadinessFormDataContract2020-06-11 09-31-13.txt', sep=';',
#                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# BeredData$EcmoEnd[BeredData$EcmoEnd == ""] <- NA
# BeredData$EcmoStart[BeredData$EcmoStart == ""] <- NA
# varUt <- c("Antifungalbehandling", "AntiviralBehandling" , "HovedskjemaGUID", 'ShNavnUt',
#            'FormStatus', 'FormDate', "OverfortAnnetSykehusUtskrivning", "StatusVedUtskriving", 'Utskrivningsdato')
# KoroDataRaa <- merge(KoroDataInn, KoroDataUt[,varUt], suffixes = c('','Ut'),
#                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)

setwd('/home/rstudio/korona/Aarsrapport')

library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-01-01' #Vi har pandemi fra 1.mars..
datoTil <- '2020-12-31'	#
datoFra1aar <- '2020-01-01'

KoroDataRaa <- KoronaDataSQL(datoTil = datoTil)
KoroDataPre <- KoronaPreprosesser(RegData = KoroDataRaa[KoroDataRaa$ArsakInnleggelse==1, ], aggPers = 0)
KoroDataAllePers <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 1)
#length(unique(KoroDataRaa$PersonId))
#length(unique(KoroDataPre$PersonId))

BeredDataRaa <- NIRberedskDataSQL()
BeredData <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 0)
BeredDataPers <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 1)
#Kobler pandemi og beredskap:
persBered <- unique(BeredDataRaa$PersonId)
persPandemi <- unique(KoroDataRaa$PersonId)
persBeredUpandemi <- setdiff(persBered, persPandemi)
beredRegUpan <- BeredDataRaa[which(BeredDataRaa$PersonId %in% persBeredUpandemi),
                             c("PersonId", "SkjemaGUID", "ShNavn", "RHF")]
write.table(beredRegUpan, file = 'BeredRegUpan.csv', row.names = F, sep = ';')
#FEIL!!! Kan bare brukes for personaggregerte data. Må også ta hensyn til innleggelsestidspunkt.
# KoroData <- merge(KoroDataPre, BeredData, all.x = T, all.y = F,
#                   suffixes = c("", "Bered"), by = 'PersonId')

#Hvilke pandemiskjema har beredskapsskjema?
#Koble personaggregerte data for sjekk
KoblPers <- merge(KoroDataAllePers[which(KoroDataAllePers$Nir_beredskapsskjema_CoV2==1) ,c('PasientID', 'PersonId', 'Nir_beredskapsskjema_CoV2')],
                  BeredDataPers[ ,c('PasientID', 'PersonId')],
              all.x = T, all.y = F, by = 'PersonId', suffixes = c("", "Bered"))
KoblPers <- merge(KoroDataAllePers[ ,c('PasientID', 'PersonId')], BeredDataPers[ ,c('PasientID', 'PersonId')],
                  all.x = F, all.y = T, by = 'PersonId', suffixes = c("", "Bered"))
sum(!is.na(KoblPers$PasientID))

KoroDataPre$SkjemaGUIDBered <- as.character(BeredData$SkjemaGUID[match(KoroDataPre$PersonId, BeredData$PersonId)])
KoroDataPre$BeredPas <- !is.na(KoroDataPre$SkjemaGUIDBered)
table(table(KoroDataPre$SkjemaGUIDBered))
sum(!is.na(KoroDataPre$SkjemaGUIDBered))
length(unique(KoroDataPre$PersonId[!is.na(KoroDataPre$SkjemaGUIDBered)]))
#RegData$HFkort <- as.character(HFmap$HFnavn[match(RegData$HFresh, HFmap$HFresh)])
#Kan bare koble på personid. Flere personer har mer enn ett skjerma. Får derfor ikke entydig kobling av
#hvilket koronaopphold som har intensivopphold.
Kobl <- merge(KoroDataPre[,c("PersonId","SkjemaGUID")], #, "InnTidspunkt", "UtTidspunkt" )],
              BeredData[,c("PersonId", "SkjemaGUID")], #"Innleggelsestidspunkt", "DateDischargedIntensive")],
              all.x = T, all.y = F, # For å få med alle pandemiskjema
              suffixes = c("", "Bered"), by = 'PersonId')
sum(!is.na(unique(Kobl$SkjemaGUIDBered)))
sort(table(Kobl$SkjemaGUIDBered))
KoblRed <- Kobl %>% group_by(SkjemaGUID) %>%
  dplyr::summarise(SkjemaGUID = SkjemaGUID[1],
            AntBered = sum(!is.na(SkjemaGUIDBered)),
    BeredPas = ifelse(sum(!is.na(SkjemaGUIDBered))>0 ,1 ,0)
    )
sort(table(KoblRed$AntBered))

#KoroData  <- KoroData %>% mutate(BeredPas = ifelse(is.na(PasientIDBered), 0, 1))

KoroData <- merge(KoroDataPre, KoblRed, by = 'SkjemaGUID')

#Får vi nå et intensivopphold for alle skjema som tilhører en pasient som har hatt intensivopphold?
persInt <- length(unique(BeredData$PersonId))
pers <- unique(KoroData$PersonId[which(KoroData$BeredPas==1)])
length(pers)
test <- KoroData[which(KoroData$PersonId %in% pers), c("PersonId", "SkjemaGUID", "BeredPas")] #Har alle disse beredskapsopphold på hvert skjema
table(test$BeredPas)
table(KoroData$BeredPas)
#Skjekk ved å sjekke antall beredskapsskjema som er knyttet opp. TEll også hvor mange beredskapsskjema hver pasient har og om

#Legger til reinnleggelser osv
KoroData <- LeggTilNyInnOverf(RegData=KoroData, PasientID='PasientID')

table(KoroData$Overf)
table(KoroData$OverfortAnnetSykehusInnleggelse)
table(KoroData$OverfortAnnetSykehusUtskrivning)
test <- KoroData[KoroData$Overf==1 | KoroData$OverfortAnnetSykehusInnleggelse==1 | KoroData$OverfortAnnetSykehusUtskrivning==1,
         c('Overf', 'OverfortAnnetSykehusInnleggelse', 'OverfortAnnetSykehusUtskrivning',
         'PersonId', "InnTidspunkt", "UtTidspunkt")]
write.table(test)

#------------Alle koronapasienter pr HF---------
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

#RegData <- KoroData
Nokkeltall <- FerdigeRegTab(RegData=KoroData)
colnames(Nokkeltall$Tab) <- c('Gj.sn', 'Median', 'IQR', 'Antall opphold', 'Andel opphold')
enh <- 'Alle'


HFer <- unique(KoroData$HF)
for (enh in HFer) {
Nokkeltall <- FerdigeRegTab(RegData=KoroData,
                            valgtEnhet=enh,
                            enhetsNivaa = 'HF')
colnames(Nokkeltall$Tab) <- c('Gj.sn', 'Median', 'IQR', 'Antall opph.', 'Andel opph.')
print(xtable::xtable(Nokkeltall$Tab, align=c('l','r','r','c','r','r'),
               #digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
               caption=paste0('Nøkkeltall for ', enh,'. De ', Nokkeltall$N,
                              ' oppholdene gjelder ', Nokkeltall$AntPas, ' pasienter.')))
}

#-------------Div-------------
#Alder- og kjønnsfigur
AlderKjFig (RegData=KoroData, outfile='KoronaAlderKj.pdf')

#Covid-19 hovedårsak til innleggeslse?
Aarsak <- paste0(sprintf('%.1f', prop.table(table(KoroDataRaa$ArsakInnleggelse))*100),' %')
names(Aarsak) <- c('Ja', 'Nei', 'Ukjent')

#----------------Alle fordelingsfigurer, basert på opphold-----------------
variabler <- c('alder', 'demografi', 'liggetid', 'risikoInn',
                'antibiotikaInn', 'antibiotikaUt', 'regForsinkelseInn', 'regForsinkelseUt',
                'respSviktInn', 'respSviktUt', 'sirkSviktInn', 'sirkSviktUt', 'tilstandInn')

for (valgtVar in variabler) {
  KoronaFigAndeler(RegData=KoroData, valgtVar=valgtVar, outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

# valgtVar <- 'liggetid'
# RegData <- KoroData

#----------------Alle figurer, tidsuvikling, basert på opphold-----------------
variabler <- c('alder_u18', 'alder_u40', 'alder_o60', 'alder_o80', 'isolertInn', 'beredPas', 'dodSh')
#valgtVar <- 'alder_o80'
for (valgtVar in variabler) {
KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar, tidsenhet = 'Mnd',
                  outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}

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
