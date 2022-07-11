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
#Legger til re-/nyinnleggelser osv. NB: Hvis vi kun ser på Covid som hovedårsak, mister vi noen reinnleggelser/overføringer.
#Jeg har nå (for 2021) valgt å beregne overføringer/nye innleggelser først og så filtrere på hovedårsak covid. Vi får dermed flere overføringer sammenlignet med i fjor.

KoroDataPre <- korona::LeggTilNyInnOverf(RegData=KoroDataPre, PasientID='PasientID')
KoroData <- KoroDataPre[KoroDataPre$ArsakInnleggelse==1, ] #Bare innleggelser pga Covid
KoroData1aar <- KoroData[KoroData$InnDato >= as.Date(datoFra1aar), ]

BeredDataRaa <- NIRberedskDataSQL(datoTil = datoTil)
BeredData <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 0)
BeredData1aar <- NIRPreprosessBeredsk(RegData=NIRberedskDataSQL(datoFra = datoFra1aar, datoTil = datoTil), aggPers = 0)
#BeredDataPers <- NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = 1)

# Personnivå
KoroDataPersAlle <- KoronaPreprosesser(RegData = KoronaDataSQL(), aggPers = 1)
KoroDataPersAlle$BeredPas <-  ifelse(!is.na(match(KoroDataPersAlle$PersonId, BeredData$PersonId)), 1, 0)
KoroDataPersAlle$BeredPasMRS <- ifelse(KoroDataPersAlle$Nir_beredskapsskjema_CoV2==1, 1,0) #Variabelen finner alle personer med intensivopphold
KoroDataPersAlle$Diff <- KoroDataPersAlle$BeredPas - KoroDataPersAlle$BeredPasMRS
KoroDataPers <- KoroDataPers[KoroDataPers$InnDato < '2022-01-01', ]
# KoroDataPers <- KoroDataAllePers[KoroDataAllePers$CovidJaFinnes == 3, ] #Minst ett med Covid som hovedårsak
test <- KoroDataPersAlle[which(KoroDataPers$Diff != 0), ]
table(KoroDataPersAlle$BeredPas)
KoroDataPers <- KoroDataPers[KoroDataPers$ArsakInnleggelse == 1,]

# #Finn pandemipasienter som har vært på intensiv

# Vi har brukt innleggelse og utskrivningsdato fra pandemiregisteret som endepunkt,
# og lagt til data fra intensivregisteret i de tilfeller hvor innleggelse registrert i intensivregisteret
# er innenfor tidsrommet definert av Innleggelse og Utskrivningsdato fra pandemiregisteret.

Data_koblet = data_pandemi %>%
  dplyr::left_join(data_intensiv, by = 'PasientID') %>%
  Filter(DateAdmittedIntensive >= Innleggelse & DateAdmittedIntensiv <= Utskrivningsdato)

# Her legges hvert eneste intensivoppholdsskjema til som egen rad for hver rad hvor
# PersonId/PasientGUID matcher mellom datasettene,
# så derfor må vi filtrere bort skjema som ikke er aktuelle i etterkant.
# Dette fungerer fint hvis det kun er snakk om at ett skjema finnes for hver periode pasienten
# er registrert i pandemiregisteret, men dette ikke alltid tilfellet.
# I de tilfellene hvor det finnes flere intensivoppholdsskjema for perioden, må data gjerne aggregeres på forhånd.
# Vi har brukt definisjonene 12 timer for pandemiregisteret og 24 timer for intensivregisteret for
# å identifisere påfølgende opphold som er del av samme sykdomsforløp.

d_pandemi_forløp = d_pandemi_samlet %>%
    group_by(PersonId) %>%
    mutate(
      tid_siden_forrige = difftime(Innleggelse, lag(Utskrivningsdato), units = "hours"),
      innen_12_timer = as.numeric(tid_siden_forrige) < 12,
      radnummer = row_number(),
      overfort_nummer = cumsum(innen_12_timer & radnummer != 1L),
      sykdomsforlop = radnummer - overfort_nummer)

#Vi kan da aggregere data vha sykdomsforløp som grupperingsvariabel før en kobling gjøres.

#FULL KODE
library(tidyverse)
library(lubridate)

# Koble pandemiskjema med utskrivningsskjema - hvorfor ikke benytte hovedskjemaguid?
d_pandemi_samlet = d_pandemi_innleggelse %>%
  left_join(d_pandemi_utskrivning, by = c("PersonId", "Innleggelse"))

d_pandemi_samlet <- KoroData1aar
d_intensivopphold <- BeredData1aar
d_beredskap <- BeredData1aar

d_pandemi_forløp = d_pandemi_samlet %>%
  group_by(PersonId) %>%
  mutate(
    tid_siden_forrige = difftime(Innleggelse, lag(Utskrivningsdato), units = "hours"),
    innen_12_timer = as.numeric(tid_siden_forrige) < 12,
    radnummer = row_number(),
    overfort_nummer = cumsum(innen_12_timer & radnummer != 1L),
    sykdomsforlop = radnummer - overfort_nummer)

d_intensiv_samlet = d_intensivopphold %>%
  distinct(PersonId, DateAdmittedIntensive, .keep_all = TRUE) %>%  # Fjerner et duplikat
  #left_join(d_beredskap, by = c("PersonId", "DateAdmittedIntensive")) %>%
  group_by(PersonId) %>%
  mutate(
    tid_siden_forrige = difftime(DateAdmittedIntensive, lag(DateDischargedIntensive), units = "hours"),
    innen_12_timer = as.numeric(tid_siden_forrige) < 12,
    radnummer = row_number(),
    overfort_nummer = cumsum(innen_12_timer & radnummer != 1L),
    sykdomsforlop = radnummer - overfort_nummer)

# Aggregere Pandemi og intensivdata --------------------------------------
d_pandemi_aggregert = d_pandemi_forløp %>%
  group_by(PersonId, sykdomsforlop) %>%
  dplyr::summarise(Innleggelse = first(Innleggelse),
            Utskrivningsdato = last(Utskrivningsdato),
            .groups = "drop") %>%
  select(-sykdomsforlop)

d_intensiv_aggregert = d_intensiv_samlet %>%
  group_by(PersonId, sykdomsforlop) %>%
  dplyr::summarise(#InvasivVentilation = sum(InvasivVentilation, na.rm = TRUE),
            DateAdmittedIntensive = first(DateAdmittedIntensive),
            DateDischargedIntensive = last(DateDischargedIntensive),
            #DischargedIntensiveStatus = NeiJaUkjentIntensiv(DischargedIntensiveStatus),
            Diagnosis = first(Diagnosis),
            .groups = "drop") %>%
  select(-sykdomsforlop)

d_intensivdata_til_pandemi = d_pandemi_aggregert %>%
  left_join(d_intensiv_aggregert, by = "PersonId") %>%
  filter(DateAdmittedIntensive >= Innleggelse &
           DateAdmittedIntensive <= Utskrivningsdato)

d_fullt_datasett = d_pandemi_aggregert %>%
  anti_join(d_intensivdata_til_pandemi, by = c("PersonId", "Innleggelse")) %>%
  bind_rows(d_intensivdata_til_pandemi)


















# #Kobler pandemi og beredskap, opphold: - DET ER IKKE NOEN ENTYDIG KOBLING ML PANDEMI OG BEREDSKAP PÅ OPPHOLDSNIVÅ
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


#-----------------------TABELLER--------------------------------


#------------Nøkkeltall pr HF---------
#FerdigeRegTab pas -> opph.
#Inneholder: liggetid, alder, BMI, om pasienten har risikofaktorer, andel reinnleggelse (>24t),
#andel døde + andel isolert ved innleggelse (kval.ind), antall pasienter

KoroData1aar$BeredPas <- ifelse(KoroData1aar$Nir_beredskapsskjema_CoV2==1, 1, 0)

Nokkeltall <- FerdigeRegTab(RegData=KoroData1aar)
tab <- Nokkeltall$Tab[-3, ]
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opph.', 'Del opph.')
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
colnames(Nokkeltall$Tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal opph.', 'Del opph.')

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
