#Kjørefil for Rapporteket-Pandemi
rm(list=(ls()))
library(korona)
RegDataRaa <- KoronaDataSQL(datoFra = '2020-01-01')
RegData <- KoronaPreprosesser(RegData = RegDataRaa, aggPers = 1)
Pandemi <- KoronaPreprosesser(KoronaDataSQL(koble=1))
RegData <- Pandemi
DataBeredRaa <- NIRberedskDataSQL()
DataBered <- NIRPreprosessBeredsk(DataBeredRaa)

PasientID == "4442C54B-848C-EB11-A970-00155D0B4E21"
RegData[RegData$PatientInRegistryGuid == "4442C54B-848C-EB11-A970-00155D0B4E21",]

#Påkoblet beredskap
KoroIntData <- KoronaPreprosesser(RegData = KoronaDataSQL(), aggPers = 1, kobleBered = 1)
KoronaFigAndelTid(RegData=KoroIntData)



korona::tabAntPersOpph(RegData=RegDataPre, datoFra= '2022-06-01', datoTil=Sys.Date(), enhetsNivaa='RHF')
tabAntPersOpph(RegData=KoroDataOpph,
               datoFra = input$valgtDatoForl[1],
               datoTil=input$valgtDatoForl[1],
               enhetsNivaa = input$enhetsNivaaForl)


RegData <- KoronaDataSQL(datoFra = '2022-01-01', datoTil = '2022-02-01') #10805
DataOpph <- korona::KoronaPreprosesser(RegData = RegData, aggPers = 0, tellFlereForlop = 0)
DataOpphBered <- korona::KoronaPreprosesser(RegData = RegData, aggPers = 0, tellFlereForlop = 0, kobleBered = 1)
DataPers <- korona::KoronaPreprosesser(RegData = RegData, aggPers = 1, tellFlereForlop = 0)
DataForlop <- korona::KoronaPreprosesser(RegData = RegData, aggPers = 1, tellFlereForlop = 1, kobleBered = 1) #

table(DataOpphBered$BeredPas)
DataForlopTest4 <- korona::KoronaPreprosesser(RegData = RegData, aggPers = 1, tellFlereForlop = 1)

#Tidsenheter og oppholdstabell
KoroDataRaa <- KoronaDataSQL(datoFra = '2020-01-01')
KoroDataOpph <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0)

tabAntOpphEnhTid(RegData=KoroDataOpph, datoTil='2021-08-31',
                             enhetsNivaa = 'ShNavn', tidsEnhet = 'Kvartal', antTidsenh=4)

tidsEnhet = 'Aar'
antTidsenh=4
datoTil='2021-08-31'
(datoDum <-   switch(tidsEnhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil), 'month') - months(antTidsenh-1, abbreviate = T),
                    Kvartal = lubridate::floor_date(as.Date(datoTil), 'month') - months(antTidsenh*3-1, abbreviate = T),
                    Aar = lubridate::floor_date(as.Date(datoTil) - 365*antTidsenh-1)
))
library(lubridate)
datoFra <- max(as.Date('2020-03-01'), as.Date(datoDum)) # max(as.Date('2020-03-01'), as.Date(datoDum))

aggVar <- c(enhetsNivaa, 'InnDato', 'Aar', 'MndNum', 'Kvartal', 'Halvaar')
RegData <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                   & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]

RegDataNy <- SorterOgNavngiTidsEnhet(RegData, tidsenhet= tidsEnhet)
RegData <- RegDataNy$RegData
tidsenheter <- RegDataNy$tidtxt


# AndelTid
NutvTxt <- 4
c(5-0.75*(0:(NutvTxt-1)))
KoronaFigAndelTid(RegData=RegData, hentData=0, valgtVar='alder_u18',
                              datoFra='2019-12-15', datoTil=Sys.Date(), tidsenhet='Kvartal',
                              dod=1, reshID=0, erMann=1, minald=10, maxald=110, #
                              skjemastatusInn=9, skjemastatusUt=2, dodSh=0, aarsakInn=9,
                              #enhetsNivaa='RHF', valgtEnhet='Alle', enhetsUtvalg=0,
                              beredPas=9)




#Antall innlagte på Gjøvik
705476
HFresh <- 100091
test <- RegDataRaa[RegDataRaa$UnitId == 705476, c("Innleggelse", "Utskrivningsdato", "FormStatus", "PatientInRegistryGuid")]
RegDataRaa[RegDataRaa$UnitId == 705476 & is.na(RegDataRaa$Utskrivningsdato), ]

table(RegData$ShNavnUt)


unique(RegData[, c('HF',"HFresh", "ReshId", 'ShNavn')])


library(korona)
PandemiDataRaa <- korona::KoronaDataSQL()
PandemiData <- KoronaPreprosesser(RegData = PandemiDataRaa, aggPers = 0)
PandemiUt <- KoronaDataSQL(koble = 0, skjema = 2)
RegData <- PandemiData


  indReshEgen <- match(reshID, KoroData$HFresh) #Her skal benyttes HF-resh
  egetRHF <- as.character(KoroData$RHF[indReshEgen])
  egetHF <- as.character(KoroData$HF[indReshEgen])

#Filtreringsnivå for data:
egetEnhetsNivaa <- switch(rolle, SC = 'RHF', LC = 'RHF', LU = 'HF')
egenEnhet <- switch(rolle, SC='Alle', LC=egetRHF, LU=egetHF) #For LU vil reshID benyttes

antallTidEnhTab(RegData, tidsenhet='dag', erMann=9, datoFra=as.Date('2021-02-01'), datoTil=as.Date('2021-02-10'),
                            tilgangsNivaa='LC', valgtEnhet='Alle')$Tab


paste0('Uke ', format(aux$Tid, "%V.%y"))
RegData$Uke <- format(RegData$InnDag, "%V.%y")
table(RegData$Uke)

unique(RegData[order(RegData$UnitId),c("UnitId", "HelseenhetKortNavn", 'HF')])
unique(PandemiData[order(PandemiData$ShNavnUt),c("ReshId", "ShNavnUt", 'HF', 'RHF')])
unique(RegData[order(RegData$ReshId),c("ReshId", "ShNavn", 'HFresh', 'RHF')])
#Resh: 102919 HF: Bergen

table(PandemiDataRaa$UnitId)

#Sjekk av antall døde
BergenHF: 100082

PandemiDataRaa <- korona::KoronaDataSQL()
PandemiDataOpph <- KoronaPreprosesser(RegData = PandemiDataRaa, aggPers = 0)
PandemiData <- KoronaPreprosesser(RegData = PandemiDataRaa)

#Før aggregering:
table(PandemiDataOpph$ShNavn[which(PandemiDataOpph$StatusVedUtskriving==2 & PandemiDataOpph$HFresh==100082)])
sum(PandemiDataOpph$StatusVedUtskriving==2, na.rm = T)

persDod <- PandemiDataOpph$PersonId[which(PandemiDataOpph$StatusVedUtskriving == 2 & PandemiDataOpph$RHFresh == 100021)]
PandemiDataOpph[PandemiDataOpph$PersonId %in% persDod, c("ShNavn", "ShNavnUt")]
write.table()
#Etter aggregering:
table(PandemiData$ShNavnUt[which(PandemiData$StatusVedUtskriving==2& PandemiData$HFresh==100082)])
sum(PandemiData$StatusVedUtskriving==2, na.rm = T)

PandemiData[PandemiData$PersonId %in% persDod, c("ShNavn", "ShNavnUt")]

unique(PandemiDataOpph[PandemiDataOpph$StatusVedUtskriving==2 ,c('ReshId', "ShNavn")])
test <- PandemiDataOpph[which(PandemiDataOpph$StatusVedUtskriving==2), ]
testRaa <- PandemiDataRaa[PandemiDataRaa$StatusVedUtskriving==2, ]

#INNELIGGENDE, tabell
datoFra <- '2020-09-01'
datoTil <- '2020-11-01'
RegDataAlle <- PandemiData

if (datoFra != 0) {RegDataAlle <- RegDataAlle[RegDataAlle$UtDato >= datoFra | is.na(RegDataAlle$UtDato), ]}
if (datoTil != Sys.Date()) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato <= datoTil), ]} # filtrerer på tildato
datoer <- seq(if (datoFra!=0) datoFra else min(RegDataAlle$InnDato), datoTil, by="day") #today()


#' Funksjon som avgjør om en pasient er inneliggende på aktuell dato
#' Returnerer TRUE for datoer pasienten er inneliggende
#' @param datoer datoer som inneligging skal avgjøres for
#' @param regdata Dataramme som inneholder InnDato og Utdato per pasient
erInneliggende <- function(datoer, regdata){
  # regnes som inneliggende på aktuell dato hvis den faller mellom inn- og utdato eller
  # er etter inndato og det ikke finnes utddato. Flere betingelser kan legges til.

  auxfunc <- function(x) {(x >  regdata$InnDato & (x <= regdata$UtDato) | is.na( regdata$UtDato))}
  map_df(datoer, auxfunc)
}



if (tidsenhet=='dag') {
  names(datoer) <- format(datoer, '%d.%b')
  aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
  RegDataAlle <- bind_cols(RegDataAlle, aux)
} else {
  names(datoer) <- datoer
  aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
  aux <- bind_cols(as_tibble(RegDataAlle)[, "PasientID"], aux)
  aux <- aux %>% gather(names(aux)[-1], key=Tid, value = verdi)
  aux$Tid <- as.Date(aux$Tid)
  aux$Tid <- switch (tidsenhet,
                     'uke' = paste0('Uke ', format(aux$Tid, "%V")),
                     'maaned' = format(aux$Tid, "%b.%Y")
  )
  aux <- aux %>% group_by(PasientID, Tid) %>%
    summarise(er_inne = max(verdi))
  aux <- aux %>% spread(key=Tid, value = er_inne)
  RegDataAlle <- merge(RegDataAlle, aux, by = 'PasientID')
}




colMeans(RegData[,c("ReinnTid", "ReinnTidDum")], na.rm = T)
#RegData$Reinn==1

velgTidsenhet <- 'dag'
datoTil <- as.Date("2020-11-01")
datoFra <- switch (velgTidsenhet,
                   "dag" = datoTil - days(50-1),
                   "uke" = floor_date(datoTil - weeks(30-1), unit = 'week', week_start = 1),
                   "maaned" = floor_date(datoTil - months(20-1), unit = 'month')
)


test <- antallTidEnhTab(RegData=PandemiData, datoFra=datoFra, datoTil = )
                        # , tidsenhet='dag', erMann=9, datoFra=0, datoTil=Sys.Date(), #valgtVar='innlagt',
                        #     tilgangsNivaa='SC', valgtEnhet='Alle', #enhetsNivaa='RHF',
                        #     HF=0, skjemastatusInn=9, aarsakInn=9, dodSh=9)
test$Tab

statusNaaTab(RegData=KoroData, enhetsNivaa='HF', #
             valgtEnhet='Bergen')

UtData <- KoronaUtvalg(RegData=KoroData,  enhetsNivaa='HF', #
                       valgtEnhet='Bergen')$RegData
UtData <- KoronaUtvalg(RegData=KoroData, valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa)$RegData

#Sjekke manglende HF i Sør-Øst
unique(KoroDataRaa[ ,c("UnitId", "HelseenhetKortNavn", 'HF', 'RHF')])
unique(KoroData[ ,c("ReshId", "ShNavn", 'HF', 'RHF')])
Pandemi  <- KoronaUtvalg(RegData=KoroData, aarsakInn = 2)$RegData
as.data.frame(Pandemi[Pandemi$HF=='',] %>% dplyr::group_by(RHF, HF, HF, ShNavn) %>% dplyr::summarise(Antall = n()))
as.data.frame(Pandemi %>% dplyr::group_by(RHF, HF, HF, ShNavn) %>% dplyr::summarise(Antall = n()))
Test <- KoroData[KoroData$ShNavn == 'Radiumhospitalet', ]
705757

#Samlerapport, sjekk

#function(rnwFil, brukernavn='lluring', reshID=0, valgtEnhet = 'Alle', enhetsNivaa = 'RHF', rolle = 'SC')
test <- korona::abonnementKorona(rnwFil="KoronaRapport.Rnw", brukernavn='lenaro', reshID=700720,
                             valgtEnhet = 'Alle', enhetsNivaa = 'RHF', rolle = 'SC')
file.copy(from=test, to='~/korona/test.pdf')

testBer <- intensivberedskap::abonnementBeredsk(rnwFil='BeredskapCorona.Rnw', brukernavn='beredskap', reshID=0,
                                valgtRHF = 'Alle', Rpakke='intensivberedskap')

head(format(seq(Sys.Date(), min(Pandemi$InnDag), by=paste0('-1 day')), '%d.%b'))
class(seq(Sys.Date(), min(Pandemi$InnDag), by=paste0('-1 day')))

#INNELIGGENDE...#
library(korona)
RegData <- KoronaDataSQL(datoFra = '2020-03-01')
RegData <- KoronaPreprosesser(RegData = RegData)
RegData <- RegData[-which(RegData$SkjemaGUID %in% toupper(SkjemaGUIDInn)), ]

indSmDag <- which(as.numeric(difftime(RegData$CreationDateUt, RegData$CreationDate,
                                      units = 'hours')) < 1)
indFerdig <- which(RegData$FormStatus==2 & RegData$FormStatusUt==2)

RegData <- RegData[intersect(indSmDag, indFerdig), ]
#Opprettet samtidig. Skjer det ved inn-eller utskriving?
#time 1 - time 2
InnDiff <- median(as.numeric(difftime(RegData$CreationDate, RegData$FormDate,
                    units = 'days'))) #Fra sept: Median reg. 1.7 dager etter
UtDiff <- median(as.numeric(difftime(RegData$CreationDateUt, RegData$FormDateUt,
                                   units = 'days'))) #Fra sept. Median 1,8timer før utskrivning.
DiffInnlOpprUtskr <- median(as.numeric(difftime(RegData$CreationDate, RegData$FormDateUt,
                                               units = 'days')))
Liggetid <- median(as.numeric(difftime(RegData$FormDate, RegData$FormDateUt,
                                       units = 'days')))
# For 421 av 1330 opphold legges begge skjema inn samtidig. (Ferdigstilte registreringer f.o.m. 1.sept)
# For opphold hvor begge skjema legges inn samtidig, er median tid mellom innleggelse og registrering 1.7 dager,
# mens tid mellom utskriving og registrering er 0,07 dager (1,8t). Samtidig registrering av hele oppholdet
# tenderer derfor til å skje ved utskriving.
#
# Hvor mange legges inn samtidig med innleggelse?

#Justere inneliggene (UtDato) for ut-skjema som opprettes samme dag som innleggelse
indIkkeUtDato <- which(is.na(RegData$Utskrivningsdato)) #Mangler utskr.dato.
indSmDag <- which(as.numeric(difftime(RegData$CreationDateUt, RegData$CreationDate,
                                      units = 'hours')) < 1)

test <- intersect(indIkkeUtDato, indSmDag)
RegData$UtDato <- RegData$Utskrivningsdato #FormDateUt # #Alle som har utskrivingsskjema
#Tar bort de som har skjema opprettet samme dag som inn-skjema og mangler utskrivingsdato
RegData$UtDato[intersect(indIkkeUtDato, indSmDag)] <- NA
inneliggereInd <- is.na(RegData$UtDato)
Inneliggende <- length(unique(RegData$PatientInRegistryGuid[inneliggereInd]))

#Se nærmere på inneliggende basert på manglende utskrivingsdato.
#Ca. 290 pasienter som har en dato for utskrivingsskjema før dato for utskrivning,
#med en mediantid skjemadato - utskrivningsdato 6 dager (nedre og øvre kvartil: 3 – 14). -> Over 10% "skrives ut" omtrent når de legges inn
#Ca. 1830 pasienter har dato for utskrivingsskjema (CreationDate?) ETTER utskrivning,
# mediantid skjemadato - utskrivningsdato på 1 dag (nedre og øvre kvartil: 0,5 – 9).

#Hvis vi tar alle med ‘FirstTimeClosedUt’ (ca. 1300 pasienter) er
#mediantid mellom utskrivningsdato og skjemadato på 10 dager (nedre og øvre kvartil: 4 – 67).

# Alle disse tallene er basert på pasientens siste opphold (som for de fleste er det sin eneste).
# Jeg tolker derfor at det blir mer riktig å fortsette å bruke FormDate når utskrivningsdato mangler fordi det virker som
# for de aller fleste at utskrivningsskjema fremdeles blir registrert etter utskrivning,
# og at for halvparten at ferdigstilling av skjemaet tar 10 dager eller lengre.
# Også, som nevnt ovenfor, gir beregningen basert på FormDate et riktigere bilde av situasjonen når man sammenligner med andre kilder.


plot(PandemiDataRaa$CreationDate, PandemiDataRaa$CreationDateUt, col='blue', pch=20)
points(PandemiDataRaa$CreationDate, PandemiDataRaa$Utskrivningsdato, col='red', pch=20)

plot(PandemiUt$CreationDate, PandemiUt$FormDate)
plot(PandemiUt$Utskrivningsdato, PandemiUt$CreationDate)
points(PandemiUt$Utskrivningsdato, PandemiUt$FirstTimeClosed, col='red', pch=20, cex=0.5)
#Få med utskriving etter opprettelse, og stort sett ikke så lenge etter
#Mange skjema etterregistreres
#Skjema ferdigstilles stort sett når de opprettes, men kan bli liggende noen dager. (Creation, FirstTimeClosed)
plot(PandemiUt$CreationDate, as.Date(PandemiUt$FirstTimeClosed))

#Etterregistrering av pasienter:
plot(PandemiDataRaa$CreationDate, PandemiDataRaa$FormDate)

plot(difftime(PandemiDataRaa$CreationDateUt, PandemiDataRaa$Utskrivningsdato,
              units = 'days'))

#Ut-skjema som opprettes før utskrivning - opprettes de samtidig med innleggelse? Mange gjør det, ca 114 av 158 (72%)
indOpprettForUts <- which(as.numeric(difftime(PandemiDataRaa$CreationDateUt, PandemiDataRaa$Utskrivningsdato, units = 'mins')) <= 0)
indSmDag <- which(as.numeric(difftime(PandemiDataRaa$CreationDateUt[indOpprettForUts], PandemiDataRaa$CreationDate[indOpprettForUts],
                              units = 'days')) < 1)

#Justere inneliggene basert på at ut-skjema opprettes samme dag som innleggelse
indKladdUt <- which(PandemiDataRaa$FormStatusUt == 1 & is.na(PandemiDataRaa$Utskrivningsdato)) #I kladd og mangler utskr.dato
indSmDag <- which(as.numeric(difftime(PandemiDataRaa$CreationDateUt, PandemiDataRaa$FormDate,
                                   units = 'days')) < 1)
PandemiDataRaa$UtDato <- PandemiDataRaa$FormDateUt
PandemiDataRaa$UtDato[intersect(indKladdUt, indSmDag)] <- NA
sum(is.na(PandemiDataRaa$UtDato))
sum(is.na(PandemiDataRaa$Utskrivningsdato))

#Hvor vanlig er det å inn og ut på samme dag? 258 av 2115, 12% (3% <0,5 dager)
indFerdig <- which(PandemiDataRaa$FormStatusUt == 2)
innUtSmdag <- sum(as.numeric(difftime(PandemiDataRaa$FormDateUt[indFerdig], PandemiDataRaa$FormDate[indFerdig],
                           units = 'days')) < .25)



ManglerUtDatoRaa <- sum(is.na(PandemiDataRaa$Utskrivningsdato))
ManglerUtDatoPers <- sum(is.na(PandemiData$Utskrivningsdato))

ManglerUtSkjemaRaa <- sum(is.na(PandemiDataRaa$FormStatusUt))
ManglerKladdUtSkjemaPers <- sum(PandemiData$FormStatusUt == 1) #Regnes som kladd hvis utskjema ikke opprettet.

#Er alle ut-skjema som mangler utdato i kladd? Ja
table(PandemiDataRaa$FormStatusUt[is.na(PandemiDataRaa$Utskrivningsdato)], useNA = 'a')


#Får utskrivingsskjema FormDate fra utskrivingsdato hvis denne finnes? JA
PandemiUt <- KoronaDataSQL(skjema = 2, koble = 0)
PandemiUt$DiffForm <- difftime(PandemiUt$FormDate, PandemiUt$Utskrivningsdato, units = 'mins')
#test <- PandemiUt[order(PandemiUt$FormDate),c('Diff', 'CreationDate', "FormDate", "Utskrivningsdato","FormStatus")]

PandemiDataRaa$DiffFormUts <- difftime(PandemiDataRaa$FormDateUt, PandemiDataRaa$Utskrivningsdato, units = 'days')
PandemiDataRaa$DiffCreUts <- difftime(PandemiDataRaa$CreationDateUt, PandemiDataRaa$Utskrivningsdato, units = 'days')
PandemiDataRaa$DiffFerdigUts <- difftime(PandemiDataRaa$FirstTimeClosedUt, PandemiDataRaa$Utskrivningsdato, units = 'days')
test <- PandemiDataRaa[order(PandemiDataRaa$FormDate),
                  c('DiffFormUts','DiffCreUts','DiffFerdigUts', 'CreationDateUt', "FormDateUt", "Utskrivningsdato","FormStatus")]

MedDiffFormUts <- median(as.numeric(PandemiDataRaa$DiffFormUts), na.rm = T)
MedDiffCreUts <- median(as.numeric(PandemiDataRaa$DiffCreUts), na.rm = T)
MedDiffFerdigUts<- median(as.numeric(PandemiDataRaa$DiffFerdigUts), na.rm = T)

'CreationDate'
'CreationDateUt'
'FirstTimeClosed'
'FirstTimeClosedUt'

#Se nærmere på antall døde
sum(is.na(PandemiDataRaa$SkjemaGUIDut))
SkjemaDod <- sort(PandemiDataRaa$SkjemaGUID[which(PandemiDataRaa$StatusVedUtskriving==2)])
#SkjemaDod <- sort(PandemiUt[which(PandemiUt$StatusVedUtskriving==2), "SkjemaGUID"])


DodHSO <- read.table('DodHSO.csv', sep=';',
                     stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
DodHSO <- sort(toupper(as.vector(t(DodHSO))))
#setdiff(DodHSO, SkjemaDod)
#setdiff(DodHSO, PandemiDataRaa$SkjemaGUID) #Skjema som ikke i mitt uttrekk

# PandemiDataRaa$DodHSO <- 0
# PandemiDataRaa$DodHSO[which(PandemiDataRaa$SkjemaGUID %in% DodHSO)] <- 1
PandemiDataRaa$DodHSO <- ifelse(PandemiDataRaa$SkjemaGUID %in% DodHSO,1,0)
  #PandemiDataRaa[which(PandemiDataRaa$SkjemaGUID %in% DodHSO),
        #               c("FormDate", "SkjemaGUID", "SkjemaGUIDut", "FormStatus", "FormStatusUt",
        #                 'StatusVedUtskriving')]
StatusUt <- PandemiDataRaa[order(PandemiDataRaa$FormDate),
                           c("FormDate","SkjemaGUID", "SkjemaGUIDut","DodHSO", "StatusVedUtskriving", "FormStatus", "FormStatusUt")]

write.table(StatusUt,
            file = 'StatusUtSkjemaGUID.csv', row.names = F, sep = ';')

#write.table(test[order(test$FormDate),], file = 'SkjemaGUIDtestDod.csv', row.names = F, sep = ';')

#Antall døde
AntDodRaa <- sum(PandemiDataRaa$StatusVedUtskriving == 2, na.rm = T)
AntDodPers <- sum(PandemiData$StatusVedUtskriving == 2, na.rm = T)
table(PandemiDataRaa$StatusVedUtskriving, useNA = 'a')



table(RegData$ShNavn, useNA = 'a')
table(RegData$HFresh, useNA = 'a')
table(RegData$HF, useNA = 'a')
table(RegData$RHFresh, useNA = 'a')
table(RegData$RHF, useNA = 'a')
RegData$ReshId[is.na(RegData$HFresh)]
RegData$ShNavn[is.na(RegData$HFresh)]


PandemiData[PandemiData$ReshId == 4204086, c("HFresh", 'HF', "RHFresh", 'RHF')]

RegData <- PandemiDataRaa
RegData$HFresh <- ReshNivaa$HFresh[match(RegData$UnitId, ReshNivaa$ShResh)]
test <- RegData[is.na(RegData$HFresh), c("HFresh", "UnitId", 'RHF')]

RegData[which(RegData$UnitId == 4204086), c("HFresh", 'RHF', 'HelseenhetKortNavn')]
match(4204086, ReshNivaa$ShResh)

ind <- which(PandemiDataRaa$SkjemaGUID==toupper('893ea8aa-e6db-457d-9d4a-fe614dea8ac1'))
PandemiDataRaa[ind, "Utskrivningsdato"]


table(PandemiDataRaa$ArsakInnleggelse)
table(PandemiData$ArsakInnleggelse)
table(PandemiData$ArsakInnNy)
table(PandemiData$CovidJAalle)
table(PandemiData$CovidJaSiste)
table(PandemiData$CovidJaFinnes)
table(PandemiData$CovidNei)
table(PandemiData$CovidUkjent)

table(PandemiData$ArsakInnleggelse, useNA = 'a')
table(PandemiData$ArsakInnleggelse_NyA, useNA = 'a')
table(PandemiData$ArsakInnleggelse_NyAC, useNA = 'a')

#Kan også teste ved å se på enkeltverdier

table(RegData[RegDataRaa$StatusVedUtskriving==2, 'HF'])
RegDataRaa[RegDataRaa$StatusVedUtskriving==2 & RegDataRaa$HF=='Helse Bergen HF',
           c('SkjemaGUIDut', 'HelseenhetKortNavn')]
RegData[which(RegData$StatusVedUtskriving==2 & RegData$HF=='Helse Bergen HF'), ]

RegDataRaaHB <- RegDataRaa[RegDataRaa$HF=='Helse Bergen HF', ]
RegDataHB <- KoronaPreprosesser(RegDataRaaHB)

table(RegDataRaa$HF[which(RegDataRaa$StatusVedUtskriving==2)])

ind <- which(RegDataRaa$SkjemaGUIDut %in% toupper(c('c33a60b6-0e4f-4fd4-8413-1f15081c9687',
                                                    '508c6991-4dc5-4389-9f56-166e1647d5f3',
                                                    'cee202cd-ecfe-498a-98e6-68e3a254b13b',
                                                    'dd6c5606-cad0-4b17-9142-0bfff5944232',
                                                    'dbdf559e-ae70-4eb0-bb17-c2478ed3fcda')))
#

RegDataRaa$HF[ind]
RegDataRaa[ind,]
UtData <- KoronaUtvalg(RegData=RegDataHB,
                       enhetsNivaa='HF', valgtEnhet='Helse Bergen HF',
                       aarsakInn = 9,
                        skjemastatusInn=9,
                        erMann=9)
dim(UtData$RegData)
sum(UtData$RegData$StatusVedUtskriving==2, na.rm=T)

test <- unique(RegDataRaa[as.Date(RegDataRaa$FormDate) < '2020-03-08',c("SkjemaGUID", "FormDate", "HelseenhetKortNavn")])

pas <- RegDataRaa$PasientGUID[which(RegDataRaa$SkjemaGUID == '41FB2DF0-E9C6-4552-A10C-788C51A5B7B9')] #  'D8341A7C-04D6-4382-BAE1-014F8C13FBD1')]
t(RegDataRaa[RegDataRaa$PasientGUID==pas,c("PasientGUID","SkjemaGUID","SkjemaGUIDut", "FormDate", "HelseenhetKortNavn")])
table(RegData[RegData$Alder<18, c('Alder','ArsakInnleggelse')])

pas <- RegData$PasientID[which(RegData$ReinnTid<0,)] #Kan evt. brukes til å finne dbl.reg.
test1 <- RegDataRaa[which(RegDataRaa$PasientGUID %in% pas),
                   c('HelseenhetKortNavn','UnitId', 'PasientGUID', 'FormDate', "FormDateUt", 'SkjemaGUID')] #'Liggetid', 'LiggetidTot',
test1[order(test$PasientGUID, test1$FormDate), ]
test2 <- RegData[which(RegData$PasientID %in% pas),
                    c('ShNavn','PasientID', 'FormDate', "FormDateUt", 'Liggetid', 'LiggetidTot')] #
test2[order(test2$PasientID, test2$FormDate), ]
RegData[which(RegData$Reinn==1), c('Liggetid', 'LiggetidTot')]

#tab <- FerdigeRegTab(RegData = RegData)$Tab
alleInn <- innManglerUt(RegData = RegDataRaa)
sort(alleInn$InnDato)

test <- innManglerUt(RegData = RegDataRaa, enhetsNivaa = 'HF', valgtEnhet = egetHF) #, valgtEnhet = )
ikkeut <- RegDataRaa[which(RegDataRaa$SkjemaGUID %in% test$SkjemaGUID), ]
ikkeut[ ,c('HelseenhetKortNavn', 'SkjemaGUID', 'FormDate')]

pas <- unique(ikkeut$PasientGUID)
length(which(RegData$PasientID %in% pas))
t1 <- RegData[which(RegData$PasientID %in% pas),c('ShNavn','ShNavnUt', 'FormStatus', "FormDate", "FormDateUt",'Overf',"PasientID")]
t1[order(t1$PasientID), ]
t2 <- RegDataRaa[which(RegDataRaa$PasientGUID %in% pas),
           c('HelseenhetKortNavn', 'SkjemaGUID', 'FormDate', "FormDateUt",'PasientGUID')]
             #'OverfortAnnetSykehusInnleggelse', 'OverfortAnnetSykehusUtskrivning',
t2[order(t2$PasientGUID, t2$FormDate), ]

ifelse(0 > 0, 1,
       sort(RegDataRaa$FormStatusUt)[1])
ifelse(1<0, 0, 'a')

table(RegData$Reinn8)
table(RegData$Reinn24)
table(RegData$Reinn48)
table(RegData$Reinn7d)
table(RegData$ReinnTid>24)
pas <- RegData$PasientID[which(as.numeric(RegData$ReinnTid) < -10)]
tab <- RegDataRaa[which(RegDataRaa$PasientGUID %in% pas),
           c('PasientGUID', "FormDate", "FormDateUt", 'HelseenhetKortNavn')]
tab[order(tab$PasientGUID), ]
RegData[RegData$PasientID=='80572CB4-6E77-EA11-A96B-00155D0B4F09', ]


tab <- table(RegDataRaa$PasientGUID)
tab[tab>1]
table(round(RegData$Liggetid), useNA = 'a')
FerdigeRegTab(RegData=Pandemi,
              aarsakInn = 1)$Tab
              #,valgtEnhet=valgtEnhet,
              #enhetsNivaa = enhetsNivaa)

test <- RegData[ , c("FormDateUt", "Utskrivningsdato","FormStatus", "FormStatusUt" )]

RegData <- KoronaUtvalg(RegData=RegData, aarsakInn = 1)$RegData
table(RegData$Reinn,is.na(RegData$FormDateUt))
table(is.na(RegData$FormDateUt))

RegDataRaa <- KoronaDataSQL()
RegData <- KoronaPreprosesser(RegData = RegDataRaa, aggPers = 0)
RegData <- KoronaPreprosesser(RegData = RegDataRaa)

Utdata <- KoronaFigAndeler(valgtVar='regForsinkelseInn', RegData=RegData, datoFra = '2020-04-01')
# ,minald=minald, maxald=maxald, aarsakInn=aarsakInn,
#                  erMann=erMann, dodSh=dodSh,
#                  skjemastatusInn=skjemastatusInn, skjemastatusUt=skjemastatusUt,
#                  enhetsNivaa=enhetsNivaa, valgtEnhet=valgtEnhet,
#                  enhetsUtvalg=1)

Tab <- FerdigeRegTab(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', erMann=9, dodSh=9)$Tab


RisikoInnTab(Pandemi, valgtEnhet = valgtEnhet, enhetsNivaa = enhetsNivaa)

AlderTab(RegData=RegData)$Tab

antallTidEnhTab(RegData, tidsenhet=tidsenhet, erMann=9, tilgangsNivaa=tilgangsNivaa,
                bekr=1, skjemastatus=0, dodSh=9, valgtEnhet='Sør-Øst')



library(korona)
valgtEnhet= 'Alle' #'N-Trøndelag' #'Midt' #'Innlandet' #'Vest' #'Innlandet' #'Alle'
enhetsNivaa <- 'RHF'
rolle <- 'SC'
reshID <- 100317 #N-Trøndelag #100091
#setwd('C:/ResultattjenesteGIT/korona/inst')
setwd('/home/rstudio/korona/inst')
#knitr::knit('KoronaRapport.Rnw', encoding = 'UTF-8')
#tools::texi2pdf(file='KoronaRapport.tex')
knitr::knit2pdf('KoronaRapport.Rnw') #, encoding = 'UTF-8')

korona::abonnementKorona(rnwFil=rnwFil, brukernavn='lluring', reshID=0,
                 valgtEnhet = 'Alle',  #Rpakke='korona',
                 enhetsNivaa = 'RHF', rolle = 'SC')

unique(PandemiInn[,c('RHFresh','RHF')])
PandemiInn %>% dplyr::group_by(RHF, HF) %>% dplyr::summarise(Antall = length(unique(HealthUnitShortName)))
PandemiInn %>% dplyr::group_by(RHF) %>% dplyr::summarise(Antall = length(unique(HF)))


PandemiInn <- read.table('A:/Pandemi/Pandemiskjema2020-04-03.csv', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
PandemiInn$InnDato <- as.Date(PandemiInn$FormDate, tz= 'UTC', format="%d.%m.%Y") #DateAdmittedIntensive
PandemiInn$Dag <- format(PandemiInn$InnDato, '%d.%B')


library(korona)
KoroDataInn <- KoronaDataSQL(skjema=1, koble=0)
KoroDataUt <- KoronaDataSQL(skjema=2)
# varUt <- c("Antifungalbehandling", "AntiviralBehandling" , "HovedskjemaGUID",
#            'FormStatus', 'FormDate', "OverfortAnnetSykehusUtskrivning", "StatusVedUtskriving")
# KoroData <- merge(KoroDataInn, KoroDataUt[,varUt], suffixes = c('','Ut'),
#                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)
RegData <- KoroData

KoroDataInn <- read.table('A:/Pandemi/InklusjonSkjemaDataContract2020-04-06.csv', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
KoroDataUt <- read.table('A:/Pandemi/UtskrivningSkjemaDataContract2020-04-06.csv', sep=';',
                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
max(sort(table(KoroDataInn$SkjemaGUID)))
max(sort(table(KoroDataUt$HovedskjemaGUID)))
max(sort(table(KoroDataUt$SkjemaGUID)))

table(KoroDataInn$PasientGUID)[table(KoroDataInn$PasientGUID)>1]
sort(table(KoroDataUt$PasientGUID)) #[table(KoroDataUt$PasientGUID)>1]

ind <- which(KoroDataUt$HovedskjemaGUID==('D403C085-3F28-4840-AF72-9A6AF7954066'))
KoroDataUt$SkjemaGUID[ind] #= 'D403C085-3F28-4840-AF72-9A6AF7954066'
KoroDataUt[ind, "FormStatus"]

#----------- Datavask--------------------------------
#Feilreg, BMI
RegData[order(RegData$BMI, decreasing = T)[1:10],c('PasientID', 'InnDag', 'BMI')]
PasIDBMI <- RegData[which(RegData$BMI>50), "PasientID"]
RegDataRaa <- KoronaDataSQL()
RegDataRaa[union(which(RegDataRaa$PasientGUID %in% PasIDBMI),
                 which(RegDataRaa$Hoyde<140 & RegDataRaa$Hoyde>0)),
           c('Hoyde', 'Vekt', "PatientAge", "SkjemaGUID")]


RegDataRaa <- KoronaDataSQL()
RegData <- KoronaPreprosesser(RegData = RegDataRaa)

#Dato inn/ut
sjekk <- 240 #-10
pas <- RegData$PasientID[which(as.numeric(RegData$ReinnTid) > sjekk)]
tab <- RegDataRaa[which(RegDataRaa$PasientGUID %in% pas),
                  c('PasientGUID', "FormDate", "FormDateUt", 'HelseenhetKortNavn')]
pas <- RegData$PasientID[RegData$AntReinn>1]
tab[order(tab$PasientGUID), ]
sort(table(RegData$AntReinn))

#FINN DOBBELTREGISTERERING - overlapp to avdelinger > 24?t

#-------------Overførte pasienter - SJEKK AV PASIENTAGGREGERING------------------------
library(korona)
RegDataRaa <- KoronaDataSQL() #1026
length(unique(RegDataRaa$PasientGUID)) #935 ->91 overføringer?
RegData <- KoronaPreprosesser(RegDataRaa)
JaNeiUkjVar <- function(x) {ifelse(1 %in% x, 1, ifelse(2 %in% x, 2, 3))}
# OverfortAnnetSykehusInnleggelse,  #1-ja, 2-nei, 3-ukjent
# OverfortAnnetSykehusUtskrivning,  #1-ja, 2-nei, 3-ukjent
# RegDataRed <- RegData %>% group_by(PasientGUID) %>%
#   summarise(Overf = JaNeiUkjVar(c(OverfortAnnetSykehusInnleggelse, OverfortAnnetSykehusUtskrivning)),
#             AntInnSkjema = n(),
#             Reinn = ifelse(AntInnSkjema==1, 0,
#                            ifelse(sort(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
#                                     FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
#                                     hours)) <= 8, 0, 1)), #Beregn ja/nei. Xt
#             AntReinn = ifelse(Reinn==0, 0, #0-nei, 1-ja
#                               sum(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
#                                            FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
#                                            hours) > 8))
            # Dobbeltreg= , #Overlappende liggetid >Xt på to ulike Sh
            # Overf = , #Beregn, ja nei
            # AntOverf = , #Antall overføringer
            # LiggetidSum = , #sum av liggetider
#  )
#sort(difftime(RegData$FormDateUt[1:2], c(RegData$FormDate[1], NA)))

sum(RegData$Reinn, na.rm = T)
hvilkePas <- RegData$PasientID[which(RegData$Reinn==1)]
ReinnData <- RegDataRaa[which(RegDataRaa$PasientGUID %in% hvilkePas),
                        c('PasientGUID', "FormDate", "FormDateUt", "HelseenhetKortNavn", "SkjemaGUID")]
test <- ReinnData[order(ReinnData$PasientGUID, ReinnData$FormDate),]
#FINN DOBBEL

table(RegDataRed$Overf) #50 overf
tab <- table(RegData$PasientGUID)
table(tab)
tab[tab>2]

RegData <- KoronaPreprosesser(RegData = RegData)
length(unique(RegData$PasientID)) #926 sjekk ant før 8.mars : 9 stk OK
RegData <- KoronaUtvalg(RegData=RegData, aarsakInn = 1)$RegData #767

pas3 <- names(table(RegData$PasientGUID)[table(RegData$PasientGUID)>2])
indOverf <- which(RegData$PasientGUID %in% pas3)
var <- c('PasientGUID',"UnitId",  "HelseenhetKortNavn", 'ShNavnUt', "FormStatus", "Innleggelse",
         "Utskrivningsdato", "FormStatusUt", "OverfortAnnetSykehusInnleggelse", "OverfortAnnetSykehusUtskrivning")
#data3opph <- RegData[indOverf, var] #RegData$PasientGUID == '3E6F196C-EF7B-EA11-A96B-00155D0B4F09'
data3opph <- RegData[indOverf, ] #RegData$PasientGUID == '3E6F196C-EF7B-EA11-A96B-00155D0B4F09'
write.csv2(data3opph[order(data3opph$PasientGUID),], file='Data3opph.csv' ,fileEncoding = 'UTF-8', row.names = F)

Pandemi <- KoronaPreprosesser(KoronaDataSQL(koble=1))
varAgg <- c('PasientID',"ReshId",  "ShNavn",  'ShNavnUt', "FormStatus", "InnTidspunkt",
            "Utskrivningsdato", "FormStatusUt", "Overf")
data3opphAgg <- Pandemi[which(Pandemi$PasientID %in% pas3), ] #varAgg]
UtAgg <- data3opphAgg[order(data3opphAgg$PasientID),]
write.csv2(UtAgg, file='Data3opphAgg.csv' ,fileEncoding = 'UTF-8', row.names = F)

#-----------------------------Koble Intensiv og Pandemi------------------------------
library(korona)
library(intensivberedskap)
library(tidyverse)

IntensivData <- read.table('A:/Intensiv/BeredskapPers2020-04-23.csv', sep=';',
                          stringsAsFactors=FALSE, header=T) #, encoding = 'UTF-8')
var <- c("Fodselsnummer", "SkjemaGUID", 'FormDate', "HealthUnitShortName", "HF", "RHF")
IntDataPers <- IntensivData %>%
  group_by(Fodselsnummer) %>%
  summarise(
    SkjemaGUID = first(SkjemaGUID, order_by = FormDate),
    RHF = first(RHF, order_by = FormDate),
    HF = first(HF, order_by = FormDate),
    ShNavn =  first(HealthUnitShortName, order_by = FormDate),
    FormDate = first(FormDate, order_by = FormDate)
  )

PandemiData <- read.table('A:/Pandemi/PandemiPers2020-04-23.csv', sep=';',
                         stringsAsFactors=FALSE, header=T) #, encoding = 'UTF-8')
PanData <- PandemiData[which(PandemiData$Skjematype=='Pandemiskjema'), var]

PanDataPers <- PanData %>%
  group_by(Fodselsnummer) %>%
  summarise(
    SkjemaGUID = first(SkjemaGUID, order_by = FormDate),
    RHF = first(RHF, order_by = FormDate),
    HF = first(HF, order_by = FormDate),
    ShNavn =  first(HealthUnitShortName, order_by = FormDate),
    FormDate = first(FormDate, order_by = FormDate)
  )

#Manglende registrering
IntPan <- merge(IntDataPers, PanDataPers, suffixes = c('Int','Pan'),
                     by = 'Fodselsnummer', all.x = T, all.y=F)
IntIkkePan <- IntPan[which(is.na(IntPan$SkjemaGUIDPan)),
                     c('RHFInt', 'HFInt', 'ShNavnInt', 'FormDateInt', 'SkjemaGUIDInt')]
data.frame(IntIkkePan[order(IntIkkePan$RHFInt), ], row.names = 'SkjemaGUIDInt')


#Andel som har vært på intensiv
PanInt <- merge(IntDataPers, PanDataPers, suffixes = c('Int','Pan'),
                       by = 'Fodselsnummer', all.x = F, all.y=T)[,-1]
PanInt$PaaInt <- ifelse(is.na(PanInt$FormDateInt),0,1)

TabSh <- PanInt %>%
  dplyr::group_by(RHFPan, HFPan, ShNavnPan) %>%
  dplyr::summarise(
    AntPaaInt = sum(PaaInt),
    AntPas = n(),
    AndelPaaInt = round(sum(PaaInt)/n()*100, 1)
  )
TabHF <- PanInt %>%
  dplyr::group_by(RHFPan, HFPan) %>%
  dplyr::summarise(
    AntPaaInt = sum(PaaInt),
    AntPas = n(),
    AndelPaaInt = round(sum(PaaInt)/n()*100, 1)
  )
TabRHF <- PanInt %>%
  dplyr::group_by(RHFPan) %>%
  dplyr::summarise(
    AntPaaInt = sum(PaaInt),
    AntPas = n(),
    AndelPaaInt = round(sum(PaaInt)/n()*100, 1)
  )
TabNasj <- PanInt %>%
   dplyr::summarise(
    AntPaaInt = sum(PaaInt),
    AntPas = n(),
    AndelPaaInt = round(sum(PaaInt)/n()*100, 1)
  )
install.packages(c("xlsx","openxlsx"))
library(openxlsx)
OUT <- openxlsx::createWorkbook()
tabeller <- list('IntIkkePan'=IntIkkePan, 'TabSh'=TabSh, 'TabHF'=TabHF, 'TabRHF'=TabRHF, 'TabNasj'=TabNasj)
for(a in 1:5){
  tab <- data.frame(tabeller[[a]])
  arknavn <- names(tabeller)[a]
  addWorksheet(OUT, arknavn)
  writeData(OUT, sheet = arknavn, x = tab)
}
saveWorkbook(OUT,'AndelPaaInt.xlsx')



round(prop.table(table(PanInt[ ,c('ShNavnPan', 'PaaInt')]), margin = 1)*100,1)


#------------Kontroll, Beregning av reinnleggelse
RegDataRaa <- KoronaDataSQL()
RegData <- KoronaPreprosesser(RegData = RegDataRaa)

#PersonID som skal sjekkes:'  '
sjekkPers <- c('3d6hzE6rwd05cind5W8qRh8mqzCNUEcxWtsAemTUApU', 'V9r9pLkG3wVd7wvPt6SaEdrMDnTdxSnR9rk2bUokcQ',
               'r9O7Q5p8Y0Hc8nUIIxGRxtfbpR2Y3n5iddHVqMF8No4', 'txIDdRvRmZrsFW6IRhh3gTytaPbFDRmIKUlcpl1CY')
RaaSjekk <- RegDataRaa[which(RegDataRaa$PersonId %in% sjekkPers), ]
RaaSjekk[order(RaaSjekk$PersonId, RaaSjekk$FormDate) ,c("PersonId", "FormDate", "FormDateUt", 'FormStatusUt', "UnitId")]

Sjekk <- KoronaPreprosesser(RegData = RaaSjekk)
Sjekk[order(Sjekk$PersonId, Sjekk$FormDate) ,c("PersonId", "FormDate", "FormDateUt", "AntReinn", 'Reinn',
                                               'LiggetidSjekk', 'Liggetid')]




#----------------- COVID, belastning på sykehus SKDE-------------------------------
library(korona)

#data("belegg_ssb")
# belegg_ssb2019 <- read.table('C:/ResultattjenesteGIT/BeleggSSB2019.csv', sep=';',
#                          stringsAsFactors=FALSE, header=T, row.names = T,  fileEncoding = 'latin1')
Kapasitet <- belegg_ssb[ ,c('HF', 'HFresh', 'Dognplasser.2018')]

#Henter personid fra intensiv fordi vil ha liggetider fra intensiv. Vil derfor mangle beredskapspasienter
#som ikke er registrert i intensiv. (Skal være svært få.)
datoTil <- '2020-12-31'
KoroDataRaa <- KoronaDataSQL(datoTil = datoTil, koble=1)
KoroDataPers <- KoronaPreprosesser(RegData = KoroDataRaa)

#IntData <- intensivberedskap::BeredskIntensivData() #NIRberedskDataSQL()
  BeredskRaa <- intensivberedskap::NIRberedskDataSQL()
  datoFra <- min(as.Date(BeredskRaa$FormDate))
  IntDataRaa <- intensiv::NIRRegDataSQL(datoFra = datoFra, datoTil = datoTil) #Kun ferdigstilte intensivdata på Rapporteket
  #Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
  varFellesInt <- c('DateAdmittedIntensive', 'DateDischargedIntensive',	'DaysAdmittedIntensiv',
                    'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                    'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                    'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
                    'UnitId') # PatientInRegistryGuid', 'FormStatus', 'ShNavn',
  BeredRaa <- BeredskRaa[ ,-which(names(BeredskRaa) %in% varFellesInt)]
  BeredIntRaa <- merge(BeredRaa, IntDataRaa, suffixes = c('','Int'),
                        by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
IntDataPers <- intensivberedskap::NIRPreprosessBeredsk(RegData=BeredIntRaa, kobleInt = 1)
IntDataPers$Int <- 1

KoroIntKoblet <- merge(KoroDataPers, IntDataPers, suffixes = c('','Int'),
                       by = 'PersonId', all.x = T, all.y=F)


#Kobler på kapasitet
#RegData <-  merge(KoroIntKoblet, Kapasitet, by = 'HFresh', all.x = T, all.y=F)
RegData <- KoroIntKoblet
RegData$HF <- as.factor(RegData$HF)



#Gjennomsnittlig liggetid på sykehus
LiggetidKoroHFgjsn <- round(tapply(RegData$Liggetid, INDEX = RegData$HF,  FUN = function(x) {mean(x,na.rm=T)}),1)
indInt <- which(RegData$Int==1)

#Gjennomsnittlig liggetid på sykehus for intensivpasienter
LiggetidIntHFgjsn <- round(tapply(RegData$LiggetidInt[indInt], INDEX = RegData$HF[indInt],
                                  FUN = function(x) {mean(x,na.rm=T)}),1)
#Gjennomsnittlig liggetid på intensiv
LiggetidKoroHFgjsnIntpas <- round(tapply(RegData$Liggetid[indInt], INDEX = RegData$HF[indInt],
                                         mean, na.rm=T), 1)
#Total liggetid
LiggetidKoroHFtot <- round(tapply(RegData$Liggetid, INDEX = RegData$HF,  sum, na.rm=T),1)
LiggetidKoroHFmndTot <- round(tapply(RegData$Liggetid, INDEX = list(RegData$HF, RegData$MndNum),  sum, na.rm=T),1)
write.table(LiggetidKoroHFtot, file = 'LiggetidKoroHFmndTot.csv', sep=';')

LiggetidIntHFtot <- round(tapply(RegData$LiggetidInt, INDEX = RegData$HF,  sum, na.rm=T), 1)
LiggetidIntHFmndTot <- round(tapply(RegData$LiggetidInt, INDEX = list(RegData$HF, RegData$MndNum),  sum, na.rm=T), 1)
write.table(LiggetidIntHFtot, file = 'LiggetidIntHFmndTot.csv', sep=';')
#Sjekk: RegData <- KoronaPreprosesser(KoronaDataSQL(datoTil = '2020-12-31'))


KapasitetHF <- tapply(RegData$Dognplasser.2018,  INDEX = RegData$HF,  median)
antDager <- as.numeric(as.Date(datoTil) - as.Date(datoFra))+1
BeleggHF <- round(100*LiggetidKoroHFtot/(KapasitetHF*antDager),1)

Tab <- cbind('Antall pas.' = table(RegData$HF),
      'Antall, intensiv' = table(RegData$HF[indInt]),
  'Liggetid, alle' = LiggetidKoroHFgjsn,
      'Liggetid, int.pas.' = LiggetidKoroHFgjsnIntpas,
      'Liggetid på intensiv' = LiggetidIntHFgjsn
  # ,'Belegg, prosent' = BeleggHF,
  # 'Kapasitet/dag' = KapasitetHF
      )

#write.table(Tab, file = 'CovBelastning.csv', fileEncoding = 'ASCII', sep=';')
write.table(Tab, file = 'CovLiggetider.csv', sep=';') #fileEncoding = 'ASCII',


#----Belegg per måned
LiggetidKoroHFmnd <- round(tapply(RegData$Liggetid, INDEX = RegData[ ,c('HF', 'MndNum')],  sum, na.rm=T))
KapasitetHF <- tapply(RegData$Dognplasser.2018,  INDEX = RegData$HF,  median)
antDager <- as.numeric(as.Date(datoTil) - as.Date(datoFra))+1
BeleggHF <- round(100*LiggetidKoroHFtot/(KapasitetHF*antDager),1)





#--Se på antall inneliggende per dag. Benytter rådata, dvs. ikke-personaggregerte data.
#Lag datasett som inneholder liggetid per måned per HF

#Tabell, tot.liggetid på sykehus pr.mnd og HF, tilsv liggetid på intensiv


RegData <- KoroDataRaa[ ,c("FormDate", 'FormDateUt', "UnitId", 'PersonId')]
RegData$InnDato <- as.Date(RegData$FormDate)
RegData$UtDato <- as.Date(RegData$FormDateUt)
#RegData$MndNum <- as.numeric(format(RegData$InnDato, '%m'))
#RegData$MndAar <- format(RegData$InnDato, '%b%y')
RegData <- RegData[,-which((names(RegData) %in% c("FormDate", 'FormDateUt')))]

inneliggendeSum <- function(x) { #x-dato, summerer antall inneliggende for hver dato
  sum((x >  RegData$InnDato & (x <= RegData$UtDato) | is.na( RegData$UtDato)))}

inneliggende <- function(x) { #Om en pasient/skjema er inneliggende på gitt dato, TRUE/FALSAE
  (x >  RegData$InnDato & (x <= RegData$UtDato) | is.na( RegData$UtDato))}

RegData$InnDato[is.na( RegData$UtDato)]
# inneligendeMatr <- as.data.frame(map_df(datoer, inneliggende))
# RegDataAlleDatoer <- bind_cols(RegData, inneligendeMatr)

#FEIL:
AntInneliggendeGr <- function(dato) { #Antall inneliggende for gitt dato, gruppert på variabel "gr"
  #GrNavn <- levels(RegData[,gr])
  inne <- (dato >  RegData$InnDato & (dato <= RegData$UtDato) | is.na( RegData$UtDato))
  data <- tapply(RegData[inne,'PersonId'], INDEX = RegData[inne, 'HF'], FUN= function(x){length(unique(x))})
  return(data)
  #data$Grupper <- GrNavn
}
sum(AntInneliggendeGr('2020-07-03'), na.rm = T)


# datoer <- seq(as.Date('2020-03-01'), as.Date('2020-09-30'), by="day")
# RegData <- KoroDataPers
beregnBelegg <- function(datoer){
  names(datoer) <- format(datoer, '%d.%B')
  #data <- as.data.frame(map_df(datoer, inneliggende))

  data <- erInneliggende(datoer = datoer, regdata = RegData)
  #RegData <- bind_cols(RegData, data)

antDager <- length(datoer)
antPrDagPers <- colSums(data)
  belegg <- 100*rowSums(data) / (antDager*RegData$Dognplasser.2018)
  tot <- 100*sum(colSums(data)) / (antDager*sum(Kapasitet$Dognplasser.2018))
  return(utData = list(belegg=belegg, tot=tot))
}

datoerMars <- seq(as.Date('2020-03-01'), as.Date('2020-03-31'), by="day")
RegData$mar <- beregnBelegg(datoerMars)$belegg

datoerApril <- seq(as.Date('2020-04-01'), as.Date('2020-04-30'), by="day")
RegData$apr <- beregnBelegg(datoerApril)$belegg

datoerMai <- seq(as.Date('2020-05-01'), as.Date('2020-05-31'), by="day")
RegData$mai <- beregnBelegg(datoerMai)$belegg

datoerJun <- seq(as.Date('2020-06-01'), as.Date('2020-06-30'), by="day")
RegData$jun <- beregnBelegg(datoerJun)$belegg

datoerJuli <- seq(as.Date('2020-07-01'), as.Date('2020-07-31'), by="day")
RegData$jul <- beregnBelegg(datoerJuli)$belegg

datoerAug <- seq(as.Date('2020-08-01'), as.Date('2020-08-31'), by="day")
RegData$aug <- beregnBelegg(datoerAug)$belegg

datoerSept <- seq(as.Date('2020-09-01'), as.Date('2020-09-30'), by="day")
RegData$sep <- beregnBelegg(datoerSept)$belegg

mnd <- c('mar', 'apr', 'mai', 'jun', 'jul', 'aug', 'sep')
BeleggLandet <- c(beregnBelegg(datoerMars)$tot,
                  beregnBelegg(datoerApril)$tot,
                  beregnBelegg(datoerMai)$tot,
                  beregnBelegg(datoerJun)$tot,
                  beregnBelegg(datoerJuli)$tot,
                  beregnBelegg(datoerAug)$tot,
                  beregnBelegg(datoerSept)$tot)
names(BeleggLandet) <- mnd
#NB: Endre til å telle unike personid'er.

LiggeDogn <-
  RegData[,c('HF', mnd)] %>%
  group_by(HF) %>%
  summarise_all(sum)
test <-

BeleggData <- rbind(as.matrix(LiggeDogn),
                    c('HeleLandet', BeleggLandet))

write.table(BeleggData, file = 'BeleggData.csv', row.names = F, fileEncoding = 'UTF-8', sep = ';')

#-------------Figur
#dplyr::add_row(LiggeDogn, c('Landet', BeleggLandet))
#LiggeDogn <- data.frame(LiggeDogn, c('Landet', BeleggLandet))

#LiggeDogn$Tot <- rowSums(as.data.frame(LiggeDogn[,mnd]))
#tapply(KoroDataPers$Liggetid, KoroDataPers$HF, sum, na.rm=T)

LiggeDognData <- as.data.frame(tidyr::gather(LiggeDogn, key = 'mnd', value='Belegg', mnd, factor_key=TRUE))

p <- ggplot(LiggeDognData, aes(mnd, Belegg))
p + geom_point() + facet_wrap(~ HF)
# pdf(file = plott.pdf, width=7, height=7*height/width, family=fonttype, #family='arial',
#     pointsize=pointsizePDF)
#ggplot(LiggeDognData, aes(mnd, Belegg)) + geom_point() + facet_wrap(~ HF) +

       # Use vars() to supply variables from the dataset:
       p + facet_grid(rows = vars(drv))

df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30))
ds <- do.call(rbind, lapply(split(df, df$gp),
                            function(d) {data.frame(mean = mean(d$y), sd = sd(d$y), gp = d$gp)}))
#ggplot(df, aes(gp, y)) +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3)

p <-
  ggplot(mpg, aes(displ, cty)) + geom_point()

# Use vars() to supply variables from the dataset:
p + facet_grid(rows = vars(drv))

belegg_ssb$RHFresh <- ReshNivaa$RHFresh[match(belegg_ssb$HFresh, ReshNivaa$HFresh)]
belegg_rhf <- belegg_ssb %>% group_by(RHFresh) %>% summarise("Dognplasser.2018" = sum(Dognplasser.2018))
belegg_rhf$RHF <- as.character(RegData$RHF)[match(belegg_rhf$RHFresh, RegData$RHFresh)]



#---NY RUNDE
#Tabell, tot.liggetid pr.mnd og HF, tilsv liggetid på intensiv.

datoTil <- '2020-12-31'
KoroDataRaa <- KoronaDataSQL(datoTil = datoTil, koble=1)
BeredskRaa <- intensivberedskap::NIRberedskDataSQL()
datoFra <- min(as.Date(BeredskRaa$FormDate))
IntDataRaa <- intensiv::NIRRegDataSQL(datoFra = datoFra, datoTil = datoTil) #Kun ferdigstilte intensivdata på Rapporteket
#Felles variabler som skal hentes fra intensiv (= fjernes fra beredskap)
varFellesInt <- c('DateAdmittedIntensive', 'DateDischargedIntensive',	'DaysAdmittedIntensiv',
                  'DeadPatientDuring24Hours',	'MechanicalRespirator',	'RHF', 'TransferredStatus',
                  'VasoactiveInfusion',	'MoreThan24Hours',	'Morsdato',
                  'MovedPatientToAnotherIntensivDuring24Hours',	'PatientAge',	'PatientGender',
                  'UnitId') # PatientInRegistryGuid', 'FormStatus', 'ShNavn',
BeredRaa <- BeredskRaa[ ,-which(names(BeredskRaa) %in% varFellesInt)]
BeredIntRaa <- merge(BeredRaa, IntDataRaa, suffixes = c('','Int'),
                     by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = F, all.y=F)
IntDataPers <- intensivberedskap::NIRPreprosessBeredsk(RegData=BeredIntRaa, kobleInt = 1)
IntDataPers$Int <- 1

KoroIntKoblet <- merge(KoroDataPers, IntDataPers, suffixes = c('','Int'),
                       by = 'PersonId', all.x = T, all.y=F)


#FLYTTEDE REGISTRERINGER:

  SkjemaGUIDInn <- c(
'150e9c2f-77b3-44f0-916d-77605d36fece','a7d7d201-8da8-44ae-96ce-b6d0540a0a60',
'3561a23e-566a-4bab-a632-364eed12cbdd','4e6c27bc-6063-4835-a457-f18a3f9c938e',
'0334823d-5bb7-4dcf-91a6-1fe610d56b33','6110fae8-fe06-4038-aea2-6bea9dfc7c12',
'52411918-ebaf-444b-bbb1-077dab6d7552','c39bf236-8c56-4581-813a-9f83b217c468',
'4a9559e6-deab-4143-a21c-8b7eab07b526','be852097-0c51-451b-951d-ced0decbaca4',
'e5752e24-daff-455a-9c10-8c6648afa685','49bc1fda-871d-4d61-b07d-478efc835b18',
'390d5325-63ae-4c98-9675-bd83e726708c','3aafd47a-98f1-4121-9b72-cedd26539357',
'44ad0657-1039-4f69-a491-21d47d2b603e','89d68e91-e19d-487b-b65f-29576bd30d7d',
'bd58f45d-2bdc-40b8-9cdf-ae4e50af119e','54a7cf9b-d4f0-43dc-be4a-15b5c2e99173',
'df2d4d27-96e9-4461-9ef9-f902d01e9ea2','6ccd3de7-7af6-41e6-aff0-8d4fedcb8047',
'e9bde1b0-692e-468d-ba0d-0e7eda32b2da','bc3dfd58-0db9-4376-a011-49e293d97a1a',
'8ecc2b2f-d2eb-46b5-81d3-6636195f4c3f','c3049634-05e7-453b-bd22-74ff364feb00',
'091def5c-0b3f-4a39-9d84-16c80563689b','510d11df-a3fe-4a3f-855c-1acb3fc0d99e',
'9cf2a0be-2541-45b1-8a83-4c50185aebab','99e1774a-6e91-440d-a56f-27904a4cb1c8',
'5ded1bb3-188d-48f0-8a57-07d8265f6b00','35c6b20e-db86-4140-992f-336b93d6e612',
'f8875228-2055-4f46-a0b6-1c14c30cc4c7','78093e0c-06c1-4692-90a0-1ecbe73045c2',
'2a0b0ad1-a5c3-4f2b-ad57-708d555dfd55','14ce5c38-829d-4549-85d6-a88615348130',
'1c337b67-42ac-4677-8a2b-861d09117cd1')

SkjemaGUIDUt <- c(
  '6cb85138-ba28-4618-9183-0bd706b7e5c0',  'f8012564-1b18-4ade-97b1-313389a0ed0a',
  '8f406c23-6fca-431e-9017-4ac0ad67dc5d',  '0d31c410-ca03-4cfe-b9a5-ce5bfb3d9e20',
  'ad96b388-dc88-476d-b3b0-628a3269e9f5',  '23ed777d-d9f5-44a2-932b-a80754476df6',
  'dac534ce-25d7-4bae-abbd-6f9b57980971',  'a2b67b9b-7bd0-4e3b-bfec-a9b5f386380c',
  'c8b46001-045d-4057-8b35-50cab9618e77',  '4b91acd8-474d-44a7-8737-124ef97c14c5',
  'cdf0335c-e2f6-47af-bc61-a03e488ef811',  'ad75bea7-7230-463a-8c18-fe5752b5256e',
  '085ffdf5-96d6-4652-a4d3-b4d66db03d42',  '7b569753-a287-4236-ab2f-d9269775fc3e',
  '1284ccce-06e0-4b2a-900b-284a6f31b6c5',  '6a7b0bf0-4d1f-4a96-9194-cf10d5a304f4',
  'd2dc5df1-d714-4356-b941-96208f7790ef',  '0bb683ca-c975-45ae-905c-b34f5f89307f',
  '14396ac9-4711-407d-90be-a9a30d44dec2',  '86ef80af-d029-40db-8ac6-526a6ac9ca23',
  '1168b0a9-4bf4-40e7-a780-3da602adee8c',  '193702b0-52a7-43b2-8544-5554f4babe4e',
  '759bd541-4da4-4bda-a543-abca9fbb6195',  '0a661671-276e-4291-9877-903a5a2bfca7',
  'b900cde7-c8fa-4005-a4cb-167cb37c1639',  'f0765a25-b09a-420b-afc1-e3161e234831',
  'bc8c5e82-76ed-40a0-ba3c-32028673afcf',  'd868857f-0350-4202-89cc-f7924deba976'
)
