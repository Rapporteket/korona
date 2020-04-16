#Kjørefil for Rapporteket-Pandemi
library(tidyverse)
library(korona)
RegData <- KoronaDataSQL(koble=1)
Pandemi <- KoronaPreprosesser(KoronaDataSQL(koble=1))
RegData <- Pandemi
tidsenhet='dag'
datoFra <- '2020-01-01'
datoTil <- Sys.Date()
erMann=9
aarsakInn=9
skjemastatusInn=9
skjemastatusUt <- 9
aarsakInn<- 9  #covid-19 som hovedårsak til innleggelse 1-ja, 2-nei
dodSh=9
minald <- 0
maxald <- 110
#reshID: 101719 (UNN HF), '100100' (Vestfold)
valgtEnhet='Sykehuset i Vestfold HF' #'Alle'
enhetsNivaa <- 'HF'
enhetsUtvalg <- 0
valgtVar <- 'demografi'


tab <- unique(RegData[,c("HFresh", 'HF', 'HFny')])
tab[order(tab$HFresh),]

tab2 <- unique(RegData[,c("HFresh", 'HF')])
tab2[order(tab2$HFresh),]

unique(RegData[, c("UnitId", 'HelseenhetKortNavn')])

RegData[RegData$UnitId==100065,] #108595
table(RegData$ShNavn[RegData$HF=='Helgelandssykehuset HF'])

FerdigeRegTab(RegData=Pandemi,
              aarsakInn = 1,
              valgtEnhet=valgtEnhet,
              enhets)

test <- RegData[ , c("FormDateUt", "Utskrivningsdato",'UtskrivningsdatoInnSkjema', "FormStatus", "FormStatusUt" )]

test <- KoronaUtvalg(RegData=RegData, erMann=1, skjemastatusInn=2, aarsakInn=1)
test$utvalgTxt
test$hovedgrTxt

Utdata <- KoronaFigAndeler(valgtVar='demografi', RegData=Pandemi,
                 minald=minald, maxald=maxald, aarsakInn=aarsakInn,
                 erMann=erMann, dodSh=dodSh,
                 skjemastatusInn=skjemastatusInn, skjemastatusUt=skjemastatusUt,
                 enhetsNivaa=enhetsNivaa, valgtEnhet=valgtEnhet,
                 enhetsUtvalg=1)

Tab <- FerdigeRegTab(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', erMann=9, dodSh=9)$Tab


RisikoInnTab(Pandemi, valgtEnhet = valgtEnhet, enhetsNivaa = enhetsNivaa)

AlderTab(RegData=RegData)$Tab

antallTidEnhTab(RegData, tidsenhet=tidsenhet, erMann=9, tilgangsNivaa=tilgangsNivaa,
                bekr=1, skjemastatus=0, dodSh=9, valgtEnhet='Sør-Øst')



library(knitr)
library(korona)
#setwd('C:/ResultattjenesteGIT/korona/inst')
setwd('/home/rstudio/korona/inst')
knitr::knit('KoronaRapport.Rnw', encoding = 'UTF-8')
tools::texi2pdf(file='KoronaRapport.tex')
knitr::knit2pdf('KoronaRapport.Rnw') #, encoding = 'UTF-8')


unique(PandemiInn[,c('RHFresh','RHF')])
PandemiInn %>% dplyr::group_by(RHF, HF) %>% dplyr::summarise(Antall = length(unique(HealthUnitShortName)))
PandemiInn %>% dplyr::group_by(RHF) %>% dplyr::summarise(Antall = length(unique(HF)))


PandemiInn <- read.table('A:/Pandemi/Pandemiskjema2020-04-03.csv', sep=';',
                       stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
PandemiInn$InnDato <- as.Date(PandemiInn$FormDate, tz= 'UTC', format="%d.%m.%Y") #DateAdmittedIntensive
PandemiInn$Dag <- format(PandemiInn$InnDato, '%d.%B')


# Antall uferdige registreringer (skjemastatus = 1?) - se på data

# Antall med fristbrudd (24t) - også ferdigstilte? Kun fra 1.april? Spør

library(korona)
KoroDataInn <- KoronaDataSQL(skjema=1, koble=0)
KoroDataUt <- KoronaDataSQL(skjema=2)
# varUt <- c("Antifungalbehandling", "AntiviralBehandling" , "HovedskjemaGUID",
#            'FormStatus', 'FormDate', "OverfortAnnetSykehusUtskrivning", "StatusVedUtskriving")
# KoroData <- merge(KoroDataInn, KoroDataUt[,varUt], suffixes = c('','Ut'),
#                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)
KoroDataInn <- KoronaDataSQL(skjema=1, koble = 0)
KoroDataUt <- KoronaDataSQL(skjema=2, koble=0)

KoroData <- KoronaPreprosesser(RegData = KoronaDataSQL(koble=1))
RegData <- KoroData

UtData <- KoronaUtvalg(RegData=KoroData, dodSh = 2)

RisikoInnTab(RegData = RegData, dodSh = 2)


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

datoTil=Sys.Date()
reshID=0
erMann=''
bekr=9
skjemastatus=9
dodSh=9
valgtEnhet='Alle'
enhetsNivaa='RHF'
minald=0
maxald=110



RisikoInnTab(RegData, erMann='', skjemastatus=2, dodSh=9,
                         valgtEnhet='Alle', enhetsNivaa='RHF',
                         minald=0, maxald=110)


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

library(korona)
#Overførte pasienter - SJEKK AV PASIENTAGGREGERING
RegData <- KoronaDataSQL(koble=1)
pas3 <- names(table(RegData$PasientGUID)[table(RegData$PasientGUID)>2])
indOverf <- which(RegData$PasientGUID %in% pas3)
var <- c('PasientGUID',"UnitId",  "HelseenhetKortNavn", 'ShNavnUt', "FormStatus", "Innleggelse",
         "Utskrivningsdato", "FormStatusUt", "OverfortAnnetSykehusInnleggelse", "OverfortAnnetSykehusUtskrivning")
data3opph <- RegData[indOverf, var] #RegData$PasientGUID == '3E6F196C-EF7B-EA11-A96B-00155D0B4F09'
write.csv2(data3opph[order(data3opph$PasientGUID),], file='Data3opph.csv' ,fileEncoding = 'UTF-8', row.names = F)

Pandemi <- KoronaPreprosesser(KoronaDataSQL(koble=1))
varAgg <- c('PasientID',"ReshId",  "ShNavn",  'ShNavnUt', "FormStatus", "InnTidspunkt",
            "Utskrivningsdato", "FormStatusUt", "Overf")
data3opphAgg <- Pandemi[which(Pandemi$PasientID %in% pas3), varAgg]
UtAgg <- data3opphAgg[order(data3opphAgg$PasientID),]
write.csv2(UtAgg, file='Data3opphAgg.csv' ,fileEncoding = 'UTF-8', row.names = F)
