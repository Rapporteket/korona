#Kjørefil for Rapporteket-Pandemi
library(korona)
PandemiInn <- KoronaPreprosesser(KoronaDataSQL(skjema=1))

tidsenhet='dag'
erMann=9
bekr=9
skjemastatus=9
dodSh=9
valgtEnhet='Alle'
tilgangsNivaa <- 'SC'

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
KoroData <- KoronaPreprosesser(RegData = KoroData)

RegData <- KoronaPreprosesser(RegData = KoroData)

UtData <- KoronaUtvalg(RegData=KoroData, dodSh = 2)

RisikoInnTab(RegData = RegData, dodSh = 2)

max(sort(table(KoroDataInn$SkjemaGUID)))
sort(table(KoroDataUt$HovedskjemaGUID))
sort(table(KoroDataUt$SkjemaGUID))
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

AlderTab(RegData)






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



