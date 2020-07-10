#Kjørefil for Rapporteket-Pandemi
rm(list=(ls()))
library(tidyverse)
library(korona)
RegDataRaa <- KoronaDataSQL()
RegData <- KoronaPreprosesser(RegData = RegDataRaa)
Pandemi <- KoronaPreprosesser(KoronaDataSQL(koble=1))
RegData <- Pandemi
tidsenhet='dag'
datoFra <- '2020-01-01'
datoTil <- Sys.Date()
erMann=9
aarsakInn=1  #covid-19 som hovedårsak til innleggelse 1-ja, 2-nei
skjemastatusInn=9
skjemastatusUt <- 9
aarsakInn<- 1  #covid-19 som hovedårsak til innleggelse 1-ja, 2-nei
dodSh=9
minald <- 0
maxald <- 110
#reshID: 101719 (UNN HF), '100100' (Vestfold)
valgtEnhet='Alle' #'Sykehuset i Vestfold HF' #
enhetsNivaa <- 'RHF'
enhetsUtvalg <- 0
valgtVar <- 'demografi'

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

valgtEnhet='Sør-Øst' #'Alle'
enhetsNivaa <- 'RHF'
rolle <- 'LC'
reshID <- 100091
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
