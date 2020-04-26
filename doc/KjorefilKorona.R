#Kjørefil for Rapporteket-Pandemi
library(tidyverse)
library(korona)
RegData <- KoronaDataSQL()
RegData <- KoronaPreprosesser(RegData = RegData)
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

FerdigeRegTab(RegData=Pandemi,
              aarsakInn = 1,
              valgtEnhet=valgtEnhet,
              enhets)

test <- RegData[ , c("FormDateUt", "Utskrivningsdato","FormStatus", "FormStatusUt" )]

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




#-------------Overførte pasienter - SJEKK AV PASIENTAGGREGERING------------------------
library(korona)
RegData <- KoronaDataSQL() #1026
length(unique(RegData$PasientGUID)) #935 ->91 overføringer?
JaNeiUkjVar <- function(x) {ifelse(1 %in% x, 1, ifelse(2 %in% x, 2, 3))}
# OverfortAnnetSykehusInnleggelse,  #1-ja, 2-nei, 3-ukjent
# OverfortAnnetSykehusUtskrivning,  #1-ja, 2-nei, 3-ukjent
RegDataRed <- RegData %>% group_by(PasientGUID) %>%
  summarise(Overf = JaNeiUkjVar(c(OverfortAnnetSykehusInnleggelse, OverfortAnnetSykehusUtskrivning)),
            AntInnSkjema = n(),
            Reinn = ifelse(AntInnSkjema==1, 0,
                           ifelse(sort(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
                                    FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                    hours)) <= 8, 0, 1)), #Beregn ja/nei. Xt
            AntReinn = ifelse(Reinn==0, 0, #0-nei, 1-ja
                              sum(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
                                           FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                           hours) > 8))
            # Dobbeltreg= , #Overlappende liggetid >Xt på to ulike Sh
            # Overf = , #Beregn, ja nei
            # AntOverf = , #Antall overføringer
            # LiggetidSum = , #sum av liggetider
  )
#sort(difftime(RegData$FormDateUt[1:2], c(RegData$FormDate[1], NA)))

sum(RegDataRed$Reinn, na.rm = T)
hvilkePas <- RegDataRed$PasientGUID[which(RegDataRed$Reinn==1)]
ReinnData <- RegData[which(RegData$PasientGUID %in% hvilkePas), c('PasientGUID', "FormDate", "FormDateUt", "HelseenhetKortNavn")]

RegDataSort <- RegData[order(RegData[ ,'PasientGUID'], RegData$FormDate,     #Denne tar mest tid
              RegData$FormDateUt), ]

#FinnReinnleggelser <- function(RegData, PasientID='PasientID'){
  #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive og PasientID
  RegDataSort <- RegData[order(RegData[ ,PasientID], RegData$DateAdmittedIntensive,     #Denne tar mest tid
                               RegData$DateDischargedIntensive), ]
  RegDataSort$OpphNr <- ave(RegDataSort[ ,PasientID], RegDataSort[ ,PasientID], FUN=seq_along)
  indPasFlereOpph <- which(RegDataSort$OpphNr>1) #intersect(which(RegDataSort$AntOpph>1), which(RegDataSort$OpphNr>1))
  RegDataSort$TidUtInn <- NA
  RegDataSort$TidUtInn[indPasFlereOpph] <-
    difftime(as.POSIXlt(RegDataSort$DateAdmittedIntensive[indPasFlereOpph], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             as.POSIXlt(RegDataSort$DateDischargedIntensive[indPasFlereOpph-1], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             units = 'hour')
  RegDataSort$Reinn <- 2 #Ikke reinnleggelse
  RegDataSort$Reinn[RegDataSort$TidUtInn<72 & RegDataSort$TidUtInn >= 0] <- 1 #Reinnleggelse

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

