#  FIGURER OG TABELLER TIL ÅRSRAPPORT, NiPar, Pandemi-data
# Henter alle data, aggregerer til personnivå, men tar høyde for flere smitteforløp (3 forløp: 6pers, 4 forl:1pers)
#Filtrerer på smitteforløp hvor covid er hovedårsak til første innleggelse i smitteforløpet

#--------Klargjøre data-----------
#Legger til re-/nyinnleggelser osv. NB: Hvis vi kun ser på Covid som hovedårsak, mister vi noen reinnleggelser/overføringer.
#2021: valgt å beregne overføringer/nye innleggelser først og så filtrere på hovedårsak covid.
#Vi får dermed flere overføringer sammenlignet med 2020.
#2022: Vi skal ha personaggregerte data hvor vi teller flere smitteforløp. Smitteforløp hvor ALLE opphold hvor Covid hovedårsak.
#2022: Intensivpasient: pasienter/opphold hvor vi faktisk har et tilhørende beredskapsskjema. Ikke benyttet variabelen Nir_beredskapsskjema_CoV2.


setwd('~/Aarsrappresultater/NiPar22/')
#library(intensiv)
library(intensivberedskap)
library(korona)
datoFra <- '2020-03-01' #Vi har pandemi fra 1.mars 2020
datoTil <- '2022-12-31'	#
datoFra1aar <- '2022-01-01'

KoroDataRaa <- KoronaDataSQL(datoTil = datoTil)
KoroDataOpph <- KoronaPreprosesser(RegData=KoroDataRaa, aggPers=0, kobleBered=1)
KoroDataPers <- KoronaPreprosesser(RegData=KoroDataRaa, aggPers=1, kobleBered=1, tellFlereForlop=1)
#write.table(KoroDataPers, file = '~/mydata/KoroDataPers.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')
#ELLER hent fra staging: (pass på at staging oppdatert) Oppdatert med data fra hentet 25.april
 KoroDataRaa <- rapbase::loadStagingData("korona", "KoroDataRaa") # KoronaDataSQL(koble=1)
 KoroDataOpph <- rapbase::loadStagingData("korona", "KoroDataOpph") #(RegData = KoroDataRaa, aggPers = 0, kobleBered = 1)
 KoroDataOpph <- KoroDataOpph[KoroDataOpph$InnDato <= as.Date(datoTil), ]
 KoroDataPers <- rapbase::loadStagingData("korona", "KoroData") #(RegData = KoroDataRaa, aggPers = 1, tellFlereForlop = 1, kobleBered = 1)
 KoroDataPers <- KoroDataPers[KoroDataPers$InnDato <= as.Date(datoTil), ]

#Bare smitteforløp hvor alle opph har Covid som hovedårsak
KoroData <- KoronaUtvalg(RegData = KoroDataPers, aarsakInn = 1, datoTil = datoTil)$RegData
KoroData1aar <- KoroData[KoroData$InnDato >= as.Date(datoFra1aar), ]

# sjekk <- KoroDataOpph[KoroDataOpph$Nir_beredskapsskjema_CoV2==1 & KoroDataOpph$BeredReg==0 &
#                          KoroDataOpph$Aar==2022 & KoroDataOpph$ArsakInnleggelse==1,
#                       c("SkjemaGUID", 'HovedskjemaGUID', "FormDate", 'ShNavn',"ReshId", 'Aar')]
# write.table(sjekk, file = 'SkalHaBeredSkjema.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')
#table(KoroDataOpph[KoroDataOpph$BeredReg==0 ,c('Nir_beredskapsskjema_CoV2', 'Aar')])

# Filer <- korona::lagDatafilerTilFHI(personIDvar='PersonId',
#                                     bered=1, pand=1, influ=0,
#                                     raa=1, aggP=0)

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
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal smitteforløp', 'Del smitteforløp')
intBehPas <- round(100*prop.table(table(KoroData1aar$BeredReg))[2],1)

print(xtable::xtable(tab, align=c('l','r','r','c','r','r'),
                     label = 'tab:pan_tot',
                     caption=paste0('Nøkkeltal for pandemipasientar. Dei ', Nokkeltall$N,
                                    ' smitteforløpa gjeld ', Nokkeltall$AntPas, ' pasientar. '
                              ,intBehPas, '\\% av pasientane er intensivbehandla.')
))

Nokkeltall <- FerdigeRegTab(RegData=KoroData)
tab <- Nokkeltall$Tab[-3, ]
colnames(tab) <- c('Gj.sn', 'Median', 'IQR', 'Tal smitteforløp', 'Del smitteforløp')
intBehPas <- round(100*prop.table(table(KoroData$BeredReg))[2],1)

print(xtable::xtable(tab, align=c('l','r','r','c','r','r'),
                     label = 'tab:pan_nokstart',
                     caption=paste0('Nøkkeltal for pandemipasientar frå pandemistart, mars 2020.
                                    Dei ', Nokkeltall$N,
                                    ' smitteforløpa gjeld ', Nokkeltall$AntPas, ' pasientar. '
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

#Antibiotika
variabler <- c('Penicillin', 'PenicillinEnzymhemmer', 'Aminoglykosid',
              'AndreGencefalosporin', 'TredjeGencefalosporin', 'Kinolon',
              'Karbapenem', 'Makrolid', 'AntibiotikaAnnet', 'AntibiotikaUkjent', 'Antibiotika')
grtxt <- c('Penicillin', 'Penicillin m/enzymhemmer', 'Aminoglykosid',
           '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
           'Karbapenem', 'Makrolid', 'Annet', 'Ukjent type', 'Antibiotika, tot.')


#aggregate(KoroData[ ,variabler], by=list(KoroData$HF), FUN=function(x) {sum(x==1)})
tapply(KoroData$Antibiotika, INDEX = KoroData$HF, FUN = function(x){sum(x %in% 1:2)})

#Bare de med kjent antibiotikastatus:
ind <- which(KoroData1aar$Antibiotika %in% 1:2)
tabAntibiotikaInn <- aggregate(KoroData1aar[ind ,variabler], by=list(KoroData1aar$HF[ind]), FUN=function(x) {100*sum(x==1)/length(x)})

colnames(tabAntibiotikaInn) <- c('HF', grtxt)

print(xtable::xtable(tabAntibiotikaInn,
               digits = 1,
               align = c('l', rep('r',ncol(tabAntibiotikaInn))),
               caption = 'Antibiotikabruk ved innleggelse, pandemipasientar. Hele pandemiperioden',
               label = 'tab:pan_antibiotika_inn'
),
include.rownames = FALSE)


#Antibiotika, ut
variablerUt <- paste0('Uts', c('Penicillin', 'PenicillinEnzymhemmer', 'Aminoglykosid',
                            'AndreGencefalosporin', 'TredjeGencefalosporin', 'Kinolon',
                            'Karbapenem', 'Makrolid', 'Antifungalbehandling',
                            'AntibiotikaAnnet', 'AntibiotikaUkjent', 'Antibiotika'))
grtxt <- c('Penicillin', 'Penicillin m/enzymhemmer', 'Aminoglykosid',
           '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
           'Karbapenem', 'Makrolid', 'Antimykotisk beh.',
           'Annet', 'Ukjent type', 'Antibiotika, tot.')

#tapply(KoroData$UtsAntibiotika, INDEX = KoroData$HF, FUN = function(x){sum(x %in% 1:2)})
table(KoroData1aar$UtsAntibiotika)

#Bare de med kjent antibiotikastatus:
ind <- which(KoroData1aar$UtsAntibiotika %in% 1:2)
tabAntibiotikaUt <- aggregate(KoroData1aar[ind ,variablerUt], by=list(KoroData1aar$HF[ind]), FUN=function(x) {100*sum(x==1)/length(x)})

colnames(tabAntibiotikaUt) <- c('HF', grtxt)

print(xtable::xtable(tabAntibiotikaUt,
                     digits = 1,
                     align = c('l', rep('r',ncol(tabAntibiotikaUt))),
                     caption = 'Antibiotikabruk ved utskriving, pandemipasientar. Hele pandemiperioden',
                     label = 'tab:pan_antibiotika_ut'
                     ),
      include.rownames = FALSE)


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
#
variabler <- c('liggetid','regForsinkelseInn', 'regForsinkelseUt')
for (valgtVar in variabler) {
  KoronaFigAndeler(RegData=KoroData, datoFra=datoFra1aar, valgtVar=valgtVar, #aarsakInn=1,
                   outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

#Kun for 2020 - 1.kvartal 2022
variabler <- c('respSviktInn', 'respSviktUt', 'risikoInn', 'sirkSviktInn', 'sirkSviktUt', 'antibiotikaInn', 'antibiotikaUt')
for (valgtVar in variabler) {
   KoronaFigAndeler(RegData=KoroData, valgtVar=valgtVar, aarsakInn=1,
                    datoTil='2022-04-11', outfile = paste0('KoronaFord_', valgtVar, '.pdf'))
}

#Kun for 2020 - 1.kvartal 2022 / hele 2022 for isolert
   KoronaFigAndeler(RegData=KoroData, valgtVar='tilstandInnAarsRapp22', datoTil = datoTil, #aarsakInn=1, tilstandInnAarsRapp22
                    outfile = paste0('KoronaFord_tilstandInnAarsRapp.pdf'))

#----------------Alle figurer, tidsuvikling, basert på smitteforløp-----------------
variabler <- c('alder_u18', 'alder_u40', 'alder_o60', 'alder_o80', 'beredPas', 'dodSh',
               'isolertInn')
for (valgtVar in variabler) {
   KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar,
                     tidsenhet = 'Kvartal', aarsakInn=1,
                     outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}

for (valgtVar in c('respSviktInn', 'risikoInn')) {
   KoronaFigAndelTid(RegData=KoroData, valgtVar=valgtVar, datoTil = '2022-03-31',
                  tidsenhet = 'Kvartal', aarsakInn=1,
                  outfile = paste0('KoronaUtvTid_', valgtVar, '.pdf'))
}


KoronaFigGjsnTid(RegData=KoroData, preprosess = 0, valgtVar='liggetid', #datoTil = '2022-03-31',
                  tidsenhet = 'Kvartal', valgtMaal = 'gjsn', aarsakInn=1,
                  outfile = paste0('KoronaUtvTid_liggetid.pdf'))


#----------Tidsutvikling per RHF, samt alle-------------
#Figurer tidsutvikling, kvartalsvis, ei linje for hvert RHF + hele landet for:

tidsinfo <- SorterOgNavngiTidsEnhet(RegData=KoroData, tidsenhet = 'Kvartal')
KoroData <- tidsinfo$RegData

RHFtid <- function(RegData = KoroData, valgtVar=valgtVar, tittel = '', yAkseTxt = '',
                   statFunk = ''){

   KoronaVarSpes <- KoronaVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelTid')
   KoroData <- KoronaVarSpes$RegData
   if (tittel=='') {tittel <- KoronaVarSpes$tittel}
   if (yAkseTxt == 'andel (%)') {KoroData$Variabel <- 100*KoroData$Variabel}

   RHFrekkef <- c('Nord', 'Midt', 'Vest', 'Sør-Øst')
   Ngr <- table(KoroData$RHF)[RHFrekkef]
   NtidRHF <- tapply(KoroData$Variabel, INDEX = list(KoroData$TidsEnhet, KoroData$RHF), statFunk) #Tot. ant. per tidsenhet
   NtidLand <- tapply(KoroData$Variabel, INDEX = KoroData$TidsEnhet, statFunk) #Tot. ant. per tidsenhet

   if (valgtVar == 'antall100K') {
      innbRHF <- c(482210, 744580,1135740, 3112710)
   NtidRHF <- t(NtidRHF[,RHFrekkef])/innbRHF*100000
   NtidRHF <- t(NtidRHF)
   NtidLand <- NtidLand/sum(innbRHF)*100000
   }

   FigTypUt <- rapFigurer::figtype(outfile=paste0(valgtVar, '_tidRHF.pdf')) #, fargepalett=fargepalett)
   farger <- FigTypUt$farger
   xskala <- 1:length(levels(KoroData$TidsEnhet)) #length(tidtxt)
   xmax <- max(xskala)
   ymax <- ifelse(yAkseTxt=='andel (%)',
                  min(1.25*max(NtidRHF,na.rm=T), 119),
                  1.2*max(c(NtidRHF, NtidLand), na.rm=T))
   plot(xskala, rep(0, length(xskala)),  type='o', pch="'", col='white',
        xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,
        cex=2, xlab='år-kvartal', ylab=yAkseTxt, ylim=c(0,ymax), yaxs = 'i')
   grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")
   axis(side=1, at = xskala, labels = tidsinfo$tidtxt) #tidtxt)
   title(tittel, line=1, font.main=1)


   lines(xskala, NtidRHF[,'Nord'], col=farger[1], lwd=2, lty=5)
   lines(xskala, NtidRHF[,'Midt'], col=farger[2], lwd=2, lty=4)
   lines(xskala, NtidRHF[,'Vest'], col=farger[3], lwd=2, lty=5)
   lines(xskala, NtidRHF[,'Sør-Øst'], col=farger[4], lwd=2, lty=4)
   lines(xskala, NtidLand, col='black', lwd=2)

   pos <- ifelse(min(NtidRHF, na.rm = T)>80, 'left', 'topleft')
  # if (valgtVar == 'risikoInn') {pos <- 'right'}
   legend(pos,
          legend = paste0(c(RHFrekkef, 'Hele landet'),
                                      ' (N=', c(Ngr, sum(Ngr)),')'),
          col=c(farger, 'black'), lwd=2, lty = c(5,4,5,4,1), bty='n')

   dev.off()
}



# Alder og liggetid (median?),
# andel døde, andel på intensiv, andel isolerte,
# andel som har risikofaktorer (frem til 11 april, Q1),
# tall innlagte,

RHFtid(RegData = KoroData, valgtVar='alder', tittel = 'Median alder ved innleggelse',
       yAkseTxt = 'alder (år)', statFunk <- 'median')
RHFtid(RegData = KoroData, valgtVar='liggetid', tittel = 'Median liggetid på sykehus',
       yAkseTxt = 'døgn', statFunk <- 'median')
RHFtid(RegData = KoroData, valgtVar='dodSh',
       yAkseTxt = 'andel (%)', statFunk = 'mean')
RHFtid(RegData = KoroData, valgtVar='beredPas',
       yAkseTxt = 'andel (%)', statFunk = 'mean')
RHFtid(RegData = KoroData[which(KoroData$InnDato <= '2022-03-31'), ], valgtVar='risikoInn',
       yAkseTxt = 'andel (%)', statFunk = 'mean')
RHFtid(RegData = KoroData, valgtVar='isolertInn',
       yAkseTxt = 'andel (%)', statFunk = 'mean')
RHFtid(RegData = KoroData, valgtVar='ant', tittel = 'Antall innlagte',
       yAkseTxt = 'antall', statFunk = 'length')
RHFtid(RegData = KoroData, valgtVar='antall100K', tittel = 'Antall innlagte per 100 000 innbyggere',
       yAkseTxt = "antall per 100' ", statFunk = 'length')


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

 # RegData <- KoroDataRaa[as.Date(KoroDataRaa$FormDate) >= as.Date(datoFra1aar) &
 #                           as.Date(KoroDataRaa$FormDate) <= as.Date(datoTil) &
 #                           KoroDataRaa$ArsakInnleggelse ==1, ]
 # manglerUt <- innManglerUt(RegData = RegData) #

#----Innleggelsesskjema
 KoroInnData <- korona::KoronaDataSQL(datoTil = datoTil, skjema = 1, koble = 0) #datoFra = datoFra1aar,
 KoroInnData <- KoroInnData[KoroInnData$ArsakInnleggelse == 1, ]
 #varNavnInn <- intersect(names(KoroInnDataRaa), names(KoroData1aar))
 #KoroInnData <- KoronaPreprosesser(RegData, aggPers = 0) #
 LogVar <- names(KoroInnData)[unique(which(KoroInnData[1,] %in% c('True','False')),
                                     which(KoroInnData[15,] %in% c('True','False')))]
 KoroInnData[, LogVar] <- apply(KoroInnData[, LogVar], 2, as.logical)

 #table(KoroInnData$KjentRisikofaktor, useNA = 'a') # 1-ja, 2-nei, 3-ukjent, -1 velg verdi
 #Skal ikke ha med hver enkelt risikovariabel, bare totalen
 # risikoVar <- c('Kreft', 'NedsattimmunHIV', 'Diabetes', 'Hjertesykdom',
 #                'AceHemmerInnkomst', 'Astma', 'KroniskLungesykdom', 'Nyresykdom',
 #                'Leversykdom', 'KroniskNevro', 'Gravid', 'Royker')
sjekkVar <- c('AkuttSirkulasjonsvikt', 'AkuttRespirasjonsvikt', 'AkuttNyresvikt',
               'Antibiotika', 'Bilirubin', 'Ddimer', 'DiastoliskBlodtrykk',
               'Hjertefrekvens', 'Hoyde', 'Isolert', 'Kreatinin',
               'KjentRisikofaktor', 'Leukocytter', 'Oksygenmetning', 'RontgenThorax',
               'Respirasjonsfrekvens', 'SystoliskBlodtrykk', 'Temp', 'Trombocytter', 'Vekt')
 #head(KoroInnData[,sjekkVar])

#For risikofaktorer er det nok med kun modervariabel (t.o.m 11 april 2022) Det samme for antibiotika- nok med modervariabel (tom. 11.04.22)

 antUkj <- colSums(KoroInnData ==3, na.rm = T)
 antUkj[which(names(antUkj) == 'AkuttSirkulasjonsvikt')] <- sum(KoroInnData$AkuttSirkulasjonsvikt == 999) #har ukjent kodet 999
 antUkj[which(names(antUkj) == 'AkuttRespirasjonsvikt')] <- sum(KoroInnData$AkuttRespirasjonsvikt == 999) #har ukjent kodet 999
 antUkj[which(names(antUkj) == 'RontgenThorax')] <- sum(KoroInnData$RontgenThorax == 5) #RontgenThorax har ukjent kodet 5
 #antUkj[which(names(antUkj) == 'Antibiotika')] <- sum(KoroInnData$AntibiotikaUkjent) #FEIL MÅ IKKE BRUKES
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
 #antUkj[which(names(antUkj) %in% risikoVar)] <- length(indUkjRisk)

 # fjernVarInn <- c('AntibiotikaUkjent', 'BilirubinUkjent', 'DdimerUkjent', 'DiastoliskBlodtrykkUkjent',
 #   'HjertefrekvensUkjent', 'HoydeUkjent', 'LeukocytterUkjent', 'OkysgenmetningUkjent',
 #   'RespirasjonsfrekvensUkjent', 'SystoliskBlodtrykkUkjent', 'SkreatininUkjent', 'TempUkjent', 'TrombocytterUkjent', 'VektUkjent',
 #   'FormDate', 'Helseenhet', 'HelseenhetID', 'HelseenhetKortNavn', 'HF', 'PersonId', 'SkjemaGUID',
 #   'RHF', 'Sykehus', 'CurrentMunicipalNumber', 'DistrictCode', 'MunicipalNumber', 'Municipal', 'PersonIdBC19Hash',
 #   'Importert', 'FormStatus', 'FormTypeId', 'CreationDate', 'Innleggelse',
 #   'ArsakInnleggelse')

 antUkj <- antUkj[sjekkVar]
antNA <- colSums(is.na(KoroInnData[, sjekkVar])) + colSums(KoroInnData[, sjekkVar]==-1, na.rm = T)
Nkoro <- dim(KoroInnData)[1]
tabInn <- cbind(
  'Tal tomme' = antNA,
  'Tal ukjend' = antUkj,
  'Del tomme' = paste0(sprintf('%.1f', 100*antNA/Nkoro), '%'),
  'Del ukjend' = paste0(sprintf('%.1f', 100*antUkj/Nkoro), '%')
)

indDato <- which(as.Date(KoroInnData$Innleggelse) <= '2022-04-11')
Ndato <- length(indDato)
# AkuttNyresvikt (tom 11.04.22) -1,1,2,3
tomme <- sum(is.na(KoroInnData$AkuttNyresvikt[indDato])) + sum(KoroInnData$AkuttNyresvikt[indDato] == -1)
ukj <- sum(KoroInnData$AkuttNyresvikt[indDato] == 3)
tabInn[ 'AkuttNyresvikt', ] <- c(tomme, ukj, paste0(sprintf('%.1f', 100*c(tomme, ukj)/Ndato), '%'))

# Antibiotika (tom. 11.04.22) 1,2,3
tomme <- sum(is.na(KoroInnData$Antibiotika[indDato])) + sum(KoroInnData$Antibiotika[indDato] == -1)
ukj <- sum(KoroInnData$Antibiotika[indDato] == 3)
tabInn[ 'Antibiotika', ] <- c(tomme, ukj, paste0(sprintf('%.1f', 100*c(tomme, ukj)/Ndato), '%'))

# KjentRisikofaktor (tom 11.04.22) 1,2,3
tomme <- sum(is.na(KoroInnData$KjentRisikofaktor[indDato])) + sum(KoroInnData$KjentRisikofaktor[indDato] == -1)
ukj <- sum(KoroInnData$KjentRisikofaktor[indDato] == 3)
tabInn[ 'KjentRisikofaktor', ] <- c(tomme, ukj, paste0(sprintf('%.1f', 100*c(tomme, ukj)/Ndato), '%'))


#Endre til longtable
 xtable::xtable(tabInn,
                digits = 0,
                align = c('l','r','r','r','r'),
        caption = 'Komplettheit for sentrale pandemivariablar registrert ved innlegging.
        Variablane Antibiotika, AkuttNyresvikt og KjentRisikofaktor er berre registrerte til og med 11.04.22',
        label = 'tab:pan_kompl_inn'
 )


#--------ut
 varNavnUt <- c('UtsAkuttNyresvikt','UtsAkuttRespirasjonsvikt'
 ,'UtsAkuttSirkulasjonsvikt','UtsAminoglykosid','UtsAndreGencefalosporin','UtsAntibiotika'
 ,'UtsAntibiotikaAnnet' ,'UtsAntibiotikaUkjent','UtsAntifungalbehandling','UtsAntiviralBehandling'
 ,'FirstTimeClosedUt','UtsKarbapenem','UtsKinolon','UtsMakrolid','UtsPenicillin'
 ,'UtsPenicillinEnzymhemmer','OverfortAnnetSykehusUtskrivning'
 ,'UtsTredjeGencefalosporin','StatusVedUtskriving','Utskrivningsdato')

 KoroUtData <- korona::KoronaDataSQL(datoTil = '2022-12-31', koble = 1) #Benytter variablene fra ut-skjema
 KoroUtData <- KoroUtData[KoroUtData$ArsakInnleggelse == 1, ]
 KoroUtData <- KoronaPreprosesser(RegData=KoroUtData, aggPers=0, kobleBered=0)

 Nkoro <- dim(KoroUtData)[1]
 which(colSums(is.na(KoroUtData))>0)
 antNA <- colSums(is.na(KoroUtData)) + colSums(KoroUtData==-1, na.rm = T)
 #Ukjente.
 #AkuttSirkulasjonsvikt og  AkuttRespirasjonsvikt har ukjent kodet 999
 #RontgenThorax har ukjent kodet 5
 antUkj <- colSums(KoroUtData ==3, na.rm = T)
 antUkj[which(names(antUkj) == 'UtsAkuttSirkulasjonsvikt')] <- sum(KoroUtData$UtsAkuttSirkulasjonsvikt == 999, na.rm = T)
 antUkj[which(names(antUkj) == 'UtsAkuttRespirasjonsvikt')] <- sum(KoroUtData$UtsAkuttRespirasjonsvikt == 999, na.rm = T)

 VarUkjentKode3 <-
    c('UtsAkuttNyresvikt', 'UtsAntibiotika', 'UtsAntifungalbehandling',
      'UtsAntiviralBehandling', 'OverfortAnnetSykehusUtskrivning', 'StatusVedUtskriving')
 VarNA <- c('Utskrivningsdato')
 #table(KoroUtData$OverfortAnnetSykehusUtskrivning, useNA = 'a')
 #sum(is.na(KoroUtData$UtsKj))

 varMinus1 <- c('UtsSteroideBehandling', 'UtsImmunmodBehandling', 'UtsMonoklonaleantistoff')

   TF <-  c('UtsAminoglykosid', 'UtsAndreGencefalosporin', 'UtsAntibiotikaAnnet',
   'UtsAntibiotikaUkjent', 'UtsKarbapenem', 'UtsKinolon', 'UtsMakrolid', 'UtsPenicillin', 'UtsPenicillinEnzymhemmer',
   'UtsTredjeGencefalosporin')

 varUtMed <- c('UtsAkuttSirkulasjonsvikt', 'UtsAkuttRespirasjonsvikt', VarUkjentKode3, VarNA)

 table(KoroUtData$UtsAntiviralBehandling, useNA = 'a')

 # Legg til:
 # SteroideBehandling  (01.05.21- 11.04.2022) SteroideBehandling -1     1     2     3
 # ImmunmodelerendeBehandling (01.05.21- 11.042022) ImmunmodBehandling -1     1     2     3
 # Monoklonaleantistoff (fra 15.03.22 11.04.22) Monoklonaleantistoff  -1     1     2     3
 # AnnenImmunmodelerendeBehandling (fra 15.03.22-11.04.22) ???NyImmunmodBehandling, TypeImmunmodBehandling
 # Antiviralia (15.03.21-11-04.22) AntiviralBehandling 1,2,3

 KoroUtData$uts

 tabUt <- cbind(
   'Tal tomme' = antNA,
   'Tal ukjend' = antUkj,
   'Del tomme' = paste0(sprintf('%.1f', 100*antNA/Nkoro), '%'),
   'Del ukjend' = paste0(sprintf('%.1f', 100*antUkj/Nkoro), '%')
 )

 #tabUt <- tabUt[-which(names(antUkj) %in% 'UtsAntibiotikaUkjent'), ]
 tabUt <- tabUt[which(names(antUkj) %in% varUtMed), ]

 xtable::xtable(tabUt,
              digits = 0,
        caption = 'Komplettheit for variablar registrert ved utskriving i pandemidelen av registeret.',
        label = 'tab:pan_kompl_ut'
 )
