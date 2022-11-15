#Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned', 'aar'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' @param HF benytte kortnavn for HF 0-nei, 1-ja
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
#'
antallTidEnhTab <- function(RegData, tidsenhet='dag', erMann=9, datoFra=0, datoTil=Sys.Date(), #valgtVar='innlagt',
                            tilgangsNivaa='SC', valgtEnhet='Alle', #enhetsNivaa='RHF', HF=0,
                            skjemastatusInn=9, aarsakInn=9, dodSh=9){
  #valgtEnhet representerer eget RHF/HF

  datoFra <- if (datoFra!=0) datoFra else min(RegData$InnDato, na.rm = T)
  #if (datoFra != 0) {RegData <- RegData[which(RegData$InnDato >= datoFra), ]}
  RegData <- KoronaUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil)$RegDataAlle

  RegData$TidsVar <- switch (tidsenhet,
                             dag = factor(format(RegData$InnDato, '%d.%m.%y'),
                                          levels = format(rev(seq(datoTil, datoFra,
                                                                      by=paste0('-1 day'))), '%d.%m.%y')),
                             uke = factor(paste0('U', format(RegData$InnDato, '%V.%Y')),
                                              levels = paste0('U', format(rev(seq(datoTil, datoFra,
                                                                      by=paste0('-1 week'))), '%V.%Y'))),
                             maaned = factor(format(RegData$InnDato, '%b %y'),
                                                 levels = format(seq(datoFra, datoTil, by="month"), "%b %y")),
                             aar = factor(format(RegData$InnDato, '%Y'),
                                          levels = format(seq(datoFra, datoTil, by="year"), "%Y")))

  RegData <- RegData[!is.na(RegData$TidsVar), ]

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa,
                                SC = RegData$RHF,
                                LC = RegData$HF,
                                LU = RegData$ShNavn)

  enhetsNivaa <- switch(tilgangsNivaa,'LC'='RHF', 'LU'='HF')

  #Skal også ha oppsummering for hele landet
  UtData <- KoronaUtvalg(RegData=RegData, #datoFra=datoFra, datoTil=0,
                         erMann=erMann, #minald=0, maxald=110
                         enhetsNivaa = enhetsNivaa, valgtEnhet = valgtEnhet,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn,
                         dodSh=dodSh)

  RegDataAlle <- UtData$RegDataAlle
  RegData <- UtData$RegData

  Ntest <- dim(RegData)[1]

  kolNavnSum <- ifelse(tilgangsNivaa=='SC',
                       'Hele landet',
                       paste0(valgtEnhet, ', totalt'))
  if (Ntest==0) {
    TabTidEnh <- matrix(0, ncol=1, nrow=length(levels(RegData$TidsVar)) + 1,
                        dimnames = list(c(levels(RegData$TidsVar), 'Totalt'), valgtEnhet)) #table(RegData$TidsVar)
  }else{
    TabTidEnh <- table(RegData[ , c('TidsVar', 'EnhNivaaVis')]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)
    TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt'=sum, 'Hele landet' = sum), quiet=TRUE)
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (tilgangsNivaa != 'SC'){
    TabTidEnh <- cbind(TabTidEnh,
                       'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}

  Tab_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames = "Tid")
  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption='Antall Coronatilfeller.')
  if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(Tab=TabTidEnh, utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1], Tab_tidy=Tab_tidy))
}



#' tabAntOpphEnhTid antall opphold siste X (antMnd) mnd
#' RegData må inneholde ikke-aggregerte data, dvs. data på oppholdsnivå
#' Summerer antall opphold for hele tidsperioder
#' @param RegData dataramme
#' @param enhetsNivaa Aggregeringsnivå RHF, HF, ShNavn (sykehus, standard)
#' @param tidsEnhet - Mnd, Kvartal, Aar
#' @param antTidsenh antall måneder som skal vises
#' @param datoTil siste registrering som vises
#'
#' @export
tabAntOpphEnhTid <- function(RegData, datoTil=Sys.Date(),
                            enhetsNivaa = 'ShNavn', tidsenhet = 'Mnd', antTidsenh=6){

  #Må legge på "levels" for å unngå at f.eks. siste måned ikke har registreringer

datoDum <-   switch(tidsenhet,
                    Mnd = lubridate::floor_date(as.Date(datoTil), 'month') - months(antTidsenh, abbreviate = T), #antTidsenh-1
                    Kvartal = lubridate::floor_date(as.Date(datoTil), 'month') - months(antTidsenh*3, abbreviate = T), #antTidsenh*3-1
                    #Mnd = lubridate::floor_date(as.Date(datoTil) - months(antTidsenh-1, abbreviate = T), 'month'),
                     # Kvartal = lubridate::floor_date(as.Date(datoTil) -months(antTidsenh*3-1, abbreviate = T), 'month'),
                      Aar = lubridate::floor_date(as.Date(datoTil) - 365*antTidsenh-1)
                      )
datoFra <- max(as.Date('2020-03-10'), as.Date(datoDum)) # max(as.Date('2020-03-01'), as.Date(datoDum))
datoTil <- max(as.Date(datoTil), as.Date(datoFra))

    aggVar <- c(enhetsNivaa, 'InnDato', 'Aar', 'MndNum', 'Kvartal', 'Halvaar')
    RegData <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                          & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
if (dim(RegData)[1]>0){
    RegDataNy <- SorterOgNavngiTidsEnhet(RegData, tidsenhet = tidsenhet)
    RegData <- RegDataNy$RegData
    tidsenheter <- RegDataNy$tidtxt

  tabEnhTid <- table(RegData[ , c(enhetsNivaa, 'TidsEnhet')])
  #colnames(tabEnhTid) <- tidsenheter #format(ymd(colnames(tabAvdMnd1)), '%b %y')
  tabEnhTid <- addmargins((tabEnhTid))

  tabEnhTid <- xtable::xtable(tabEnhTid, digits = 0)
} else {
  tabEnhTid <- 'Ingen registreringer'
}
  return(tabEnhTid)
}



#' Status nå
#' @param RegData pandemiskjema
#' @return
#' @export
statusNaaTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF',
                         aarsakInn=9, erMann=9){

  RegData$ShNavn <- RegData$ShNavnUt
  RegData$HF <- RegData$HFut
  UtData <- KoronaUtvalg(RegData=RegData, valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa,
                         aarsakInn=aarsakInn, erMann=erMann)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]
  inneliggere <- is.na(RegData$UtDato)
  AntPaaShNaa <- sum(inneliggere) #N - sum(!(is.na(RegData$DateDischargedIntensive)))
  LiggetidNaa <- as.numeric(difftime(Sys.Date(),
                                     RegData$InnTidspunktSiste, units='days'))[inneliggere]
  LiggetidNaaGjsn <- round(mean(LiggetidNaa[LiggetidNaa < 90], na.rm = T), 1) #<120

  igaar <- Sys.Date()-1 #  '2020-04-10' #
  innIgaar <- length(which(RegData$InnDato == as.Date(igaar)))
  utIgaar <- length(which(RegData$UtDato == as.Date(igaar)))
  dodIgaar <- length(which(RegData$UtDato[RegData$StatusVedUtskriving==2] == as.Date(igaar)))

  statusTab <- rbind(
    'På sykehus nå' = c(AntPaaShNaa, paste0(LiggetidNaaGjsn, ' dager')),
    'Innlagt i går' = c(innIgaar,''),
    'Utskrevet i går' = c(utIgaar,''),
    'Døde i går' = c(dodIgaar,'')
  )
  colnames(statusTab) <- c('Antall', 'Liggetid (gj.sn)')
  xtable::xtable(statusTab,
                 digits=0,
                 #align = c('l','r','r','r'),
                 caption='Inneliggende på sykehus nå')
  UtData <- list(Tab=statusTab, utvalgTxt=UtData$utvalgTxt, PaaShNaa=inneliggere)
  return(UtData)
}



#' Ferdigstilte registreringer, utskrevne pasienter
#' @param RegData korona-registreringer
#' @inheritParams KoronaUtvalg
#' @return
#' @export
FerdigeRegTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF',
                          minN=0,
                          datoFra='2020-03-01', datoTil=Sys.Date(), aarsakInn=9, erMann=9, dodSh=9){

  Utvalg <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa,
                         datoFra = datoFra, datoTil = datoTil,
                         aarsakInn=aarsakInn, erMann = erMann, dodSh = dodSh,
                         skjemastatusInn=2, skjemastatusUt = 2)
  RegData <- Utvalg$RegData

  N <- dim(RegData)[1]
  Liggetid <- summary(as.numeric(RegData$Liggetid)) #, na.rm = T)
  Alder <- summary(RegData$Alder) #, na.rm = T)
  BMI <- summary(RegData$BMI[RegData$BMI<60]) #Filtrerer bort de med BMI over 60
  AntReinn <- sum(RegData$Reinn, na.rm = T)
  PstReinn <- 100*AntReinn/sum(RegData$Reinn %in% 0:1)
  AntDod <- sum(RegData$StatusVedUtskriving==2, na.rm=T)
  # NrisikoKjent <- sum(RegData$KjentRisikofaktor %in% 1:2, na.rm=T)
  # Nrisiko <- sum(RegData$KjentRisikofaktor==1, na.rm=T)
  # pstRisiko <- 100*Nrisiko/NrisikoKjent
  NisolertKjent <- sum(RegData$Isolert %in% 1:2, na.rm=T)    #Tar bort ukjente
  Nisolert <- sum(RegData$Isolert == 1, na.rm=T)
  pstIsolert <- 100*Nisolert/NisolertKjent
  AntBered <- sum(RegData$BeredPas)
  PstBered <- 100*AntBered/N

  med_IQR <- function(x){
    #x[is.na(x)]<-0
    c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
  }
  formatPst <- function(x, antDes){paste0(sprintf(paste0('%.', antDes,'f'),x),'%')}

  TabFerdigeReg <- rbind(
    'Alder (år)' = c(med_IQR(Alder), N, ''),
    'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
    'BMI' = c(med_IQR(BMI), N-BMI[7], ''),
    # 'Har risikofaktorer' = c('','','', Nrisiko, pstRisiko),
    'Isolert ved innleggelse' = c('','','', Nisolert, pstIsolert),
    'Ny innleggelse (>24t)' = c('','','', AntReinn, PstReinn),
    'Intensivbehandlet' = c('','','', AntBered, PstBered),
    'Døde' = c('','','',AntDod, 100*AntDod/N) #paste0(sprintf('%.f',100*AntDod/N),'%'))
  )
  TabFerdigeReg[4:7,5] <- paste0(sprintf('%.1f', as.numeric(TabFerdigeReg[4:7,5])),' %')
  colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall pasienter', 'Andel pasienter')

  AntPas <- length(unique(RegData$PersonId))

  if (N>3){
    if (minN>0){
      underMin <- which(as.numeric(TabFerdigeReg[,4]) < minN)
      ant <- length(underMin)
      TabFerdigeReg[underMin, ] <- c(rep('', 3*ant), rep('<3', ant), rep('', ant))
    }

  xtable::xtable(TabFerdigeReg,
                 digits=0,
                 align = c('l','r','r','c', 'r','r'),
                 caption='Ferdigstilte opphold.
                 IQR (Inter quartile range) - 50 \\% av registreringene er i dette intervallet.')

  } else {TabRiTabFerdigeReg <- 'Færre enn 3 observasjoner'}
  return(invisible(UtData <- list(Tab=TabFerdigeReg,
                                  utvalgTxt = Utvalg$utvalgTxt,
                                  Ntest=N,
                                  AntPas=AntPas)))
}



#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#' @param RegData data
#' @param sens 0: standard, 1: Maskere verdier <3
#' @inheritParams KoronaUtvalg
#' @export
#' @return
RisikoInnTab <- function(RegData, datoFra='2020-03-01', datoTil=Sys.Date(),
                         erMann='', skjemastatusInn=9, dodSh=9, aarsakInn=9,
                         sens=0,
                         valgtEnhet='Alle', enhetsNivaa='RHF', minald=0, maxald=110){

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         erMann=erMann, skjemastatusInn=skjemastatusInn, dodSh=dodSh,
                         minald=minald, maxald=maxald, aarsakInn=aarsakInn,
                         valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa)

  RegData <- UtData$RegData
  RegData <- RegData[which(RegData$KjentRisikofaktor %in% 1:2), ]

  N <- dim(RegData)[1]

  AntAndel <- function(Var, Nevner){
    Ant <- sum(Var, na.rm=T)
    c(Ant, paste0(sprintf('%.0f', 100*(Ant/Nevner)),' %'))}

  #KjentRisikofaktor # 1-ja, 2-nei, 3-ukjent, -1 velg verdi

  TabRisiko <- rbind(
    Kreft = AntAndel(RegData$Kreft, N),
    'Nedsatt immunforsvar' = AntAndel(RegData$NedsattimmunHIV, N),
    Diabetes	= AntAndel(RegData$Diabetes, N),
    Hjertesykdom = AntAndel(RegData$Hjertesykdom, N),
    'Bruker ACE-hemmer' = AntAndel(RegData$AceHemmerInnkomst==1, sum(RegData$AceHemmerInnkomst %in% 1:2)),
    Astma	= AntAndel(RegData$Astma, N),
    'Kronisk lungesykdom' = AntAndel(RegData$KroniskLungesykdom, N),
    Nyresykdom =	AntAndel(RegData$Nyresykdom, N),
    Leversykdom = AntAndel(RegData$Leversykdom, N),
    'Nevrologisk/nevromusk.' = AntAndel(RegData$KroniskNevro, N),
    Gravid	= AntAndel(RegData$Gravid, N),
    'Fedme (BMI>30)' =	AntAndel(RegData$BMI>30, sum(!is.na(RegData$BMI))),
    'Røyker' =	AntAndel(RegData$Royker, N),
    'Risikofaktorer (minst en)' = AntAndel(RegData$KjentRisikofaktor==1, N),
    'Pasienter, totalt' = c(N, ''),
    '  * Antall besvart BMI:' = c(sum(!is.na(RegData$BMI)),'')
  )

  TabRisiko['Fedme (BMI>30)',2] <- paste0(TabRisiko['Fedme (BMI>30)',2], '*')
  #colnames(TabRisiko) <- c('Antall pasienter', 'Andel pasienter')


  if (N>3){
    if (sens==1){
      under3 <- which(as.numeric(TabRisiko[,1]) < 3)
      TabRisiko[under3, ] <- c(rep('<3', length(under3)), rep('', length(under3)))
    }
    colnames(TabRisiko) <- c('Antall', 'Andel') #c('Antall pasienter', 'Andel pasienter')
} else {TabRisiko <- 'Færre enn 3 observasjoner'}
  return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt, Ntest=N))
}




#' Aldersfordeling, tabell
#' @param RegData datatabell, beredskapsdata
#' @inheritParams KoronaUtvalg
#' @param enhetsNivaa styres av tilgangsnivå 'Alle', 'RHF', 'HF'
#' @return
#' @export
AlderTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', minN=0,
                     skjemastatusInn=9,  aarsakInn=9, dodSh=9, erMann=9){

  UtData <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet,
                         enhetsNivaa=enhetsNivaa,
                         dodSh = dodSh,
                         aarsakInn=aarsakInn,
                         erMann = erMann,
                         skjemastatusInn=skjemastatusInn
  )
  RegData <- UtData$RegData

  finnGrupper <- function(minN, gr, RegData){
    RegData$Gr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
    AntHoved <- table(RegData$Gr)
    minAnt <- min(AntHoved)
    DataUt <- list(minAnt=minAnt, RegData=RegData)
    return(DataUt)
  }

  gr1 <- seq(0, 90, 10)
  gr2 <- c(0,20, seq(30, 80, 10) )
  gr3 <- c(0,30, seq(40, 80, 10) )
  gr4 <- c(0,40,60,80)
  gr5 <- c(0,60)
  grupperinger <- list(gr1, gr2, gr3, gr4, gr5)

  minAnt <- 0
  tell <- 0
  while (minAnt <= minN) {
    tell <- tell + 1
    gr <- grupperinger[[tell]]
    finnGr <- finnGrupper(RegData=RegData,
                          gr=gr,
                          minN = minN)
    minAnt <- finnGr$minAnt
  }
  RegData <- finnGr$RegData
  antGr <- length(gr)
  grtxt <- c(paste0(gr[1:antGr-1], '-', gr[2:antGr]-1), paste0(gr[antGr], '+'))
  levels(RegData$Gr) <- grtxt

  N <- dim(RegData)[1]

  TabAlder <- table(RegData$Gr)
  TabAlderPst <-100*prop.table(TabAlder)

  TabAlderAlle <- cbind(
    'Antall' = c(TabAlder, N),#[,'Sum'],
    'Andel' = paste0(sprintf('%.0f', c(TabAlderPst, 100)), ' %')
  )
  row.names(TabAlderAlle)[nrow(TabAlderAlle)] <- 'Totalt'
  TabAlderUt <-  TabAlderAlle

  return(invisible(UtData <-
                     list(Tab=TabAlderUt,
                          utvalgTxt=UtData$utvalgTxt)))


  # TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
  # TabAlder <- addmargins(TabAlder) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtEnhet, ', totalt'))
  #
  # if (valgtEnhet == 'Ukjent') {
  #   TabAlder <- as.matrix(TabAlder[,ncol(TabAlder)], ncol=1) } else {
  #     if (valgtEnhet != 'Alle') {TabAlder <- TabAlder[,c(valgtEnhet, 'Sum')]}}
  # colnames(TabAlder)[ncol(TabAlder)] <- 'Hele landet'
  #
  # return(invisible(UtData <- list(Tab=TabAlder, utvalgTxt=UtData$utvalgTxt)))
}

#' Vise figurdata som tabell
#' @param UtDataFraFig data fra figurfunksjoner, dvs. beregnede verdier
#' @export
lagTabavFigFord <- function(UtDataFraFig){
  tab <-cbind(UtDataFraFig$Ngr$Hoved,
              UtDataFraFig$N$Hoved,
              UtDataFraFig$AggVerdier$Hoved,
              UtDataFraFig$Ngr$Rest,
              UtDataFraFig$N$Rest,
              UtDataFraFig$AggVerdier$Rest)
  grtxt <- UtDataFraFig$grtxt
  if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
    grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
  rownames(tab) <- grtxt
  kolnavn <- c('Teller', 'Nevner' , 'Andel (%)')
  colnames(tab) <- c(kolnavn, if(!is.null(UtDataFraFig$Ngr$Rest)){kolnavn})
  return(tab)
}

#' Finn innleggelser som mangler utskriving
#'
#' @param RegData Koblet dataramme uten aggregering
#' @param valgtEnhet egen/valgt enhet
#' @param enhetsNivaa eget/valgt enhetsnivå
#'
#' @return
#' @export
innManglerUt <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF'){
  RegData <- KoronaPreprosesser(RegData, aggPers = 0)

  UtData <- KoronaUtvalg(RegData=RegData, valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa)
  RegData <- UtData$RegData
  N <- dim(RegData)[1]

  variabler <- c('HF', 'ShNavn', 'InnDato', 'SkjemaGUID')
  tab <- RegData[which(is.na(RegData$SkjemaGUIDut)), variabler]
  tab$InnDato <- as.character(tab$InnDato)
  tabUt <- tab[with(tab, order(HF, ShNavn, InnDato)), ] #
}

#' Tabell med oversikt over hvilke beredskapsskjema som mangler pandemiskjema
#'
#' @param datoFra - startdato for innleggelse på intensiv
#'
#' @return
#' @export
#'
finnBeredUpandemi <- function(datoFra='2020-01-01', datoTil=Sys.Date(), HF='Alle', ...){
   #library(korona)
   #library(magrittr)
   datoFraPan <- as.Date(datoFra) - 90 #For å ta høyde for at pasienten kan ha ligget en stund på avd. før opphold på intensiv
   if (!exists('KoroDataOpph')){
   KoroData <- korona::KoronaDataSQL(datoFra = datoFraPan)
   KoroDataOpph <- KoronaPreprosesser(RegData = KoroData, aggPers = 0, kobleBered = 0)
   }

   BeredData <- intensivberedskap::NIRPreprosessBeredsk(
      RegData=intensivberedskap::NIRberedskDataSQL(datoFra = datoFra, datoTil = datoTil), aggPers = 0)
   #Kun ferdigstilte beredskapsskjema
   BeredData <- BeredData[BeredData$FormStatus==2,]
   if (HF != 'Alle'){
      BeredData <- BeredData[BeredData$HF == HF, ]
   }

   # (<5% skal mangle pandemiskjema)
   # For de som mangler: Sjekk om haket av for inneliggende på pandemi av annen årsak inntil 30 (lek med antall) dager før innleggelse på intensiv

   #! Mange pandemiskjema har flere tilhørende beredskapsskjema. Ved kobling i preprosess, kobles bare ett på.
   # dagerFoer <- 30
   BeredMedPand <- as.data.frame(
      BeredData %>%
         dplyr::group_by(PersonId, Innleggelsestidspunkt)%>% #, UtTidspunkt
         dplyr::mutate(
            vecMatchPanTilBered=match(TRUE,
                                      PersonId == KoroDataOpph$PersonId &
                                         HF == KoroDataOpph$HFlang &
                                         DateAdmittedIntensive  >= KoroDataOpph$InnDato & #- dagerFoer &  #Lagt inn før lagt inn intensiv
                                         DateAdmittedIntensive < KoroDataOpph$UtTidspunkt) #Ut fra pandemi etter at lagt inn intensiv (IKKE:Skrevet ut etter utskriv. int.
         ))
   #Sjekker om det er mange pandemiskjema som har flere beredskapsskjema:
   # table(table(BeredMedPand$vecMatchPanTilBered)) #197 pandemiskjema har to eller flere beredskapsskjema
   # ind <- as.numeric(names(table(BeredMedPand$vecMatchPanTilBered)[table(BeredMedPand$vecMatchPanTilBered)==5]))
   # testKoro <- KoroDataMberedOpph[ind[1], ]
   # testBered <- BeredMedPand[which(BeredMedPand$vecMatchPanTilBered == ind[1]), ]
   # sum(is.na(BeredMedPand$vecMatchPanTilBered)) #285

   TabBeredUPand <- BeredMedPand[is.na(BeredMedPand$vecMatchPanTilBered) , c("HF", "ShNavn", "DateAdmittedIntensive","SkjemaGUID")]  #BeredUPand
   TabBeredUPand$DateAdmittedIntensive <- as.character(TabBeredUPand$DateAdmittedIntensive)
   tabUt <- TabBeredUPand[with(TabBeredUPand, order(HF, ShNavn, DateAdmittedIntensive)), ]

   #Har alle med Nir_beredskapsskjema_CoV2==1 BeredPas==1: JA

   #TEST: Disse "skal" ha bered-skjema:
   # pers <- KoroDataMberedOpph$PersonId[which(KoroDataMberedOpph$BeredPas==0)]
   # KoroDataMberedOpph[KoroDataMberedOpph$PersonId==pers[2], c("InnTidspunkt", "UtTidspunkt", "ShNavn",  "HF", 'BeredPas')]
   # BeredData[BeredData$PersonId==pers[2], c("DateAdmittedIntensive", "DateDischargedIntensive", "ShNavn",  "HF")]
   # KoroDataMberedOpph$InnTidspunkt[KoroDataMberedOpph$PersonId==pers[2]] <= BeredData$DateAdmittedIntensive[BeredData$PersonId==pers[2]]

   #return(TabBeredUPand)
   #return(UtData <- list(TabBeredUPand=TabBeredUPand, TabSmBeredToPand=TabSmBeredToPand))

}


#' Finner pasienter med dobbeltregistrerte skjema
#'
#' @param RegData dataramme fra pandemi registeret, inn og utskr.skjema
#' @param tidssavik - maks tidsavvik (minutter) mellom to påfølgende registreringer som sjekkes
#'
#' @return
#' @export
PasMdblReg <- function(RegData, tidsavvik=0){
  DblReg <- RegData %>% group_by(PersonId) %>%
    dplyr::summarise(N = n(),
              #MinTid = ifelse(N>1, min(difftime(FormDate[order(FormDate)][2:N], FormDate[order(FormDate)][1:(N-1)], units = 'mins'), na.rm = T), NA),
              LikTid = ifelse(N>1,
                              ifelse(difftime(FormDate[order(FormDate)][2:N], FormDate[order(FormDate)][1:(N-1)], units = 'mins') <= tidsavvik,
                                     1, 0), 0),
              PatientInRegistryGuid = PatientInRegistryGuid[1]
    )



  PasMdbl <- DblReg$PatientInRegistryGuid[which(DblReg$LikTid == 1)]
  TabDbl <- RegData[which(RegData$PatientInRegistryGuid %in% PasMdbl),
                    c("PatientInRegistryGuid", "FormDate", "HelseenhetKortNavn", "UnitId",
                      'SkjemaGUID', "FormDateUt",'SkjemaGUIDut')]
  TabDbl <- TabDbl[order(TabDbl$FormDate), ]
  N <- dim(TabDbl)[1]
  if (N>0) {
  indSmTid <- which(difftime(TabDbl$FormDate[2:N], TabDbl$FormDate[1:(N-1)], units = 'mins') <= tidsavvik)
  TabDbl <- TabDbl[unique(sort(c(indSmTid, (indSmTid+1)))), ]
 # }
  TabDbl$FormDate <- format(TabDbl$FormDate, format="%Y-%m-%d %H:%M:%S")
  TabDbl$FormDateUt <- format(TabDbl$FormDateUt, format="%Y-%m-%d %H:%M:%S")

  tabUt <- TabDbl[order(TabDbl$PatientInRegistryGuid, TabDbl$FormDate), ]
  } else {tabUt <- paste0('Ingen registreringer med mindre enn ', tidsavvik, 'minutter mellom registreringene for samme pasient.')}
}


#' Antall personer, smitteforløp, forløp i samme tabell per HF
#'
#' @param RegData dataramme
#' @param datoFra startdato
#' @param datoTil sluttdato
#' @param enhetsNivaa 'HF' eller 'RHF'
#'
#' @return
#' @export
#'
tabAntPersOpph <- function(RegData, datoFra, datoTil=Sys.Date(), enhetsNivaa){

  datoFra <- min(as.Date(datoFra), as.Date(datoTil)) # max(as.Date('2020-03-01'), as.Date(datoDum))
  datoTil <- max(as.Date(datoTil), as.Date(datoFra))
  RegData <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                     & RegData$InnDato > as.Date(datoFra, tz='UTC'),]

  RegData$Dato <- as.Date(RegData$FormDate)
  RegData$Enhetsnivaa <- RegData[,enhetsNivaa]

  #Identifiserer inntil 3 forløp
  PasFlere <- RegData %>% dplyr::group_by(PersonId) %>%
    dplyr::summarise(.groups = 'drop',
                     SkjemaGUID = SkjemaGUID,
                     InnNr0 = ifelse(Dato-min(Dato)>90, 2, 1),
                     InnNr = ifelse(InnNr0>1, ifelse(Dato - min(Dato[InnNr0==2])>90, 3, 2), 1),
                     PersonId_sforl = paste0(PersonId, '_', InnNr)
                     #Tid = as.numeric(Dato-min(Dato))
    )
  RegData <- merge(RegData, PasFlere[,c("SkjemaGUID", "PersonId_sforl")], by='SkjemaGUID')

  if (dim(RegData)[1]>0){

    Tab <- as.data.frame(
      RegData %>%
        dplyr::group_by(Enhetsnivaa)%>%
        summarise(
          AntOpph = n(),
          AntSforl = length(unique(PersonId_sforl)),
          AntPas = length(unique(PersonId))
        ), row.names = NULL)
  } else {
    Tab <- 'Ingen registreringer'
  }
  return(Tab)
}
