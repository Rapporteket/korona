#Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' @param HF benytte kortnavn for HF 0-nei, 1-ja
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidEnhTab <- function(RegData, tidsenhet='dag', erMann=9, datoFra=0, datoTil=Sys.Date(), #valgtVar='innlagt',
                            tilgangsNivaa='SC', valgtEnhet='Alle', #enhetsNivaa='RHF',
                            HF=0, skjemastatusInn=9, aarsakInn=9, dodSh=9){
  #valgtEnhet representerer eget RHF/HF
#if (valgtVar == 'utskrevet') {}

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
                             maaned = factor(format(RegData$InnDato, '%b.%Y'),
                                                 levels = format(rev(seq(datoTil, datoFra,
                                                                         by=paste0('-1 month'))), '%b.%Y')))

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
  # indKladdUt <- which(RegData$FormStatusUt == 1)
  # ind <- which(as.numeric(difftime(RegData$CreationDateUt[indKladdUt], RegData$CreationDate[indKladdUt],
  #                                  units = 'days')) < 1)
  AntPaaShNaa <- sum(inneliggere) #N - sum(!(is.na(RegData$DateDischargedIntensive)))
  LiggetidNaa <- as.numeric(difftime(Sys.Date(),
                                     RegData$InnTidspunktSiste, units='days'))[inneliggere]
  LiggetidNaaGjsn <- round(mean(LiggetidNaa[LiggetidNaa < 120], na.rm = T), 1)

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
                          datoFra='2020-03-01', datoTil=Sys.Date(), aarsakInn=9, erMann=9, dodSh=9){

  Utvalg <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa,
                         datoFra = datoFra, datoTil = datoTil,
                         aarsakInn=aarsakInn, erMann = erMann, dodSh = dodSh,
                         skjemastatusInn=2, skjemastatusUt = 2)
  RegData <- Utvalg$RegData

  N <- dim(RegData)[1]
  Liggetid <- summary(as.numeric(RegData$Liggetid), na.rm = T)
  Alder <- summary(RegData$Alder, na.rm = T)
  BMI <- summary(RegData$BMI[RegData$BMI<60]) #Filtrerer bort de med BMI over 60
  AntReinn <- sum(RegData$Reinn, na.rm = T)
  PstReinn <- 100*AntReinn/sum(RegData$Reinn %in% 0:1)
  AntDod <- sum(RegData$StatusVedUtskriving==2, na.rm=T)
  NrisikoKjent <- sum(RegData$KjentRisikofaktor %in% 1:2, na.rm=T)
  Nrisiko <- sum(RegData$KjentRisikofaktor==1, na.rm=T)
  pstRisiko <- 100*Nrisiko/NrisikoKjent
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
    'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
    'Alder (år)' = c(med_IQR(Alder), N, ''),
    'BMI' = c(med_IQR(BMI), N, ''),
    'Har risikofaktorer' = c('','','', Nrisiko, pstRisiko),
    'Isolert ved innleggelse' = c('','','', Nisolert, pstIsolert),
    'Ny innleggelse (>24t)' = c('','','', AntReinn, PstReinn),
    'Intensivbehandlet' = c('','','', AntBered, PstBered),
    'Døde' = c('','','',AntDod, 100*AntDod/N) #paste0(sprintf('%.f',100*AntDod/N),'%'))
  )
  TabFerdigeReg[4:8,5] <- paste0(sprintf('%.1f', as.numeric(TabFerdigeReg[4:8,5])),' %')
  colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall pasienter', 'Andel pasienter')

  AntPas <- length(unique(RegData$PersonId))

  xtable::xtable(TabFerdigeReg,
                 digits=0,
                 align = c('l','r','r','c', 'r','r'),
                 caption='Ferdigstilte opphold.
                 IQR (Inter quartile range) - 50% av registreringene er i dette intervallet.')
  return(invisible(UtData <- list(Tab=TabFerdigeReg,
                                  utvalgTxt = Utvalg$utvalgTxt,
                                  Ntest=N,
                                  AntPas=AntPas)))
}



#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#' @param RegData data
#' @inheritParams KoronaUtvalg
#' @export
#' @return
RisikoInnTab <- function(RegData, datoFra='2020-03-01', datoTil=Sys.Date(),
                         erMann='', skjemastatusInn=9, dodSh=9, aarsakInn=9,
                         valgtEnhet='Alle', enhetsNivaa='RHF', minald=0, maxald=110){

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         erMann=erMann, skjemastatusInn=skjemastatusInn, dodSh=dodSh,
                         minald=minald, maxald=maxald, aarsakInn=aarsakInn,
                         valgtEnhet=valgtEnhet, enhetsNivaa = enhetsNivaa)

  RegData <- UtData$RegData
  RegData <- RegData[which(RegData$KjentRisikofaktor %in% 1:2), ]

  N <- dim(RegData)[1] #Sjekk hvilke som kan benytte felles N

  #AntAndel <- function(Var, Nevner){c(sum(Var), sum(Var)/Nevner)}
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
    'Antall pasienter (i tabellen)' = c(N, '')
    #   'Risikofaktorer (av alle)' = AntAndel(RegData$KjentRisikofaktor==1, dim(RegData)[1])
  )

  #TabRisiko[,2] <- paste0(sprintf('%.0f', 100*(TabRisiko[ ,2])),'%')

  #if (Ntest>3){

  colnames(TabRisiko) <- c('Antall', 'Andel')

  xtable::xtable(TabRisiko,
                 digits=0,
                 align = c('l',rep('r',ncol(TabRisiko))),
                 caption='Risikofaktorer')
  return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt, Ntest=N))
}




#' Aldersfordeling, tabell
#' @param RegData datatabell, beredskapsdata
#' @inheritParams KoronaUtvalg
#' @param enhetsNivaa styres av tilgangsnivå 'Alle', 'RHF', 'HF'
#' @return
#' @export
AlderTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF',
                     skjemastatusInn=9,  aarsakInn=9, dodSh=9, erMann=9){

  UtData <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet,
                         enhetsNivaa=enhetsNivaa,
                         dodSh = dodSh,
                         aarsakInn=aarsakInn,
                         erMann = erMann,
                         skjemastatusInn=skjemastatusInn
  )
  RegData <- UtData$RegData #[UtData$ind$Hoved, ]

  N <- dim(RegData)[1]
  gr <- seq(0, 90, ifelse(N<100, 25, 10) )
  RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
  grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
    c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
  levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))

  TabAlder <- table(RegData$AldersGr) #, RegData$EnhetsNivaaVar)
  #TabAlder <- addmargins(TabAlder) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtRHF, ', totalt'))
  TabAlderPst <-TabAlder/N*100 #[-nrow(TabAlder),]
  #paste0(sprintf('%.0f', as.numeric(TabHjelp[1:2,'Andel'])),'%')

  TabAlderAlle <- cbind(
    'Antall' = c(TabAlder, N),#[,'Sum'],
    'Andel' = paste0(sprintf('%.0f', c(TabAlderPst, 100)), ' %') #[,'Sum']
  )
  row.names(TabAlderAlle)[nrow(TabAlderAlle)] <- 'Totalt'
  TabAlderUt <-  TabAlderAlle
  #   if (valgtRHF %in% levels(RegData$RHF)){
  #   TabAlderUt <- cbind(
  #     'Antall, eget' = TabAlder[ ,valgtRHF],
  #     'Andel, eget' = paste0(sprintf('%.0f', c(TabAlderPst[ ,valgtRHF], 100)), ' %'),
  #     TabAlderAlle)
  # } else {TabAlderAlle}

  return(invisible(UtData <-
                     list(Tab=TabAlderUt,
                          utvalgTxt=UtData$utvalgTxt))) #c(UtData$utvalgTxt, paste0('Valgt RHF: ', valgtRHF)))))


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
  # colnames(tab) <- c(paste0(UtDataFraFig$hovedgrTxt,', Antall'),
  #                    paste0(UtDataFraFig$hovedgrTxt, ', Andel (%)'),
  #                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt,', Antall')},
  #                    if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt, ', Andel (%)')})

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


#' Finner pasienter med dobbeltregistrerte skjema
#'
#' @param RegData dataramme fra pandemi registeret, inn og utskr.skjema
#' @param tidssavik - maks tidsavvik (minutter) mellom to påfølgende registreringer som sjekkes
#'
#' @return
#' @export
PasMdblReg <- function(RegData, tidsavvik=0){
  DblReg <- RegData %>% group_by(PersonId) %>%
    summarise(N = n(),
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
  indSmTid <- which(difftime(TabDbl$FormDate[2:N], TabDbl$FormDate[1:(N-1)], units = 'mins') <= tidsavvik)
  TabDbl <- TabDbl[unique(sort(c(indSmTid, (indSmTid+1)))), ]
  TabDbl$FormDate <- format(TabDbl$FormDate, format="%Y-%m-%d %H:%M:%S")
  TabDbl$FormDateUt <- format(TabDbl$FormDateUt, format="%Y-%m-%d %H:%M:%S")

  tabUt <- TabDbl[order(TabDbl$PatientInRegistryGuid, TabDbl$FormDate), ]
}


