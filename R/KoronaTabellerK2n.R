# Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

#' Antall avdøde per tidsenhet (basert på avdøddato) og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå. Datointervaller baseres følgelig på "ut-dato"
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidAvdode <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC',
                            datoFra=0, datoTil=Sys.Date(),
                            skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF

  RegData$ShNavnUt[is.na(RegData$ShNavnUt)] <- RegData$ShNavn[is.na(RegData$ShNavnUt)] # der ShNavnUt mangler, benytt ShNavn

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HFut,
                                LU = RegData$ShNavnUt)

  UtData <- KoronaUtvalg(RegData=RegData, erMann=erMann, #datoFra=0, datoTil=0, minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn,
                         dodSh=2)

  datoFra <- if (datoFra!=0) datoFra else min(RegData$UtDato, na.rm = T)
  RegDataAlle <- UtData$RegData
  RegDataAlle$UtDato[is.na(RegDataAlle$UtDato)] <- RegDataAlle$FormDateUt[is.na(RegDataAlle$UtDato)]


  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato >= datoFra), ]} # filtrerer på fradato
  if (datoTil != Sys.Date()) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato <= datoTil), ]} # filtrerer på tildato

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%m.%y'),
                                              levels = format(rev(seq(datoTil, datoFra,
                                                                      by=paste0('-1 day'))), '%d.%m.%y')),
                                 uke = factor(paste0('U', format(RegDataAlle$UtDato, '%V.%Y')),
                                              levels = paste0('U', format(rev(seq(datoTil, datoFra,
                                                                                     by=paste0('-1 week'))), '%V.%Y'))),
                                 maaned = factor(format(RegDataAlle$UtDato, '%b.%Y'),
                                                 levels = format(rev(seq(datoTil, datoFra,
                                                                         by=paste0('-1 month'))), '%b.%Y')))

  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]
  #RegData <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]

  #Trenger utvalg når totalen ikke er summen av det som vises.
  #Flytte denne for å ikke få feil tilhørighet for døde...
  RegData <- if (tilgangsNivaa == 'SC') { RegDataAlle
  } else {
    enhetsnivaa <- switch(tilgangsNivaa,'LC'='RHF', 'LU'='HFut')
    subset(RegDataAlle, RegDataAlle[ ,enhetsnivaa] == valgtEnhet)
  }

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




#' Antall utskrevne per tidsenhet (basert på avdøddato) og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidUtskrevne <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC', datoFra=0,
                               datoTil=Sys.Date(), skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF
  datoFra <- if (datoFra!=0) datoFra else min(RegData$UtDato, na.rm = T)
  RegData$ShNavnUt[is.na(RegData$ShNavnUt)] <- RegData$ShNavn[is.na(RegData$ShNavnUt)] # der ShNavnUt mangler, benytt ShNavn

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HFut,
                                #LC = RegData$HFkort2,
                                LU = RegData$ShNavnUt)

  UtData <- KoronaUtvalg(RegData=RegData, erMann=erMann, #datoFra=0, datoTil=0, minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn)


  RegDataAlle <- UtData$RegData
  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$UtDato), ]
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato >= datoFra), ]} # filtrerer på fradato
  if (datoTil != Sys.Date()) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato <= datoTil), ]} # filtrerer på tildato

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%m.%y'),
                                              levels = format(rev(seq(datoTil, datoFra,
                                                                      by=paste0('-1 day'))), '%d.%m.%y')),
                                 uke = factor(paste0('U', format(RegDataAlle$UtDato, '%V.%Y')),
                                              levels = paste0('U', format(rev(seq(datoTil, datoFra ,
                                                                                     by=paste0('-1 week'))), '%V.%Y'))),
                                 maaned = factor(format(RegDataAlle$UtDato, '%b.%Y'),
                                                 levels = format(rev(seq(datoTil, datoFra ,
                                                                         by=paste0('-1 month'))), '%b.%Y')))

  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$TidsVar), ]

  #Trenger utvalg når totalen ikke er summen av det som vises.
  RegData <- if (tilgangsNivaa == 'SC') { RegDataAlle
  } else {
    enhetsnivaa <- switch(tilgangsNivaa,'LC'='RHF', 'LU'='HF')
    subset(RegDataAlle, RegDataAlle[ ,enhetsnivaa] == valgtEnhet)
  }

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



#' Transponer output fra tidyr::summarize
#'
#' Denne funksjonen tar som input resultatet av tidyr::summarize og returnerer dens
#' transponerte uten at formatene endres.
#'
#' @param x En dataramme med output fra en gruppert summarise
#' @param grvarnavn Navnet som skal gis til første kolonne i output
#' @return tr_frame Den transponerte av inputen
#'
#' @export
tr_summarize_output <- function(x, grvarnavn=''){

  rekkefolge <- names(x)[-1]
  x <- x %>% mutate_if(is.factor, as.character)
  rekkefolge_col <- x[[1]]
  y <- x %>% gather(names(x)[-1], key=nokkel, value = verdi) %>%
    spread(key=names(x)[1], value = verdi)
  y <- y[match(rekkefolge, y$nokkel), ]
  names(y)[1] <- grvarnavn
  y <- y[,  c(1, match(rekkefolge_col, names(y)))]
  y
}

#' Antall inneliggende per tidsenhet  og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data, NB: IKKE personaggregert
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @param aarsakInn 1-ja, 2-nei, 9-alle reg
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidInneliggende <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC',
                                  datoFra=0, datoTil=Sys.Date(),
                                  skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){

  # tilgangsNivaa='SC'
  # datoFra=0
  # datoTil=Sys.Date()
  # valgtEnhet='Alle'
  # tidsenhet='dag'
  #valgtEnhet representerer eget RHF/HF

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HF, #RegData$HFnavn,
                                LU = RegData$ShNavn)

  UtData <- KoronaUtvalg(RegData=RegData, erMann=erMann, #datoFra=0, datoTil=0, minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn) #, aarsakInn=aarsakInn
  RegData <- UtData$RegData
  if (aarsakInn !=9) {
    RegData <- subset(RegData, RegData$ArsakInn==aarsakInn)
    UtData$utvalgTxt <- paste0('Covid-19, hovedårsak? ', c('Ja', 'Nei')[aarsakInn])
  }


  RegDataAlle <- RegData[!(is.na(RegData$EnhNivaaVis)), ]
  #Hvis skal regne alle med utskjema som utskrevne: Alle meRegDataAlle$UtDato[is.na(RegDataAlle$UtDato)] <- as.Date(RegDataAlle$FormDateUt[is.na(RegDataAlle$UtDato)],
  #                                                         tz= 'UTC', format="%Y-%m-%d")
  # filtrerer på dato -  UtDato >= datoFra, dvs. tar med de som er utskrervet senere enn datoFra
  #dato til? InnDato <= datoTil, dvs. tar med de som er innlagt før datoTil
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[RegDataAlle$UtDato >= datoFra | is.na(RegDataAlle$UtDato), ]}
  if (datoTil != Sys.Date()) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato <= datoTil), ]} # filtrerer på tildato
  datoer <- seq(if (datoFra!=0) datoFra else min(RegDataAlle$InnDato), datoTil, by="day") #today()

  # if (tidsenhet=='dag') {
  #   names(datoer) <- format(datoer, '%d.%b%y')
  #   aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
  #   RegDataAlle <- bind_cols(RegDataAlle, aux)
  # } else {
    #names(datoer) <- if (tidsenhet == 'dag') format(datoer, '%d.%m.%y') else datoer
  names(datoer) <- datoer
    aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
    aux <- bind_cols(as_tibble(RegDataAlle)[, 'PasientID'], aux) #"PasientID" #"EnhNivaaVis", 'RHF', 'HF',
    aux <- aux %>% gather(names(aux)[-1], key=Tid, value = verdi) #-(1:5)
    aux$Tid <- as.Date(aux$Tid)
    aux$Tid <- switch (tidsenhet,
                       'dag' = format(aux$Tid, '%d.%m.%y'),
                        'uke' = paste0('U', format(aux$Tid, "%V.%Y")),
                        'maaned' = format(aux$Tid, "%b.%Y")
    )
    aux <- aux %>% group_by(PasientID, Tid) %>%
      summarise(er_inne = max(verdi) #TRUE/FALSE
                #,EnhNivaaVis = EnhNivaaVis[1]
                #,RHF = last(RHF, order_by = InnDato),
                #HF = last(HF, order_by = InnDato)
                )
    aux <- aux %>% spread(key=Tid, value = er_inne)
    enh <- RegDataAlle %>% group_by(PasientID) %>%
      summarise(EnhNivaaVis = last(EnhNivaaVis, order_by = InnDato),
                RHF = last(RHF, order_by = InnDato),
                HF = last(HF, order_by = InnDato)
      )
    RegDataAlle <- merge(enh, aux, by = 'PasientID') #merge(RegDataAlle, aux, by = 'PasientID')
  #}

  switch(tidsenhet,
          dag = datoer <- unique(format(datoer, '%d.%m.%y')),
                    uke = datoer <- unique(paste0('U', format(datoer, '%V.%Y'))),
                    maaned = datoer <- unique(format(datoer, '%b.%Y')))
  #if (tidsenhet %in% c("dag", "uke", "maaned")) {
    names(datoer) <- datoer
  #}

  # if (datoFra != 0) {RegDataAlle <- RegDataAlle[RegDataAlle$InnDato >= datoFra, ]}
  #Trenger utvalg når totalen ikke er summen av det som vises.
  RegData <- if (tilgangsNivaa == 'SC') { RegDataAlle
  } else {
    enhetsnivaa <- switch(tilgangsNivaa,'LC'='RHF', 'LU'='HF')
    subset(RegDataAlle, RegDataAlle[ ,enhetsnivaa] == valgtEnhet)

  }

  Ntest <- dim(RegData)[1]

  kolNavnSum <- ifelse(tilgangsNivaa=='SC',
                       'Hele landet',
                       paste0(valgtEnhet, ', totalt'))
  if (Ntest==0) {
    TabTidEnh <- matrix(0, ncol=1, nrow=length(datoer) + 1,
                        dimnames = list(c(names(datoer), 'Totalt'), valgtEnhet)) #table(RegData$TidsVar)
  }else{
    total <- RegData %>%
      group_by(EnhNivaaVis) %>%
      summarise(Totalt = length(unique(PasientID))) %>%
      tr_summarize_output(grvarnavn = 'Tid')

    TabTidEnh <-
      RegData[,c("EnhNivaaVis", names(datoer))] %>%
      group_by(EnhNivaaVis) %>%
      summarise_all(sum) %>%
      tr_summarize_output(grvarnavn = 'Tid') %>%
      bind_rows(total) %>%
      mutate('Hele landet' = select(., names(.)[-1]) %>% rowSums())
      # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Totalt")))
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (tilgangsNivaa != 'SC'){
    aux <- bind_cols(kol1 = 'Hele landet', RegDataAlle[, c(names(datoer))] %>% summarise_all(sum)) %>%
      tr_summarize_output(grvarnavn = 'Tid') %>%
      bind_rows(tibble(Tid = "Totalt", "Hele landet"=as.integer(length(unique(RegDataAlle$PasientID)))))
    TabTidEnh[, "Hele landet"] <- aux[, "Hele landet"]
  }

  if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1], Tab_tidy=TabTidEnh))
}


#' Belegg per tidsenhet  og HF. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidBelegg <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC',
                            skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle', reshID=0){

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn)
  # RegData <- RegData[!(RegData$HFresh %in% c(107505, 106635, 100170)), ] #### TEST, skal fjernes!!!!!!!!!!!!

  RegData <- UtData$RegData
  datoer <- seq(min(RegData$InnDato), lubridate::today(), by="day")
  names(datoer) <- format(datoer, '%d.%m.%y')
  aux <- erInneliggende(datoer = datoer, regdata = RegData) #Matrise boolske verdier hver pasient/dag om inneliggende
  RegData <- bind_cols(RegData, aux)

  TabTidHF <-
    RegData[,c("HFresh", names(datoer))] %>%
    group_by(HFresh) %>%
    summarise_all(sum) %>%
    merge(belegg_ssb[, c("HFresh", "Dognplasser.2018", "HF")], by.x = "HFresh", by.y = "HFresh", all.x = T) %>%
    mutate(HFresh = HF) %>% select(-HF) %>%
    # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Hele landet"))) %>%
    tr_summarize_output(grvarnavn = 'Tid')

  belegg_ssb$RHFresh <- ReshNivaa$RHFresh[match(belegg_ssb$HFresh, ReshNivaa$HFresh)]
  belegg_rhf <- #table(belegg_ssb$Dognplasser.2018, useNA = 'a')
    as.data.frame(belegg_ssb %>% group_by(RHFresh) %>% summarise("Dognplasser.2018" = sum(Dognplasser.2018)))
  belegg_rhf$RHF <- as.character(RegData$RHF)[match(belegg_rhf$RHFresh, RegData$RHFresh)]

  TabTidRHF <-
    RegData[,c("RHFresh", names(datoer))] %>%
    group_by(RHFresh) %>%
    summarise_all(sum) %>%
    merge(belegg_rhf[, c("RHFresh", "Dognplasser.2018", "RHF")], by.x = "RHFresh", by.y = "RHFresh", all.x = T) %>%
    mutate(RHFresh = RHF) %>% select(-RHF) %>%
    # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Hele landet"))) %>% #KAN DENNE FJERNES?
    tr_summarize_output(grvarnavn = 'Tid')

  Samlet <- bind_cols(TabTidHF, TabTidRHF[,-1])
  reshID_rhf <- RegData[match(reshID, RegData$HFresh), "RHFresh"]

  if (tilgangsNivaa == 'LU'){
    enhet <- c(belegg_ssb$HF[match(reshID, belegg_ssb$HFresh)],
               belegg_rhf$RHF[match(reshID_rhf, belegg_rhf$RHFresh)], "Hele landet")
    TabTidEnh <- select(Samlet, c("Tid", intersect(enhet, names(Samlet))))
  }
  if (tilgangsNivaa == 'LC'){
    enhet <- c(belegg_ssb$HF[belegg_ssb$RHFresh %in% reshID_rhf],
               belegg_rhf$RHF[match(reshID_rhf, belegg_rhf$RHFresh)], "Hele landet")
    TabTidEnh <- select(Samlet, c("Tid", intersect(enhet, names(Samlet))))
  }
  if (tilgangsNivaa == 'SC'){
    TabTidEnh <- TabTidRHF
  }

  belegg_anslag <- TabTidEnh[,-1] %>% map_df(function(x) {x[-length(x)]/x[length(x)]*100})
  belegg_anslag <- bind_rows(belegg_anslag, TabTidEnh[dim(TabTidEnh)[1], 2:dim(TabTidEnh)[2]])
  belegg_anslag$Tid <- TabTidEnh$Tid
  belegg_anslag <- belegg_anslag[, c(dim(belegg_anslag)[2], 1:(dim(belegg_anslag)[2]-1))]
  belegg_anslag_txt <- belegg_anslag %>% map_df(as.character)
  belegg_anslag_txt[-dim(belegg_anslag_txt)[1], 2:dim(belegg_anslag_txt)[2]] <-
    belegg_anslag_txt[-dim(belegg_anslag_txt)[1], 2:dim(belegg_anslag_txt)[2]] %>%
    map_df(function(x) {paste0(round(as.numeric(x),1), ' %')})

  if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1], Tab_tidy=TabTidEnh,
                        belegg_anslag=belegg_anslag, belegg_anslag_txt=belegg_anslag_txt))
}

