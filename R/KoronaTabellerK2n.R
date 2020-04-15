# Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

#' Antall avdøde per tidsenhet (basert på avdøddato) og enhetsnivå. Filtreringer kan også gjøres.
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
antallTidAvdode <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC', datoFra=0,
                            skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HF,
                                LU = RegData$ShNavn)

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn,
                         dodSh=2)


  RegDataAlle <- UtData$RegData
  RegDataAlle$UtDato[is.na(RegDataAlle$UtDato)] <- RegDataAlle$InnDato[is.na(RegDataAlle$UtDato)]
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato >= datoFra), ]} # filtrerer på dato

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%B'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 day'))), '%d.%B')),
                                 uke = factor(format(RegDataAlle$UtDato, '%V'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 week'))), '%V')),
                                 maaned = factor(format(RegDataAlle$UtDato, '%b.%Y'),
                                                 levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 month'))), '%b.%Y')))

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
    TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt, 2020'=sum, 'Hele landet' = sum), quiet=TRUE)
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (tilgangsNivaa != 'SC'){
    TabTidEnh <- cbind(TabTidEnh,
                       'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}

  Tab_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames = "Dato")
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
                               skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HF,
                                LU = RegData$ShNavn)

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn)


  RegDataAlle <- UtData$RegData
  RegDataAlle <- RegDataAlle[!is.na(RegDataAlle$UtDato), ]
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$UtDato >= datoFra), ]} # filtrerer på dato

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%B'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 day'))), '%d.%B')),
                                 uke = factor(format(RegDataAlle$UtDato, '%V'),
                                              levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 week'))), '%V')),
                                 maaned = factor(format(RegDataAlle$UtDato, '%b.%Y'),
                                                 levels = format(rev(seq(Sys.Date(), if (datoFra!=0) datoFra else min(RegDataAlle$UtDato), by=paste0('-1 month'))), '%b.%Y')))

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
    TabTidEnh <- addmargins(TabTidEnh, FUN=list('Totalt, 2020'=sum, 'Hele landet' = sum), quiet=TRUE)
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (tilgangsNivaa != 'SC'){
    TabTidEnh <- cbind(TabTidEnh,
                       'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}

  Tab_tidy <- tidyr::as_tibble(as.data.frame.matrix(TabTidEnh), rownames = "Dato")
  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption='Antall Coronatilfeller.')
  if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(Tab=TabTidEnh, utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1], Tab_tidy=Tab_tidy))
}


#' Funksjon som avgjør om en pasient er inneliggende på aktuell dato
#'
#' Returnerer TRUE for datoer pasienten er inneliggende
#'
#' @param datoer datoer som inneligging skal avgjøres for
#' @param regdata Dataramme som inneholder InnDato og Utdato per pasient
#'
#' @return
#' @export
erInneliggende <- function(datoer, regdata){
  # regnes som inneliggende på aktuell dato hvis den faller mellom inn- og utdato eller
  # er etter inndato og det ikke finnes utddato. Flere betingelser kan legges til.

  auxfunc <- function(x) {(x >=  regdata$InnDato & x <= regdata$UtDato) | (x >=  regdata$InnDato & is.na( regdata$UtDato))}
  map_df(datoer, auxfunc)
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
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidInneliggende <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC', datoFra=0,
                                  skjemastatusInn=9, aarsakInn=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HF,
                                LU = RegData$ShNavn)

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #minald=0, maxald=110,
                         skjemastatusInn=skjemastatusInn, aarsakInn=aarsakInn)


  RegDataAlle <- UtData$RegData
  if (datoFra != 0) {RegDataAlle <- RegDataAlle[which(RegDataAlle$InnDato >= datoFra), ]} # filtrerer på dato
  # datoer <- seq(min(RegDataAlle$InnDato), today(), by="day")
  datoer <- seq(if (datoFra!=0) datoFra else min(RegDataAlle$InnDato), today(), by="day")

  if (tidsenhet=='dag') {
    names(datoer) <- format(datoer, '%d.%B')
    aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
    RegDataAlle <- bind_cols(RegDataAlle, aux)
  } else {
    names(datoer) <- datoer
    aux <- erInneliggende(datoer = datoer, regdata = RegDataAlle)
    aux <- bind_cols(as_tibble(RegDataAlle)[, "PasientID"], aux)
    aux <- aux %>% gather(names(aux)[-1], key=Dato, value = verdi)
    aux$Dato <- as.Date(aux$Dato)
    aux$Dato <- switch (tidsenhet,
                        'uke' = format(aux$Dato, "%V"),
                        'maaned' = format(aux$Dato, "%b.%Y")
    )
    aux <- aux %>% group_by(PasientID, Dato) %>%
      summarise(er_inne = max(verdi))
    aux <- aux %>% spread(key=Dato, value = er_inne)
    RegDataAlle <- merge(RegDataAlle, aux, by = 'PasientID')
  }

    switch (tidsenhet,
                    uke = datoer <- unique(format(datoer, '%V')),
                    maaned = datoer <- unique(format(datoer, '%b.%Y')))
  if (tidsenhet %in% c("uke", "maaned")) {
    names(datoer) <- datoer
  }

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
    TabTidEnh <-
      RegData[,c("EnhNivaaVis", names(datoer))] %>%
      group_by(EnhNivaaVis) %>%
      summarise_all(sum) %>%
      tr_summarize_output(grvarnavn = 'Dato') %>%
      mutate('Hele landet' = select(., names(.)[-1]) %>% rowSums()) %>%
      bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Totalt, 2020")))
    colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
  }

  if (tilgangsNivaa != 'SC'){
    TabTidEnh[, "Hele landet"] <- c(colSums(RegDataAlle[, names(datoer)]), sum(colSums(RegDataAlle[, names(datoer)])))
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

  RegData <- UtData$RegData
  datoer <- seq(min(RegData$InnDato), today(), by="day")
  names(datoer) <- format(datoer, '%d.%B')
  aux <- erInneliggende(datoer = datoer, regdata = RegData)
  RegData <- bind_cols(RegData, aux)

  TabTidHF <-
    RegData[,c("HFresh", names(datoer))] %>%
    group_by(HFresh) %>%
    summarise_all(sum) %>%
    merge(belegg_ssb[, c("HFresh", "Dognplasser.2018", "HF")], by.x = "HFresh", by.y = "HFresh", all.x = T) %>%
    mutate(HFresh = HF) %>% select(-HF) %>%
    # bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Hele landet"))) %>%
    tr_summarize_output(grvarnavn = 'Dato')

  belegg_ssb$RHFresh <- ReshNivaa$RHFresh[match(belegg_ssb$HFresh, ReshNivaa$HFresh)]
  belegg_rhf <- belegg_ssb %>% group_by(RHFresh) %>% summarise("Dognplasser.2018" = sum(Dognplasser.2018))
  belegg_rhf$RHF <- as.character(RegData$RHF)[match(belegg_rhf$RHFresh, RegData$RHFresh)]

  TabTidRHF <-
    RegData[,c("RHFresh", names(datoer))] %>%
    group_by(RHFresh) %>%
    summarise_all(sum) %>%
    merge(belegg_rhf[, c("RHFresh", "Dognplasser.2018", "RHF")], by.x = "RHFresh", by.y = "RHFresh", all.x = T) %>%
    mutate(RHFresh = RHF) %>% select(-RHF) %>%
    bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Hele landet"))) %>%
    tr_summarize_output(grvarnavn = 'Dato')

  Samlet <- bind_cols(TabTidHF, TabTidRHF[,-1])
  reshID_rhf <- RegData[match(reshID, RegData$HFresh), "RHFresh"]

  if (tilgangsNivaa == 'LU'){
    enhet <- c(belegg_ssb$HF[match(reshID, belegg_ssb$HFresh)],
               belegg_rhf$RHF[match(reshID_rhf, belegg_rhf$RHFresh)], "Hele landet")
    TabTidEnh <- select(Samlet, c("Dato", intersect(enhet, names(Samlet))))
  }
  if (tilgangsNivaa == 'LC'){
    enhet <- c(belegg_ssb$HF[belegg_ssb$RHFresh %in% reshID_rhf],
               belegg_rhf$RHF[match(reshID_rhf, belegg_rhf$RHFresh)], "Hele landet")
    TabTidEnh <- select(Samlet, c("Dato", intersect(enhet, names(Samlet))))
  }
  if (tilgangsNivaa == 'SC'){
    TabTidEnh <- TabTidRHF
  }

  belegg_anslag <- TabTidEnh[,-1] %>% map_df(function(x) {x[-length(x)]/x[length(x)]*100})
  belegg_anslag <- bind_rows(belegg_anslag, TabTidEnh[dim(TabTidEnh)[1], 2:dim(TabTidEnh)[2]])
  belegg_anslag$Dato <- TabTidEnh$Dato
  belegg_anslag <- belegg_anslag[, c(dim(belegg_anslag)[2], 1:(dim(belegg_anslag)[2]-1))]
  belegg_anslag_txt <- belegg_anslag %>% map_df(as.character)
  belegg_anslag_txt[-dim(belegg_anslag_txt)[1], 2:dim(belegg_anslag_txt)[2]] <-
    belegg_anslag_txt[-dim(belegg_anslag_txt)[1], 2:dim(belegg_anslag_txt)[2]] %>%
    map_df(function(x) {paste0(round(as.numeric(x),1), ' %')})

  if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1], Tab_tidy=TabTidEnh,
                        belegg_anslag=belegg_anslag, belegg_anslag_txt=belegg_anslag_txt))
}

