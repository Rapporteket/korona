#Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

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
antallTidAvdode <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC', #enhetsNivaa='RHF',
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

  RegDataAlle$TidsVar <- switch (tidsenhet,
                             dag = factor(format(RegDataAlle$UtDato, '%d.%B'),
                                          levels = format(seq(min(RegDataAlle$UtDato), max(RegDataAlle$UtDato), by="day"), '%d.%B')),
                             uke = format(RegDataAlle$UtDato, '%V'),
                             maaned = format(RegDataAlle$InnTidspunkt, '%b%y'))

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
antallTidUtskrevne <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa='SC', #enhetsNivaa='RHF',
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

  RegDataAlle$TidsVar <- switch (tidsenhet,
                                 dag = factor(format(RegDataAlle$UtDato, '%d.%B'),
                                              levels = format(seq(min(RegDataAlle$UtDato), max(RegDataAlle$UtDato), by="day"), '%d.%B')),
                                 uke = format(RegDataAlle$UtDato, '%V'),
                                 maaned = format(RegDataAlle$InnTidspunkt, '%b%y'))

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
