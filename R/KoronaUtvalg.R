#' Gjøre utvalg i koronaskjema
#'
#' Returnerer filtrert dataramme og utvalgstekst.
#' NB: RegData filtreres IKKE på enhet. Velg enhetsutvalg 1 el 2 for
#' å få ind$Hoved for "egen enhet"
#'
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet (kan være både RHF eller HF)
#'    	}
#'

#'
#' @param RegData data, beredskapsskjema
#' @param datoFra startdato 'yyyy-mm-dd'
#' @param datoTil sluttdato 'yyyy-mm-dd'
#' @param minald kjønn, 0-kvinne, 1-mann
#' @param erMann kjønn, 0-kvinne, 1-mann
#' @param skjemastatusInn status på inklusjonsskjema 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param skjemastatusUt status på utskrivingsskjema 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param aarsakInn covid-19 som hovedårsak til innleggelse 1-ja, alle opph, 2-ja, minst siste opphold,
#' 3-ja, minst ett opph, 5-nei, ingen opph, 9-ukj
#' @param enhetsNivaa organisatorisk nivå, dvs. filtreringsnivå for egen enhet.
#'  RHF (SC og LC-brukere), HF (LU-brukere)
#' @param valgtEnhet - gis ut fra resh
#' @param enhetsUtvalg 0: hele landet,
#'                     1: egen enhet mot resten av landet,
#'                     2: egen enhet (NB: returnerer hele landet i ind$Rest)
#' @param dodSh død på sykehus 1-'nei', 2-'ja', 3-'ja og nei' (=ferdigstilte?)
#'
#' @return
#' @export
#'
KoronaUtvalg <- function(RegData, datoFra=0, datoTil=0, erMann=9, minald=0, maxald=110,
                             skjemastatusInn=9, skjemastatusUt=9, dodSh=9, aarsakInn=9,
                         enhetsNivaa='RHF', valgtEnhet='Alle', enhetsUtvalg=2) { #reshID=0,

 if (skjemastatusInn %in% 1:2){RegData <- subset(RegData, RegData$FormStatus==skjemastatusInn)}
 if (skjemastatusUt %in% 1:2){RegData <- subset(RegData, RegData$FormStatusUt==skjemastatusUt)}
# if (aarsakInn %in% 1:2){RegData <- subset(RegData, RegData$ArsakInnleggelse==aarsakInn)}
  if (aarsakInn %in% 1:4){
    ind <- switch(as.character(aarsakInn),
                  '1' = which(RegData$ArsakInnNy==1),
                  '2' = which(RegData$ArsakInnNy %in% 1:2),
                  '3' = which(RegData$ArsakInnNy %in% 1:3),
                  '4' = which(RegData$ArsakInnNy == 4)
                  )
    RegData <- RegData[ind, ]
    }
  if (dodSh %in% 1:3){
  RegData <- if (dodSh %in% 1:2) {subset(RegData, RegData$StatusVedUtskriving==dodSh)
    } else {subset(RegData, RegData$StatusVedUtskriving %in% 1:2)}} #Alle utskrevne
  if (erMann %in% 0:1){
   vec <- (RegData$erMann == erMann)
   RegData <- subset(RegData, vec)}
  if(minald>0 | maxald<110) {RegData <- subset(RegData,
                                               RegData$Alder >= minald & RegData$Alder <= maxald)}
 if(datoFra!=0) {RegData <- subset(RegData, RegData$InnDato >= as.Date(datoFra, tz= 'UTC'))}
 if(datoTil!=0) {RegData <- subset(RegData, RegData$InnDato <= as.Date(datoTil, tz= 'UTC'))}


  N <- dim(RegData)[1]

  utvalgTxt <- c(
    if(datoFra!=0 | datoTil!=0) {paste0(
      'Innleggelsesdatoer: ', if (N>0) {min(as.Date(RegData$InnDato), na.rm=T)} else {datoFra},
      ' til ', if (N>0) {max(as.Date(RegData$InnDato), na.rm=T)} else {datoTil})} else {NULL},
    if ((minald>0) | (maxald<110)) {paste0('Pasienter fra ', minald, ' til ', maxald)},   #if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
    if (skjemastatusInn %in% 0:5){paste('Skjemastatus, inklusjon:',
                                     c('ingen', 'kladd', 'ferdigstilt', '','slettet', 'returnert')[skjemastatusInn+1])},
    if (skjemastatusUt %in% 0:5){paste('Skjemastatus, utskriving:',
                                        c('ingen', 'kladd', 'ferdigstilt', '','slettet', 'returnert')[skjemastatusInn+1])},
    if (aarsakInn %in% 1:4){paste0('Covid-19, hovedårsak? ',
                                   c('Ja, alle opph.', 'Ja, (minst) siste opph.', 'Ja, minst ett opph.', 'Nei, ingen opph.')[aarsakInn])},
    if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
    if (dodSh %in% 1:3) {paste0('Tilstand ved utskriving: ', c('Levende','Død','Alle utskrevne')[as.numeric(dodSh)])},
    if ((valgtEnhet != 'Alle') &(enhetsUtvalg == 2)) {paste('Valgt enhet:', valgtEnhet)}
  )
# utvalgTxt <- ifelse(is.null(utvalgTxt), '', utvalgTxt)
if (is.null(utvalgTxt)) {utvalgTxt <- ''}

  ind <- list(Hoved=0, Rest=0)

  medSml <- 0
  smltxt <- ''
  if (valgtEnhet == 'Alle' |(enhetsUtvalg == 0)  ) {
    enhetsUtvalg <- 0
    ind$Hoved <- 1:N
    hovedgrTxt <- 'Hele landet'
  } else {
    ind$Hoved <- which(RegData[ ,enhetsNivaa] == valgtEnhet)
    hovedgrTxt <- valgtEnhet
    ind$Rest <- 1:N #Nødvendig??
    if (enhetsUtvalg==1){
      ind$Rest <- setdiff(1:N, ind$Hoved)
      medSml <- 1
      smltxt <- 'resten av landet' #switch(enhetsNivaa, RHF='andre RHF', HF = 'andre HF')
      }
  }
  RegDataAlle <- RegData
  if ((valgtEnhet != 'Alle') &(enhetsUtvalg == 2)) {RegData <- subset(RegData, RegData[,enhetsNivaa] == valgtEnhet)}


  #RegData er ikke filtrert på enhet. Velg enhetsutvalg 1 el 2 for å få ind$Hoved for egen enhet
  UtData <- list(RegData=RegData, RegDataAlle=RegDataAlle, utvalgTxt=utvalgTxt, ind=ind, medSml=medSml,
               smltxt=smltxt, hovedgrTxt=hovedgrTxt) #fargepalett=fargepalett,
 return(invisible(UtData))
}
