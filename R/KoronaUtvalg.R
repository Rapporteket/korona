#' Gjøre utvalg i koronaskjema
#'
#' Returnerer filtrert dataramme og utvalgstekst.
#' Aktuelt med utvalg på HF og RHF-nivå
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
#' @param erMann kjønn, 0-kvinne, 1-mann
#' @param skjemastatusInn status på inklusjonsskjema 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param skjemastatusUt status på utskrivingsskjema 0-ingen, 1-kladd, 2-ferdigstilt, 4-slettet, 5-returnert
#' @param aarsakInn covid-19 som hovedårsak til innleggelse
#' @param enhetsUtvalg enhetsutvalg...
#' @param dodSh død på sykehus 0-nei, 1-ja
#' @param reshID reshID fra innlogging
#'
#' @return
#' @export
#'
KoronaUtvalg <- function(RegData, datoFra=0, datoTil=0, erMann=9, minald=0, maxald=110,
                             skjemastatusInn=9, skjemastatusUt=9, dodSh=9, aarsakInn=9,
                         reshID=0, enhetsNivaa='RHF', valgtEnhet='Alle', enhetsUtvalg=0) {
  #Enhetsutvalg:
  #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
  #trengs ikke data for hele landet:
  reshID <- as.numeric(reshID)
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg ==2) {
    RegData <- RegData[which(RegData$ReshId == reshID),]}	#kun egen enhet



 if (skjemastatusInn %in% 1:2){RegData <- subset(RegData, RegData$FormStatus==skjemastatusInn)}
 if (skjemastatusUt %in% 1:2){RegData <- subset(RegData, RegData$FormStatusUt==skjemastatusUt)}
 if (aarsakInn %in% 1:2){RegData <- subset(RegData, RegData$ArsakInnleggelse==aarsakInn)}
if (dodSh %in% 1:3){
  RegData <- if (dodSh %in% 1:2) {subset(RegData, RegData$StatusVedUtskriving==dodSh)
    } else {subset(RegData, RegData$StatusVedUtskriving %in% 1:2)}} #Alle utskrevne
  if (erMann %in% 0:1){
   vec <- (RegData$erMann == erMann)
   RegData <- subset(RegData, vec)}

  if (valgtEnhet != 'Alle'){RegData <- subset(RegData, RegData[,enhetsNivaa] == valgtEnhet)}

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
    if (aarsakInn %in% 1:2){paste0('Covid-19, hovedårsak? ', c('Ja','Nei')[aarsakInn])},
    if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
    if (dodSh %in% 1:3) {paste0('Tilstand ved utskriving: ', c('Levende','Død','Alle utskrevne')[as.numeric(dodSh)])},
    if (valgtEnhet != 'Alle'){paste('Valgt enhet:', valgtEnhet)}
  )


  #Enhetsutvalg:
  indEgen1 <- match(reshID, RegData$ReshId)
  if (enhetsUtvalg %in% 1:2) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[indEgen1]) } else {
      hovedgrTxt <- 'Hele landet'}


  ind <- list(Hoved=0, Rest=0)
  smltxt <- ''
  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2 (egen)
    ind$Rest <- NULL}
    if (enhetsUtvalg ==1 ) {	#Involverer egen enhet
      medSml <- 1
      ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID)
      smltxt <- 'landet forøvrig'
      ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
   }




  UtData <- list(utvalgTxt=utvalgTxt, ind=ind, medSml=medSml, #fargepalett=fargepalett, grTypeTxt=grTypeTxt,
                 smltxt=smltxt, hovedgrTxt=hovedgrTxt, RegData=RegData)


 #UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt) #ind=ind, medSml=medSml, smltxt=smltxt, hovedgrTxt=hovedgrTxt, grTypeTxt=grTypeTxt,

 return(invisible(UtData))
}
