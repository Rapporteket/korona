#' Beregner og legger til variabel for reinnleggelse, ny innleggelse og overføring
#' NB: Basert på opphold, ikke aggregerte tall!
#'
#' Reinn: ny innleggelse, def. >= 24t, uansett enhet
#' Overf: overført, def. < 24t ikke samme enhet
#'
#' @param RegData Dataramme. RegData må inneholde Inntidspunkt, Uttidspunkt og PasientID
#' @param PasientID variabelnavn for pasient-id
#'
#' @return Dataramme som inneholder de beregnede variablene
#' @export
#'
LeggTilNyInnOverf <- function(RegData, PasientID='PasientID'){
  N <- dim(RegData)[1]
  RegData$PasientID <- RegData[ ,PasientID]
  RegDataSort <- RegData[order(RegData$PasientID, RegData$InnTidspunkt,     #Denne tar mest tid
                               RegData$UtTidspunkt), ]
  RegDataSort$OpphNr <- as.numeric(ave(RegDataSort$PasientID, RegDataSort$PasientID, FUN=seq_along))
  indPasFlereOpph <- which(RegDataSort$OpphNr>1)
  RegDataSort$TidUtInn <- NA
  RegDataSort$TidUtInn[indPasFlereOpph] <-
    difftime(as.POSIXlt(RegDataSort$InnTidspunkt[indPasFlereOpph], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             as.POSIXlt(RegDataSort$UtTidspunkt[indPasFlereOpph-1], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             units = 'hour')
  RegDataSort$SmResh <- c(FALSE, RegDataSort$ReshId[2:N] == RegDataSort$ReshId[1:N-1]) #Har sm. resh som forrige..

  RegDataSort$Reinn <- 0 #Ikke ny innleggelse
  RegDataSort$Reinn[RegDataSort$TidUtInn > 24] <- 1 #Ny innleggelse 107

  RegDataSort$Overf <- 0 #Ikke overføring
  RegDataSort$Overf[RegDataSort$TidUtInn > -2 & RegDataSort$TidUtInn < 24 ] <- 1 #Overført 85
  RegDataSort$Overf[(RegDataSort$SmResh)] <- 0 #Ikke overført hvis sm. resh
return(RegDataSort)
}


#' Funksjon som produserer rapporten som skal lastes ned av mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})
#' @param filnavn dummy
#' @param datoFra dato
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#'
#' @return Full path of file produced
#' @export

henteSamlerapporterKorona <- function(filnavn, rnwFil, Rpakke='korona', rolle='SC',
                                      valgtEnhet = 'Alle', enhetsNivaa = 'RHF',
                                      reshID = 0
                                ) {
  #Sjekker at data er relativt oppdaterte:
  RegData <- korona::KoronaDataSQL(skjema=2, koble=1)
  minAnt <- min(length(unique(RegData$SkjemaGUID)), length(unique(RegData$SkjemaGUIDut)))#min(dim(skjemaInn)[1], dim(skjemaUt)[1])
  antInnlagte <- sum(is.na(RegData$FormDateUt))
  rnwFil <- ifelse(minAnt < 25000 | antInnlagte > 1000, 'KoroFeilmld.Rnw', rnwFil)

  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  setwd(tempdir())
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(tmpFile)

  gc() #Opprydning gc-"garbage collection"
  file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), filnavn)
}

#' Funksjon som produserer rapporten som skal sendes til mottager.
#' (The actual call to this function is made through do.call and
#' has the effect of providing the parameters as class
#' \emph{list}. Verdier gis inn som listeparametre
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis MED filending (\emph{dvs "filnavn.Rnw"})
#' @param reshID Aktuell reshid
#' @param datoFra dato
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export

abonnementKorona <- function(rnwFil, brukernavn='lluring', reshID=0,
                               valgtEnhet = 'Alle',
                             enhetsNivaa = 'RHF', rolle = 'SC'){


  #Sjekker at data er relativt oppdaterte:
  RegData <- korona::KoronaDataSQL(skjema=2, koble=1)
  minAnt <- min(length(unique(RegData$SkjemaGUID)), length(unique(RegData$SkjemaGUIDut)))#min(dim(skjemaInn)[1], dim(skjemaUt)[1])
  antInnlagte <- sum(is.na(RegData$FormDateUt))
  rnwFil <- ifelse(minAnt < 26000 | antInnlagte > 1000, 'KoroFeilmld.Rnw', rnwFil)

  # rapbase::autLogger(user = brukernavn, registryName = 'Pandemi',
  #                   reshId = reshID[[1]],
  #                   msg = paste0('1)starter abonnementkjøring: Pandemi-rapport med PARAMETRE: rnwFil: ',
  #                                rnwFil, ', brukernavn: ', brukernavn,
  #              ', reshID: ', reshID, ', valgtEnhet: ', valgtEnhet,
  # ', enhetsNivaa: ', enhetsNivaa, ', rolle: ', rolle)
  # )


  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='korona'))


# gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <-
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(input=tmpFile)

  # rapbase::autLogger(user = brukernavn, registryName = 'Pandemi',
  #                   reshId = reshID[[1]],
  #                   msg ="4) Kjørt knit2pdf")

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  # rapbase::autLogger(user = brukernavn, registryName = 'Pandemi',
  #                   reshId = reshID[[1]],
  #                   msg = paste("5) Leverer abonnementsfil: ", utfil))
  return(utfil)
}


#' Funksjon som avgjør om en pasient er inneliggende på aktuell dato
#'
#' Returnerer TRUE for datoer pasienten er inneliggende
#'
#' @param datoer datoer som inneligging skal avgjøres for
#' @param regdata Dataramme som inneholder InnDato og Utdato per pasient
#'
#' @return antall inneliggende på gitte datoer
#' @export
erInneliggende <- function(datoer, regdata){
  # regnes som inneliggende på aktuell dato hvis den faller mellom inn- og utdato eller
  # er etter inndato og det ikke finnes utddato. Flere betingelser kan legges til.
  #NY: For hver dato - tell antall PID

  auxfunc <- function(x) {(x >  regdata$InnDato & x <= regdata$UtDato) | (x >  regdata$InnDato & is.na( regdata$UtDato))}
  #map_df(datoer, auxfunc)
  purrr::map_dfr(datoer, auxfunc)
}



#' Tilrettelegge tidsenhetvariabel:
#' @param RegData dataramme
#' @param tidsenhet tidsenhet: 'Mnd' (standard), 'Kvartal', 'Halvaar', 'Aar',
#'
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Mnd', sluttDato='ikkeAngitt') {



  #Lager sorteringsvariabel for tidsenhet:
  RegData$TidsEnhetSort <- switch(tidsenhet,
                                  Aar = RegData$Aar-min(RegData$Aar)+1,
                                  Mnd = RegData$MndNum - min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                  + (RegData$Aar - min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                  Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                    (RegData$Aar-min(RegData$Aar))*4,
                                  Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                    (RegData$Aar-min(RegData$Aar))*2
  )

  tidtxt <- switch(tidsenhet,
                   #Henter fullt månedsnavn og forkorter etterpå.
                   Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$InnDato), na.rm = T)), 'month'),
                                         to=max(as.Date(RegData$InnDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                   Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                   sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                   Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                   sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                   Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))

  substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
  if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[0], substrRight(tidtxt, 2))}

  RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort), labels=tidtxt)

    UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
  return(UtData)
}

