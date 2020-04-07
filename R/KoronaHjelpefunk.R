
#' Funksjon som produserer rapporten som skal sendes til mottager.
#'
#' @param rnwFil Navn på fila som skal kjøres. Angis uten ending (\emph{dvs uten  ".Rnw"})
#' @param filnavn dummy
#' @param datoFra dato
#' @param Rpakke hvilken R-pakke fila som lager rapporten ligger i
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#'
#' @return Full path of file produced
#' @export

henteSamlerapporterKorona <- function(filnavn, rnwFil, Rpakke='korona'
                                 #    valgtRHF = 'Alle', #rolle='LU',
                                #datoFra=Sys.Date()-180, datoTil=Sys.Date()
                                ) {
  tmpFile <- paste0('tmp',rnwFil)
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <-
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
                              valgtRHF = 'Alle', Rpakke='korona') {

  #function(baseName, reshId, registryName,author, hospitalName, type) {
  valgtRHF <- valgtRHF[[1]]
  raplog::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = "starter Abonnement: Pandemi-rapport")
  # raplog::subLogger(author = author[[1]], registryName = registryName[[1]],
  #                     reshId = reshId[[1]],
  #                     msg = "Subscription report: stent/prosedyre")
  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package=Rpakke))
  # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <-
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)
  knitr::knit2pdf(input=tmpFile)

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')
  #utfil <- file.copy(from = paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'),
  #         to = paste0(filbase, digest::digest(brukernavn),'.pdf')) #filnavn)

  raplog::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = paste("Leverer: ", utfil))
  return(utfil)
}