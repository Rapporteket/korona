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

henteSamlerapporterKorona <- function(filnavn, rnwFil, Rpakke='korona', rolle='SC',
                                      valgtEnhet = 'Alle', enhetsNivaa = 'RHF',
                                      reshID = 0 #datoFra=Sys.Date()-180, datoTil=Sys.Date()
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
                               Rpakke='korona', valgtEnhet = 'Alle',
                             enhetsNivaa = 'RHF', rolle = 'SC'){
  #valgtRHF <- valgtRHF[[1]]
  raplog::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = "starter Abonnement: Pandemi-rapport")
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

#' Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
#' og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
#' person, for hvert register
#'
#' @param zipFilNavn Navn på fila som skal kjøres. DataFHIPanBered, Testfil
#' @param parametre Liste med valgfrie parametre, avhengig av type rapport
#' @return Full path of file produced
#' @export

sendDataFilerFHI <- function(zipFilNavn='Testfil', brukernavn = 'testperson'){ #

  raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                    msg = "starter filgenerering for dataoverføring")

  setwd(tempdir())
  dir <- getwd()

  #zipFilNavn <- paste0(zipFilNavn, Sys.Date())
  if (zipFilNavn == 'DataFHIPanBered') {
    Filer <- lagDatafilerTilFHI()
    #Følgende kan gjøres i lagDatafilerTilFHI()
    datasett <- c('PandemiDataRaaFHI', 'PandemiDataPpFHI', 'BeredskapDataRaaFHI', 'BeredskapDataPpFHI')
    for (fil in datasett){
      Fil <- Filer[[fil]]
      write.table(Fil, file = paste0(fil, '.csv'),
                  fileEncoding = 'UTF-8', row.names=F, sep=';', na='')}

    utils::zip(zipfile = zipFilNavn, files = paste0(datasett, '.csv')) #'PandemiBeredskapTilFHI'
  }

  if (zipFilNavn == 'Testfil') {

    Testfil1 <- data.frame('Test1'=1:5, 'Test2'=letters[1:5])
    Testfil2 <- data.frame('Hei' = c(pi, 3,1), 'Nei' = c(log(2), 200, 3))
    write.table(Testfil1, file = paste('Testfil1.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
    write.table(Testfil2, file = paste('Testfil2.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
    utils::zip(zipfile = zipFilNavn, files = c('Testfil1.csv', 'Testfil2.csv'))

    #file.info(c(paste0(zipFilNavn, '.zip'), 'Testfil1.csv', 'Testfil2.csv'))['size']
    #unzip(paste0(zipFilNavn, '.zip'), list = FALSE) #list	If TRUE, list the files and extract none
  }
  zipfilSti <- paste0(dir, '/', zipFilNavn, '.zip')


  #For each recipient a list of available vessels (transport methods) is defined and must include relevant credentials.
  #Functions used here rely on local configuration (sship.yml - må oppdateres av hn-ikt) to access such credentials.
  sship::sship(content=zipfilSti,
               recipient = 'nhn', #Character string: user name uniquely defining the recipient both in terms of the public
               #key used for securing the content and any identity control upon docking
               pubkey_holder = 'file', #Character string: the holder of the (recipient's) public key. Per nå kun github?
               vessel = 'sftp', # ut fra beskrivelsen bare ftp
               declaration = "HerErJeg")

  raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                    msg = paste("Leverer data til NHN/FHI ")) #, utfil))
  write.table(zipfilSti, file = 'zipfilSti.csv',fileEncoding = 'UTF-8')
  utfil <- paste0(dir, '/', 'zipfilSti.csv')
  utdata <- list('zipfilStiFil' = utfil, 'zipFilSti' = zipfilSti)
  return(utdata)
}

