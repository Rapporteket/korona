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
  indPasFlereOpph <- which(RegDataSort$OpphNr>1) #intersect(which(RegDataSort$AntOpph>1), which(RegDataSort$OpphNr>1))
  RegDataSort$TidUtInn <- NA
  RegDataSort$TidUtInn[indPasFlereOpph] <-
    difftime(as.POSIXlt(RegDataSort$InnTidspunkt[indPasFlereOpph], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             as.POSIXlt(RegDataSort$UtTidspunkt[indPasFlereOpph-1], tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
             units = 'hour')
  RegDataSort$SmResh <- c(FALSE, RegDataSort$ReshId[2:N] == RegDataSort$ReshId[1:N-1]) #Har sm. resh som forrige..
  # RegDataSort$Reinn <- 0 #Ikke reinnleggelse
  # RegDataSort$Reinn[RegDataSort$TidUtInn<12 & RegDataSort$TidUtInn >= 0] <- 1 #Reinnleggelse
  # RegDataSort$Reinn[!(RegDataSort$SmResh)] <- 0 #Bare reinnleggelse hvis samme resh

  RegDataSort$Reinn <- 0 #Ikke ny innleggelse
  RegDataSort$Reinn[RegDataSort$TidUtInn > 24 ] <- 1 #Ny innleggelse 107

  RegDataSort$Overf <- 0 #Ikke overføring
  RegDataSort$Overf[RegDataSort$TidUtInn > -2 & RegDataSort$TidUtInn < 24 ] <- 1 #Overført 85
  RegDataSort$Overf[(RegDataSort$SmResh)] <- 0 #Ikke overført hvis sm. resh
return(RegDataSort)
}

# ind <- sort(unique(c(indPasFlereOpph-1, indPasFlereOpph))) #Opphold for Pasienter med mer enn ett opp
# test <- RegDataSort[ind, c("PasientID","Reinn", "Overf", "OverfortAnnetSykehusInnleggelse", "OverfortAnnetSykehusUtskrivning",
#                    "TidUtInn", "InnTidspunkt", "UtTidspunkt", "ShNavn", "SkjemaGUID", "SkjemaGUIDut", "SkjemaGUIDBered")]


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
                               valgtEnhet = 'Alle',
                             enhetsNivaa = 'RHF', rolle = 'SC'){

  rnwFil <- rnwFil[[1]]
  brukernavn <- brukernavn[[1]]
  reshID <- reshID[[1]]
  valgtEnhet <- valgtEnhet[[1]]
  enhetsNivaa <- enhetsNivaa[[1]]
  rolle <- rolle[[1]]

  rapbase::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = paste0('1)starter abonnementkjøring: Pandemi-rapport med PARAMETRE: rnwFil: ',
                                 rnwFil, ', brukernavn: ', brukernavn,
               ', reshID: ', reshID, ', valgtEnhet: ', valgtEnhet,
  ', enhetsNivaa: ', enhetsNivaa, ', rolle: ', rolle)
  )

  rapbase::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = paste0('2)klasse:', 'reshID: ', class(reshID), ',
                                 valgtEnhet: ', class(valgtEnhet),
                                 ', enhetsNivaa: ', class(enhetsNivaa), ',
                                                          rolle: ', class(rolle))
  )

  filbase <- substr(rnwFil, 1, nchar(rnwFil)-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn), '.Rnw')
  src <- normalizePath(system.file(rnwFil, package='korona'))

  rapbase::subLogger(author = brukernavn, registryName = 'Pandemi',
                    reshId = reshID[[1]],
                    msg = "3) filbase, tmpFile, src ok")


# gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  #owd <-
  setwd(tempdir())
  dir <- getwd()
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(input=tmpFile)

  # raplog::subLogger(author = brukernavn, registryName = 'Pandemi',
  #                   reshId = reshID[[1]],
  #                   msg ="4) Kjørt knit2pdf")

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0(dir, '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  # raplog::subLogger(author = brukernavn, registryName = 'Pandemi',
  #                   reshId = reshID[[1]],
  #                   msg = paste("5) Leverer abonnementsfil: ", utfil))
  return(utfil)
}


#' Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
#' og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
#' person, for hvert register
#'
#' @param zipFilNavn Navn på fila som skal kjøres. DataFHIPanBeredInflu, Testfil
#' @param brukernavn Innlogget brukernavn
#' @return Filsti til fil med filsti til zip...
#' @export

sendDataFilerFHI <- function(zipFilNavn='Testfil', brukernavn = 'testperson'){ #

  # brukernavn <- brukernavn[[1]]
  # zipFilNavn <- zipFilNavn[[1]]

  raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                    msg = paste0("Vil lage filer for dataoverføring: ", zipFilNavn))

  #opprKat <- getwd()
  opprKat <- setwd(tempdir())
  kat <- getwd()

  #zipFilNavn <- paste0(zipFilNavn, Sys.Date())
  if (zipFilNavn == 'DataFHIPanBeredInflu') {
    Filer <- korona::lagDatafilerTilFHI()

    raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                      msg = paste0("Har hentet ekte filer for sending til FHI"))

    datasett <- c('PandemiDataRaaFHI', 'PandemiDataPpFHI', 'BeredskapDataRaaFHI', 'BeredskapDataPpFHI', 'InfluensaDataRaaFHI')
    for (fil in datasett){
      Fil <- Filer[[fil]]
      write.table(Fil, file = paste0(fil, '.csv'),
                  fileEncoding = 'UTF-8', row.names=F, sep=';', na='')}

    raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                      msg = paste0("Har lagret ekte filer for sending til FHI"))

    #utils::zip(zipfile = zipFilNavn, files = paste0(datasett, '.csv')) #'PandemiBeredskapTilFHI'

    zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = paste0(datasett, '.csv'))

  }

  if (zipFilNavn == 'Testfil') {

    Testfil1 <- data.frame('Test1'=1:5, 'Test2'=letters[1:5])
    Testfil2 <- data.frame('Hei' = c(pi, 3,1), 'Nei' = c(log(2), 200, 3))
    write.table(Testfil1, file = paste('Testfil1.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
    write.table(Testfil2, file = paste('Testfil2.csv'),
                fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

    raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                      msg = paste0("Har lagret testfiler"))
    #utils::zip(zipfile = paste0(zipFilNavn), files = c('Testfil1.csv', 'Testfil2.csv'))
    #utils::zip(zipfile = file.path(kat, zipFilNavn), files = c(file.path(kat, 'Testfil1.csv'), file.path(kat, 'Testfil2.csv')))

    zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = c('Testfil1.csv', 'Testfil2.csv'))


    #file.info(c(paste0(zipFilNavn, '.zip'), 'Testfil1.csv', 'Testfil2.csv'))['size']
    #unzip(paste0(zipFilNavn, '.zip'), list = FALSE) #list	If TRUE, list the files and extract none
  }
  zipfilSti <- paste0(kat, '/', zipFilNavn, '.zip')

  raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                    msg = paste0("Har laget zip-fil: ", zipfilSti))

  #For each recipient a list of available vessels (transport methods) is defined and must include relevant credentials.
  #Functions used here rely on local configuration (sship.yml - må oppdateres av hn-ikt) to access such credentials.
  sship::sship(content=zipfilSti,
               recipient = 'nhn', #Character string: user name uniquely defining the recipient both in terms of the public
               #key used for securing the content and any identity control upon docking
               pubkey_holder = 'file', #Character string: the holder of the (recipient's) public key. Per nå kun github?
               vessel = 'sftp', # ut fra beskrivelsen bare ftp
               declaration = paste0("HerErJeg_hilsen_", zipFilNavn))
  # test <- warnings()
  # if (length(test) >0 ){
  # raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
  #                  msg = warnings()) #, utfil))}
  raplog::subLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                    msg = paste("Har levert data til NHN/FHI ")) #, utfil))
  write.table(zipfilSti, file = 'zipfilSti.csv',fileEncoding = 'UTF-8')
  utfilsti <- paste0(kat, '/', 'zipfilSti.csv')

  #Fjern filer.. unntatt filstifila
  if (zipFilNavn == 'Testfil') {
    dum <- file.remove(c('Testfil1.csv', 'Testfil2.csv', 'Testfil.zip')) }
  if (zipFilNavn == 'DataFHIPanBeredInflu') {
    dum <- file.remove(paste0(zipFilNavn, '.zip'), paste0(datasett, '.csv'))
    }

  setwd(opprKat)

  return(utfilsti)
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
  #NY: For hver dato - tell antall PID

  auxfunc <- function(x) {(x >  regdata$InnDato & x <= regdata$UtDato) | (x >  regdata$InnDato & is.na( regdata$UtDato))}
  map_df(datoer, auxfunc)
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

