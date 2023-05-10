#Samling av funksjoner som tilrettelegger datafiler, samt funksjon for oversendelse

#' Henter data og velger variabler for overføring til FHI
#'
#' @return #Influensadata tilrettelagt for FHI
#' @export
#'
lagInfluDataFHI <- function(personIDvar='PersonIdBC19Hash'){
#Rådata
library(intensivberedskap) #  library(tidyverse) #


 influVar <- paste0(personIDvar,'\n' ,
                ',PatientAge
                ,PatientGender
                ,HF
                ,RHF
                ,MunicipalNumber
                ,CreationDate
                ,DateAdmittedIntensive
                ,DateDischargedIntensive
                ,DaysAdmittedIntensiv
                ,DischargedIntensiveStatus
                ,FormDate
                ,FormStatus
                ,ICD10_1
                ,IsEcmoTreatmentAdministered
                ,IsRiskFactor
                ,IsAsthmaticPatient
                ,IsCancerPatient
                ,IsChronicLungDiseasePatient
                ,IsChronicNeurologicNeuromuscularPatient
                ,IsDiabeticPatient
                ,IsHeartDiseaseIncludingHypertensionPatient
                ,IsImpairedImmuneSystemIncludingHivPatient
                ,IsKidneyDiseaseIncludingFailurePatient
                ,IsLiverDiseaseIncludingFailurePatient
                ,IsObesePatient
                ,IsPregnant
                ,MechanicalRespirator
                ,Morsdato
                ,RiskFactor')

 queryInflu <- paste0('SELECT ', influVar, ' FROM InfluensaFormDataContract')
  #queryInflu <- 'select * from InfluensaFormDataContract'
InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")
  #setdiff(names(InfluDataAlle), names(InfluDataRaa))

UtData <- InfluDataRaa

return(UtData)
}



#' Henter data og velger variabler for overføring til FHI
#'
#' @inheritParams lagDatafilerTilFHI
#' @return Pandemidata tilrettelagt for FHI
#' @export
#'
hentPandemiDataFHI <- function(personIDvar='PersonIdBC19Hash', raa=1, aggP=1 ){

RegDataRaa <- korona::KoronaDataSQL()

varFHIraa <- c(
   personIDvar
   ,'AceHemmerInnkomst'
   ,'ArsakInnleggelse'
   ,'Astma'
   ,'Diabetes'
   ,'ErAnsattMikrobiologisk'
   ,'ErHelsepersonell'
   ,'FormDate'
   ,'FormDateUt'
   ,'FormStatus'
   ,'FormStatusUt'
   ,'Gravid'
   ,'HF'
   ,'Hjertesykdom'
   ,'Hoyde'
   ,'HoydeUkjent'
   ,'Vekt'
   ,'VektUkjent'
   ,'Isolert'
   ,'KjentRisikofaktor'
   ,'Kreft'
   ,'KroniskLungesykdom'
   ,'KroniskNevro'
   ,'Leversykdom'
   ,'MunicipalNumber'
   ,'NedsattimmunHIV'
   ,'NerkontaktCovid'
   ,'Nyresykdom'
   ,'PatientAge'
   ,'PatientGender'
   ,'ReiseUtenfor'
   ,'RHF'
   ,'Royker'
   ,'StatusVedUtskriving'
   #,'Status30Dager'
   ,'UtsAntibiotika'
   ,'UtsAntifungalbehandling'
   ,'UtsAntiviralBehandling'
   ,'Utskrivningsdato'
   ,'CreationDate'
   ,'CreationDateUt'
   ,'FirstTimeClosed'
   ,'FirstTimeClosedUt'
   ,'FoerstePositivProeve')

PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]


if (aggP == 1){
   #Preprossesserte data
   RegData <- korona::KoronaPreprosesser(RegDataRaa, tellFlereForlop = 1)
   varBort <- c('PatientAge', 'PatientGender',
                'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent',
                "CreationDate", "CreationDateUt", "FirstTimeClosed", "FirstTimeClosedUt",
                'FoerstePositivProeve')
   varNy <- c('Alder', 'erMann', 'BMI', 'Reinn', 'FormDateSiste', 'Liggetid')
   varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
                 'AntInnSkjema', 'ReinnTid', 'ReinnNaar', 'ArsakInnNy')
   PandemiDataPpFHI <- RegData[ ,varFHIpp]
}

UtData <- NULL
if (raa==1){
   UtData <- append(UtData,
                    list(PandemiDataRaaFHI = PandemiDataRaaFHI))}
if (aggP==1) {
   UtData <- append(UtData,
                    list(PandemiDataPpFHI = PandemiDataPpFHI))}

return(UtData)
}



#' Henter data og velger variabler for overføring til FHI
#'
#' @inheritParams lagDatafilerTilFHI
#' @return Beredskapsdata tilrettelagt for FHI
#' @export
#'
hentBeredDataFHI <- function(personIDvar='PersonIdBC19Hash', raa=1, aggP=1){

   RegDataRaa <- intensivberedskap::NIRberedskDataSQL() #BeredskapData #

      varFHIraa <- c(
       personIDvar
      ,'PatientAge'
      ,'PatientGender'
      ,'MunicipalNumber'
      ,'HF'
      ,'RHF'
      ,'DateAdmittedIntensive'
      ,'DateDischargedIntensive'
      ,'DaysAdmittedIntensiv'
      ,'Diagnosis'
      ,'Kreft'
      ,'IsImpairedImmuneSystemIncludingHivPatient'
      ,'Diabetes'
      ,'IsHeartDiseaseIncludingHypertensionPatient'
      ,'IsObesePatient'
      ,'Astma'
      ,'IsChronicLungDiseasePatient'
      ,'IsKidneyDiseaseIncludingFailurePatient'
      ,'IsLiverDiseaseIncludingFailurePatient'
      ,'IsChronicNeurologicNeuromuscularPatient'
      ,'Graviditet'
      ,'IsActiveSmoker'
      ,'MechanicalRespirator'
      ,'MechanicalRespiratorStart'
      ,'MechanicalRespiratorEnd'
      ,'IsEcmoTreatmentAdministered'
      ,'EcmoStart'
      ,'EcmoEnd'
      ,'Morsdato'
      ,'DischargedIntensiveStatus'
      ,'FormStatus'
      ,'FormDate'
      ,'AgeAdmitted'
      ,'CreationDate'
      ,'FirstTimeClosed'
   ) #De nye variablene må enten legges til i varBort, eller FHI må varsles om at de kommer på ny plass i den aggregerte fila

      BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]


if (aggP==1) {
   RegData <- intensivberedskap:: NIRPreprosessBeredsk(RegData=RegDataRaa, tellFlereForlop = 1)
   varBort <- c('AgeAdmitted','PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive',
                'CreationDate', 'FirstTimeClosed', 'DaysAdmittedIntensiv') #'PatientInRegistryGuid',
   varNy <- c('Alder', 'erMann', 'Bekreftet', 'Liggetid', 'ReinnResp', 'RespTid') #'PersonId',
   varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
                 'FormDateSiste', 'Reinn', 'AntRegPrPas', 'ReinnTid', 'ReinnNaar',
                 'ReinnRespTid', 'ReinnRespNaar', 'MechanicalRespiratorStartSiste',
                 'AgeAdmitted')
   BeredskapDataPpFHI <- RegData[ ,varFHIpp]
   #setdiff(varFHIpp, sort(names(RegData)))
   # write.table(BeredskapDataPpFHI, file = paste0('BeredskapDataPpFHI', Sys.Date(), '.csv'),
   #             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
}
      UtData <- NULL
      if (raa==1){
      UtData <- append(UtData,
                       list(BeredskapDataRaaFHI = BeredskapDataRaaFHI))}
      if (aggP==1) {
         UtData <- append(UtData,
                          list(BeredskapDataPpFHI = BeredskapDataPpFHI))}
   return(UtData)
}


#' Henter data tilrettelagt for oversendelse til FHI
#'
#' @param personIDvar angi variabel for personid
#' 'PersonIdBC19Hash' - standard, 'PersonId' - alternativ
#' @param bered hente beredskapsdata? 1-ja (standard), 0-nei
#' @param pand hente pandemidata? 1-ja (standard), 0-nei
#' @param influ hente influensadata? 1-ja (standard), 0-nei
#' @param raa hente rådata= 1-ja (standard), 0-nei
#' @param aggP hente persondata aggregert til smitteforløp? 1-ja (standard), 0-nei
#'
#' @return datafiler samlet i ei liste
#' @export
lagDatafilerTilFHI <- function(personIDvar='PersonIdBC19Hash',
                               bered=1, pand=1, influ=1,
                               raa=1, aggP=1){

  UtData <- NULL
  if (pand==1) {
    dataPandemi <- korona::hentPandemiDataFHI(personIDvar=personIDvar, raa=raa, aggP=aggP)
    UtData <- append(UtData,
                     dataPandemi)
                     }
  if (bered==1) {
    dataBered <- korona::hentBeredDataFHI(personIDvar=personIDvar, raa=raa, aggP=aggP)
    UtData <- append(UtData,
                     dataBered)
  }
  if (influ==1) {
    InfluensaDataRaaFHI <- korona::lagInfluDataFHI(personIDvar=personIDvar)
    UtData <- append(UtData,
                     list(InfluensaDataRaaFHI=InfluensaDataRaaFHI)
    )
    }

  return(UtData)
}


#' Funksjon som henter filer som skal sendes til FHI. To filer fra intensivopphold
#' og to filer fra sykehusopphold. Dvs. Ei fil for hvert opphold og ei aggregert til
#' person, for hvert register
#'
#' @param zipFilNavn Navn på fila som skal kjøres. DataFHICovMonitor, DataFHIPanBeredInflu, Testfil
#' @param brukernavn Innlogget brukernavn
#' @param recipient Character string: brukernavn for unik definisjon av mottager. Benyttes i sship.
#' recipient er også hardkodet ut fra hvilken filpakke som er valgt, men må kunne velges for å sende testfil til valgt mottager.
#' Standard: 'nhn' Valg: 'nhn', 'nhn_covmonitor'
#' @return Filsti til fil med filsti til zip...
#' @export

sendDataFilerFHI <- function(zipFilNavn='Testfil', brukernavn = 'testperson', recipient = 'nhn'){ #

   opprKat <- setwd(tempdir())
   kat <- getwd()

   #Legger på ekstra betingelse for å sikre at ikke data sendes til feil mottager
   if (zipFilNavn == 'DataFHICovMonitor') {
      #Data til FHIs covid-overvåkning. Kun rådata,

      recipient == 'nhn_covmonitor' #For å sikre at ikke sendes feil
      Filer <- korona::lagDatafilerTilFHI(personIDvar='PersonId',
                                           bered=1, pand=1, influ=0,
                                           raa=1, aggP=0)
      for (filnr in 1:length(Filer)){
         write.table(Filer[[filnr]], file = paste0(names(Filer)[filnr], '.csv'),
                     fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
      }
      zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = paste0(names(Filer), '.csv'))
   }


   if (zipFilNavn == 'DataFHIPanBeredInflu') {
      recipient == 'nhn'
      Filer <- korona::lagDatafilerTilFHI()
      datasett <- c('PandemiDataRaaFHI', 'PandemiDataPpFHI', 'BeredskapDataRaaFHI', 'BeredskapDataPpFHI', 'InfluensaDataRaaFHI')
      for (fil in datasett){
         Fil <- Filer[[fil]]
         write.table(Fil, file = paste0(fil, '.csv'),
                     fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
         }
     zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = paste0(datasett, '.csv'))
   }

   if (zipFilNavn == 'Testfil') {

      Testfil1 <- data.frame('Test1'=1:5, 'Test2'=letters[1:5])
      Testfil2 <- data.frame('Hei' = c(pi, 3,1), 'Nei' = c(log(2), 200, 3))
      write.table(Testfil1, file = paste('Testfil1.csv'),
                  fileEncoding = 'UTF-8', row.names=F, sep=';', na='')
      write.table(Testfil2, file = paste('Testfil2.csv'),
                  fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

      rapbase::autLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
                         msg = paste0("Har lagret testfiler"))

      zip::zipr(zipfile = paste0(zipFilNavn, '.zip'), files = c('Testfil1.csv', 'Testfil2.csv'))
   }

   zipfilSti <- paste0(kat, '/', zipFilNavn, '.zip')


   #For each recipient a list of available vessels (transport methods) is defined and must include relevant credentials.
   #Functions used here rely on local configuration (sship.yml - må oppdateres av hn-ikt) to access such credentials.
   sship::sship(content=zipfilSti,
                recipient = recipient, #Character string: user name uniquely defining the recipient both in terms of the public
                #key used for securing the content and any identity control upon docking
                pubkey_holder = 'file', #Character string: the holder of the (recipient's) public key. Per nå kun github?
                vessel = 'sftp', # ut fra beskrivelsen bare ftp
                declaration = paste0("HerErJeg_hilsen_", zipFilNavn))
   # if (length(warnings()) >0 ){
   # rapbase::autLogger(author = brukernavn, registryName = 'Pandemi', reshId = 0,
   #                  msg = warnings()) #, utfil))}
   write.table(zipfilSti, file = 'zipfilSti.csv',fileEncoding = 'UTF-8')
   utfilsti <- paste0(kat, '/', 'zipfilSti.csv')

   #Fjern filer.. unntatt filstifila
   if (zipFilNavn == 'Testfil') {
      dum <- file.remove(c('Testfil1.csv', 'Testfil2.csv', 'Testfil.zip')) }
   if (zipFilNavn %in% c('DataFHIPanBeredInflu', 'DataFHICovMonitor')) {
      dum <- file.remove(paste0(zipFilNavn, '.zip'), paste0(datasett, '.csv'))
   }

   setwd(opprKat)
   return(utfilsti)
}

