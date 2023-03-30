

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
hentBeredDataFHI <- function(personIDvar='PersonIdBC19Hash', raa=1, Pagg=1){

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
                               raa=1, Pagg=1){

  UtData <- NULL
  if (pand==1) {
    dataPandemi <- korona::hentPandemiDataFHI(personIDvar=personIDvar, raa=raa, Pagg=Pagg)
    UtData <- append(UtData,
                     dataPandemi)
                     }
  if (bered==1) {
    dataBered <- korona::hentBeredDataFHI(personIDvar=personIDvar, raa=raa, Pagg=Pagg)
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


