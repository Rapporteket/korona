

#' Henter data og tilrettelegger filer for overføring til FHI
#'
#' @return
#' @export
#'
lagDatafilerTilFHI <- function(){
#Rådata
  library(tidyverse)
RegDataRaa <- KoronaDataSQL() #KoroDataRaa
varFHIraa <- c(
  #PasientGUID',
                'PersonIdBC19Hash'
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
               ,'FirstTimeClosedUt')


PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
# write.table(PandemiDataRaaFHI, file = paste0('A:/Pandemi/PandemiDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#Preprossesserte data
RegData <- KoronaPreprosesser(RegDataRaa)
varBort <- c('PatientAge', 'PatientGender', #PasientIdXX
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent',
             "CreationDate", "CreationDateUt", "FirstTimeClosed", "FirstTimeClosedUt")
varNy <- c('Alder', 'erMann', 'BMI', 'Reinn', 'FormDateSiste', 'Liggetid') #'PasientID',
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
              'AntInnSkjema', 'ReinnTid', 'ReinnNaar')
setdiff(varFHIpp, names(RegData))
PandemiDataPpFHI <- RegData[ ,varFHIpp]
# write.table(PandemiDataPpFHI, file = paste0('A:/Pandemi/PandemiDataPpFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#---------Beredskap--------------

library(intensivberedskap)
# BeredskapData <-read.table('A:/intensiv/ReadinessFormDataContract2020-04-29.csv', sep=';',
#                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#setdiff(varFHIraa, sort(names(BeredskapData)))
RegDataRaa <- NIRberedskDataSQL() #BeredskapData #
varFHIraa <- c(
   'PersonIdBC19Hash', #  'PatientInRegistryGuid'
  'PatientAge'
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
 ,'AgeAdmitted')
BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
# write.table(BeredskapDataRaaFHI, file = paste0('A:/Pandemi/BeredskapDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- NIRPreprosessBeredsk(RegData=RegDataRaa)
varBort <- c('AgeAdmitted','PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive', 'DaysAdmittedIntensiv') #'PatientInRegistryGuid',
varNy <- c('Alder', 'erMann', 'Bekreftet', 'Liggetid', 'ReinnResp', 'RespTid') #'PersonId',
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
              'FormDateSiste', 'Reinn', 'AntRegPrPas', 'ReinnTid', 'ReinnNaar',
              'ReinnRespTid', 'ReinnRespNaar', 'MechanicalRespiratorStartSiste',
              'AgeAdmitted')
BeredskapDataPpFHI <- RegData[ ,varFHIpp]
#setdiff(varFHIpp, sort(names(RegData)))
# write.table(BeredskapDataPpFHI, file = paste0('A:/Pandemi/BeredskapDataPpFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

UtData <- list(PandemiDataRaaFHI = PandemiDataRaaFHI, PandemiDataPpFHI = PandemiDataPpFHI,
               BeredskapDataRaaFHI = BeredskapDataRaaFHI, BeredskapDataPpFHI = BeredskapDataPpFHI)
return(UtData)

}
