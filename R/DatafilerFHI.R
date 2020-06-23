

#' Henter data og tilrettelegger filer for overføring til FHI
#'
#' @return
#' @export
#'
lagDatafilerTilFHI <- function(){
#Rådata
RegDataRaa <- KoronaDataSQL() #KoroDataRaa
varFHIraa <- c(
  #PasientGUID',
                'PersonId'
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
               ,'Municipal'
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
               ,'Utskrivningsdato')


PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
# write.table(PandemiDataRaaFHI, file = paste0('A:/Pandemi/PandemiDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#Preprossesserte data
RegData <- KoronaPreprosesser(RegDataRaa)
varBort <- c('PatientAge', 'PatientGender', #PasientIdXX
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent')
varNy <- c('Alder', 'erMann', 'BMI', 'Reinn', 'FormDateSiste', 'Liggetid') #'PasientID',
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])
#setdiff(varFHIpp, names(RegData))
PandemiDataPpFHI <- RegData[ ,varFHIpp]
# write.table(PandemiDataPpFHI, file = paste0('A:/Pandemi/PandemiDataPpFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#---------Beredskap--------------

library(intensivberedskap)
# BeredskapData <-read.table('A:/intensiv/ReadinessFormDataContract2020-04-29.csv', sep=';',
#                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
##setdiff(varFHIraa, sort(names(BeredskapData)))
RegDataRaa <- NIRberedskDataSQL() #BeredskapData #
varFHIraa <- c(
#  'PatientInRegistryGuid'
#   ,'PersonId',
  'PatientAge'
  ,'PatientGender'
  ,'Municipal'
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
  ,'IsActivSmoker'
  ,'MechanicalRespirator'
  ,'MechanicalRespiratorStart'
  ,'MechanicalRespiratorEnd'
  ,'IsEcmoTreatmentAdministered'
  ,'EcmoStart'
  ,'EcmoEnd'
  ,'Morsdato'
  ,'DischargedIntensiveStatus'
  ,'FormStatus'
  ,'FormDate')
BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
setdiff(varFHIraa, names(RegDataRaa))
# write.table(BeredskapDataRaaFHI, file = paste0('A:/Pandemi/BeredskapDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- NIRPreprosessBeredsk(RegData=RegDataRaa)
setdiff(varFHIpp, sort(names(RegData)))
varBort <- c('PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive', 'DaysAdmittedIntensiv') #'PatientInRegistryGuid',
varNy <- c('Alder', 'erMann', 'Bekreftet', 'Liggetid', 'ReinnResp', 'RespTid') #'PersonId',
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])
BeredskapDataPpFHI <- RegData[ ,varFHIpp]
# write.table(BeredskapDataPpFHI, file = paste0('A:/Pandemi/BeredskapDataPpFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

UtData <- list(PandemiDataRaaFHI = PandemiDataRaaFHI, PandemiDataPpFHI = PandemiDataPpFHI,
               BeredskapDataRaaFHI = BeredskapDataRaaFHI, BeredskapDataPpFHI = BeredskapDataPpFHI)
return(UtData)

}
