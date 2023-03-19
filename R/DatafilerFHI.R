

#' Henter data og tilrettelegger filer for overføring til FHI
#'
#' @return datafiler
#' @export
#'
lagDatafilerTilFHIgml <- function(){

  #Rådata
library(korona)

  RegDataRaa <- KoronaDataSQL()
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
               ,'FirstTimeClosedUt'
               ,'FoerstePositivProeve')


PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
# write.table(PandemiDataRaaFHI, file = paste0('A:/Pandemi/PandemiDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#Preprossesserte data
RegData <- KoronaPreprosesser(RegDataRaa, tellFlereForlop = 1)
varBort <- c('PatientAge', 'PatientGender', #PasientIdXX
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent',
             "CreationDate", "CreationDateUt", "FirstTimeClosed", "FirstTimeClosedUt",
             'FoerstePositivProeve')
varNy <- c('Alder', 'erMann', 'BMI', 'Reinn', 'FormDateSiste', 'Liggetid') #'PasientID',
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)],
              'AntInnSkjema', 'ReinnTid', 'ReinnNaar', 'ArsakInnNy')
#setdiff(varFHIpp, names(RegData))
PandemiDataPpFHI <- RegData[ ,varFHIpp]

#---------Beredskap--------------

library(intensivberedskap)

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
 ,'AgeAdmitted'
 ,'CreationDate'
 ,'FirstTimeClosed'
 ) #De nye variablene må enten legges til i varBort, eller FHI må varsles om at de kommer på ny plass i den aggregerte fila
BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
# write.table(BeredskapDataRaaFHI, file = paste0('BeredskapDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- NIRPreprosessBeredsk(RegData=RegDataRaa, tellFlereForlop = 1)
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

InfluensaDataRaaFHI <- korona::lagInfluDataFHI()
PandemiDataRaaFHI = PandemiDataRaaFHI
PandemiDataPpFHI = PandemiDataPpFHI
BeredskapDataRaaFHI = BeredskapDataRaaFHI
BeredskapDataPpFHI = BeredskapDataPpFHI

UtData <- list(PandemiDataRaaFHI = PandemiDataRaaFHI, PandemiDataPpFHI = PandemiDataPpFHI,
               BeredskapDataRaaFHI = BeredskapDataRaaFHI, BeredskapDataPpFHI = BeredskapDataPpFHI,
               InfluensaDataRaaFHI = InfluensaDataRaaFHI)
return(UtData)

}
