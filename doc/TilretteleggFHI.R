#Variabler til FHI

#Pandemiregister
library(korona)
# KoroDataInn <- read.table('A:/Pandemi/InklusjonSkjemaDataContract2020-04-29.csv', sep=';',
#                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# KoroDataInn <- KoroDataInn[ ,-which(names(KoroDataInn)=='Utskrivningsdato')]
# KoroDataUt <- read.table('A:/Pandemi/UtskrivningSkjemaDataContract2020-04-29.csv', sep=';',
#                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
# names(KoroDataUt)[names(KoroDataUt) == "HelseenhetKortNavn"] <- "ShNavnUt"
# varUt <- c("Antifungalbehandling", "AntiviralBehandling" , "HovedskjemaGUID", 'ShNavnUt',
#            'FormStatus', 'FormDate', "OverfortAnnetSykehusUtskrivning", "StatusVedUtskriving", 'Utskrivningsdato')
# KoroDataRaa <- merge(KoroDataInn, KoroDataUt[,varUt], suffixes = c('','Ut'),
#                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)

#Rådata
RegDataRaa <- KoronaDataSQL() #KoroDataRaa
varFHIraa <- c('PasientGUID',
               'AceHemmerInnkomst'
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
               ,'Status30Dager'
               ,'UtsAntibiotika'
               ,'UtsAntifungalbehandling'
               ,'UtsAntiviralBehandling'
               ,'Utskrivningsdato')


PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]
##setdiff(varFHIraa, names(RegDataRaa))
# write.table(PandemiDataRaaFHI, file = paste0('A:/Pandemi/PandemiDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

#Preprossesserte data
RegData <- KoronaPreprosesser(RegDataRaa)
varBort <- c('PasientGUID', 'PatientAge', 'PatientGender',
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent')
varNy <- c('PasientID', 'Alder', 'erMann', 'BMI', 'Reinn', 'FormDateSiste', 'Liggetid')
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])
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
  'PatientInRegistryGuid'
  ,'PatientAge'
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
  ,'DischargedIntensivStatus'
  ,'FormStatus'
  ,'FormDate')
BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
##setdiff(varFHIraa, names(RegDataRaa))
# write.table(BeredskapDataRaaFHI, file = paste0('A:/Pandemi/BeredskapDataRaaFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- NIRPreprosessBeredsk(RegData=RegDataRaa)
#setdiff(varFHIpp, sort(names(RegData)))
varBort <- c('PatientInRegistryGuid', 'PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive')
varNy <- c('PasientID', 'Alder', 'erMann', 'Bekreftet', 'Liggetid', 'ReinnResp', 'RespTid')
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])
BeredskapDataPpFHI <- RegData[ ,varFHIpp[8]]
# write.table(BeredskapDataPpFHI, file = paste0('A:/Pandemi/BeredskapDataPpFHI', Sys.Date(), '.csv'),
#             fileEncoding = 'UTF-8', row.names=F, sep=';', na='')




