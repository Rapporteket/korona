#Variabler til FHI

#Pandemiregister
library(korona)
KoroDataInn <- read.table('A:/Pandemi/InklusjonSkjemaDataContract2020-04-29.csv', sep=';',
                          stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
KoroDataInn <- KoroDataInn[ ,-which(names(KoroDataInn)=='Utskrivningsdato')]
KoroDataUt <- read.table('A:/Pandemi/UtskrivningSkjemaDataContract2020-04-29.csv', sep=';',
                         stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
names(KoroDataUt)[names(KoroDataUt) == "HelseenhetKortNavn"] <- "ShNavnUt"
varUt <- c("Antifungalbehandling", "AntiviralBehandling" , "HovedskjemaGUID", 'ShNavnUt',
           'FormStatus', 'FormDate', "OverfortAnnetSykehusUtskrivning", "StatusVedUtskriving", 'Utskrivningsdato')
KoroDataRaa <- merge(KoroDataInn, KoroDataUt[,varUt], suffixes = c('','Ut'),
                     by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)
names(KoroDataRaa)[which(names(KoroDataRaa) == 'AceHemmerInnkomst')] <- 'AceHemmerInnkomst2'

KoroDataInt <-  read.table('I:/nir/ReadinessFormDataContract2020-04-03 16-38-35.txt', sep=';',
                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')


RegDataRaa <- KoroDataRaa #KoronaDataSQL()
PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
write.table(PandemiDataRaaFHI, file = paste0('A:/Pandemi/PandemiDataRaaFHI', Sys.Date(), '.csv'),
            fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- RegDataRaa #For manuell kjøring
RegData <- KoronaPreprosesser(RegDataRaa)
PandemiDataPpFHI <- RegData[ ,varFHIpp]
write.table(PandemiDataPpFHI, file = paste0('A:/Pandemi/PandemiDataPpFHI', Sys.Date(), '.csv'),
            fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

varBort <- c('PasientGUID', 'PatientAge', 'PatientGender',
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent', 'AceHemmerInnkomst2')
varNy <- c('PasientID', 'Alder', 'erMann', 'BMI', 'AceHemmerInnkomst')
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])

varFHIraa <- c('PasientGUID',
  'AceHemmerInnkomst2'
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

#---------Beredskap--------------


library(intensivberedskap)
BeredskapData <-read.table('A:/intensiv/ReadinessFormDataContract2020-04-29.csv', sep=';',
                           stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
#setdiff(varFHIraa, sort(names(BeredskapData)))
RegDataRaa <- BeredskapData #KoronaDataSQL()
BeredskapDataRaaFHI <- RegDataRaa[,varFHIraa]
#setdiff(varFHIraa, names(RegDataRaa))
write.table(BeredskapDataRaaFHI, file = paste0('A:/Pandemi/BeredskapDataRaaFHI', Sys.Date(), '.csv'),
            fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

RegData <- NIRPreprosessBeredsk(RegDataRaa)
#Kjørt manuelt
setdiff(varFHIpp, sort(names(RegData)))
BeredskapDataPpFHI <- RegData[ ,varFHIpp]
write.table(BeredskapDataPpFHI, file = paste0('A:/Pandemi/BeredskapDataPpFHI', Sys.Date(), '.csv'),
            fileEncoding = 'UTF-8', row.names=F, sep=';', na='')

varBort <- c('PatientInRegistryGuid', 'PatientAge', 'PatientGender', 'Diagnosis', 'DateAdmittedIntensive')
varNy <- c('PasientID', 'Alder', 'erMann', 'Bekreftet', 'liggetid')
varFHIpp <- c(varNy, varFHIraa[-which(varFHIraa %in% varBort)])

varFHIraa <- c(
'PatientInRegistryGuid'
,'PatientAge'
,'PatientGender'
,'Municipal'
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


