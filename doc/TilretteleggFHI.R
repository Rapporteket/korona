#Variabler til FHI

#Pandemiregister
library(korona)

RegDataRaa <- KoronaDataSQL()
PandemiDataRaaFHI <- RegDataRaa[,varFHIraa]

RegData <- KoronaPreprosesser(RegDataRaa)
PandemiDataPpFHI <- RegData[ ,varFHIpp]

varBort <- c('PasientGUID', 'PatientAge', 'PatientGender',
             'Vekt', 'VektUkjent', 'Hoyde', 'HoydeUkjent', 'AceHemmerInnkomst2')
varNy <- c('PasientID', 'Alder', 'erMann', 'BMI', 'AceHemmerInnkomst')
varFHIpp <- c(varNy, varFHI[-which(varFHIraa %in% varBort)])

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
,'Status30Dager'
,'UtsAntibiotika'
,'UtsAntifungalbehandling'
,'UtsAntiviralBehandling'
,'Utskrivningsdato')


