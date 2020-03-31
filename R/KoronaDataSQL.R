#' SKAL Hente data fra database for koronaregisteringer.NB MÅ OPPDATERES
#'
#' @param skjema 1-innleggelse, 2-utskriving
#' @return Henter dataramma RegData for Intensivregisteret
#' @export
#'
#'
KoronaDataSQL <- function(skjema=1) { #datoFra = '2020-03-01', datoTil = Sys.Date()

#VARIABELLISTE MÅ OPPDATERES NÅR FÅR KORONA-DATA...
  varBeredsk <- c("UPPER(SkjemaGUID) AS SkjemaGUID
-- ,AddressQuality
,AgeAdmitted
,Astma
-- ,Birthdate
-- ,CurrentMunicipalNumber
,DateAdmittedIntensive
,DateDischargedIntensive
,DaysAdmittedIntensiv
,DeadPatientDuring24Hours
,Diabetes
,Diagnosis
,DischargedIntensivStatus
-- ,DistrictCode
,EcmoEnd
,EcmoStart
,FormDate
,FormStatus
,FormTypeId
,Graviditet
,Helseenhet
-- ,HelseenhetID
,HelseenhetKortnavn
,HF
,HovedskjemaGUID
,IsActivSmoker
,IsChronicLungDiseasePatient
,IsChronicNeurologicNeuromuscularPatient
,IsEcmoTreatmentAdministered
,IsHeartDiseaseIncludingHypertensionPatient
,IsImpairedImmuneSystemIncludingHivPatient
,IsKidneyDiseaseIncludingFailurePatient
,IsLiverDiseaseIncludingFailurePatient
,IsObesePatient
,IsRiskFactor
,Kreft
,LastUpdate
,MajorVersion
,MechanicalRespirator
,MechanicalRespiratorEnd
,MechanicalRespiratorStart
,MinorVersion
,MoreThan24Hours
-- ,Morsdato
,MorsdatoOppdatert
,MovedPatientToAnotherIntensivDuring24Hours
,Municipal
,MunicipalNumber
,PatientAge
,PatientGender
,PatientInRegistryGuid
-- ,PostalCode
,RHF
,ShNavn
-- ,SkjemaGUID
,Sykehus
,TransferredStatus
,UnitId")

varPandemiInn <- c('UPPER(SkjemaGUID) AS SkjemaGUID
  ,AceHemmerInnkomst
  -- ,AddressQuality
  ,AkuttNyresvikt
  ,AkuttRespirasjonsvikt
  ,AkuttSirkulasjonsvikt
  ,Aminoglykosid
  ,AndreGencefalosporin
  ,Antibiotika
  ,AntibiotikaAnnet
  ,AntibiotikaUkjent
  ,ArsakInnleggelse
  ,Astma
  ,Bilirubin
  ,BilirubinUkjent
  ,CurrentMunicipalNumber
  ,Ddimer
  ,DdimerUkjent
  ,Diabetes
  ,DiastoliskBlodtrykk
  ,DiastoliskBlodtrykkUkjent
  ,DistrictCode
  ,EndretBevissthet
  ,ErAnsattMikrobiologisk
  ,ErHelsepersonell
  ,FormDate
  ,FormStatus
  ,FormTypeId
  ,Gravid
  ,HealthUnitId
  ,HealthUnitName
  ,HealthUnitShortName
  ,HF
  ,Hjertefrekvens
  ,HjertefrekvensUkjent
  ,Hjertesykdom
  ,Hospital
  ,Hoyde
  ,HoydeUkjent
  ,IkkeFerdigstillt30Dager
  ,Innleggelse
  ,Isolert
  ,Karbapenem
  ,Kinolon
  ,KjentRisikofaktor
  ,Kreatinin
  ,Kreft
  ,KroniskLungesykdom
  ,KroniskNevro
  ,LastUpdate
  ,Leukocytter
  ,LeukocytterUkjent
  ,Leversykdom
  ,MajorVersion
  ,Makrolid
  ,MigrationInformation
  ,MinorVersion
  ,Municipal
  ,MunicipalNumber
  ,NedsattimmunHIV
  ,NerkontaktCovid
  ,Nyresykdom
  ,Oksygenmetning
  ,OkysgenmetningUkjent
  ,PasientGUID
  ,PatientAge
  ,PatientGender
  ,Penicillin
  ,PenicillinEnzymhemmer
  ,PostalCode
  ,ReiseUtenfor
  ,RelevantDato
  ,Respirasjonsfrekvens
  ,RespirasjonsfrekvensUkjent
  ,RHF
  ,RontgenThorax
  ,Royker
  ,Skjematype
  ,SkreatininUkjent
  ,Status30Dager
  ,Status90Dager
  ,SystoliskBlodtrykk
  ,SystoliskBlodtrykkUkjent
  ,Temp
  ,TempUkjent
  ,TimerSidenRelevantDato
  ,TredjeGencefalosporin
  ,Trombocytter
  ,TrombocytterUkjent
  ,UnitId
  ,UtsAkuttNyresvikt
  ,UtsAkuttRespirasjonsvikt
  ,UtsAkuttSirkulasjonsvikt
  ,UtsAminoglykosid
  ,UtsAndreGencefalosporin
  ,UtsAntibiotika
  ,UtsAntibiotikaAnnet
  ,UtsAntibiotikaukjent
  ,UtsAntifungalbehandling
  ,UtsAntiviralBehandling
  ,UtsKarbapenem
  ,UtsKinolon
  ,Utskrivningsdato
  ,UtsMakrolid
  ,UtsPenicillin
  ,UtsPenicillinEnzymhemmer
  ,UtsTredjeGencefalosporin
  ,Vekt
  ,VektUkjent')


varPandemiUt <- c('UPPER(SkjemaGUID) AS SkjemaGUID
  ,AddressQuality
  ,AkuttNyresvikt
  ,AkuttRespirasjonsvikt
  ,AkuttSirkulasjonsvikt
  ,Aminoglykosid
  ,AndreGencefalosporin
  ,Antibiotika
  ,AntibiotikaAnnet
  ,AntibiotikaUkjent
  ,Antifungalbehandling
  ,AntiviralBehandling
  ,CurrentMunicipalNumber
  ,DistrictCode
  ,FormDate
  ,FormStatus
  ,FormTypeId
  ,HealthUnitId
  ,HealthUnitName
  ,HealthUnitShortName
  ,HF
  ,Hospital
  ,HovedskjemaGUID
  ,IkkeFerdigstillt30Dager
  ,Karbapenem
  ,Kinolon
  ,LastUpdate
  ,MajorVersion
  ,Makrolid
  ,MigrationInformation
  ,MinorVersion
  ,Municipal
  ,MunicipalNumber
  ,PasientGUID
  ,PatientAge
  ,PatientGender
  ,Penicillin
  ,PenicillinEnzymhemmer
  ,PostalCode
  ,RelevantDato
  ,RHF
  ,SkjemaGUID
  ,Skjematype
  ,TimerSidenRelevantDato
  ,TredjeGencefalosporin
  ,UnitId
  ,Utskrivningsdato')

        query <- paste0('SELECT ',
                        varBeredsk,
                        ' FROM ReadinessFormDataContract Q')
                      #WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')


      RegData <- rapbase::LoadRegData(registryName="nir", query=query, dbType="mysql")
      return(RegData)
}
