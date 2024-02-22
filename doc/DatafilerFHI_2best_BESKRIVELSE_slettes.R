#eksisterendeVar <- varFHIraa

nyeVarFHIoverv <- c(
   'Fødselsnummer',
   'PatientAge',
   'PatientGender',
   'Municipal',
   'HF',
   'RHF',
   'DateAdmittedIntensive',
   'DateDischargedIntensive',
   'DaysAdmittedIntensiv',
   'Diagnosis',
   'IsRiskFactorKreft',
   'IsImpairedImmuneSystemIncludingHivPatient',
   'Diabetes',
   'IsHeartDiseaseIncludingHypertensionPatient',
   'IsObesePatient',
   'Astma',
   'IsChronicLungDiseasePatient',
   'IsKidneyDiseaseIncludingFailurePatient',
   'IsLiverDiseaseIncludingFailurePatient',
   'IsChronicNeurologicNeuromuscularPatient',
   'Graviditet',
   'IsActiveSmoker',
   'MechanicalRespirator',
   'MechanicalRespiratorStart',
   'MechanicalRespiratorEnd',
   'IsEcmoTreatmentAdministered',
   'EcmoStart',
   'EcmoEnd',
   "Morsdato",
   "DischargedIntensiveStatus",
   "FormStatus",
   "FormDate",
   'Kreft')

lubridate::setdiff(eksisterendeVar, nyeVarFHIoverv)
# Fjernes/endres?:
# ? "AgeAdmitted" - fjernes (PatientAge er fortsatt med)
# ? "CreationDate" - fjernes
# ? "FirstTimeClosed" - fjernes

lubridate::setdiff(nyeVarFHIoverv, eksisterendeVar)














#Variabler til FHI, uttrekk annenhver uke.
#Sender den fullstendige lista selv om mange nå ikke er obligatoriske. Så slipper vi
# å gjøre endring hvis situasjonen endrer seg.

#Fra NIR ønsker vi tilsvarende variabler som FHI allerede mottar i Beredt C19, inkludert fødselsnummer.
# Variabler fra beredskapsskjema/intensiv, avvik fra det som per nå sendes til FHI:
#    PersonIdBC19Hash: nå PersonId (annen hash, som avtalt!)
# MunicipalNumber: nå bes om Municipal
# AgeAdmitted: nå bes om PatientAge
# CreationDate og FirstTimeClosed - ikke bedt om i det nye uttrekket.
# Venter på avklaring: Ønskes samme variabler eller skal jeg holde meg til den nye lista?

#Venter avklaring: Variabellista og det meste av beskrivelsene hespeiler på opphold (ikke aggregerte) data. Betyr det at de "bare" skal ha
#rådata? Det er slik jeg tolker bestillinga.


   #Fødselsnummer hashet med Hemits? hash
beredVariabler <- c('PersonId',
'PatientAge',
'PatientGender',
'Municipal',
'HF',
'RHF',
'DateAdmittedIntensive',
'DateDischargedIntensive',
'DaysAdmittedIntensiv',
'Diagnosis',
'Kreft',
'IsImpairedImmuneSystemIncludingHivPatient',
'Diabetes',
'IsHeartDiseaseIncludingHypertensionPatient',
'IsObesePatient',
'Astma',
'IsChronicLungDiseasePatient',
'IsKidneyDiseaseIncludingFailurePatient',
'IsLiverDiseaseIncludingFailurePatient',
'IsChronicNeurologicNeuromuscularPatient',
'Graviditet',
'IsActiveSmoker',
'MechanicalRespirator',
'MechanicalRespiratorStart',
'MechanicalRespiratorEnd',
'IsEcmoTreatmentAdministered',
'EcmoStart',
'EcmoEnd',
'Morsdato',
'DischargedIntensiveStatus',
'FormStatus',
'FormDate')


#Inn-skjema:
varInn <-
c('PasientGUID',
'Skjematype',
'SkjemaGUID',
'UnitId',
'FormTypeId',
'FormVersionNumber',
'FormStatus',
'CreationDate',
'FormDate',
'LastUpdate',
'RHF',
'HF',
'Hospital',
'PatientAge',
'PatientGender',
'MunicipalNumber',
'CurrentMunicipalNumber',
'Municipal',
'FirstTimeClosed',
'Innleggelse',
'ArsakInnleggelse',
'SymptomDebut',
'SymptomDebutUkjent',
'FoerstePositivProeve',
'FoerstePositivProeveUkjent',
'KjentRisikofaktor',
'Kreft',
'NedsattimmunHIV',
'Diabetes',
'Astma',
'KroniskLungesykdom',
'Hjertesykdom',
'Nyresykdom',
'Leversykdom',
'KroniskNevro',
'Gravid',
'Royker',
'Hoyde',
'HoydeUkjent',
'Vekt',
'VektUkjent',
'AkuttNyresvikt',
'AkuttSirkulasjonsvikt',
'AkuttRespirasjonsvikt',
'Importert',
'DeathDate',
'DeathDateUpdateTime')


#Utskrivingsskjema
varUt <-
c('PasientGUID',
'Skjematype',
'SkjemaGUID',
'HovedskjemaGUID',
'UnitId',
'FormTypeId',
'FormVersionNumber',
'FormStatus',
'CreationDate',
'FormDate',
'LastUpdate',
'RHF',
'HF',
'Hospital',
'PatientAge',
'PatientGender',
'MunicipalNumber',
'CurrentMunicipalNumber',
'Municipal',
'FirstTimeClosed',
'Utskrivningsdato',
'ErFerdigBehandlet',
'AkuttNyresvikt',
'AkuttSirkulasjonsvikt',
'AkuttRespirasjonsvikt',
'SteroideBehandling',
'NyImmunmodBehandling',
'TypeImmunmodBehandling',
'AntiviralBehandling',
'TypeAntiviralBehandling',
'Monoklonaleantistoff',
'TypeMonoklonaleantistoff',
'ImmunmodBehandling',
'StatusVedUtskriving',
'Importert',
'IkkeFerdigstillt30Dager')



























