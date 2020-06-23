#' Henter data fra database for koronaregisteringer.
#'
#' @param skjema 1-innleggelse, 2-utskriving
#' @param koble 0-kun valgt skjema, 1 koble inn og ut-skjema
#' @return Henter dataramma RegData for Pandemiregisteret
#' @export
#'
#'
KoronaDataSQL <- function(skjema=1, koble=1) { #datoFra = '2020-03-01', datoTil = Sys.Date()

varPandemiInn <- c('UPPER(Inn.SkjemaGUID) AS SkjemaGUID
  ,Inn.AceHemmerInnkomst
  -- AS AceHemmerInnkomst2
  -- ,Inn.AddressQuality
  ,Inn.AkuttNyresvikt
  ,Inn.AkuttRespirasjonsvikt
  ,Inn.AkuttSirkulasjonsvikt
  ,Inn.Aminoglykosid
  ,Inn.AndreGencefalosporin
  ,Inn.Antibiotika
  ,Inn.AntibiotikaAnnet
  ,Inn.AntibiotikaUkjent
  ,Inn.ArsakInnleggelse
  ,Inn.Astma
  ,Inn.Bilirubin
  ,Inn.BilirubinUkjent
  ,Inn.CurrentMunicipalNumber
  ,Inn.Ddimer
  ,Inn.DdimerUkjent
  ,Inn.DeathDate
  ,Inn.Diabetes
  ,Inn.DiastoliskBlodtrykk
  ,Inn.DiastoliskBlodtrykkUkjent
  ,Inn.DistrictCode
  ,Inn.EndretBevissthet
  ,Inn.ErAnsattMikrobiologisk
  ,Inn.ErHelsepersonell
  ,Inn.FormDate
  ,Inn.FormStatus
  ,Inn.FormTypeId
  ,Inn.Gravid
  ,Inn.Helseenhet
  ,Inn.HelseenhetID
  ,Inn.HelseenhetKortNavn
  ,Inn.HF
  ,Inn.Hjertefrekvens
  ,Inn.HjertefrekvensUkjent
  ,Inn.Hjertesykdom
  -- ,Inn.Hospital
  ,Inn.Hoyde
  ,Inn.HoydeUkjent
  ,Inn.Importert
  ,Inn.Innleggelse
  ,Inn.Isolert
  ,Inn.Karbapenem
  ,Inn.Kinolon
  ,Inn.KjentRisikofaktor
  ,Inn.Kreatinin
  ,Inn.Kreft
  ,Inn.KroniskLungesykdom
  ,Inn.KroniskNevro
  ,Inn.LastUpdate
  ,Inn.Leukocytter
  ,Inn.LeukocytterUkjent
  ,Inn.Leversykdom
  ,Inn.MajorVersion
  ,Inn.Makrolid
  -- ,Inn.MigrationInformation
  ,Inn.MinorVersion
  ,Inn.Municipal
  ,Inn.MunicipalNumber
  ,Inn.NedsattimmunHIV
  ,Inn.NerkontaktCovid
  ,Inn.Nyresykdom
  ,Inn.Oksygenmetning
  ,Inn.OkysgenmetningUkjent
  ,Inn.OverfortAnnetSykehusInnleggelse
  ,Inn.PersonId
  ,Inn.PatientInRegistryGuid
  -- ,Inn.PasientGUID
  ,Inn.PatientAge
  ,Inn.PatientGender
  ,Inn.Penicillin
  ,Inn.PenicillinEnzymhemmer
  -- ,Inn.PostalCode
  ,Inn.ReiseUtenfor
  -- ,Inn.RelevantDato
  ,Inn.Respirasjonsfrekvens
  ,Inn.RespirasjonsfrekvensUkjent
  ,Inn.RHF
  ,Inn.RontgenThorax
  ,Inn.Royker
  ,Inn.SkreatininUkjent
  ,Inn.Sykehus
  -- ,Inn.Status30Dager #Fjernet fra MRS ved oppdatering 4.juni
  -- ,Inn.Status90Dager #Fjernet fra MRS ved oppdatering 4.juni
  ,Inn.SystoliskBlodtrykk
  ,Inn.SystoliskBlodtrykkUkjent
  ,Inn.Temp
  ,Inn.TempUkjent
  -- ,Inn.TimerSidenRelevantDato
  ,Inn.TredjeGencefalosporin
  ,Inn.Trombocytter
  ,Inn.TrombocytterUkjent
  ,Inn.UnitId
  ,Inn.UtsAkuttNyresvikt
  ,Inn.UtsAkuttRespirasjonsvikt
  ,Inn.UtsAkuttSirkulasjonsvikt
  ,Inn.UtsAminoglykosid
  ,Inn.UtsAndreGencefalosporin
  ,Inn.UtsAntibiotika
  ,Inn.UtsAntibiotikaAnnet
  ,Inn.UtsAntibiotikaUkjent
  ,Inn.UtsAntifungalbehandling
  ,Inn.UtsAntiviralBehandling
  ,Inn.UtsKarbapenem
  ,Inn.UtsKinolon
  -- ,Inn.Utskrivningsdato AS UtskrivningsdatoInnSkjema
  ,Inn.UtsMakrolid
  ,Inn.UtsPenicillin
  ,Inn.UtsPenicillinEnzymhemmer
  ,Inn.UtsTredjeGencefalosporin
  ,Inn.Vekt
  ,Inn.VektUkjent
  ')

varPandemiUt <- c('UPPER(SkjemaGUID) AS SkjemaGUID
  -- ,AddressQuality
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
  -- ,CurrentMunicipalNumber
  -- ,DistrictCode
  ,FormDate
  ,FormStatus
  ,FormTypeId
  ,HelseenhetID
  ,Helseenhet
  ,HelseenhetKortNavn
  ,HF
  -- ,Hospital
  ,HovedskjemaGUID
  -- ,IkkeFerdigstillt30Dager
  ,Karbapenem
  ,Kinolon
  ,LastUpdate
  -- ,MajorVersion
  ,Makrolid
  -- ,MigrationInformation
  ,MinorVersion
  ,Municipal
  ,MunicipalNumber
  ,OverfortAnnetSykehusUtskrivning
  ,PatientInRegistryGuid
  -- ,PasientGUID
  ,PatientAge
  ,PatientGender
  ,Penicillin
  ,PenicillinEnzymhemmer
  -- ,PostalCode
  -- ,RelevantDato
  ,RHF
  -- ,Skjematype
  ,Sykehus
  ,StatusVedUtskriving
  -- ,TimerSidenRelevantDato
  ,TredjeGencefalosporin
  ,UnitId
  ,Utskrivningsdato
  ')


alle <- '*'

varUtKoblet <- c('UPPER(Ut.HovedskjemaGUID) AS HovedskjemaGUID
  ,Ut.Antifungalbehandling
  ,Ut.AntiviralBehandling
  ,Ut.HelseenhetKortNavn AS ShNavnUt
  ,Ut.FormStatus AS FormStatusUt
  ,Ut.FormDate AS FormDateUt
  ,Ut.Importert AS ImportertUt
  ,Ut.OverfortAnnetSykehusUtskrivning
  ,Ut.StatusVedUtskriving
  ,Ut.Utskrivningsdato
  ,Ut.SkjemaGUID AS SkjemaGUIDut
  ')

if (koble==0){
        query <- paste0('SELECT ',
                       ifelse(skjema==1, varPandemiInn, varPandemiUt),
                        ' FROM ',
                        ifelse(skjema==1, 'InklusjonSkjemaDataContract Inn', 'UtskrivningSkjemaDataContract'))
                      #WHERE cast(DateAdmittedIntensive as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
}
if (koble==1){
        query <- paste0('SELECT ',
                        varPandemiInn,
                        ',',
                        varUtKoblet,
                        ' FROM InklusjonSkjemaDataContract Inn
                        LEFT JOIN UtskrivningSkjemaDataContract Ut
                        ON UPPER(Inn.SkjemaGUID) = UPPER(Ut.HovedskjemaGUID)')
        }



#query <- 'select * from UtskrivningSkjemaDataContract'

      RegData <- rapbase::LoadRegData(registryName="korona", query=query, dbType="mysql")
      return(RegData)
}
