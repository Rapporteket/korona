#' Henter data fra database for koronaregisteringer.
#'
#' @param skjema 1-innleggelse, 2-utskriving
#' @param koble 0-kun valgt skjema, 1 koble inn og ut-skjema
#' @return Henter dataramma RegData for Pandemiregisteret
#' @export
#'
#'
KoronaDataSQL <- function(datoFra = '2020-03-01', datoTil = Sys.Date(), skjema=1, koble=1) { #

varPandemiInn <- c('Inn.SkjemaGUID
  ,Inn.AceHemmerInnkomst
  -- AS AceHemmerInnkomst2
  -- ,Inn.AddressQuality
  -- ,Inn.AgeAdmitted
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
  ,Inn.CreationDate
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
  ,Inn.FirstTimeClosed
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
  ,Inn.Nir_beredskapsskjema_CoV2
  ,Inn.Nyresykdom
  ,Inn.Oksygenmetning
  ,Inn.OkysgenmetningUkjent
  ,Inn.OverfortAnnetSykehusInnleggelse
  ,Inn.PersonId
  ,Inn.PersonIdBC19Hash
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
--  ,Inn.UtsAkuttNyresvikt
--    ,Inn.UtsAkuttRespirasjonsvikt
--    ,Inn.UtsAkuttSirkulasjonsvikt
--    ,Inn.UtsAminoglykosid
--    ,Inn.UtsAndreGencefalosporin
--    ,Inn.UtsAntibiotika
--    ,Inn.UtsAntibiotikaAnnet
--    ,Inn.UtsAntibiotikaUkjent
--    ,Inn.UtsAntifungalbehandling
--    ,Inn.UtsAntiviralBehandling
--    ,Inn.UtsKarbapenem
--    ,Inn.UtsKinolon
  -- ,Inn.Utskrivningsdato AS UtskrivningsdatoInnSkjema
--    ,Inn.UtsMakrolid
--    ,Inn.UtsPenicillin
--    ,Inn.UtsPenicillinEnzymhemmer
--    ,Inn.UtsTredjeGencefalosporin
  ,Inn.Vekt
  ,Inn.VektUkjent
  ')

varPandemiUt <- c('SkjemaGUID
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
  ,CreationDate
  -- ,CurrentMunicipalNumber
  -- ,DistrictCode
  ,FirstTimeClosed
  ,FormDate
  ,FormStatus
  ,FormTypeId
  ,HelseenhetID
  ,Helseenhet
  ,HelseenhetKortNavn
  ,HF
  -- ,Hospital
  ,UPPER(HovedskjemaGUID) AS HovedskjemaGUID
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

varUtKoblet <- c(
'UPPER(Ut.HovedskjemaGUID) AS HovedskjemaGUID
  ,Ut.Antifungalbehandling
  ,Ut.AntiviralBehandling
    ,Ut.AkuttNyresvikt AS UtsAkuttNyresvikt
  ,Ut.AkuttRespirasjonsvikt AS UtsAkuttRespirasjonsvikt
  ,Ut.AkuttSirkulasjonsvikt AS UtsAkuttSirkulasjonsvikt
  ,Ut.Aminoglykosid AS UtsAminoglykosid
  ,Ut.AndreGencefalosporin AS UtsAndreGencefalosporin
  ,Ut.Antibiotika AS UtsAntibiotika
  ,Ut.AntibiotikaAnnet AS UtsAntibiotikaAnnet
  ,Ut.AntibiotikaUkjent AS UtsAntibiotikaUkjent
  ,Ut.Antifungalbehandling AS UtsAntifungalbehandling
  ,Ut.AntiviralBehandling AS UtsAntiviralBehandling
  ,Ut.CreationDate AS CreationDateUt
  ,Ut.FirstTimeClosed AS FirstTimeClosedUt
  ,Ut.FormStatus AS FormStatusUt
  ,Ut.FormDate AS FormDateUt
  ,Ut.HelseenhetKortNavn AS ShNavnUt
  ,Ut.Importert AS ImportertUt
  ,Ut.Karbapenem AS UtsKarbapenem
  ,Ut.Kinolon AS UtsKinolon
  -- ,Ut.Utskrivningsdato AS UtskrivningsdatoInnSkjema
  ,Ut.Makrolid AS UtsMakrolid
  ,Ut.Penicillin AS UtsPenicillin
  ,Ut.PenicillinEnzymhemmer AS UtsPenicillinEnzymhemmer
  ,Ut.OverfortAnnetSykehusUtskrivning
  ,Ut.StatusVedUtskriving
  ,Ut.TredjeGencefalosporin AS UtsTredjeGencefalosporin
  ,Ut.Utskrivningsdato
  ,Ut.SkjemaGUID AS SkjemaGUIDut
  ')


if (koble==0){
        query <- paste0('SELECT ',
                       ifelse(skjema==1, varPandemiInn, varPandemiUt),
                        ' FROM ',
                        c('InklusjonSkjemaDataContract Inn', 'UtskrivningSkjemaDataContract')[skjema] #)
                      ,' WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
        #query <- 'SELECT * FROM InklusjonSkjemaDataContract'
        RegData <- rapbase::loadRegData(registryName="korona", query=query, dbType="mysql")
}

if (koble==1){
        # query <- paste0('SELECT ',
        #                 varPandemiInn,
        #                 ',',
        #                 varUtKoblet,
        #                 ' FROM InklusjonSkjemaDataContract Inn
        #                 LEFT JOIN UtskrivningSkjemaDataContract Ut
        #                 ON Inn.SkjemaGUID = Ut.HovedskjemaGUID' #)
        #                 ,' WHERE cast(Inn.FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')

  queryInn <- paste0('SELECT ',
                     varPandemiInn,
                    ' FROM InklusjonSkjemaDataContract Inn
                     WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  KoroDataInn <- rapbase::loadRegData(registryName="korona", query=queryInn, dbType="mysql")

  queryUt <- paste0('SELECT ',
                    varUtKoblet,
                     ' FROM UtskrivningSkjemaDataContract Ut')
                     #WHERE cast(FormDate as date) BETWEEN \'', datoFra, '\' AND \'', datoTil, '\'')
  KoroDataUt <- rapbase::loadRegData(registryName="korona", query=queryUt, dbType="mysql")

  RegData <- merge(KoroDataInn, KoroDataUt, #suffixes = c('','Ut'),
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T, all.y=F)

        }

#query <- 'select * from UtskrivningSkjemaDataContract'
#query <- 'select * from InklusjonSkjemaDataContract'
#RegData <- rapbase::loadRegData(registryName="korona", query=query, dbType="mysql")

return(RegData)
}
