# Uttrekk Korona-kommisjon
#
# Vi har i dag mottatt en ny henvendelse fra Koronakommisjonen.
# Her er bestillingen:
#
# •	Antall barn 0-18 år innlagt i sykehus (inkludert intensiv)
# •	Antall barn 0-18 år innlagt på intensiv
# •	Antall barn som har vært respiratorbehandlet (‘InvasivVentilation’)
# •	Antall barn døde på sykehus
# •	Antall barn døde på intensiv
#
# 	Studieperiode: mars 2020 til – d.d.
# Eirik: mtp intensivpasienter, skal vi si cut-off er 31.12.2021? Da er i hvert fall alle opphold ferdigstilt.
# 	De ønsket at vi delte inn dette i aldersgrupper, gruppering kunne vi velge selv. Kan 5-års grupper være ok? 0-5, 6-10, 11-15, 16-18? Andre forslag mottas med takk.
# 	Datautlevering er ønsket som en normal excel-fil. Det skal ikke produseres figurer, men kanskje de laget en tabell. CSV var ikke nødvendig.

# Bonus: Barn med innleggelsesårsak/diagnosekode (‘diagnosis’) 107, ‘Mistenkt SARS-CoV-2 med annen organmanifestasjon’ (barn med MISC (Multiorgan inflammatorisk syndrom assosiert med covid-19 (MIS-C)). Kan du sortere mellom bekreftet SARS-CoV-2 (verdi 100) og 107?


# les inn data ------------------------------------------------------------

library(tidyverse)
library(lubridate)

# År for innhenting
dagens_aar = year(Sys.Date())

# Grunnmappe
grunnmappe_fodselsnummer = "\\\\ihelse.net\\Nøkler\\Kvalitetsregister\\HBE\\2009-7537_Nøkkel\\Datadumpar\\DataDump\\Fodselsnummer\\"

# Adresse
mappe_dd_fodselsnummer = paste0(grunnmappe_fodselsnummer, dagens_aar, "\\")

# hente siste dato
dato = dir(mappe_dd_fodselsnummer, pattern = "^[0-9]{4}-[0-1][0-9]-[0-9]{2}$", full.names = FALSE) %>%
  sort() %>%
  last()

dato = as_date(dato)

mappe_fods = paste0(mappe_dd_fodselsnummer, dato, "\\")

# Datainnlesing Pandemidata -----------------------------------------------------------

# Datadump:
pandemiskjema = paste0("DataDump_MRS-PROD_Pandemiskjema_", dato, ".csv")
filnavn_pandemi = paste0(mappe_fods, pandemiskjema)

d_pandemi = read_csv2(
  file = filnavn_pandemi,
  col_names = TRUE,
  locale = locale(decimal_mark = ",", grouping_mark = ""),
  trim_ws = FALSE,
  col_types = cols(
    Fødselsnummer = col_character(),
    Skjematype = col_character(),
    SkjemaGUID = col_character(),
    UnitId = col_integer(),
    FormTypeId = col_integer(),
    MajorVersion = col_integer(),
    MinorVersion = col_integer(),
    FormStatus = col_integer(),
    CreationDate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    FormDate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    LastUpdate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    RHF = col_character(),
    HF = col_character(),
    Hospital = col_character(),
    HealthUnitName = col_character(),
    HealthUnitShortName = col_character(),
    HealthUnitId = col_integer(),
    MigrationInformation = col_character(),
    PatientAge = col_integer(),
    PatientGender = col_integer(),
    CurrentMunicipalNumber = col_integer(),
    MunicipalNumber = col_integer(),
    Municipal = col_character(),
    PostalCode = col_integer(),
    DistrictCode = col_integer(),
    AddressQuality = col_integer(),
    FirstTimeClosed = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    Innleggelse = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    ArsakInnleggelse = col_integer(),
    ReiseUtenfor = col_integer(),
    NerkontaktCovid = col_integer(),
    ErHelsepersonell = col_integer(),
    ErAnsattMikrobiologisk = col_integer(),
    KjentRisikofaktor = col_integer(),
    Kreft = col_logical(),
    NedsattimmunHIV = col_logical(),
    Astma = col_logical(),
    KroniskLungesykdom = col_logical(),
    Hjertesykdom = col_logical(),
    Nyresykdom = col_logical(),
    Leversykdom = col_logical(),
    KroniskNevro = col_logical(),
    Gravid = col_logical(),
    Royker = col_logical(),
    AceHemmerInnkomst = col_integer(),
    Hoyde = col_integer(),
    HoydeUkjent = col_logical(),
    Vekt = col_double(),
    VektUkjent = col_logical(),
    Respirasjonsfrekvens = col_integer(),
    RespirasjonsfrekvensUkjent = col_logical(),
    Temp = col_double(),
    TempUkjent = col_logical(),
    Oksygenmetning = col_integer(),
    OkysgenmetningUkjent = col_logical(),
    SystoliskBlodtrykk = col_integer(),
    SystoliskBlodtrykkUkjent = col_logical(),
    DiastoliskBlodtrykk = col_integer(),
    DiastoliskBlodtrykkUkjent = col_logical(),
    Hjertefrekvens = col_integer(),
    HjertefrekvensUkjent = col_logical(),
    AkuttNyresvikt = col_integer(),
    AkuttSirkulasjonsvikt = col_integer(),
    AkuttRespirasjonsvikt = col_integer(),
    EndretBevissthet = col_integer(),
    Leukocytter = col_double(),
    LeukocytterUkjent = col_logical(),
    Trombocytter = col_double(),
    TrombocytterUkjent = col_logical(),
    Kreatinin = col_integer(),
    SkreatininUkjent = col_logical(),
    Bilirubin = col_integer(),
    BilirubinUkjent = col_logical(),
    Ddimer = col_double(),
    DdimerUkjent = col_logical(),
    RontgenThorax = col_integer(),
    Antibiotika = col_integer(),
    AntibiotikaUkjent = col_logical(),
    Penicillin = col_logical(),
    PenicillinEnzymhemmer = col_logical(),
    Aminoglykosid = col_logical(),
    AndreGencefalosporin = col_logical(),
    TredjeGencefalosporin = col_logical(),
    Kinolon = col_logical(),
    Karbapenem = col_logical(),
    Makrolid = col_logical(),
    AntibiotikaAnnet = col_logical(),
    Isolert = col_integer(),
    Status30Dager = col_integer(),
    Status90Dager = col_integer(),
    OverfortAnnetSykehusInnleggelse = col_integer(),
    IkkeFerdigstillt30Dager = col_logical(),
    TimerSidenRelevantDato = col_integer(),
    RelevantDato = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    Utskrivningsdato = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    UtsAkuttNyresvikt = col_integer(),
    UtsAkuttSirkulasjonsvikt = col_integer(),
    UtsAkuttRespirasjonsvikt = col_integer(),
    UtsAntiviralBehandling = col_integer(),
    UtsAntifungalbehandling = col_integer(),
    UtsAntibiotika = col_integer(),
    UtsAntibiotikaukjent = col_logical(),
    UtsPenicillin = col_logical(),
    UtsPenicillinEnzymhemmer = col_logical(),
    UtsAminoglykosid = col_logical(),
    UtsAndreGencefalosporin = col_logical(),
    UtsTredjeGencefalosporin = col_logical(),
    UtsKinolon = col_logical(),
    UtsKarbapenem = col_logical(),
    UtsMakrolid = col_logical(),
    UtsAntibiotikaAnnet = col_logical(),
    Importert = col_logical(),
    DeathDate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    DeathDateUpdateTime = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    HovedskjemaGUID = col_character(),
    AntiviralBehandling = col_integer(),
    Antifungalbehandling = col_integer(),
    StatusVedUtskriving = col_integer(),
    OverfortAnnetSykehusUtskrivning = col_integer(),
    Bestillingstidspunkt = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    Varslingskanal = col_character(),
    Promis25 = col_integer(),
    PromisProxy25 = col_integer()
  )
)

# Fjerner kladdeskjema
d_pandemi_ferdigstilt = d_pandemi %>%
  filter(FormStatus == 2)

# Datainnlesing Intensivdata ----------------------------------------------------
intensivoppholdskjema = paste0("DataDump_MRS-PROD_Intensivopphold_", dato, ".csv")
intensiv_fods = paste0(mappe_fods, intensivoppholdskjema)


d_intensiv = read_csv2(
  file = intensiv_fods,
  col_names = TRUE,
  locale = locale(decimal_mark = ",", grouping_mark = ""),
  trim_ws = FALSE,
  col_types = cols(
    Fødselsnummer = col_character(),
    Skjematype = col_character(),
    SkjemaGUID = col_character(),
    UnitId = col_integer(),
    FormTypeId = col_integer(),
    MajorVersion = col_integer(),
    MinorVersion = col_integer(),
    FormStatus = col_integer(),
    FormDate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    LastUpdate = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    RHF = col_character(),
    HF = col_character(),
    Hospital = col_character(),
    HealthUnitName = col_character(),
    HealthUnitShortName = col_character(),
    HealthUnitId = col_integer(),
    PatientAge = col_integer(),
    PatientGender = col_integer(),
    CurrentMunicipalNumber = col_integer(),
    MunicipalNumber = col_integer(),
    Municipal = col_character(),
    PostalCode = col_integer(),
    DistrictCode = col_integer(),
    AddressQuality = col_integer(),
    MoreThan24Hours = col_integer(),
    MechanicalRespirator = col_integer(),
    DeadPatientDuring24Hours = col_integer(),
    MovedPatientToAnotherIntensivDuring24Hours = col_integer(),
    VasoactiveInfusion = col_integer(),
    Importert = col_integer(),
    DateAdmittedIntensive = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    PrimaryReasonAdmitted = col_integer(),
    PrimaryReasonAdmittedDescription = col_character(),
    SecondaryReasonAdmitted = col_integer(),
    SecondaryReasonAdmittedDescription = col_character(),
    DateDischargedIntensive = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    DaysAdmittedIntensiv = col_double(),
    AgeAdmitted = col_double(),
    Respirator = col_double(),
    NonInvasivVentilation = col_double(),
    InvasivVentilation = col_double(),
    KidneyReplacingTreatment = col_integer(),
    Kontinuerlig = col_logical(),
    KontinuerligDays = col_integer(),
    Intermitterende = col_logical(),
    IntermitterendeDays = col_integer(),
    Peritonealdialyse = col_logical(),
    PeritonealdialyseDays = col_integer(),
    Trakeostomi = col_integer(),
    Bukleie = col_integer(),
    ExtendedHemodynamicMonitoring = col_integer(),
    SpecialMeasures = col_logical(),
    TerapetiskHypotermi = col_logical(),
    EcmoEcla = col_logical(),
    Iabp = col_logical(),
    Impella = col_logical(),
    Icp = col_logical(),
    Oscillator = col_logical(),
    No = col_logical(),
    Leverdialyse = col_logical(),
    Hyperbar = col_logical(),
    Eeg = col_logical(),
    Ingen = col_logical(),
    Isolation = col_integer(),
    IsolationDaysTotal = col_integer(),
    Nems = col_integer(),
    Nas = col_integer(),
    KompHypoglykemi = col_logical(),
    KompPneumotoraks = col_logical(),
    KompLuftveisproblem = col_logical(),
    KompDekubitus = col_logical(),
    KomIngen = col_logical(),
    KompIkkeUtfylt = col_logical(),
    TypeOfAdmission = col_integer(),
    TransferredStatus = col_integer(),
    PatientTransferredFromHospital = col_integer(),
    PatientTransferredFromHospitalName = col_character(),
    PatientTransferredFromHospitalText = col_character(),
    PatientTransferredFromHospitalReason = col_integer(),
    PatientTransferredToHospital = col_integer(),
    PatientTransferredToHospitalName = col_character(),
    PatientTransferredToHospitalText = col_character(),
    PatientTransferredToHospitalReason = col_integer(),
    DischargedIntensiveStatus = col_integer(),
    DischargedHospitalStatus = col_integer(),
    FrailtyIndex = col_integer(),
    ChronicDiseases = col_integer(),
    Glasgow = col_integer(),
    Age = col_integer(),
    SystolicBloodPressure = col_integer(),
    HeartRate = col_integer(),
    Temperature = col_integer(),
    MvOrCpap = col_integer(),
    UrineOutput = col_integer(),
    SerumUreaOrBun = col_integer(),
    Leukocytes = col_integer(),
    Potassium = col_integer(),
    Sodium = col_integer(),
    Hco3 = col_integer(),
    Bilirubin = col_integer(),
    Saps2ScoreNumber = col_integer(),
    Saps2Score = col_double(),
    PIM_SystolicBloodPressure = col_integer(),
    PIM_SystolicBloodPressure_Value = col_integer(),
    PIM_PupilsReactionLight = col_integer(),
    PIM_SuppliedO2 = col_logical(),
    PIM_FiO2 = col_double(),
    PIM_PaO2 = col_double(),
    PIM_FiO2_PaO2 = col_double(),
    PIM_BaseExcessBlood = col_double(),
    PIM_MechanicalVentilation = col_integer(),
    PIM_ElectiveAdmission = col_integer(),
    PIM_MajorReasonHospitalization = col_integer(),
    PIM_WeightedDiagnosis = col_integer(),
    PIM_LowRiskDiagnosis = col_integer(),
    PIM_HighRiskDiagnosis = col_integer(),
    PIM_VeryHighRiskDiagnosis = col_integer(),
    PIM_Score = col_double(),
    PIM_Probability = col_double(),
    PIM_OxygenSaturation = col_double(),
    PIM_Lactate = col_double(),
    Sofa = col_logical(),
    SofaRespiration = col_integer(),
    SofaCirculation = col_integer(),
    SofaCns = col_integer(),
    SofaCidny = col_integer(),
    SofaLiver = col_integer(),
    SofaCoagulation = col_integer(),
    SofaScore = col_integer(),
    BrainDamage = col_integer(),
    CerebralCirculationAbolished = col_integer(),
    CerebralCirculationAbolishedReasonForNo = col_integer(),
    OrganDonationCompletedStatus = col_integer(),
    OrganDonationCompletedReasonForNoStatus = col_integer(),
    ShType = col_integer(),
    ShNavn = col_character(),
    ReshId = col_integer(),
    Morsdato = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    MorsdatoOppdatert = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    FirstTimeClosed = col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    Diagnosis = col_integer(),
    LivsLengdeSkaaring = col_integer(),
    LivssluttKomforSkaaring = col_integer(),
    PasientRelasjonAnnet = col_character()
  )
)

# Fjerner kladdeskjema
d_intensiv_ferdigstilt = d_intensiv %>%
  filter(FormStatus == 2)


# Dataprep ----------------------------------------------------------------
d_pandemi_alder = d_pandemi_ferdigstilt %>%
   dplyr::filter(PatientAge < 18)

d_pandemi_innleggelse = d_pandemi_alder %>%
   dplyr::filter(Skjematype == "Pandemiskjema") %>%
   dplyr::select(Fødselsnummer, PatientAge, Innleggelse)

d_pandemi_utskrivning = d_pandemi_alder %>%
  filter(Skjematype == "UtskrivningSkjema") %>%
   dplyr::select(Fødselsnummer, StatusVedUtskriving, Innleggelse, Utskrivningsdato)

d_intensiv_alder = d_intensiv_ferdigstilt %>%
   dplyr::filter(PatientAge < 18)

d_intensivopphold = d_intensiv_alder %>%
   dplyr::filter(Skjematype == "Intensivopphold",
         date(DateAdmittedIntensive) < "2022-01-01") %>%
   dplyr::select(Fødselsnummer, InvasivVentilation, DateAdmittedIntensive,
         DateDischargedIntensive, PatientAge, DischargedIntensiveStatus)

d_beredskap = d_intensiv_alder %>%
   dplyr::filter(Skjematype == "Beredskap",
         date(DateAdmittedIntensive) < "2022-01-01") %>%
   dplyr::select(Fødselsnummer, Diagnosis, DateAdmittedIntensive)

# Koble pandemiskjema med utskrivningsskjema
d_pandemi_samlet = d_pandemi_innleggelse %>%
  left_join(d_pandemi_utskrivning, by = c("Fødselsnummer", "Innleggelse"))

d_pandemi_forløp = d_pandemi_samlet %>%
  group_by(Fødselsnummer) %>%
  mutate(
    tid_siden_forrige = difftime(Innleggelse, lag(Utskrivningsdato), units = "hours"),
    innen_12_timer = as.numeric(tid_siden_forrige) < 12,
    radnummer = row_number(),
    overfort_nummer = cumsum(innen_12_timer & radnummer != 1L),
    sykdomsforlop = radnummer - overfort_nummer)

d_intensiv_samlet = d_intensivopphold %>%
  distinct(Fødselsnummer, DateAdmittedIntensive, .keep_all = TRUE) %>%  # Fjerner et duplikat
  left_join(d_beredskap, by = c("Fødselsnummer", "DateAdmittedIntensive")) %>%
  group_by(Fødselsnummer) %>%
  mutate(
    tid_siden_forrige = difftime(DateAdmittedIntensive, lag(DateDischargedIntensive), units = "hours"),
innen_12_timer = as.numeric(tid_siden_forrige) < 12,
radnummer = row_number(),
overfort_nummer = cumsum(innen_12_timer & radnummer != 1L),
sykdomsforlop = radnummer - overfort_nummer)

NeiJaUkjent = function(x) {
  ifelse(2 %in% x, 2L, ifelse(1 %in% x, 1L, NA_integer_))
}

NeiJaUkjentIntensiv = function(x) {
  ifelse(1 %in% x, 1L, ifelse(0 %in% x, 0L, NA_integer_))
}


# Aggregere Pandemi og intensivdata --------------------------------------
d_pandemi_aggregert = d_pandemi_forløp %>%
   dplyr::group_by(Fødselsnummer, sykdomsforlop) %>%
   dplyr::summarise(PatientAge = first(PatientAge),
            Innleggelse = first(Innleggelse),
            StatusVedUtskriving = NeiJaUkjent(StatusVedUtskriving),
            Utskrivningsdato = last(Utskrivningsdato),
            .groups = "drop") %>%
   dplyr::select(-sykdomsforlop)

d_intensiv_aggregert = d_intensiv_samlet %>%
   dplyr::group_by(Fødselsnummer, sykdomsforlop) %>%
   dplyr::summarise(InvasivVentilation = sum(InvasivVentilation, na.rm = TRUE),
            PatientAge = first(PatientAge),
            DateAdmittedIntensive = first(DateAdmittedIntensive),
            DateDischargedIntensive = last(DateDischargedIntensive),
            DischargedIntensiveStatus = NeiJaUkjentIntensiv(DischargedIntensiveStatus),
            Diagnosis = first(Diagnosis),
            .groups = "drop") %>%
   dplyr::select(-sykdomsforlop)

d_intensivdata_til_pandemi = d_pandemi_aggregert %>%
  left_join(d_intensiv_aggregert, by = "Fødselsnummer") %>%
  filter(DateAdmittedIntensive >= Innleggelse &
           DateAdmittedIntensive <= Utskrivningsdato)

d_fullt_datasett = d_pandemi_aggregert %>%
  anti_join(d_intensivdata_til_pandemi, by = c("Fødselsnummer", "Innleggelse")) %>%
  dplyr::bind_rows(d_intensivdata_til_pandemi)

d_aldersgrupper = d_fullt_datasett %>%
  dplyr::mutate(alderskategori = case_when(PatientAge >= 0 & PatientAge <= 5 ~ "0-5 år",
                                    PatientAge >= 6 & PatientAge <= 10 ~ "6-10 år",
                                    PatientAge >= 11 & PatientAge <= 15 ~ "11-15 år",
                                    PatientAge >= 16 & PatientAge <= 18 ~ "16-18 år",
                                    TRUE ~ "FEIL"),
         fått_invasiv = !is.na(InvasivVentilation) & InvasivVentilation != 0,
         aldersnummer = case_when(alderskategori == "0-5 år" ~ 1,
                                  alderskategori == "6-10 år" ~ 2,
                                  alderskategori == "11-15 år" ~ 3,
                                  alderskategori == "16-18 år" ~ 4)) %>%
  arrange(aldersnummer)

d_summer_alderskategori = d_aldersgrupper %>%
   dplyr::group_by(aldersnummer) %>%
   dplyr::summarise(`alder` = first(alderskategori),
            `antall innlagt på sykehus` = dplyr::n(),
            `antall innlagt på intensiv` = sum(!is.na(DateAdmittedIntensive)),
            `antall respiratorbehandlet (Invasiv ventilasjon)` = sum(!is.na(InvasivVentilation) & InvasivVentilation != 0),
            `antall døde på sykehus` = sum(StatusVedUtskriving == 2, na.rm = TRUE),
            `antall døde på intensiv` = sum(DischargedIntensiveStatus == 1, na.rm = TRUE),
            `antall påvist COV-19` = sum(Diagnosis %in% 100:103, na.rm = TRUE),
            `antall Mistenkt SARS-CoV-2 med annen organmanifestasjon` = sum(Diagnosis == 107, na.rm = TRUE)) %>%
   dplyr::select(-aldersnummer)


write_csv2(d_summer_alderskategori,
           file = "\\\\ihelse.net\\Kvalitetsregister\\hbe\\2009-7537\\Utlevering av data\\2022\\Koronakommisjonen\\utlevering-nipar.csv")
