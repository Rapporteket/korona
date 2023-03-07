#' Preprosesser data fra Koronaregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#'
#' @param RegData Koronaskjema
#' @param kobleBered Koble data med beredskapsdata? 0: nei(standard), 1:ja. NB: Kobler bare til ett beredskapsskjema. Kan ha flere.
#' @param aggPers 1: aggregere til personnivå (standard), 0: ikke aggregere
#' @param tellFlereForlop 0: aggregerer til personnivå
#'             1: Identifiserer inntil 3 forløp per person
#'
#' @return Preprosesserte data
#'
#' @export
#'
KoronaPreprosesser <- function(RegData=RegData, aggPers=1, kobleBered=0, tellFlereForlop=0)	#, reshID=reshID)
{

   #------Preprosessering-------

  ReshNivaa <- korona::ReshNivaa
  if (aggPers==0 & tellFlereForlop==1){
     warning('Ugyldig kombinasjon: aggPers==0 & tellFlereForlop==1. "tellFlereForlop" endres til 0')
     tellFlereForlop <- 0
  }
  # Endre variabelnavn:
  names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
  names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
  names(RegData)[
    names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'
  RegData$ShNavn <- trimws(as.character(RegData$HelseenhetKortNavn)) #Fjerner mellomrom (før) og etter navn

  RegData$ShNavn[RegData$ReshId == 4204086] <- 'Drammen, psyk.'
  RegData$ShNavn[RegData$ReshId == 108595] <- 'Innlandet, psyk.'
  RegData$ShNavn[RegData$ReshId == 111487] <- 'Aker'
  RegData$ShNavn[RegData$ReshId == 705757] <- 'Radiumhospitalet'
  RegData$ShNavn[RegData$ReshId == 4207357] <- 'Nordagutu'
  RegData$ShNavn[RegData$ReshId == 700138] <- 'Stavern, rehab.'
  RegData$ShNavn[RegData$ReshId == 102919] <- 'Bergen, psyk.'

  #RegData$RHF[RegData$ReshId %in% c(108595, 111487, 705757)] <- 'Sør-Øst'

  # Enhetsnivå-mapping
  #Legger på HFresh
  # data("ReshNivaa")
  RegData$HFresh <- ReshNivaa$HFresh[match(RegData$ReshId, ReshNivaa$ShResh)]
  RegData$HFresh[RegData$ReshId==108595] <- 100091  #Innlandet, psyk.
  RegData$HFresh[RegData$ReshId == 700138] <- 100100 #Stavern, rehab
  RegData$HFresh[RegData$ReshId == 1085970] <- 100089 #Kongsvinger
  RegData$HFresh[RegData$ReshId == 114358] <- 100082 #Bergen
  #RegData$HF[RegData$ReshId==108595] <- 'Sykehuset Innlandet HF'
  #RegData$HF[RegData$ReshId==705757] <- 'Oslo universitetssykehus HF'
  #FEIL! Skal være: 4001031 RegData$HFresh[RegData$ReshId %in% c(111487, 705757)] <- 110628   #Aker, Radiumhospitalet



  #Endrer til kortnavn på HF:
  HFmap <- as.data.frame(cbind(
    HFresh = c("100065", "100082", "100083", "100084", "100085", "100089", "100091",
               "100092", "100093", "100100", "100132", "100133", "100170",
               "100317", "100320", "101051", "101719", "101971", "106635",
               "106640", "106816", "106819", "106834", "106838", "106839", "107505",
               "110628", "700272", "4001031", "4201115", "4208278", "4216267"),
    HFnavn = c("Helgeland", "Bergen", "Stavanger", "Fonna",  "Førde",  "AHUS", "Innlandet",
               "Østfold",  "Sunnaas", "Vestfold", "Telemark", "Sørlandet", "Haraldsplass",
               "N-Trøndelag", "St.Olavs", "Nordland", "UNN", "Finnmark", "Lovisenberg",
               "MEDI 3", "Olaviken", "NKS", "Haugesund", "Solli", "Voss", "Diakonhjemmet",
               "Martina H.", "V. Viken", "OUS", "Møre og Romsdal", "LHL", "Betanien")))

  #Registreringer gjort på HF-nivå, dvs. HFresh registrert i ReshId..:
  # 100092  Sykehuset Østfold HF - Østfold
  # 101971 Finnmarkssykehuset HF . Finnmark HF.
  # 101051 Nordlandssykehuset HF Nordland HF
  indRegHF <- which(RegData$ReshId %in% HFmap$HFresh)
  RegData$HFresh[indRegHF] <- RegData$ReshId[indRegHF]


  RegData$HFkort <- as.character(HFmap$HFnavn[match(RegData$HFresh, HFmap$HFresh)])
  RegData$HFkort[RegData$HFkort==''] <- 'Mangler HF'
  RegData$HFlang <- RegData$HF
  RegData$HF <- RegData$HFkort
  RegData$ShNavn[indRegHF] <- RegData$HFkort[indRegHF]

  RegData$RHFresh <- ReshNivaa$RHFresh[match(RegData$HFresh, ReshNivaa$HFresh)]
  RegData$RHF <- as.character(factor(RegData$RHFresh, levels=c(100021, 100022, 100024, 111919),
                                     labels = c('Vest','Nord','Midt', 'Sør-Øst')))


  #Kjønn
  RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
  RegData$erMann[RegData$PatientGender == 1] <- 1
  RegData$erMann[RegData$PatientGender == 2] <- 0
  RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

  RegData$BMI <- ifelse(RegData$Vekt>0 & RegData$Hoyde>0,
                        RegData$Vekt/(RegData$Hoyde/100)^2,
                        NA)
  #Konvertere boolske variable fra tekst til boolske variable...
  # TilLogiskeVar <- function(Skjema){
  #   verdiGML <- c('True','False')
  #   verdiNY <- c(TRUE,FALSE)
  #   mapping <- data.frame(verdiGML,verdiNY)
  #   LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
  #   if (length(LogVar)>0) {
  #     for (k in 1:length(LogVar)) {
  #       Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
  #     }}
  #   return(Skjema)
  # }
  # RegData <- TilLogiskeVar(RegData)

  LogVarSjekk <- names(RegData)[unique(which(RegData[1,] %in% c('True','False')), which(RegData[15,] %in% c('True','False')))]
  LogVar <- unique(c(LogVarSjekk,
                     "Aminoglykosid", "AndreGencefalosporin", "AntibiotikaAnnet", "AntibiotikaUkjent", "Astma",
                     "BilirubinUkjent", "DdimerUkjent",  "Diabetes", "DiastoliskBlodtrykkUkjent", "Gravid",
                     "HjertefrekvensUkjent", "Hjertesykdom", "HoydeUkjent", "Importert", "Karbapenem", "Kinolon",
                     "Kreft", "KroniskLungesykdom", "KroniskNevro", "LeukocytterUkjent", "Leversykdom", "Makrolid",
                     "NedsattimmunHIV", "Nyresykdom", "OkysgenmetningUkjent" ,"Penicillin", "PenicillinEnzymhemmer",
                     "RespirasjonsfrekvensUkjent", "Royker" ,"SkreatininUkjent", "SystoliskBlodtrykkUkjent", "TempUkjent",
                     "TredjeGencefalosporin", "TrombocytterUkjent", "UtsAminoglykosid", "UtsAndreGencefalosporin",
                     "UtsAntibiotikaAnnet", "UtsAntibiotikaUkjent", "UtsKarbapenem", "UtsKinolon", "UtsMakrolid",
                     "UtsPenicillin", "UtsPenicillinEnzymhemmer", "UtsTredjeGencefalosporin", "VektUkjent", "ImportertUt"))

  RegData[, intersect(names(RegData), LogVar)] <-
    apply(RegData[, intersect(names(RegData), LogVar)], 2, as.logical)



  RegData$UtDato <- RegData$FormDateUt #Alle som har utskrivingsskjema
  #Regner de som har ut- og innskjema opprettet samtidig og mangler utskrivingsdato, som ikke utskrevet
  #Inneliggende: Alle uten utsskrivingsdato + de med utskrivingsskjema som ikke opprettet samtidig med inn-skjema.
  indIkkeUtDato <- which(is.na(RegData$Utskrivningsdato)) #Mangler utskr.dato.
  indSmDag <- which(as.numeric(difftime(RegData$CreationDateUt, RegData$CreationDate,
                                        units = 'hours')) < 1)
  RegData$UtDato[intersect(indIkkeUtDato, indSmDag)] <- NA

  RegData$Liggetid = as.numeric(difftime(
     as.POSIXct(RegData$Utskrivningsdato, tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
     as.POSIXct(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d %H:%M:%S"),
                                         units = "days")) #Bare for utskrevne pasienter

#------SLÅ SAMMEN TIL PER PASIENT--------------
  if (aggPers == 1) {
    #Variabler med 1-ja, 2-nei, 3-ukjent: Prioritet: ja-nei-ukjent. Ikke utfylt får også ukjent
    JaNeiUkjVar <- function(x) {ifelse(1 %in% x, 1, ifelse(2 %in% x, 2, 3))}
    #Variabler med 1-nei, 2:5 ja, 999 ukjent. Velger mest alvorlige (høyeste) nivå. Ikke utfylt får også ukjent
    SviktVar <- function(x) {
      test <- x %in% 1:5
      ifelse(sum(test)>0, max(x[test]), 999)} #1-nei, 2:5 ja, 999 ukjent.


    Aarsak <- function(x, N, FormDate) {
      dplyr::case_when(
        sum(x == 1) == N ~ 1, #alle
        dplyr::last(x, order_by = FormDate) == 1  ~ 2, #siste, men ikke alle
        1 %in% x  ~ 3,      #Minst ett, ikke siste alle
        #sum(x == 1) == N ~ 1,
        sum(x == 2) == N  ~ 4, #Ingen
        (sum (x == 3) == N) | (sum(x == -1))  ~ 9 #Ukjent
      )}

    #Identifisere pasienter med flere innleggelser
    if (tellFlereForlop==1) { #Tar med flere forløp for hver pasient

      RegData$Dato <- as.Date(RegData$FormDate)
      RegData$PasientIDgml <- RegData$PasientID
      #Identifiserer inntil 5 forløp
      PasFlere <- RegData %>% dplyr::group_by(PasientIDgml) %>%
        dplyr::summarise(.groups = 'drop',
                  SkjemaGUID = SkjemaGUID,
                  InnNrDum1 = ifelse(Dato-min(Dato)>90, 2, 1),
                  InnNrDum2 = ifelse(InnNrDum1>1, ifelse(Dato - min(Dato[InnNrDum1==2])>90, 3, 2), 1),
                  InnNrDum3 = ifelse(InnNrDum2>2, ifelse(Dato - min(Dato[InnNrDum2==3])>90, 4, 3), InnNrDum2),
                  InnNr   =   ifelse(InnNrDum3>3, ifelse(Dato - min(Dato[InnNrDum3==4])>90, 5, 4), InnNrDum3),
                  PasientID = paste0(PasientID, '_', InnNr)
                  #Tid = as.numeric(Dato-min(Dato))
        )
      # Test <- RegData[which(RegData$PasientIDgml.x == PasFlere$PasientIDgml[PasFlere$InnNr==5]),
      #                 c("PasientID", 'FormDate', "ReshId")]

      RegData <- merge(RegData[ ,-which(names(RegData)=="PasientID")],
                       PasFlere[ ,c("SkjemaGUID", "InnNr", "PasientID")],
                       by='SkjemaGUID')
    }

    RegDataRed <- RegData %>% dplyr::group_by(PasientID) %>%
      dplyr::summarise(PersonId = PersonId[1],
                PersonIdBC19Hash = PersonIdBC19Hash[1],
                Alder = Alder[1],
                AceHemmerInnkomst = JaNeiUkjVar(AceHemmerInnkomst), #1-ja, 2-nei, 3-ukjent
                AkuttNyresvikt = JaNeiUkjVar(AkuttNyresvikt), #1-ja, 2-nei, 3-ukjent
                AkuttRespirasjonsvikt = SviktVar(AkuttRespirasjonsvikt), #1-nei, 2:5 ja, 999 ukjent
                AkuttSirkulasjonsvikt = SviktVar(AkuttSirkulasjonsvikt),  #1-nei, 2:5 ja, 999 ukjent
                Aminoglykosid = sum(Aminoglykosid)>0,
                AndreGencefalosporin = sum(AndreGencefalosporin)>0,
                Antibiotika = Antibiotika[1], #1 ja, 2-nei 3-ukjent
                AntibiotikaAnnet = sum(AntibiotikaAnnet)>0,
                AntibiotikaUkjent = sum(AntibiotikaUkjent)>0,
                AntInnSkjema = dplyr::n(),
                CovidJAalle = ifelse(sum(ArsakInnleggelse == 1) == AntInnSkjema, 1,0),
                CovidJaSiste = ifelse(dplyr::last(ArsakInnleggelse, order_by = FormDate) == 1, 2,0),
                CovidJaFinnes = ifelse(1 %in% ArsakInnleggelse, 3, 0),
                CovidNei = ifelse(sum(ArsakInnleggelse == 2) == AntInnSkjema, 5, 0),
                CovidUkjent = ifelse((sum (ArsakInnleggelse == 3) == AntInnSkjema) | (sum(ArsakInnleggelse == -1)), 9,0),
                ArsakInnNy = Aarsak(ArsakInnleggelse, N=AntInnSkjema, FormDate=FormDate),
                #1-ja, alle opph, 2-ja, siste opphold, men ikke alle, 3-ja, minst ett opph, men ikke siste, 4-nei, ingen opph, 9-ukj
                ArsakInnleggelse = JaNeiUkjVar(ArsakInnleggelse), #1-ja, 2-nei, 3-ukjent
                Astma = sum(Astma)>0,
                #Bilirubin,
                BMI = sort(BMI, decreasing = T)[1],
                CreationDate =  dplyr::first(CreationDate, order_by = FormDate),
                CreationDateUt =  dplyr::last(CreationDateUt, order_by = FormDateUt),
                CurrentMunicipalNumber = dplyr::first(CurrentMunicipalNumber, order_by = FormDate),
                Diabetes = sum(Diabetes)>0,
                DistrictCode = dplyr::first(DistrictCode, order_by = FormDate),
                EndretBevissthet = JaNeiUkjVar(EndretBevissthet), #1-ja, 2-nei, 3-ukjent
                ErAnsattMikrobiologisk = JaNeiUkjVar(ErAnsattMikrobiologisk), #1-ja, 2-nei, 3-ukjent
                ErHelsepersonell = JaNeiUkjVar(ErHelsepersonell), #1-ja, 2-nei, 3-ukjent
                FirstTimeClosed = dplyr::first(FirstTimeClosed, order_by = FormDate),
                FirstTimeClosedUt = dplyr::last(FirstTimeClosedUt, order_by = FormDateUt),
                FoerstePositivProeve = dplyr::first(as.Date(FoerstePositivProeve), order_by = as.Date(FoerstePositivProeve)),
                FormStatus = sort(FormStatus)[1], #1-kladd, 2-ferdigstilt
                Gravid = sum(Gravid)>0,
                HFut = dplyr::last(HF, order_by = FormDate),
                HF = dplyr::first(HF, order_by = FormDate),
                HFlang = dplyr::first(HFlang, order_by = FormDate),
                HFresh = dplyr::first(HFresh, order_by = FormDate),
                #Hjertefrekvens,
                Hjertesykdom = sum(Hjertesykdom)>0,
                Isolert = JaNeiUkjVar(Isolert), #1-ja, 2-nei, 3-ukjent
                InnNr = max(InnNr),
                Karbapenem = sum(Karbapenem)>0,
                Kinolon = sum(Kinolon),
                KjentRisikofaktor = JaNeiUkjVar(KjentRisikofaktor), #1-ja, 2-nei, 3-ukjent
                #Kreatinin,
                Kreft = sum(Kreft)>0,
                KroniskLungesykdom = sum(KroniskLungesykdom)>0,
                KroniskNevro = sum(KroniskNevro)>0,
                #Leukocytter,
                Leversykdom = sum(Leversykdom)>0,
                Makrolid = sum(Makrolid)>0,
                Municipal = dplyr::first(Municipal, order_by = FormDate),
                MunicipalNumber = dplyr::first(MunicipalNumber, order_by = FormDate),
                NedsattimmunHIV = sum(NedsattimmunHIV)>0,
                NerkontaktCovid = JaNeiUkjVar(NerkontaktCovid), #1-ja, 2-nei, 3-ukjent
                Nir_beredskapsskjema_CoV2 = JaNeiUkjVar(Nir_beredskapsskjema_CoV2),
                Nyresykdom = sum(Nyresykdom)>0,
                Overf = JaNeiUkjVar(c(OverfortAnnetSykehusInnleggelse, OverfortAnnetSykehusUtskrivning)),
                # OverfortAnnetSykehusInnleggelse,  #1-ja, 2-nei, 3-ukjent
                # OverfortAnnetSykehusUtskrivning,  #1-ja, 2-nei, 3-ukjent
                erMann = erMann[1], #PatientGender = PatientGender[1],
                Penicillin = sum(Penicillin)>0,
                PenicillinEnzymhemmer = sum(PenicillinEnzymhemmer)>0,
                ReiseUtenfor = JaNeiUkjVar(ReiseUtenfor), #1-ja, 2-nei, 3-ukjent
                ReshId = dplyr::first(ReshId, order_by = FormDate),
                RHFut = dplyr::last(RHF, order_by = FormDate),
                RHF = dplyr::first(RHF, order_by = FormDate),
                RHFresh = dplyr::first(RHFresh, order_by = FormDate),
                RontgenThorax = RontgenThorax[1], #1-5...?
                Royker = sum(Royker)>0,
                #Sykehus"
                TredjeGencefalosporin = sum(TredjeGencefalosporin)>0,
                UtsAkuttNyresvikt = JaNeiUkjVar(UtsAkuttNyresvikt),  #1-ja, 2-nei, 3-ukjent
                UtsAkuttRespirasjonsvikt = SviktVar(UtsAkuttRespirasjonsvikt), #1-nei, 2:5-ja, 999-ukjent
                UtsAkuttSirkulasjonsvikt = SviktVar(UtsAkuttSirkulasjonsvikt), #1-nei, 2:5-ja, 999-ukjent
                UtsAminoglykosid = sum(UtsAminoglykosid)>0,
                UtsAndreGencefalosporin = sum(UtsAndreGencefalosporin)>0,
                UtsAntibiotika = UtsAntibiotika[1], #1-ja, 2-nei, 3-ukjent
                UtsAntibiotikaAnnet = sum(UtsAntibiotikaAnnet)>0,
                UtsAntibiotikaUkjent = sum(UtsAntibiotikaUkjent)>0,
                #?Antifungalbehandling = Antifungalbehandling[1], #1-ja, 2-nei, 3-ukjent
                #?AntiviralBehandling"
                UtsAntifungalbehandling = JaNeiUkjVar(UtsAntifungalbehandling), #1-ja, 2-nei, 3-ukjent
                UtsAntiviralBehandling = JaNeiUkjVar(UtsAntiviralBehandling),  #1-ja, 2-nei, 3-ukjent
                UtsKarbapenem = sum(UtsKarbapenem)>0,
                UtsKinolon = sum(UtsKinolon)>0,
                UtsMakrolid = sum(UtsMakrolid)>0,
                UtsPenicillin = sum(UtsPenicillin)>0,
                UtsPenicillinEnzymhemmer = sum(UtsPenicillinEnzymhemmer)>0,
                UtsTredjeGencefalosporin = sum(UtsTredjeGencefalosporin)>0,
                #HovedskjemaGUID
                #OverfortAnnetSykehusUtskrivning #1-ja, 2-nei
                StatusVedUtskriving = sort(StatusVedUtskriving, decreasing = T)[1],  #1-levende, 2-død
                #Status30Dager = sort(Status30Dager, decreasing = T)[1], #0-levende, 1-død
                #Status90Dager= sort(Status90Dager, decreasing = T)[1], #0-levende, 1-død
                ShNavnUt = dplyr::last(ShNavn, order_by = FormDate),
                ShNavn = dplyr::first(ShNavn, order_by = FormDate),
                FormStatusUt = ifelse(sum(is.na(FormStatusUt)) > 0, 1, #Regnes som kladd hvis utskjema ikke opprettet.
                                      as.numeric(sort(FormStatusUt)[1])), #1-kladd, 2-ferdigstilt
                Utskrivningsdato = dplyr::last(Utskrivningsdato, order_by = FormDate), #, FormDateUt
                #FormDateUtLastForm = last(FormDateUt, order_by = FormDate),
                # Dobbeltreg= , #Overlappende liggetid >Xt på to ulike Sh
                # Overf = , #Beregn, ja nei
                # AntOverf = , #Antall overføringer
                ReinnTidDum = ifelse((AntInnSkjema > 1) & (FormStatusUt==2), #Tid mellom utskrivning og neste innleggelse.
                                     sort(difftime(sort(FormDate)[2:AntInnSkjema],
                                                   FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                                   units = "hours"), decreasing = T)[1],
                                     0), #Sum når større enn 24
                Reinn = ifelse(ReinnTidDum > 24 & ReinnTidDum <= 90*24, 1, 0),
                AntReinn = ifelse(Reinn==0, 0, #0-nei, 1-ja
                                  sum(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
                                               FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                               units = "hours") > 24, na.rm = T)),
                ReinnTid = ifelse(Reinn==0, 0, #Tid mellom faktiske reinnleggelser
                                  sum(sort(difftime(sort(FormDate)[2:AntInnSkjema],
                                                    FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                                    units = "hours"), decreasing = T)[1:AntReinn])),
                NyttTilfelle = ifelse(ReinnTidDum > 90*24, 1, 0),
                ReinnNaar = ifelse(Reinn==0, 0, #0-nei, 1-ja
                                   max(which(difftime(sort(FormDate)[2:AntInnSkjema],
                                                      FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                                      units = "hours") > 24))),
                UtDato =  dplyr::last(UtDato, order_by = FormDate),
                FormDateSiste = dplyr::nth(FormDate, ReinnNaar+1, order_by = FormDate),
                FormDateUt = dplyr::last(FormDateUt, order_by = FormDate), #IKKE!!: sort(FormDateUt, decreasing = T)[1],
                FormDate = dplyr::first(FormDate, order_by = FormDate), #sort(FormDate)[1])
                # LiggetidGml1 = ifelse(Reinn==0, #Bare for de med utskrivingsskjema
                #                   difftime(FormDateUt, FormDate, units = "days"),
                #                   difftime(FormDateUt, FormDate, units = "days") - ReinnTidDum/24),
                # LiggetidGml2 = ifelse(Reinn==0, #Bare for de med utskrivingsskjema
                #                       difftime(FormDateUt, FormDate, units = "days"),
                #                       difftime(FormDateUt, FormDate, units = "days") - ReinnTid/24) #Får for lang tid hvis har flere enn 1 reinnleggelse
                Liggetid = as.numeric(sum(Liggetid)) #, na.rm=T Vil ikke ha liggetid hvis forløp inneholder NA
      )
    #----------------------------
    RegData <- data.frame(RegDataRed)
    RegData$InnTidspunktSiste <- as.POSIXct(RegData$FormDateSiste, tz= 'UTC',
                                            format="%Y-%m-%d %H:%M:%S" )
  }

  #Riktig format på datovariable:
  RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
  RegData$InnTidspunkt <- as.POSIXct(RegData$FormDate, tz= 'UTC',
                                     format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive
  RegData$CreationDate <- as.POSIXct(RegData$CreationDate, tz= 'UTC',
                                     format="%Y-%m-%d %H:%M:%S" )

  RegData$CreationDateUt <- as.POSIXct(RegData$CreationDateUt, tz= 'UTC',
                                       format="%Y-%m-%d %H:%M:%S" )
  RegData$UtTidspunkt <- as.POSIXct(RegData$Utskrivningsdato, tz= 'UTC',
                                    format="%Y-%m-%d %H:%M:%S" )
  RegData$FormDateUt <- as.Date(RegData$FormDateUt, tz= 'UTC', format="%Y-%m-%d")
  #RegData$UtDato <- as.Date(RegData$Utskrivningsdato, tz= 'UTC', format="%Y-%m-%d") #Endret fra FormDateUt siden noen oppretter ut-skjema før utskriving
  RegData$UtDato <- as.Date(RegData$UtDato, tz= 'UTC', format="%Y-%m-%d")
  RegData$FirstTimeClosedUt <- as.POSIXct(RegData$FirstTimeClosedUt, tz= 'UTC',
                                          format="%Y-%m-%d %H:%M:%S" )

  #Beregnede variabler
  RegData$LiggetidTot <- as.numeric(difftime(RegData$UtTidspunkt,
                                             RegData$InnTidspunkt,
                                             units = 'days'))
  RegData$Liggetid[RegData$Liggetid<0] <- 0

  #Fjerne feilregisteringer - skal være rettet
  RegData <- RegData[which((RegData$InnDato>'2020-01-01') & (RegData$InnDato <= Sys.Date())),]

  # Nye tidsvariable:
  RegData$MndNum <- as.numeric(format(RegData$InnTidspunkt, '%m'))
  RegData$MndAar <- format(RegData$InnTidspunkt, '%b%y')
  RegData$Kvartal <- ceiling(RegData$MndNum/3)
  RegData$Halvaar <- ceiling(RegData$MndNum/6)
  RegData$Aar <- as.numeric(format(RegData$InnDato, '%Y'))
  RegData$UkeNr <- format(RegData$InnDato, '%V')
  RegData$Dag <- factor(format(RegData$InnDato, '%d.%m.%y'),
                        levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by="day"), '%d.%m.%y'))
  RegData$InnDag <- RegData$InnDato


  if (kobleBered==1){
     if (Sys.getenv("R_RAP_INSTANCE") != "") {
    BeredDataRaa <- intensivberedskap::NIRberedskDataSQL()}
    BeredData <- intensivberedskap::NIRPreprosessBeredsk(RegData=BeredDataRaa, aggPers = aggPers, tellFlereForlop = tellFlereForlop)

    if ((aggPers==1) & (tellFlereForlop == 0)){
        RegData <- merge(RegData, BeredData, all.x = T, all.y = F, suffixes = c("", "Bered"),
                         by = 'PersonId')
    } else {  if ((aggPers == 1 & tellFlereForlop==1) | aggPers == 0) {
       # personid
       # samme HF
       # inn, pandemi < inn, intensiv
       # inn, intensiv < ut, pandemi

      BeredData <- BeredData %>% dplyr::rename(PersonIdBered = PersonId,
                                               HFbered = HF,
                                               #SkjemaGUIDBered = SkjemaGUID,
                                               InnDatoBered = InnDato)


      RegDataNy <- as.data.frame(
        RegData %>%
          dplyr::group_by(PersonId, InnTidspunkt)%>% #, UtTidspunkt
          dplyr::mutate(
             vecMatchBeredTilPan=match(TRUE,
                                       PersonId == BeredData$PersonIdBered &
                                          HFlang == BeredData$HFbered &
                                          InnTidspunkt <= BeredData$FormDate &  #Lagt inn før lagt inn intensiv, DateAdmittedIntensive
                                          UtTidspunkt > BeredData$FormDate) #Ut fra pandemi etter at lagt inn intensiv (IKKE:Skrevet ut etter utskriv. int.
                       #  PersTest = sum(PersonId == BeredData$PersonIdBered, na.rm = T),
                       #  InnTest = sum(InnTidspunkt <= BeredDataRaa$DateAdmittedIntensive, na.rm = T),
                       # InnTest2 = sum(InnTidspunkt < as.POSIXct(BeredData$DateAdmittedIntensive), na.rm = T),
                       # UtTest = match(TRUE, UtTidspunkt >= as.POSIXct(BeredData$DateDischargedIntensive))
      ))


      sum(!is.na(RegDataNy$vecMatchBeredTilPan))
      fellesNavn <- which(names(BeredData) %in% names(RegData))
      BeredDataNyeNavn <- rename_with(BeredData, ~paste0(names(BeredData)[fellesNavn], 'Bered'), fellesNavn)
      RegDataMbered <- cbind(RegDataNy,
                             BeredDataNyeNavn[RegDataNy$vecMatchBeredTilPan, ])

# #Testing
      # RegDataMbered[1:3, c('PersonId', 'InnTidspunkt', "UtTidspunkt")]
      # BeredData[RegDataMber$vecMatchBeredTilPan[1:3], c('PersonIdBered', 'Innleggelsestidspunkt', "DateDischargedIntensive")]
      #
      # per2bered <- names(table(BeredData$PersonId)[table(BeredData$PersonId)>1])
      # per2pand <- names(table(RegDataMber$PersonId)[table(RegDataMber$PersonId)>1])
      #
      # RegDataMber[RegDataMber$PersonId == '0x3029E3F3B7B757E4EFB60D142FDF5911E8A3FF759B4E4C761C228E1D585D1589', c('PersonId', 'InnTidspunkt', "UtTidspunkt")]
      # BeredData[BeredData$PersonId == '0x3029E3F3B7B757E4EFB60D142FDF5911E8A3FF759B4E4C761C228E1D585D1589', c('PersonIdBered', 'Innleggelsestidspunkt', "DateDischargedIntensive")]
      #KoroData[c(3,89,345, 678, 2000), c('PersonId', 'InnTidspunkt', "UtTidspunkt", 'PersonIdBered', 'Innleggelsestidspunkt', "DateDischargedIntensive")]
    }}
  RegData  <- RegDataMbered %>% dplyr::mutate(BeredPas = ifelse(is.na(PersonIdBered), 0, 1))
} #koble bered


  ##Kode om  pasienter som er overført til/fra egen avdeling til "ikke-overført"
  #1= ikke overført, 2= overført
  #KOMMER
  # names(RegData)[which(names(RegData) == 'TransferredStatus')] <- 'Overf'
  # ind <- union(which(RegData$ReshId == RegData$PatientTransferredFromHospital),
  #              which(RegData$ReshId == RegData$PatientTransferredToHospital))
  # RegData$Overf[ind] <- 1

  #De som har Morsdato før utskriving fra intensiv:
  #ind <- which(as.Date(RegData$Morsdato) <= as.Date(RegData$DateDischargedIntensive))
  #RegData$DischargedIntensivStatus[ind] <- 1

  return(invisible(RegData))
}

