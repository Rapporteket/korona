#' Preprosesser data fra Koronaregisteret
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#'
#' @param RegData Koronaskjema
#' @param skjema 1: innleggelse, 2: utskriving,
#'
#' @return Preprosesserte data
#'
#' @export
#'
KoronaPreprosesser <- function(RegData=RegData, aggPers=1)	#, reshID=reshID)
{
   # Endre variabelnavn:
   names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
   names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
   #Avvik ml. test og prod-data:
   names(RegData)[
      names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'
   RegData$ShNavn <- trimws(as.character(RegData$HelseenhetKortNavn)) #Fjerner mellomrom (før) og etter navn


   RegData$BMI <- ifelse(RegData$Vekt>0 & RegData$Hoyde>0,
                         RegData$Vekt/(RegData$Hoyde/100)^2,
                         NA)
#FEIL I KODEBOK LAGER KRØLL!
   # boolske_var_inklusjon <-
   #    as.character(kodebok$inklusjon$Variabelnavn)[which(as.character(kodebok$inklusjon$Felttype) == 'Avkrysning')]
   # RegData[, intersect(names(RegData), boolske_var_inklusjon)] <-
   #    apply(RegData[, intersect(names(RegData), boolske_var_inklusjon)], 2, as.logical)

   #Konvertere boolske variable fra tekst til boolske variable...
   TilLogiskeVar <- function(Skjema){
     verdiGML <- c('True','False')
     verdiNY <- c(TRUE,FALSE)
     mapping <- data.frame(verdiGML,verdiNY)
     LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
     if (length(LogVar)>0) {
       for (k in 1:length(LogVar)) {
         Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
       }}
     return(Skjema)
   }

   RegData <- TilLogiskeVar(RegData)


   #------SLÅ SAMMEN TIL PER PASIENT
if (aggPers == 1) {
   #Variabler med 1-ja, 2-nei, 3-ukjent: Prioritet: ja-nei-ukjent. Ikke utfylt får også ukjent
   JaNeiUkjVar <- function(x) {ifelse(1 %in% x, 1, ifelse(2 %in% x, 2, 3))}
# x <- RegData$AceHemmerInnkomst2
# table(x)
# test <- ifelse(2 %in% x, 2, 3)
   #Variabler med 1-nei, 2:5 ja, 999 ukjent. Velger mest alvorlige (høyeste) nivå. Ikke utfylt får også ukjent
   SviktVar <- function(x) {
      test <- x %in% 1:5
      ifelse(sum(test)>0, max(x[test]), 999)} #1-nei, 2:5 ja, 999 ukjent.

   RegDataRed <- RegData %>% group_by(PasientID) %>%
      summarise(Alder = Alder[1],
                AceHemmerInnkomst = JaNeiUkjVar(AceHemmerInnkomst), #1-ja, 2-nei, 3-ukjent
                AkuttNyresvikt = JaNeiUkjVar(AkuttNyresvikt), #1-ja, 2-nei, 3-ukjent
                AkuttRespirasjonsvikt = SviktVar(AkuttRespirasjonsvikt), #1-nei, 2:5 ja, 999 ukjent
                AkuttSirkulasjonsvikt = SviktVar(AkuttSirkulasjonsvikt),  #1-nei, 2:5 ja, 999 ukjent
                Aminoglykosid = sum(Aminoglykosid)>0,
                AndreGencefalosporin = sum(AndreGencefalosporin)>0,
                Antibiotika = Antibiotika[1], #1 ja, 2-nei 3-ukjent
                AntibiotikaAnnet = sum(AntibiotikaAnnet)>0,
                ArsakInnleggelse = JaNeiUkjVar(ArsakInnleggelse), #1-ja, 2-nei, 3-ukjent
                Astma = sum(Astma)>0,
                #Bilirubin,
                BMI = sort(BMI, decreasing = T)[1],
                CurrentMunicipalNumber = first(CurrentMunicipalNumber, order_by = FormDate),
                #Ddimer,
                Diabetes = sum(Diabetes)>0,
                #DiastoliskBlodtrykk,
                DistrictCode = first(DistrictCode, order_by = FormDate),
                EndretBevissthet = JaNeiUkjVar(EndretBevissthet), #1-ja, 2-nei, 3-ukjent
                ErAnsattMikrobiologisk = JaNeiUkjVar(ErAnsattMikrobiologisk), #1-ja, 2-nei, 3-ukjent
                ErHelsepersonell = JaNeiUkjVar(ErHelsepersonell), #1-ja, 2-nei, 3-ukjent
                FormStatus = sort(FormStatus)[1], #1-kladd, 2-ferdigstilt
                Gravid = sum(Gravid)>0,
                HFut = last(HF, order_by = FormDate),
                HF = first(HF, order_by = FormDate),
                #Hjertefrekvens,
                Hjertesykdom = sum(Hjertesykdom)>0,
                Isolert = JaNeiUkjVar(Isolert), #1-ja, 2-nei, 3-ukjent
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
                Municipal = first(Municipal, order_by = FormDate),
                #MunicipalNumber,
                NedsattimmunHIV = sum(NedsattimmunHIV)>0,
                NerkontaktCovid = JaNeiUkjVar(NerkontaktCovid), #1-ja, 2-nei, 3-ukjent
                Nyresykdom = sum(Nyresykdom)>0,
                #Oksygenmetning
                Overf = JaNeiUkjVar(c(OverfortAnnetSykehusInnleggelse, OverfortAnnetSykehusUtskrivning)),
                # OverfortAnnetSykehusInnleggelse,  #1-ja, 2-nei, 3-ukjent
                # OverfortAnnetSykehusUtskrivning,  #1-ja, 2-nei, 3-ukjent
                PatientGender = PatientGender[1],
                Penicillin = sum(Penicillin)>0,
                PenicillinEnzymhemmer = sum(PenicillinEnzymhemmer)>0,
                ReiseUtenfor = JaNeiUkjVar(ReiseUtenfor), #1-ja, 2-nei, 3-ukjent
                ReshId = first(ReshId, order_by = FormDate),
                #Respirasjonsfrekvens,
                #RHF,
                RontgenThorax = RontgenThorax[1], #1-5...?
                Royker = sum(Royker)>0,
                #Sykehus"
                #SystoliskBlodtrykk,
                #Temp,
                TredjeGencefalosporin = sum(TredjeGencefalosporin)>0,
                #Trombocytter,
                UtsAkuttNyresvikt = JaNeiUkjVar(UtsAkuttNyresvikt),  #1-ja, 2-nei, 3-ukjent
                UtsAkuttRespirasjonsvikt = SviktVar(UtsAkuttRespirasjonsvikt), #1-nei, 2:5-ja, 999-ukjent
                UtsAkuttSirkulasjonsvikt = SviktVar(UtsAkuttSirkulasjonsvikt), #1-nei, 2:5-ja, 999-ukjent
                UtsAminoglykosid = sum(UtsAminoglykosid)>0,
                UtsAndreGencefalosporin = sum(UtsAndreGencefalosporin)>0,
                UtsAntibiotika = UtsAntibiotika[1], #1-ja, 2-nei, 3-ukjent
                UtsAntibiotikaAnnet = sum(UtsAntibiotikaAnnet)>0,
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
                Status30Dager = sort(Status30Dager, decreasing = T)[1], #0-levende, 1-død
                Status90Dager= sort(Status90Dager, decreasing = T)[1], #0-levende, 1-død
                ShNavnUt = last(ShNavn, order_by = FormDate),
                ShNavn = first(ShNavn, order_by = FormDate),
                FormStatusUt = ifelse(sum(is.na(FormStatusUt)) > 0, 1,
                                      as.numeric(sort(FormStatusUt)[1])), #1-kladd, 2-ferdigstilt
                Utskrivningsdato = last(Utskrivningsdato, order_by = FormDate), #, FormDateUt
                #FormDateUtLastForm = last(FormDateUt, order_by = FormDate),
                AntInnSkjema = n(),
                # Dobbeltreg= , #Overlappende liggetid >Xt på to ulike Sh
                # Overf = , #Beregn, ja nei
                # AntOverf = , #Antall overføringer
                ReinnTid = ifelse((AntInnSkjema > 1) & (FormStatusUt==2), #Tid mellom utskrivning og neste innleggelse.
                                 sort(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
                                                      FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                                      units = "hours"), decreasing = T)[1],
                                 0),
                Reinn = ifelse(ReinnTid>48, 1, 0),
                AntReinn = ifelse(Reinn==0, 0, #0-nei, 1-ja
                                  sum(difftime(sort(FormDate)[2:AntInnSkjema], #sort hopper over NA
                                               FormDateUt[order(FormDate)][1:(AntInnSkjema-1)],
                                               units = "hours") > 48, na.rm = T)),
                FormDateSiste = last(FormDate, order_by = FormDate),
                FormDateUt = last(FormDateUt, order_by = FormDate), #IKKE!!: sort(FormDateUt, decreasing = T)[1],
                FormDate = first(FormDate, order_by = FormDate), #sort(FormDate)[1])
                Liggetid = ifelse(Reinn==0, #Bare for de med utskrivingsskjema
                                  difftime(FormDateUt, FormDate, units = "days"),
                                  difftime(FormDateUt, FormDate, units = "days") - ReinnTid/24) #Får for lang tid hvis har flere enn 1 reinnleggelse
      )
  #----------------------------
   RegData <- data.frame(RegDataRed)
   RegData$InnTidspunktSiste <- as.POSIXct(RegData$FormDateSiste, tz= 'UTC',
                                           format="%Y-%m-%d %H:%M:%S" )
}
      #Kjønn
      RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 1] <- 1
      RegData$erMann[RegData$PatientGender == 2] <- 0
      RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

      # Enhetsnivånavn
      RegData$HFresh <- ReshNivaa$HFresh[match(RegData$ReshId, ReshNivaa$ShResh)]
      RegData$HFresh[RegData$ReshId==108595] <- 100091
      RegData$HF[RegData$ReshId==108595] <- 'Sykehuset Innlandet HF'
      RegData$HFresh[is.na(RegData$HFresh)] <- RegData$ReshId[is.na(RegData$HFresh)]
      #Endrer til kortnavn på HF:
      #RegData$HFny <- enc2utf8(ReshNivaa$HFnavnKort[match(RegData$HFresh, ReshNivaa$HFresh)])
      # ReshNivaa <- as.data.frame(ReshNivaa, stringsAsFactors=FALSE)
      # RegData$HF <- ReshNivaa$HFnavnKort[match(RegData$HFresh, ReshNivaa$HFresh)]
       #HFmap <- unique(ReshNivaa[order(ReshNivaa$HFresh), c('HFnavnKort', 'HFresh')])
      HFmap <- as.data.frame(cbind(
       HFresh = c("100065", "100082", "100083", "100084", "100085", "100089", "100091", "100092",
         "100093", "100100", "100132", "100133", "100170", "100317", "100320", "101051",
         "101719", "101971", "106635", "106640", "106816", "106819", "106834", "106838",
         "106839", "107505", "110628", "700272", "4001031", "4201115", "4208278", "4216267"),
       HFnavn = c("Helgeland", "Bergen", "Stavanger", "Fonna",  "Førde",  "AHUS", "Innlandet",
                   "Østfold",  "Sunnaas", "Vestfold", "Telemark", "Sørlandet", "Haraldspl.",
                   "N-Trøndelag", "St.Olavs", "Nordland", "UNN", "Finnmark", "Lovisenb.",
                   "MEDI 3", "Olaviken", "NKS", "Haugesund", "Solli", "Voss", "Diakonhj.",
                   "Martina H.", "V. Viken", "OUS", "Møre og Romsdal", "LHL", "Betanien")))
       RegData$HFkort <- as.character(HFmap$HFnavn[match(RegData$HFresh, HFmap$HFresh)])

      RegData$RHFresh <- ReshNivaa$RHFresh[match(RegData$HFresh, ReshNivaa$HFresh)]
      #Får encoding-feil hvis bruker denne:
      #RegData$RHF <- ReshNivaa$RHFnavn[match(RegData$HFresh, ReshNivaa$HFresh)]
      #RegData$RHF <- gsub('HELSE | RHF', '', RegData$RHF) #factor()
      #Kode om private
      #RegData$RHF <- as.factor(RegData$RHFresh)
      #levels(RegData$RHF) <- c('Vest','Nord','Midt', 'Sør-Øst')
      RegData$RHF <- as.character(factor(RegData$RHFresh, labels = c('Vest','Nord','Midt', 'Sør-Øst')))


      #Riktig format på datovariable:
      RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
      RegData$InnTidspunkt <- as.POSIXct(RegData$FormDate, tz= 'UTC',
                                                  format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive

      RegData$UtTidspunkt <- as.POSIXct(RegData$Utskrivningsdato, tz= 'UTC',
                                        format="%Y-%m-%d %H:%M:%S" )
      RegData$UtDato <- as.Date(RegData$FormDateUt, tz= 'UTC', format="%Y-%m-%d") #Evt. Utskrivningsdato
      RegData$FormDateUt <- as.Date(RegData$FormDateUt, tz= 'UTC', format="%Y-%m-%d")

      #Beregnede variabler
      #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      #!! MÅ TA HENSYN TIL REINNLEGGELSE
      #indUReinn <- RegData$Reinn==0
      RegData$LiggetidTot <- as.numeric(difftime(RegData$UtTidspunkt,
                                              RegData$InnTidspunkt,
                                              units = 'days'))


#Fjerne feilregisteringer
      RegData <- RegData[which((RegData$InnDato>'2020-03-07') & (RegData$InnDato <= Sys.Date())),]

      # Nye tidsvariable:
      # RegData$InnTidspunkt <- as.POSIXct(RegData$FormDate, tz= 'UTC',
      #                                    format="%Y-%m-%d %H:%M:%S" )
      #RegData$MndNum <- RegData$InnTidspunkt$mon +1
      RegData$MndNum <- as.numeric(format(RegData$InnTidspunkt, '%m'))
      RegData$MndAar <- format(RegData$InnTidspunkt, '%b%y')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- format(RegData$InnDato, '%Y')
      RegData$UkeNr <- format(RegData$InnDato, '%V')
      #RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
      #RegData$UkeAar <- as.factor(RegData$UkeAar)
      #RegData$Dag <- format(RegData$InnDato, '%d.%B')
      RegData$Dag <- factor(format(RegData$InnDato, '%d.%B'),
                            levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by="day"), '%d.%B'))
      RegData$InnDag <- RegData$InnDato

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

