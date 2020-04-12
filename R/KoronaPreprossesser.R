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
KoronaPreprosesser <- function(RegData=RegData)	#, reshID=reshID)
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

   boolske_var_inklusjon <- as.character(kodebok$inklusjon$Variabelnavn)[which(as.character(kodebok$inklusjon$Felttype) == 'Avkrysning')]
   RegData[, intersect(names(RegData), boolske_var_inklusjon)] <-
      apply(RegData[, intersect(names(RegData), boolske_var_inklusjon)], 2, as.logical)


   #------SLÅ SAMMEN TIL PER PASIENT
#SJEKK ALLE SOM HAR FÅTT [1] -
#vil første innleggesle komme først, eller må det sorteres på FormDate?
   RegDataRed <- RegData %>% group_by(PasientID) %>%
      summarise(Alder = Alder[1],
                #AceHemmerInnkomst,
                AkuttNyresvikt = AkuttNyresvikt[1], #1-ja, 2-nei, 3-ukjent
                AkuttRespirasjonsvikt = AkuttRespirasjonsvikt[1], #1-nei, 2:5 ja, 999 ukjent
                AkuttSirkulasjonsvikt = AkuttSirkulasjonsvikt[1], #1-nei, 2:5 ja, 999 ukjent
                Aminoglykosid = sum(Aminoglykosid)>0,
                AndreGencefalosporin = sum(AndreGencefalosporin)>0,
                Antibiotika = Antibiotika[1], #1 ja, 2-nei 3-ukjent
                AntibiotikaAnnet = sum(AntibiotikaAnnet)>0,
                ArsakInnleggelse = ArsakInnleggelse[1], #1-ja, 2-nei, 3-ukjent
                Astma = sum(Astma)>0,
                #Bilirubin,
                BMI = mean(BMI, na.rm = T),
                CurrentMunicipalNumber = CurrentMunicipalNumber[1],
                #Ddimer,
                Diabetes = sum(Diabetes)>0,
                #DiastoliskBlodtrykk,
                DistrictCode = DistrictCode[1],
                EndretBevissthet = EndretBevissthet[1], #1-ja, 2-nei, 3-ukjent
                ErAnsattMikrobiologisk = ErAnsattMikrobiologisk[1], #1-ja, 2-nei, 3-ukjent
                ErHelsepersonell = ErHelsepersonell[1], #1-ja, 2-nei, 3-ukjent
                FormStatus = min(FormStatus, na.rm=T), #1-kladd, 2-ferdigstilt
                Gravid = sum(Gravid)>0,
                HF = HF[1],
                #Hjertefrekvens,
                Hjertesykdom = sum(Hjertesykdom)>0,
                Isolert = Isolert[1], #1-ja, 2-nei, 3-ukjent
                Karbapenem = sum(Karbapenem)>0,
                Kinolon = sum(Kinolon),
                KjentRisikofaktor = KjentRisikofaktor[1], #1-ja, 2-nei, 3-ukjent
                #Kreatinin,
                Kreft = sum(Kreft),
                KroniskLungesykdom = sum(KroniskLungesykdom),
                KroniskNevro = sum(KroniskNevro),
                #Leukocytter,
                Leversykdom = sum(Leversykdom)>0,
                Makrolid = sum(Makrolid)>0,
                #Municipal
                #MunicipalNumber,
                NedsattimmunHIV = sum(NedsattimmunHIV)>0,
                NerkontaktCovid = NerkontaktCovid[1], #1-ja, 2-nei, 3-ukjent
                Nyresykdom = sum(Nyresykdom)>0,
                #Oksygenmetning
                # OverfortAnnetSykehusInnleggelse,
                # OverfortAnnetSykehusUtskrivning,
                PatientGender = PatientGender[1],
                Penicillin = sum(Penicillin)>0,
                PenicillinEnzymhemmer = sum(PenicillinEnzymhemmer)>0,
                ReiseUtenfor = ReiseUtenfor[1], #1-ja, 2-nei, 3-ukjent
                ReshId = first(ReshId, order_by = FormDate),
                #Respirasjonsfrekvens,
                #RHF,
                RontgenThorax = RontgenThorax[1], #1-5...?
                Royker = sum(Royker)>0,
                #Sykehus"
                Status30Dager = sort(Status30Dager)[1],
                Status90Dager= sort(Status90Dager)[1],
                #SystoliskBlodtrykk,
                #Temp,
                TredjeGencefalosporin = sum(TredjeGencefalosporin)>0,
                #Trombocytter,
                UtsAkuttNyresvikt = UtsAkuttNyresvikt[1],  #1-ja, 2-nei, 3-ukjent
                UtsAkuttRespirasjonsvikt = UtsAkuttRespirasjonsvikt[1], #1-nei, 2:5-ja, 999-ukjent
                UtsAkuttSirkulasjonsvikt = UtsAkuttSirkulasjonsvikt[1], #1-nei, 2:5-ja, 999-ukjent
                UtsAminoglykosid = sum(UtsAminoglykosid)>0,
                UtsAndreGencefalosporin = sum(UtsAndreGencefalosporin)>0,
                UtsAntibiotika = UtsAntibiotika[1], #1-ja, 2-nei, 3-ukjent
                UtsAntibiotikaAnnet = sum(UtsAntibiotikaAnnet)>0,
                #?Antifungalbehandling = Antifungalbehandling[1], #1-ja, 2-nei, 3-ukjent
                #?AntiviralBehandling"
                UtsAntifungalbehandling = UtsAntifungalbehandling[1], #1-ja, 2-nei, 3-ukjent
                UtsAntiviralBehandling = UtsAntiviralBehandling[1],  #1-ja, 2-nei, 3-ukjent
                UtsKarbapenem = sum(UtsKarbapenem)>0,
                UtsKinolon = sum(UtsKinolon)>0,
                Utskrivningsdato = last(Utskrivningsdato, order_by = FormDate),
                UtsMakrolid = sum(UtsMakrolid)>0,
                UtsPenicillin = sum(UtsPenicillin)>0,
                UtsPenicillinEnzymhemmer = sum(UtsPenicillinEnzymhemmer)>0,
                UtsTredjeGencefalosporin = sum(UtsTredjeGencefalosporin)>0,
                #HovedskjemaGUID
                #OverfortAnnetSykehusUtskrivning #1-ja, 2-nei
                StatusVedUtskriving = sort(StatusVedUtskriving, decreasing = T)[1],  #1-levende, 2-død
                ShNavn = first(ShNavn, order_by = FormDate),
                ShNavnUt = last(ShNavnUt, order_by = FormDate),
                FormStatusUt = sort(FormStatusUt)[1], #1-kladd, 2-ferdigstilt
                FormDateUt = sort(FormDateUt, decreasing = T)[1],
                FormDate = sort(FormDate)[1])
  #----------------------------
   RegData <- data.frame(RegDataRed)

      #Kjønn
      RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 1] <- 1
      RegData$erMann[RegData$PatientGender == 2] <- 0
      RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

      # Enhetsnivånavn
      RegData$HFresh <- ReshNivaa$HFresh[match(RegData$ReshId, ReshNivaa$ShResh)]
      RegData$HFresh[is.na(RegData$HFresh)] <- RegData$ReshId[is.na(RegData$HFresh)]
      RegData$RHFresh <- ReshNivaa$RHFresh[match(RegData$HFresh, ReshNivaa$HFresh)]
      #Får encoding-feil hvis bruker denne:
      #RegData$RHF <- ReshNivaa$RHFnavn[match(RegData$HFresh, ReshNivaa$HFresh)]
      #RegData$RHF <- gsub('HELSE | RHF', '', RegData$RHF) #factor()
      #Kode om private
      RegData$RHF <- as.factor(RegData$RHFresh)
      levels(RegData$RHF) <- c('Vest','Nord','Midt', 'Sør-Øst')
      #head(as.character(RegData$RHF))
      #RegData$RHF <- sub('Helse ', '', RegData$RHF) #factor()

      #Riktig format på datovariable:
      RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
      RegData$InnTidspunkt <- as.POSIXct(RegData$FormDate, tz= 'UTC',
                                                  format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive
      RegData$UtDato <- as.Date(RegData$Utskrivningsdato, tz= 'UTC', format="%Y-%m-%d") #Evt. FormDateUt
      RegData$UtTidspunkt <- as.POSIXct(RegData$Utskrivningsdato, tz= 'UTC',
                                        format="%Y-%m-%d %H:%M:%S" )

      #Beregnede variabler
      #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      RegData$Liggetid <- as.numeric(difftime(RegData$UtTidspunkt,
                                              RegData$InnTidspunkt,
                                              units = 'days'))


#Fjerne feilregisteringer
      RegData <- RegData[which((RegData$InnDato>'2020-02-15') & (RegData$InnDato <= Sys.Date())),]

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

