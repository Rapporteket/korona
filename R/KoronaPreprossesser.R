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
      #Kjønn
      RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 1] <- 1
      RegData$erMann[RegData$PatientGender == 2] <- 0
      RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

      # Endre variabelnavn:
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      #	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
      names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
      #Avvik ml. test og prod-data:
      names(RegData)[
            names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'

      # Enhetsnivånavn
      RegData$ShNavn <- trimws(as.character(RegData$HelseenhetKortNavn)) #Fjerner mellomrom (før) og etter navn
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

      #Liggetider
      #names(RegData)[which(names(RegData) == 'DaysAdmittedIntensiv')] <- 'liggetid'
      RegData$liggetid <- as.numeric(difftime(RegData$UtTidspunkt,
                                              RegData$InnTidspunkt,
                                              units = 'days'))

#Fjerne feilregisteringer
      RegData <- RegData[which((RegData$InnDato>'2020-02-15') & (RegData$InnDato <= Sys.Date())),]

      # Nye tidsvariable:
      # RegData$MndNum <- RegData$InnTidspunkt$mon +1
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

#RIKTIG FORMAT PÅ VARIABLER
      RegData$FormDate <- as.numeric(RegData$FormDate)
      #Konvertere boolske variable fra tekst til boolske variable...
      # TilLogiskeVar <- function(Skjema){
      #       verdiGML <- c('True','False')
      #       verdiNY <- c(TRUE,FALSE)
      #       mapping <- data.frame(verdiGML,verdiNY)
      #       LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
      #       if (length(LogVar)>0) {
      #             for (k in 1:length(LogVar)) {
      #                   Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
      #             }}
      #       return(Skjema)
      # }
      #
      # RegData <- TilLogiskeVar(RegData)

      boolske_var_inklusjon <- as.character(kodebok$inklusjon$Variabelnavn)[which(as.character(kodebok$inklusjon$Felttype) == 'Avkrysning')]
      RegData[, intersect(names(RegData), boolske_var_inklusjon)] <-
         apply(RegData[, intersect(names(RegData), boolske_var_inklusjon)], 2, as.logical)

      return(invisible(RegData))
}

