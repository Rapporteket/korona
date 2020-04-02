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
#MÅ OPPDATERES NÅR FÅR DATA!!!!
      #Kjønn
      RegData$erMann <- NA #1=Mann, 2=Kvinne, 0=Ukjent
      RegData$erMann[RegData$PatientGender == 1] <- 1
      RegData$erMann[RegData$PatientGender == 2] <- 0
      RegData$Kjonn <- factor(RegData$erMann, levels=0:1, labels=c('kvinner','menn'))

      #Diagnoser:
      #RegData$Bekreftet <- 0
      #RegData$Bekreftet[which(RegData$Diagnosis %in% 100:103)] <- 1


      # Endre variabelnavn:
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      #	names(RegData)[which(names(RegData) == 'ReAdmitted')] <- 'Reinn'
      names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
      #Avvik ml. test og prod-data:
      names(RegData)[
            names(RegData) %in% c('PatientInRegistryGuid', 'PasientGUID')] <- 'PasientID'

      # Enhetsnivånavn
      RegData$ShNavn <- trimws(as.character(RegData$HelseenhetKortNavn)) #Fjerner mellomrom (før) og etter navn
      RegData$RHF <- sub('Helse ', '', RegData$RHF) #factor()
      # Kode om fra Haraldsplass til RHF Vest og Lovisenberg diakonhjemmet til RHF Øst, fra priv
      #FÅ PÅ PLASS OMKODING FOR ALLE
      RegData$RHF[RegData$ReshId == 100180] <- 'Vest' #Haraldsplass
      RegData$RHF[RegData$ReshId %in% c(42088921, 108897)] <- 'Sør-Øst' #Lovisenberg Diakonale


      #Riktig format på datovariable:
      RegData$InnDato <- as.Date(RegData$FormDate, tz= 'UTC', format="%Y-%m-%d") #DateAdmittedIntensive
      RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$FormDate, tz= 'UTC',
                                                  format="%Y-%m-%d %H:%M:%S" ) #DateAdmittedIntensive

      # Nye tidsvariable:
      RegData$MndNum <- RegData$Innleggelsestidspunkt$mon +1
      RegData$MndAar <- format(RegData$Innleggelsestidspunkt, '%b%y')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- format(RegData$InnDato, '%Y')
      RegData$UkeNr <- format(RegData$InnDato, '%V')
      #RegData$UkeAar <- format(RegData$InnDato, '%G.%V') #%G -The week-based year, %V - Week of the year as decimal number (01–53) as defined in ISO 8601
      #RegData$UkeAar <- as.factor(RegData$UkeAar)
      #RegData$Dag <- format(RegData$InnDato, '%d.%B')
      RegData$Dag <- factor(format(RegData$InnDato, '%d.%B'),
                            levels = format(seq(min(RegData$InnDato), max(RegData$InnDato), by="day"), '%d.%B'))

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


      return(invisible(RegData))
}

