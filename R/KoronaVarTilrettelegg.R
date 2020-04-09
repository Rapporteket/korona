#' Funksjon for å tilrettelegge variable for beregning.
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk.
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen.
#' Her kan mye hentes til analysebok... Og mye kunne vært hentet fra ei god kodebok.
#'
#' @inheritParams KoronaFigAndeler
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel, samt flere andre parametre som
#' tittel, xAkseTxt, sortAvtagende (standard: TRUE)
#'       #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst.
#' Variabeltyper: Numeriske, kategoriske, indikator
#' For hver valgtVar: Definer og gjør utvalg for variabelen
#' @export
#'

KoronaVarTilrettelegg  <- function(RegData, valgtVar, grVar='ShNavn', figurtype='andeler'){
      #, datoFra='2011-01-01', datoTil='3000-12-31',
      #		minald=0, maxald=110, erMann='',InnMaate='', dodInt='',outfile='',
      #		preprosess=1, hentData=0, reshID, enhetsUtvalg=1)


      "%i%" <- intersect


      #----------- Figurparametre - MÅ RYDDES !!!! ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      sortAvtagende <- TRUE  #Sortering av resultater
      varTxt <- 'hendelser'

      aarsakInn <-9
      skjemastatusInn <- 9
      skjemastatusUt <- 9
      aarsakInn<- 9
      dodSh <- 9
      minald <- 0
      maxald <- 110
      tittel <- 'Mangler tittel'
      variable <- 'Ingen'
      RegData$Variabel <- 0
      N <- dim(RegData)[1]


      #-------------------------------------



      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (figurtype == 'andeler') {	#Fordelingsfigur
              gr <- seq(0, 90, ifelse(N<100, 25, 10) )
              RegData$VariabelGr <- cut(RegData$Variabel, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
              grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
                c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
              levels(RegData$VariabelGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
              xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }


      if (valgtVar == 'liggetid') { #Andeler #GjsnGrVar
            #Liggetid bare >0
            RegData$Variabel  <- as.numeric(RegData$Liggetid)
            RegData <- RegData[which(RegData$Liggetid>0), ]
            tittel <- 'Liggetid'
            #if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {tittel <- 'liggetid'}
            gr <- c(0, 2, 4, 6, 8, 10, 12, 14, 21, 1000)  #c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$Liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Liggetid (døgn)'
      }

      if (valgtVar=='erMann') { #AndelTid/GrVar
        RegData <- RegData[which(RegData$erMann %in% 0:1), ]  	#Tar bort ukjente
        RegData$Variabel <- RegData$erMann
        varTxt <- 'menn'
        tittel <- 'Andel av oppholdene hvor pasienten er mann'
      }
      if (valgtVar == 'overfTil'){ #Overf: 1= ikke overført, 2= overført
        tittel <- 'Pasienter overført TIL valgt(e) enheter'
        ind <- which(RegData$Overf==2 & !is.na(RegData$PatientTransferredToHospitalName))
        RegData <- RegData[ind,]
        RegData$VariabelGr <- as.factor(RegData$PatientTransferredToHospitalName)
        grtxt <- levels(RegData$VariabelGr)
        retn <- 'H'

      }

      #---------------KATEGORISKE

      if (valgtVar == 'PrimaryReasonAdmitted') { #Andeler
            #                       1:Respiratorisk svikt, 2:Sirk./kardiovaskulær svikt, 3:Gastroenterologisk svikt,
            #                       4:Nevrologisk svikt, 5:Sepsis, 6:Skade/traume, 7:Metabolsk/intoksikasjon, 8:Hematologisk svikt,
            #                       9:Nyresvikt, 10:Postoperativt, 11:Annet
            gr <- 1:11
            RegData <- RegData[which(RegData$PrimaryReasonAdmitted %in% gr)
                                     %i% which(RegData$InnDato >= as.Date('2016-01-01', tz='UTC')), ] #Innført ila 2015
            retn <- 'H'
            tittel <- 'Primærårsak til intensivoppholdet'
            RegData$VariabelGr <- factor(RegData$PrimaryReasonAdmitted, levels=gr)
            grtxt <- c('Respiratorisk svikt', 'Sirk./kardiovaskulær svikt', 'Gastroenterologisk svikt',
                       'Nevrologisk svikt', 'Sepsis', 'Skade/traume', 'Metabolsk/intoksikasjon', 'Hematologisk svikt',
                       'Nyresvikt', 'Postoperativt (komplikasjon \ntil anestesi/kirurgi)', 'Annet')
            cexgr <- 0.9
      }
      #-------------- SAMMENSATTE variable
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #(Alternativt kan vi gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen)

      if (valgtVar == 'risikoInn' ) {   # Andeler
            tittel <- 'Risikofaktorer, innleggelse'
            retn <- 'H'
            flerevar <- 1
            RegData <- RegData[RegData$KjentRisikofaktor %in% 1:2, ]
            RegData$Fedme <- RegData$BMI>30
            statusInn <- 2
            variable <- c('Kreft',  'NedsattimmunHIV', 'Diabetes', 'Hjertesykdom', 'Astma',
                          'KroniskLungesykdom', 'Nyresykdom', 'Leversykdom', 'KroniskNevro',
                          'KroniskNevro', 'Fedme', 'Royker', 'KjentRisikofaktor')
            grtxt <- c('Kreft', 'Nedsatt immunforsvar', 'Diabetes', 'Hjertesykdom', 'Astma',
                       'Kronisk lungesykdom', 'Nyresykdom', 'Leversykdom', 'Nevrologisk/nevromusk.',
                       'Gravid', 'Fedme (KMI>30)', 'Røyker', 'Risikofaktorer (minst en)')
            #Trenger bare kode om Risikofaktorer:
            RegData$KjentRisikofaktor <- ifelse(RegData$KjentRisikofaktor==1, TRUE, FALSE)
            xAkseTxt <- 'Andel pasienter (%)'
            #Beregne direkte:
            #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }
      if (valgtVar == 'antibiotikaInn' ) {   # Andeler
        tittel <- 'Antibiotika ved innleggelse'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[RegData$Antibiotika %in% 1:2, ] #1-ja, 2-nei
        statusUt <- 2
        variable <- c('Penicillin', 'PenicillinEnzymhemmer', 'Aminoglykosid',
                      'AndreGencefalosporin', 'TredjeGencefalosporin', 'Kinolon',
                      'Karbapenem', 'Makrolid', 'AntibiotikaAnnet', 'Antibiotika')
        grtxt <- c('Penicillin', 'Penicillin med enzymhemmer', 'Aminoglykosid',
                   '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
                   'Karbapenem', 'Makrolid', 'Annet', 'Antibiotika')
        #Trenger bare kode om Antibiotika tot.:
        RegData$Antibiotika <- ifelse(RegData$Antibiotika==1, TRUE, FALSE)
        #ind01 <- which(RegData[ ,variable] %in% ..., arr.ind = T) #Alle ja/nei
        #ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
        #Kodes om til indikatorvariabel:
        #RegData[ ,variable] <- NA
        #RegData[ ,variable][ind01] <- 0
        #RegData[ ,variable][ind1] <- 1
        xAkseTxt <- 'Andel pasienter (%)'
        #Beregne direkte:
        #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }
      if (valgtVar == 'antibiotikaUt' ) {   # Andeler
        tittel <- 'Antibiotika ved utskriving'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[RegData$Antibiotika %in% 1:2, ] #1-ja, 2-nei
        statusUt <- 2
        variable <- c('Penicillin', 'PenicillinEnzymhemmer', 'Aminoglykosid',
                      'AndreGencefalosporin', 'TredjeGencefalosporin', 'Kinolon',
                      'Karbapenem', 'Makrolid', 'AntibiotikaAnnet', 'Antibiotika')
        grtxt <- c('Penicillin', 'Penicillin med enzymhemmer', 'Aminoglykosid',
                   '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
                   'Karbapenem', 'Makrolid', 'Annet', 'Antibiotika')
        #Trenger bare kode om Antibiotika tot.:
        RegData$Antibiotika <- ifelse(RegData$Antibiotika==1, TRUE, FALSE)
        #ind01 <- which(RegData[ ,variable] %in% ..., arr.ind = T) #Alle ja/nei
        #ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
        #Kodes om til indikatorvariabel:
        #RegData[ ,variable] <- NA
        #RegData[ ,variable][ind01] <- 0
        #RegData[ ,variable][ind1] <- 1
        xAkseTxt <- 'Andel pasienter (%)'
        #Beregne direkte:
        #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }



           if (valgtVar == 'spesTiltak' ) {   # Andeler
            #SpecialMeasures
            tittel <- 'Spesielle tiltak/intervensjoner'
            RegData <- RegData[which(RegData$InnDato>=as.Date('2016-01-01', tz='UTC')), ]
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('TerapetiskHypotermi', 'EcmoEcla', 'Iabp', 'Impella', 'Icp', 'Oscillator', 'No',
                          'Leverdialyse', 'Hyperbar', 'Eeg')
            #retn <- 'H'
            grtxt <- c('Terapetisk hypotermi', 'ECMO/ECLA', 'IABP Aortaballongpumpe', 'Impella/VV-assist',
                       'ICP, intrakranielt trykk', 'Oscillator', 'NO-behandling',
                       'Leverdialyse', 'Hyperbar oksygenbeh.', 'Kontinuerlig EEG')
            #ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }

      RegData$Variabel <- as.numeric(RegData$Variabel)

      UtData <- list(RegData=RegData, minald=minald, maxald=maxald,
                     grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt,
                     retn=retn,tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}

#--------------------Hjelpefunksjoner----------------------
