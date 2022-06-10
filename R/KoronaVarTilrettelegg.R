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
      #		minald=0, maxald=110, erMann='',InnMaate='',

      "%i%" <- intersect


      #----------- Figurparametre - MÅ RYDDES !!!! ------------------------------
      cexgr <- 0.9	#Kan endres for enkeltvariable
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

      #aarsakInn <-9
      # skjemastatusInn <- 9
      # skjemastatusUt <- 9
      # dodSh <- 9
      # minald <- 0
      # maxald <- 110
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
      if (valgtVar=='alder_u18') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
        RegData$Variabel[RegData$Alder < 18] <- 1
        tittel <- 'Innlagte under 18 år'
        varTxt <- 'barn'
      }

      if (valgtVar=='alder_u40') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
        RegData$Variabel[RegData$Alder < 40] <- 1
        tittel <- 'Innlagte under 40 år'
        varTxt <- 'under 40 år'
      }
      if (valgtVar=='alder_o60') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
        RegData$Variabel[RegData$Alder >= 60] <- 1
        tittel <- 'Innlagte over 60 år'
        varTxt <- 'over 60 år'
      }
      if (valgtVar=='alder_o80') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
        RegData$Variabel[RegData$Alder >= 80] <- 1
        tittel <- 'Innlagte over 80 år'
        varTxt <- 'over 80 år'
      }

      if (valgtVar=='isolertInn') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$Isolert %in% 1:2), ]    #Tar bort ukjente
        RegData$Variabel[RegData$Isolert == 1] <- 1
        tittel <- 'Isolert ved innleggelse'
        varTxt <- 'isolerte'
      }
      if (valgtVar=='beredPas') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$BeredPas %in% 0:1), ]    #Tar bort ukjente
        RegData$Variabel <- RegData$BeredPas
        tittel <- 'Pandemipasienter med intensivopphold'
        varTxt <- 'intensivinnlagte'
      }
      if (valgtVar=='dodSh') {	#AndelGrVar/Tid
        RegData <- RegData[which(RegData$StatusVedUtskriving %in% 1:2), ]    #Tar bort ukjente
        RegData$Variabel[RegData$StatusVedUtskriving == 2] <- 1
        tittel <- 'Død på sykehus'
        varTxt <- 'døde'
      }

      if (valgtVar == 'liggetid') { #Andeler #GjsnGrVar
            #Liggetid bare >0
        #indUreinn <- which(RegData$Reinn==0)
            #RegData$Liggetid  <- as.numeric(RegData$Liggetid)
            RegData <- RegData[which(RegData$Liggetid>0 & RegData$Reinn==0 & RegData$FormStatusUt==2), ]
            tittel <- 'Liggetid, utskrevne uten reinnlagte'
            #if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {tittel <- 'liggetid'}
            gr <- c(0, 2, 4, 6, 8, 10, 12, 14, 21, 1000)  #c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
            RegData$VariabelGr <- cut(RegData$Liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
            xAkseTxt <- 'Liggetid (døgn)'
      }

      if (valgtVar %in% c('regForsinkelseInn', 'regForsinkelseUt')) {  #Andeler, GjsnGrVar
        #Bare ferdigstilte?
        #Endrede skjema:
        indEndret <- c(which(as.Date(RegData$CreationDate) == '2020-10-22' & RegData$ReshId ==4211748) #Hammerfest
                       ,which(as.Date(RegData$CreationDate) == '2020-06-10' & RegData$ReshId == 101971) #Finnmark
                       ,which(as.Date(RegData$CreationDate) == '2020-10-26' & RegData$ReshId == 4209222) #Kalnes
                        ,which(as.Date(RegData$CreationDate) == '2020-06-30' & RegData$ReshId == 4209222) #Kalnes
                        ,which(as.Date(RegData$CreationDate) == '2020-07-09' & RegData$ReshId == 100092) #Østfold?
                       ,which(as.Date(RegData$CreationDate) == '2020-10-23' & RegData$ReshId == 109870) #Ullevål
                        ,which(as.Date(RegData$CreationDate) == '2020-10-28' & RegData$ReshId == 114282) #Stavanger
        )
        RegData <- RegData[-indEndret,]

          RegData$RegForsink <- switch(valgtVar,
                                       regForsinkelseInn = as.numeric(difftime(RegData$CreationDate,
                                                   RegData$InnTidspunkt, units = 'days')),
                                       regForsinkelseUt = as.numeric(difftime(RegData$FirstTimeClosedUt,
                                                     RegData$UtTidspunkt, units = 'days'))
          )
        RegData <- RegData[which(!is.na(RegData$RegForsink)), ]
        tittel <- switch(valgtVar,
                         regForsinkelseInn='Tid fra første innleggelse til opprettet inn-skjema',
                         regForsinkelseUt = 'Tid fra utskriving til ferdigstilt ut-skjema')
        subtxt <- 'døgn'
        gr <- c(0,1:7,30,5000) #gr <- c(seq(0, 90, 10), 1000)
        RegData$VariabelGr <- cut(RegData$RegForsink, breaks = gr, include.lowest = TRUE, right = TRUE)
        grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '30+') #c('1', '(1-7]', '(7-14]', #
        cexgr <- 0.9
        xAkseTxt <- 'dager'
      }

      #---------------KATEGORISKE

      if (valgtVar == 'sirkSviktInn') { #Andeler
        #-1 = Velg verdi, 1 = Nei, 2 = Ja, symptomer ved høy aktivitet/anstrengelse
        #3 = Ja, symptomer ved moderat aktivitet, 4 = Ja, symptomer ved lett aktivitet
        #5 = Ja, symptomer i hvile, 999 - Ukjent
            gr <- c(1:5,999)
            retn <- 'H'
            tittel <- 'Akutt sirkulasjonssvikt ved innleggelse'
            RegData$VariabelGr <- factor(RegData$AkuttSirkulasjonsvikt, levels=gr)
            grtxt <- c('Nei', 'Ja, høy aktivitet', 'Ja, moderat aktivitet',
                       'Ja, lett aktivitet', 'Ja, hvile', 'Ukjent')
      }
      if (valgtVar == 'sirkSviktUt') { #Andeler
        gr <- c(1:5,999)
        retn <- 'H'
        tittel <- 'Akutt sirkulasjonssvikt under oppholdet'
        RegData$VariabelGr <- factor(RegData$UtsAkuttSirkulasjonsvikt, levels=gr)
        grtxt <- c('Nei', 'Ja, høy aktivitet', 'Ja, moderat aktivitet',
                   'Ja, lett aktivitet', 'Ja, hvile', 'Ukjent')
      }

      #
      if (valgtVar == 'respSviktInn') { #Andeler
        #-1 = Velg verdi, 1 = Nei, 2 = Ja, symptomer ved høy aktivitet/anstrengelse
        #3 = Ja, symptomer ved moderat aktivitet, 4 = Ja, symptomer ved lett aktivitet
        #5 = Ja, symptomer i hvile, 999 - Ukjent
        gr <- c(1:5,999)
        retn <- 'H'
        tittel <- 'Akutt respirasjonssvikt ved innleggelse'
        RegData$VariabelGr <- factor(RegData$AkuttRespirasjonsvikt, levels=gr)
        grtxt <- c('Nei', 'Ja, høy aktivitet', 'Ja, moderat aktivitet',
                   'Ja, lett aktivitet', 'Ja, hvile', 'Ukjent')
      }
      if (valgtVar == 'respSviktUt') { #Andeler
        gr <- c(1:5,999)
        retn <- 'H'
        tittel <- 'Akutt respirasjonssvikt under oppholdet'
        RegData$VariabelGr <- factor(RegData$UtsAkuttRespirasjonsvikt, levels=gr)
        grtxt <- c('Nei', 'Ja, høy aktivitet', 'Ja, moderat aktivitet',
                   'Ja, lett aktivitet', 'Ja, hvile', 'Ukjent')
      }
      if (valgtVar == 'aarsakInn4kat') { #Andeler
        #1-ja, alle opph, 2-ja, siste opphold, men ikke alle, 3-ja, minst ett opph, men ikke siste og ikke alle, 4-nei, ingen opph, 9-ukj
        #(3kategorier: 1-ja, alle opph, 2-ja, siste opphold, 3-ja, minst ett opph, men ikke siste, nei, ingen opph, 9-ukj)
        gr <- c(1:4,9)
        retn <- 'H'
        tittel <- 'Covid-19 hovedårsak til sykehusopphold?'
        RegData$VariabelGr <- factor(RegData$ArsakInnNy, levels=gr)
        grtxt <- c('Alle opphold', 'Siste, men ikke alle', 'Minst ett, ikke siste/alle', 'Ingen opphold', 'Ukjent')
      }


      #RontgenThorax
      # -1 = Velg verdi
      # 1 = Normalt
      # 2 = Infiltrat
      # 3 = Stuvning
      # 4 = Ikke utført
      # 5 = Ukjent

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
            RegData$ACEhemmmer <- RegData$AceHemmerInnkomst==1
            RegData$ACEhemmmer[RegData$AceHemmerInnkomst==3] <- NA
            #statusInn <- 2
            variable <- c('Kreft',  'NedsattimmunHIV', 'Diabetes', 'Hjertesykdom', 'ACEhemmmer','Astma',
                          'KroniskLungesykdom', 'Nyresykdom', 'Leversykdom', 'KroniskNevro',
                          'Gravid', 'Fedme', 'Royker', 'KjentRisikofaktor')
            grtxt <- c('Kreft', 'Nedsatt immunforsvar', 'Diabetes', 'Hjertesykdom', 'ACE-hemmer', 'Astma',
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
                      'Karbapenem', 'Makrolid', 'AntibiotikaAnnet', 'AntibiotikaUkjent', 'Antibiotika')
        grtxt <- c('Penicillin', 'Penicillin m/enzymhemmer', 'Aminoglykosid',
                   '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
                   'Karbapenem', 'Makrolid', 'Annet', 'Ukjent type', 'Antibiotika, tot.')
        #Trenger bare kode om Antibiotika tot.:
        RegData$Antibiotika <- ifelse(RegData$Antibiotika==1, TRUE, FALSE)
        xAkseTxt <- 'Andel pasienter (%)'
      }
      if (valgtVar == 'antibiotikaUt' ) {   # Andeler
        tittel <- 'Antibiotika ved utskriving'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[RegData$UtsAntibiotika %in% 1:2, ] #1-ja, 2-nei
        statusUt <- 2
        variable <- paste0('Uts', c('Penicillin', 'PenicillinEnzymhemmer', 'Aminoglykosid',
                      'AndreGencefalosporin', 'TredjeGencefalosporin', 'Kinolon',
                      'Karbapenem', 'Makrolid', 'AntibiotikaAnnet', 'AntibiotikaUkjent', 'Antibiotika'))
        grtxt <- c('Penicillin', 'Penicillin m/enzymhemmer', 'Aminoglykosid',
                   '2. gen. cefalosporin', '3. gen. cefalosporin', 'Kinolon',
                   'Karbapenem', 'Makrolid', 'Annet', 'Ukjent type', 'Antibiotika, tot.')
        #Trenger bare kode om Antibiotika tot.:
        RegData$UtsAntibiotika <- ifelse(RegData$UtsAntibiotika==1, TRUE, FALSE)
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


      if (valgtVar == 'demografi' ) {   # Andeler
        #1-ja, 2-nei, 3-ukjent
        tittel <- 'Demografi og epidemiologi'
        retn <- 'H'
        flerevar <- 1
        variable <- c('ReiseUtenfor', 'NerkontaktCovid', 'ErHelsepersonell', 'ErAnsattMikrobiologisk')
        #1:ja, 2:nei
        grtxt <- c('Utenlandsreise', 'Nærkontakt, Covid', 'Helsepersonell', 'Ansatt, mikro.bio.lab.')
        #Kodes om til indikatorvariabel:
        RegData[, variable][which(RegData[ ,variable] == -1, arr.ind = T)] <- NA
        RegData[, variable][which(RegData[ ,variable] == 3, arr.ind = T)] <- NA
        RegData[, variable][which(RegData[ ,variable] == 2, arr.ind = T)] <- 0
        xAkseTxt <- 'Andel pasienter (%)'
        #Beregne direkte:
        #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }


           if (valgtVar == 'tilstandInn' ) {

            tittel <- 'Tilstand ved innleggelse'
            #AkuttRespirasjonsvikt, AkuttSirkulasjonsvikt, ja:2:5, nei:1
            #AkuttNyresvikt, EndretBevissthet, Isolert, ja:1, nei:2
            #AceHemmerInnkomst/AceHemmerInnkomst2 - tomme!
            retn <- 'H'
            flerevar <- 1
            variable <- c('AkuttRespirasjonsvikt', 'AkuttSirkulasjonsvikt', 'AkuttNyresvikt',
                          'EndretBevissthet','Isolert')
            grtxt <- c('Akutt resp.svikt (alle grader)', 'Akutt sirk.svikt (alle grader)', 'Akutt nyresvikt',
                       'Endret bevissthet','Isolert')
            RegData$AkuttRespirasjonsvikt <- ifelse(RegData$AkuttRespirasjonsvikt %in% 1:5,
                                                      ifelse(RegData$AkuttRespirasjonsvikt==1, 0, 1), NA)
            # RegData$AkuttSirkulasjonsvikt <- recode(RegData$AkuttSirkulasjonsvikt,
            #                                          '1'=0, '2'=1, '3'=1, '4'=1, '5'=1, .default=NULL)
            RegData$AkuttSirkulasjonsvikt <- ifelse(RegData$AkuttSirkulasjonsvikt %in% 1:5,
                                                    ifelse(RegData$AkuttSirkulasjonsvikt==1, 0, 1), NA)
            var <- c('AkuttNyresvikt', 'EndretBevissthet', 'Isolert')
            RegData[, var][which(RegData[ ,var] == -1, arr.ind = T)] <- NA
            RegData[, var][which(RegData[ ,var] == 3, arr.ind = T)] <- NA
            RegData[, var][which(RegData[ ,var] == 2, arr.ind = T)] <- 0
            xAkseTxt <- 'Andel pasienter (%)'
           }

      RegData$Variabel <- as.numeric(RegData$Variabel)

      UtData <- list(RegData=RegData, #minald=minald, maxald=maxald,
                     grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt,
                     retn=retn,tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData))

}

