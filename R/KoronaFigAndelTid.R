#' Tidstrend av andel opphold
#'
#' Denne funksjonen lager et linjediagram som viser utvikling over tid  for andeler av valgt variabel,
#' filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'	 \item isolering: Isolasjon av pasient
#'    }
#'
#' @inheritParams KoronaFigAndeler
#' @param tidsenhet Oppløsning på tidsaksen. Verdier: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
#'
#' @return Figur som viser tidstrend, dvs. andel av valgt variabel for hvert år.
#'
#' @export
KoronaFigAndelTid <- function(RegData=0, hentData=0, valgtVar='alder_u18',
                              datoFra='2020-03-01', datoTil=Sys.Date(), tidsenhet='Kvartal',
                              dod='', reshID=0, erMann=9, minald=0, maxald=110, #
                              skjemastatusInn=9, skjemastatusUt=9, dodSh=9, aarsakInn=9,
                              enhetsNivaa='RHF', valgtEnhet='Alle', enhetsUtvalg=0,
                              beredPas=9, outfile='', lagFig=1, ...) {


   if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = paste0("AndelTid: ", valgtVar))
   }
   if (hentData == 1) {
         RegData <- KoronaPreprosesser(KoronaDataSQL(koble=1), aggPers = 1 )
   }

      #------- Tilrettelegge variable
      varTxt <- ''
            KoronaVarSpes <- KoronaVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
            RegData <- KoronaVarSpes$RegData
            sortAvtagende <- KoronaVarSpes$sortAvtagende
            varTxt <- KoronaVarSpes$varTxt
            KImaal <- KoronaVarSpes$KImaal
            KImaaltxt <- ifelse(KoronaVarSpes$KImaaltxt=='', '', paste0('Mål: ',KoronaVarSpes$KImaaltxt))
            tittel <- KoronaVarSpes$tittel


      #------- Gjøre utvalg
      smltxt <- ''
      medSml <- 0

            if (reshID==0) {enhetsUtvalg <- 0}
            KoronaUtvalg <- KoronaUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                                      minald=minald, maxald=maxald, erMann=erMann,
                                      beredPas=beredPas, dodSh=dodSh, aarsakInn=aarsakInn,
                                      skjemastatusInn=skjemastatusInn, skjemastatusUt=skjemastatusUt,
                                      enhetsNivaa='RHF', valgtEnhet='Alle',
                                      enhetsUtvalg=enhetsUtvalg)

            smltxt <- KoronaUtvalg$smltxt
            medSml <- KoronaUtvalg$medSml
            utvalgTxt <- KoronaUtvalg$utvalgTxt
            ind <- KoronaUtvalg$ind

      RegData <- KoronaUtvalg$RegData
      Ngrense <- 10
      N <- list(Hoved = dim(RegData)[1], Rest=0)

      #--------------- Gjøre beregninger ------------------------------
      AggVerdier <- list(Hoved = 0, Rest =0)
      Ngr <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))
      grtxt2 <- ''
      vektor <- c('Aar','Halvaar','Kvartal','Mnd')
      xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
                         [which(tidsenhet==vektor)])
      yAkseTxt <- 'Andel (%)'
      hovedgrTxt <- ''

      if (N$Hoved>Ngrense) {
        #Klargjøre tidsenhet
        RegDataFunk <- intensiv::SorterOgNavngiTidsEnhet(RegData=RegData, tidsenhet = tidsenhet)
            RegData <- RegDataFunk$RegData

            NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per tidsenhet
            Ngr$Hoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per tidsenhet
            AggVerdier$Hoved <- Ngr$Hoved/NAarHoved*100
            NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)
            Ngr$Rest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
            AggVerdier$Rest <- Ngr$Rest/NAarRest*100

            grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
            hovedgrTxt=KoronaUtvalg$hovedgrTxt
      }
      FigDataParam <- list(AggVerdier=AggVerdier, N=N,
                           Ngr=Ngr,
                           KImaal <- KImaal,
                           KImaaltxt <- KImaaltxt,
                           grtxt=levels(RegData$TidsEnhet),
                           grtxt2=grtxt2,
                           varTxt=varTxt,
                           tittel=tittel,
                           retn='V',
                           xAkseTxt=xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=KoronaUtvalg$utvalgTxt,
                           medSml=medSml,
                           hovedgrTxt=hovedgrTxt,
                           smltxt=KoronaUtvalg$smltxt)

      if (lagFig == 1) {
                  #-----------Figur---------------------------------------
            #Hvis for få observasjoner..
            if (N$Hoved < Ngrense | (medSml ==1 & N$Rest < Ngrense)) {
                  FigTypUt <- rapFigurer::figtype(outfile)
                  farger <- FigTypUt$farger
                  plot.new()
                  title(main=paste0('variabel: ', valgtVar))	#, line=-6)
                  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                  text(0.5, 0.65, 'For få registreringer', cex=1.2)
                  #text(0.55, 0.55, 'eller sammenlikningsgruppe', cex=1.2)
                  if ( outfile != '') {dev.off()}

            } else {

                  #Plottspesifikke parametre:
                  FigTypUt <- rapFigurer::figtype(outfile) #, fargepalett=fargepalett)
                  farger <- FigTypUt$farger
                  fargeHoved <- farger[3]
                  fargeRest <- farger[1]
                  NutvTxt <- length(utvalgTxt)
                  hmarg <- 0.04+0.01*NutvTxt
                  par('fig' = c(0,1,0,1-hmarg))
                  cexleg <- 1	#St?rrelse p? legendtekst
                  ylabtext <- "Andel (%)"
                  xskala <- 1:length(levels(RegData$TidsEnhet)) #length(tidtxt)
                  xmax <- max(xskala)


                  ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
                  plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o',
                       xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
                       cex=2, xlab=xAkseTxt, ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i')

                  #Legge på linjer i plottet.
                  grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

                  axis(side=1, at = xskala, labels = levels(RegData$TidsEnhet)) #tidtxt)

                  title(tittel, line=1, font.main=1)


                  lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
                  points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
                  text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)

                  lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
                  points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)

                  #KImål
                  # lines(xskala,rep(KImaal,length(xskala)), col= '#FF7260', lwd=3)
                  # mtext(text=KImaaltxt, at=KImaal, side=4, las=0, adj=0.5,  cex=0.9, col='#FF7260')

                  Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')')
                  if (medSml == 1) {
                        text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
                        legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                                       paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1,
                               col=c(fargeHoved, fargeRest, NA), lwd=3, cex=cexleg)
                  } else {
                        legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt),
                               col=c(fargeHoved, NA), lwd=3, bty='n')
                  }

                  #Tekst som angir hvilket utvalg som er gjort
                  mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest,
                        #line=c(3+0.8*((NutvTxt-1):0))
                        line=c(4.5-0.75*(0:(NutvTxt-1)))
                  )

                  par('fig'=c(0, 1, 0, 1))
                  if ( outfile != '') {dev.off()}
                  #------------------------------------------------------------------------------

            }	#end else statement
      }

      #}

      return(invisible(FigDataParam))
}	#end function



