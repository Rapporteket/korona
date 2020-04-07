# Fil med samling av figurfunksjoner for Rapporteket Pamndemi

#' Aldersfordeling, tabell
#' @param RegData datatabell, beredskapsdata
#' @inheritParams KoronaUtvalg
#' @param enhetsNivaa styres av tilgangsnivå 'Alle', 'RHF', 'HF'
#' @return
#' @export
AlderKjFig <- function(RegData, valgtVar='Alder', valgtEnhet='Alle', enhetsNivaa='Alle', #tilgangsNivaa='SC', #
                     skjemastatusInn=9,  aarsakInn=9, dodSh=9, erMann=9, grvar = 'PatientGender', outfile=''){

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  # RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
  #                               SC = RegData$RHF,
  #                               LC = RegData$HF,
  #                               LU = RegData$ShNavn)
  #

  #RegData$EnhetsNivaaVar <- RegData[ , enhetsNivaa]
  # RegData$EnhetsNivaaVar <- as.factor(RegData$EnhetsNivaaVar)
  if (grvar=='PatientGender') {RegData$PatientGender <- factor(RegData$PatientGender, levels = 1:2, labels = c("Menn", "Kvinner"))}

  #ENDRING KOMMER: Utvalg skal returnere både utvalg for alle og egen enhet. Som i andre registere
  UtData <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet,
                         dodSh = dodSh,
                         aarsakInn=aarsakInn,
                         erMann = erMann,
                         skjemastatusInn=skjemastatusInn
  )
  RegData <- UtData$RegData
  utvalgTxt <- UtData$utvalgTxt

  N <- dim(RegData)[1]
  if (valgtVar=='Alder') {
    gr <- seq(0, 90, ifelse(N<100, 25, 10) )
    RegData$Gr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
    grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
      c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
    levels(RegData$Gr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
  }

  AntHoved <- table(RegData[, c(grvar, "Gr")])
  AntHovedTab <- tidyr::as_tibble(as.data.frame.matrix(addmargins(table(RegData[, c("Gr", grvar)]))), rownames=valgtVar)
  NHoved <- rowSums(AntHoved)

  tittel <- "Aldersfordeling";
  FigTypUt <- rapFigurer::figtype(outfile=outfile, pointsizePDF=12)
  retn <- 'V'; cexgr<-1

  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  if (NutvTxt>0) {par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))}

  farger <- FigTypUt$farger
  ymax <- max(c(AntHoved),na.rm=T)*1.25
  pos <- barplot(AntHoved, beside=TRUE, las=1, ylab="Antall pasienter",
                 cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, # ,	names.arg=grtxt, cex.names=cexgr,sub=subtxt,
                 col=farger[c(1,2)], border='white', ylim=c(0, ymax), xaxt='n')
  mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)

  legend('topright', paste0(levels(RegData[,grvar]), ', N=', NHoved), bty='n',
         fill=farger[c(1,2)], border=NA, ncol=1, cex=1)

  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}

  AntHovedTab$Andel <- paste0(round(AntHovedTab$Sum/AntHovedTab$Sum[dim(AntHovedTab)[1]]*100), ' %')
  names(AntHovedTab)[(dim(AntHovedTab)[2]-1):dim(AntHovedTab)[2]] <- c("Antall", "Andel")

  return(AntHovedTab)


}

