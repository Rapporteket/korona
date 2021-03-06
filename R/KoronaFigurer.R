# Fil med samling av figurfunksjoner for Rapporteket Pandemi

#' Aldersfordeling, tabell
#' @param RegData datatabell, beredskapsdata
#' @inheritParams KoronaUtvalg
#' @return
#' @export
AlderKjFig <- function(RegData, valgtVar='Alder', valgtEnhet='Alle', enhetsNivaa='RHF',
                       datoFra='2020-03-01', datoTil=Sys.Date(), skjemastatusInn=9,
                       aarsakInn=9, dodSh=9, erMann=9, grvar = 'erMann', outfile=''){

   if (grvar=='erMann') {RegData$erMann <- factor(RegData$erMann, levels = 0:1, labels = c("Kvinner", "Menn"))}

  UtData <- KoronaUtvalg(RegData=RegData,
                         valgtEnhet=valgtEnhet,
                         enhetsNivaa = enhetsNivaa,
                         datoFra = datoFra, datoTil = datoTil,
                         dodSh = dodSh,
                         aarsakInn=aarsakInn,
                         erMann = erMann,
                         skjemastatusInn=skjemastatusInn
  )
  RegData <- UtData$RegData #[UtData$ind$Hoved, ]
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

  if (sum(NHoved) < 5 ) {
    farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer i utvalget', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
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
  }

  AntHovedTab$Andel <- paste0(round(AntHovedTab$Sum/AntHovedTab$Sum[dim(AntHovedTab)[1]]*100), ' %')
  names(AntHovedTab)[(dim(AntHovedTab)[2]-1):dim(AntHovedTab)[2]] <- c("Antall", "Andel")

#UtData <- list(Tab=AntHovedTab, Ntest=N)
  return(AntHovedTab)
}


#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#'
#' @param AntTab Dataramme med nødvendige figurparametre
#'
#' @return
#' @export
FigTidEnhet <- function(AntTab, outfile=''){

  # x11()
  NutvTxt <- length(AntTab$utvalgTxt)
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett = 'OffAlleFarger')
  if (NutvTxt!=0) {par('fig'=c(0.05, 1, 0, 1-0.02*(NutvTxt-1)))}
  farger <- FigTypUt$farger
  x_labs <- AntTab$Tab_tidy$Tid[-dim(AntTab$Tab_tidy)[1]]
  legendTxt <- names(AntTab$Tab_tidy[,-c(1, dim(AntTab$Tab_tidy)[2])])
  N_kat <- dim(AntTab$Tab_tidy)[2] - 2
  ymax <- max(c(max(rowSums(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])], na.rm = T)), 1))
  pos <- barplot(t(as.matrix(AntTab$Tab_tidy[-dim(AntTab$Tab_tidy)[1],-c(1, dim(AntTab$Tab_tidy)[2])])),
                 ylim = c(0,ymax), col = farger[1:N_kat], ylab = 'Antall', main = 'Personer', names.arg = x_labs,
                 las = 2) #xaxt="n",
  # text(cex=1, x=pos-.25, y=-0.25, x_labs, xpd=TRUE, srt=45, adj = 1)
  legend('topright', rev(legendTxt), ncol=1, fill=rev(farger[1:N_kat]), border=rev(farger[1:N_kat]),
         bty='n', cex=1, xpd = T, )

  mtext(AntTab$utvalgTxt, side=3, las=1, cex=0.9, adj=0, line=c(3+0.8*((NutvTxt-1):0)))
  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}
}


