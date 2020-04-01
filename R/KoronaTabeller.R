#Fil med samling av funksjoner som lager tabeller for Rapporteket-Pandemi

#' Antall tilfeller for valgt tidsenhet og enhetsnivå. Filtreringer kan også gjøres.
#' Detaljerinsnivå er styrt av tilgangsnivå
#'
#' @param RegData dataramme med preprossesserte data
#' @param tidsenhet 'dag' (standard), 'uke', 'maaned'
#' @param tilgangsNivaa SC, LC og LU bestemmer hvilket enhetsNivaa
#' ('RHF', 'HF', 'ShNavn') resultatene skal vises for
#' @param valgtEnhet NULL for SC-bruker, ellers eget RHF/HF
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
antallTidEnhTab <- function(RegData, tidsenhet='dag', erMann=9, tilgangsNivaa, #enhetsNivaa='RHF',
                        bekr=9, skjemastatus=9, dodSh=9, valgtEnhet='Alle'){
  #valgtEnhet representerer eget RHF/HF

        RegData$TidsVar <- as.factor(RegData[ ,switch (tidsenhet,
                     dag = 'Dag',
                     uke = 'UkeNr',
                     maaned = 'MndAar')])

  #Benytter rolle som "enhetsnivå". Bestemmer laveste visningsnivå
  RegData$EnhNivaaVis <- switch(tilgangsNivaa, #RegData[ ,enhetsNivaa]
                                SC = RegData$RHF,
                                LC = RegData$HF,
                                LU = RegData$ShNavn)


  #Benytter ikke utvalgsfila til enhetsfiltrering. Skal også ha oppsummering for hele landet
  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus,
                             dodSh=dodSh)


  RegDataAlle <- UtData$RegData

  #Trenger utvalg når totalen ikke er summen av det som vises.
  RegData <- if (tilgangsNivaa == 'SC') { RegDataAlle
  } else {
    enhetsnivaa <- switch(tilgangsNivaa,'LC'='RHF', 'LU'='HF')
    subset(RegDataAlle, RegDataAlle[ ,enhetsnivaa] == valgtEnhet)
    }

  Ntest <- dim(RegData)[1]

    kolNavnSum <- ifelse(tilgangsNivaa=='SC',
                         'Hele landet',
                         paste0(valgtEnhet, ', totalt'))
    if (Ntest==0) {
      TabTidEnh <- matrix(0, ncol=1, nrow=length(levels(RegData$TidsVar)) + 1,
                          dimnames = list(c(levels(RegData$TidsVar), 'Totalt'), valgtEnhet)) #table(RegData$TidsVar)
    }else{
      TabTidEnh <- table(RegData[ , c('TidsVar', 'EnhNivaaVis')]) #ftable(RegData[ , c(TidsVar, enhetsNivaa, 'Korona')], row.vars =TidsVar)
      TabTidEnh <- addmargins(TabTidEnh, FUN=list(Totalt=sum, 'Hele landet' = sum), quiet=TRUE)
      colnames(TabTidEnh)[ncol(TabTidEnh)] <- kolNavnSum
    }

    if (tilgangsNivaa != 'SC'){
      TabTidEnh <- cbind(TabTidEnh,
                         'Hele landet'= c(table(RegDataAlle$TidsVar), dim(RegDataAlle)[1]))}

  TabTidEnh <- xtable::xtable(TabTidEnh, digits=0, #method='compact', #align=c('l', rep('r', ncol(alderDIV))),
                              caption='Antall Coronatilfeller.')
if (valgtEnhet=='Alle'){valgtEnhet<-NULL}
  return(UtData <- list(Tab=TabTidEnh, utvalgTxt=c(valgtEnhet, UtData$utvalgTxt), Ntest=dim(RegData)[1]))
}




#' Antall som er  i ECMO/respirator
#'
#' @param RegData beredskapsskjema
#'
#' @return
#' @export
#'
statusNaaTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', erMann=9, bekr=9){

  UtData <- KoronaUtvalg(RegData=RegData, valgtEnhet=valgtEnhet,
                               erMann=erMann, bekr=bekr)
                              # dodSh=dodSh)$RegData velgAvd=velgAvd
RegData <- UtData$RegData
  N <- dim(RegData)[1]
  ##MechanicalRespirator Fått respiratorstøtte. Ja=1, nei=2,
inneliggere <- is.na(RegData$DateDischargedIntensive)
AntPaaShNaa <- sum(inneliggere) #N - sum(!(is.na(RegData$DateDischargedIntensive)))
LiggetidNaa <- as.numeric(difftime(Sys.Date(), RegData$Innleggelsestidspunkt[inneliggere], units='days'))
LiggetidNaaGjsn <- mean(LiggetidNaa[LiggetidNaa < 30], na.rm = T)

statusTab <- rbind(
  Liggetid = summary(LiggetidNaa)
)
# TabHjelp <- rbind(
#   'På ECMO nå' = c(AntIECMONaa*(c(1, 100/AntPaaIntNaa)), ECMOtidNaaGjsn),
#   'På respirator nå' = c(AntIrespNaa*(c(1, 100/AntPaaIntNaa)), ResptidNaaGjsn),
#   'På intensiv nå' = c(AntPaaIntNaa,'', LiggetidNaaGjsn))
# colnames(statusTab) <- c('Antall', 'Andel', 'Liggetid (gj.sn.)')
# statusTab[1:2,'Andel'] <- paste0(sprintf('%.0f', as.numeric(TabHjelp[1:2,'Andel'])),'%')
# statusTab[1:3, 3] <- paste0(sprintf('%.1f', as.numeric(TabHjelp[1:3, 3])), ' døgn')
xtable::xtable(statusTab,
               digits=0,
               #align = c('l','r','r','r'),
               caption='Korona på sykehus nå')
UtData <- list(Tab=statusTab, utvalgTxt=UtData$utvalgTxt, PaaShNaa=inneliggere)
return(UtData)
}



#' Ferdigstilte registreringer
#'
#' @param RegData beredskapsskjema
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
#'
FerdigeRegInnTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', bekr=9, erMann=9, dodSh=9){

UtData <- KoronaUtvalg(RegData=RegData, valgtEnhet=valgtEnhet,
                             bekr = bekr,
                             erMann = erMann,
                              skjemastatus=2)
RegData <- UtData$RegData
  N <- dim(RegData)[1]
  Liggetid <- summary(RegData$liggetid[RegData$liggetid < 30], na.rm = T)
  Alder <- summary(RegData$Alder, na.rm = T)
  AntDod <- sum(RegData$DischargedIntensivStatus==1, na.rm=T)

med_IQR <- function(x){
  #x[is.na(x)]<-0
  c(sprintf('%.1f',x[4]), sprintf('%.1f',x[3]), paste(sprintf('%.1f',x[2]), sprintf('%.1f',x[5]), sep=' - '))
  }
# x <- Liggetid
#  test <- sprintf('%.2f',c(x[2],x[5]))
# test <- med_IQR(ECMOtid)
TabFerdigeReg <- rbind(
    'Liggetid (døgn)' = c(med_IQR(Liggetid), N, ''),
    'Alder (år)' = c(med_IQR(Alder), N, ''),
    'Døde' = c('','','',AntDod, paste0(sprintf('%.f',100*AntDod/N),'%'))
  )
#TabFerdigeReg[TabFerdigeReg==NA]<-""
  colnames(TabFerdigeReg) <- c('Gj.sn', 'Median', 'IQR', 'Antall opphold', 'Andel opphold')
  TabFerdigeReg[c(1:2),'Andel opphold'] <-
    paste0(sprintf('%.0f', as.numeric(TabFerdigeReg[c(1:2),'Andel opphold'])),'%')
  xtable::xtable(TabFerdigeReg,
                 digits=0,
                 align = c('l','r','r','c', 'r','r'),
                 caption='Ferdigstilte opphold.
                 IQR (Inter quartile range) - 50% av oppholdene er i dette intervallet.')
  return(invisible(UtData <- list(Tab=TabFerdigeReg,
                                  utvalgTxt=UtData$utvalgTxt,
                                  Ntest=N)))
}



#' Tabell med oversikt over tilstander som medfører økt risiko ved Coronasmitte
#'
#' @param RegData data
#' @param datoTil sluttdato
#' @param reshID enhetens resh
#' @param tidsenhet 'Dag', 'Uke' (standard)
#' @param valgtEnhet 'Alle' (standard), RHF-navn uten 'Helse '
#'
#' @export
#' @return
RisikoInnTab <- function(RegData, tidsenhet='Totalt', datoTil=Sys.Date(), reshID=0,
                              erMann='', bekr=9, skjemastatus=9, dodSh=9,
                              valgtEnhet='Alle', enhetsNivaa='RHF',
                              minald=0, maxald=110, velgAvd=0){

  UtData <- KoronaUtvalg(RegData=RegData, datoFra=0, datoTil=0, erMann=erMann, #enhetsUtvalg=0, minald=0, maxald=110,
                             bekr=bekr, skjemastatus=skjemastatus,dodSh=dodSh,
                             minald=minald, maxald=maxald,
                             reshID=reshID, valgtEnhet=valgtEnhet) #velgAvd=velgAvd
  Ntest <- dim(UtData$RegData)[1]
  RegData <- UtData$RegData


  #Kvikk fix: Totalt gir nå totalen for 2020
  Tidsvariabel <- switch(tidsenhet,
                         Uke = paste0('uke',RegData$UkeNr),
                         Dag = RegData$Dag,
                         Totalt = RegData$Aar)

    ind <- RegData$Vekt>0 & RegData$Hoyde>0
    Fedme <- 100^2*(RegData$Vekt/(RegData$Hoyde)^2)[ind]

    N <- dim(RegData)[1] #Sjekk hvilke som kan benytte felles N

AntAndel <- function(Var, Nevner){c(sum(Var), sum(Var)/Nevner)}

#KjentRisikofaktor # 1-ja, 2-nei, 3-ukjent, -1 velg verdi
#NB: HER MÅ VI VELGE OM VI VIL HA ANDEL AV ALLE (UNDERESTIMAT) ELLER AV DE VI VET
  TabRisiko <- rbind(
    'Fedme (KMI>30), kjent' =	AntAndel(Fedme>30, sum(ind)),
    'Fedme (KMI>30), alle' =	AntAndel(Fedme>30, N),
    Kreft = AntAndel(RegData$Kreft, N),
    'Nedsatt immunforsvar/HIV' = AntAndel(RegData$NedsattimmunHIV, N),
    Diabetes	= AntAndel(RegData$Diabetes, N),
    Hjertesykdom = AntAndel(RegData$Hjertesykdom, N),
    Astma	= AntAndel(RegData$Astma, N),
    'Kronisk lungesykdom' = AntAndel(RegData$KroniskLungesykdom, N),
    Nyresykdom =	AntAndel(RegData$Nyresykdom, N),
    Leversykdom = AntAndel(RegData$Leversykdom, N),
    'Nevrologisk/nevromusk.' = AntAndel(RegData$KroniskNevro, N),
    Gravid	= AntAndel(RegData$Gravid, N),
    'Røyker' =	AntAndel(RegData$Royker, N),
    'Risikofaktorer (av sikre)' = AntAndel(RegData$KjentRisikofaktor==1, sum(RegData$KjentRisikofaktor %in% 1:2)),
    'Risikofaktorer (av alle)' = AntAndel(RegData$KjentRisikofaktor==1, N)
  )

  TabRisiko[,2] <- paste0(sprintf('%.0f', 100*(TabRisiko[ ,2])),'%')

  #if (Ntest>3){

  colnames(TabRisiko) <- c('Antall', 'Andel')
  # TabRisiko <- cbind(TabRisiko,
  #                    'Andel' = paste0(sprintf('%.0f', 100*TabRisiko[,"Sum"]/dim(RegData)[1]),'%')

  # xtable::xtable(TabRisiko,
  #                digits=0,
  #                align = c('l',rep('r',ncol(TabRisiko))),
  #                caption='Risikofaktorer')
  return(UtData <- list(Tab=TabRisiko, utvalgTxt=UtData$utvalgTxt, Ntest=Ntest))
}




#' Aldersfordeling, tabell
#'
#' @param RegData datatabell, beredskapsdata
#' @inheritParams KoronaUtvalg
#'
#' @return
#' @export
#'
#' @examples TabAlder(RegData=CoroData, enhetsNivaa='HF')
AlderTab <- function(RegData, valgtEnhet='Alle', enhetsNivaa='RHF', bekr=9,
                     skjemastatus=9, dodSh=9,erMann=9){

  #if (valgtEnhet != 'Alle'){RegData$RHF <- factor(RegData$RHF, levels=unique(c(levels(as.factor(RegData$RHF)), valgtEnhet)))}
  RegData$RHF <- as.factor(RegData$RHF)
  UtData <- KoronaUtvalg(RegData=RegData,
                             #valgtEnhet=valgtEnhet,
                             bekr=bekr,
                             dodSh = dodSh,
                             erMann = erMann,
                              skjemastatus=skjemastatus
                              )
  RegData <- UtData$RegData

 RegData$EnhetsNivaaVar <- RegData[ , enhetsNivaa]

N <- dim(RegData)[1]
gr <- seq(0, 90, ifelse(N<100, 25, 10) )
RegData$AldersGr <- cut(RegData$Alder, breaks=c(gr, 110), include.lowest=TRUE, right=FALSE)
grtxt <- if(N<100){c('0-24', '25-49', "50-74", "75+")} else {
                c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')}
#grtxt <- c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))#paste(gr,sep='-')
levels(RegData$AldersGr) <- grtxt #c(levels(RegData$AldersGr)[-length(gr)], paste0(max(gr),'+'))
TabAlder <- table(RegData$AldersGr, RegData$EnhetsNivaaVar)
TabAlder <- addmargins(TabAlder) #switch(enhetsNivaa, RHF = 'Totalt', HF = paste0(valgtEnhet, ', totalt'))

if (valgtEnhet == 'Ukjent') {
  TabAlder <- as.matrix(TabAlder[,ncol(TabAlder)], ncol=1) } else {
    if (valgtEnhet != 'Alle') {TabAlder <- TabAlder[,c(valgtEnhet, 'Sum')]}}
colnames(TabAlder)[ncol(TabAlder)] <- 'Hele landet'

return(invisible(UtData <- list(Tab=TabAlder, utvalgTxt=UtData$utvalgTxt)))
}

