% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KoronaPreprossesser.R
\name{KoronaPreprosesser}
\alias{KoronaPreprosesser}
\title{Preprosesser data fra Koronaregisteret}
\usage{
KoronaPreprosesser(
  RegData = RegData,
  aggPers = 1,
  kobleBered = 0,
  tellFlereForlop = 0
)
}
\arguments{
\item{RegData}{Koronaskjema}

\item{aggPers}{1: aggregere til personnivå (standard), 0: ikke aggregere}

\item{kobleBered}{Koble data med beredskapsdata? 0: nei(standard), 1:ja. NB: Kobler bare til ett beredskapsskjema. Kan ha flere.}

\item{tellFlereForlop}{0: aggregerer til personnivå
1: Identifiserer inntil 3 forløp per person}
}
\value{
Preprosesserte data
}
\description{
Denne funksjonen navner om variabler og beregner evt. nye.
}
