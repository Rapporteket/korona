#' Make staging data for korona
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export

makeStagingData <- function() {
  library(magrittr)
  library(dplyr)

  KoroDataRaa <-  KoronaDataSQL(koble=1) #, datoFra = '2022-06-01')

  KoroDataOpph <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0, kobleBered = 1)
  KoroData <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 1, tellFlereForlop = 1, kobleBered = 1)

  BeredDataRaa <- intensivberedskap::NIRberedskDataSQL()
  BeredData <- intensivberedskap::NIRPreprosessBeredsk(RegData = BeredDataRaa, aggPers = 1, tellFlereForlop = 1)

  rapbase::saveStagingData("korona", "KoroDataRaa", KoroDataRaa)
  rapbase::saveStagingData("korona", "BeredDataRaa", BeredDataRaa)
  rapbase::saveStagingData("korona", "KoroDataOpph", KoroDataOpph)
  rapbase::saveStagingData("korona", "BeredData", BeredData)
  rapbase::saveStagingData("korona", "KoroData", KoroData)

  invisible(rapbase::listStagingData("korona"))
}
