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

  KoroDataRaa <-  KoronaDataSQL(koble=1) #28.feb-23: 2.2s

  KoroDataOpph <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0, kobleBered = 1) #28.feb-23: 14.8s
  KoroData <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 1, tellFlereForlop = 1, kobleBered = 1) #28.feb-23: 44min.

  BeredDataRaa <- intensivberedskap::NIRberedskDataSQL() #28.feb-23: 0.14
  BeredData <- intensivberedskap::NIRPreprosessBeredsk(RegData = BeredDataRaa, aggPers = 1, tellFlereForlop = 1) #28.feb-23: 17.2

  rapbase::saveStagingData("korona", "KoroDataRaa", KoroDataRaa) #28.feb-23: 0.7s
  rapbase::saveStagingData("korona", "BeredDataRaa", BeredDataRaa) #28.feb-23: 0.3s
  rapbase::saveStagingData("korona", "KoroDataOpph", KoroDataOpph) #28.feb-23: 0.8s
  rapbase::saveStagingData("korona", "BeredData", BeredData) #28.feb-23: 0.04s
  rapbase::saveStagingData("korona", "KoroData", KoroData)#28.feb-23: 0.7s

  invisible(rapbase::listStagingData("korona"))
}
