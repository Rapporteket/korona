#' Make staging data for korona
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export

makeStagingData <- function() {

  KoroDataRaa <-  KoronaDataSQL(koble=1)
  BeredDataRaa <- intensivberedskap::NIRberedskDataSQL()

  KoroData <- KoronaPreprosesser(RegData = KoroDataRaa)
  KoroDataOpph <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0)
  BeredData <- intensivberedskap::NIRPreprosessBeredsk(RegData = BeredDataRaa)

  KoroData <- merge(KoroData,
                    BeredData,
                    all.x = T,
                    all.y = F,
                    suffixes = c("", "Bered"),
                    by = 'PersonId')
  KoroData  <- KoroData %>%
    dplyr::mutate(BeredPas = ifelse(is.na(PasientIDBered), 0, 1))


  rapbase::saveStagingData("korona", "KoroDataRaa", KoroDataRaa)
  rapbase::saveStagingData("korona", "BeredDataRaa", BeredDataRaa)
  rapbase::saveStagingData("korona", "KoroDataOpph", KoroDataOpph)
  rapbase::saveStagingData("korona", "BeredData", BeredData)
  rapbase::saveStagingData("korona", "KoroData", KoroData)

  invisible(rapbase::listStagingData("korona"))
}
