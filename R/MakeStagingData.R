#' Make staging data for korona
#'
#' This function makes queries and pre-processing of registry data before
#' storing relevant staging data. Running this function may take a while so use
#' with care!
#'
#' @return Character vector of staging files, invisibly
#' @export

makeStagingData <- function() {

  koroDataRaa <-  KoronaDataSQL(koble=1)
  beredDataRaa <- intensivberedskap::NIRberedskDataSQL()

  koroData <- KoronaPreprosesser(RegData = koroDataRaa)
  koroDataOpph <- KoronaPreprosesser(RegData = koroDataRaa, aggPers = 0)
  beredData <- intensivberedskap::NIRPreprosessBeredsk(RegData = beredDataRaa)

  koroData <- merge(koroData,
                    beredData,
                    all.x = T,
                    all.y = F,
                    suffixes = c("", "Bered"),
                    by = 'PersonId')
  koroData  <- koroData %>%
    dplyr::mutate(BeredPas = ifelse(is.na(PasientIDBered), 0, 1))


  rapbase::saveStagingData("korona", "koroDataRaa", koroDataRaa)
  rapbase::saveStagingData("korona", "beredDataRaa", beredDataRaa)
  rapbase::saveStagingData("korona", "koroDataOpph", koroDataOpph)
  rapbase::saveStagingData("korona", "beredData", beredData)
  rapbase::saveStagingData("korona", "koroData", koroData)

  invisible(rapbase::listStagingData("korona"))
}
