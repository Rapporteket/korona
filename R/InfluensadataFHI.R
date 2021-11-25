

#' Henter data og velger variabler for overføring til FHI
#'
#' @return
#' @export
#'
lagInfluDataFHI <- function(){
#Rådata
library(intensivberedskap) #  library(tidyverse) #

  queryInflu <- 'SELECT
              PersonIdBC19Hash
                ,PatientInRegistryGuid
                ,PatientAge
                ,PatientGender
                ,HF
                ,RHF
                ,MunicipalNumber
                ,CreationDate
                ,DateAdmittedIntensive
                ,DateDischargedIntensive
                ,DaysAdmittedIntensiv
                ,DischargedIntensiveStatus
                ,FormDate
                ,FormStatus
                ,ICD10_1
                ,IsEcmoTreatmentAdministered
                ,IsRiskFactor
                #,IsActiveSmoker - SJEKK OM SKAL MED
                ,IsAsthmaticPatient
                ,IsCancerPatient
                ,IsChronicLungDiseasePatient
                ,IsChronicNeurologicNeuromuscularPatient
                ,IsDiabeticPatient
                ,IsHeartDiseaseIncludingHypertensionPatient
                ,IsImpairedImmuneSystemIncludingHivPatient
                ,IsKidneyDiseaseIncludingFailurePatient
                ,IsLiverDiseaseIncludingFailurePatient
                ,IsObesePatient
                ,IsPregnant
                ,MechanicalRespirator
                ,Morsdato
                ,RiskFactor
 FROM InfluensaFormDataContract'
  #queryInflu <- 'select * from InfluensaFormDataContract'
InfluDataRaa <-  rapbase::loadRegData(registryName = "nir", query = queryInflu, dbType = "mysql")
  #setdiff(names(InfluDataAlle), names(InfluDataRaa))

UtData <- InfluDataRaa #list(InfluDataFHI = InfluDataRaa)

return(UtData)
}
