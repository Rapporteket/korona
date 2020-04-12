## code to prepare `DATASET` dataset goes here


# ReshNivaa <- read.table('data-raw/EnhetsnivaaerResh.csv', sep=';',
#                         stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
ReshNivaa <- read.table(system.file(file.path('extdata', 'EnhetsnivaaerResh.csv'), package = 'korona'), sep=';',
                        stringsAsFactors=FALSE, header=T, fileEncoding = 'latin1')

usethis::use_data(ReshNivaa, overwrite = TRUE, internal = FALSE)

kodebok_inklusjon <- xlsx::read.xlsx2('I:/korona/Kodebok_pandemiregisteret01042020.xlsx', sheetIndex = '2. Pandemiskjema. Skjemaversjon')
kodebok_utskrivning <- xlsx::read.xlsx2('I:/korona/Kodebok_pandemiregisteret01042020.xlsx', sheetIndex = '1. UtskrivningSkjema. Skjemaver')

kodebok <- list("inklusjon" = kodebok_inklusjon, "utskrivning" = kodebok_utskrivning)

usethis::use_data(kodebok, overwrite = TRUE, internal = FALSE)



#IndBeskr <- readxl::read_excel("data-raw/Indikatorbeskrivelser.xlsx")
#usethis::use_data(IndBeskr, overwrite = TRUE)

belegg_ssb <- read.table(system.file(file.path('extdata', 'BeleggSSB.csv'), package = 'korona'), sep=';',
                         stringsAsFactors=FALSE, header=T, fileEncoding = 'latin1')
names(belegg_ssb)[names(belegg_ssb)=="Døgnplasser.2018"] <- "Dognplasser.2018"

belegg_ssb$HFresh <- ReshNivaa$HFresh[pmatch(trimws(tolower(belegg_ssb$region)), trimws(tolower(ReshNivaa$HFnavn)))]
belegg_ssb$HF <- ReshNivaa$HFnavn[pmatch(trimws(tolower(belegg_ssb$region)), trimws(tolower(ReshNivaa$HFnavn)))]

# belegg_ssb$region[is.na(belegg_ssb$HFresh)]
# unique(ReshNivaa[!(ReshNivaa$HFresh %in% belegg_ssb$HFresh), c("HFnavn", "HFresh")])
belegg_ssb$HFresh[belegg_ssb$region == "Finnmarkssykehuset HF"] <- 101971
belegg_ssb$HFresh[belegg_ssb$region == "Universitetssykehuset Nord-Norge HF"] <- 101719
belegg_ssb$HFresh[belegg_ssb$region == "Helse Nord Trøndelag HF"] <- 100317
belegg_ssb$HFresh[belegg_ssb$region == "St Olavs Hospital HF"] <- 100320
belegg_ssb$HFresh[belegg_ssb$region == "Helse Møre og Romsdal HF (2011-)"] <- 4201115
belegg_ssb$HFresh[belegg_ssb$region == "Vestre Viken HF (2009-)"] <- 700272
belegg_ssb$HFresh[belegg_ssb$region == "Oslo Universitetssykehus HF (2009-)"] <- 4001031
belegg_ssb$HFresh[belegg_ssb$region == "Haugesund Sanitetsforenings Revmatismesykehus AS"] <- 106834
belegg_ssb$HFresh[belegg_ssb$region == "Stiftelsen Betanien Bergen"] <- 4216267
belegg_ssb$HFresh[belegg_ssb$region == "NKS Jæren Distriktspsykiatriske senter AS"] <- 106819
belegg_ssb$HFresh[belegg_ssb$region == "NKS Olaviken alderspsykiatriske sykehus AS"] <- 106816

belegg_ssb$HF <- ReshNivaa$HFnavn[match(belegg_ssb$HFresh, ReshNivaa$HFresh)]
belegg_ssb$RHF <- ReshNivaa$RHFnavn[match(belegg_ssb$HFresh, ReshNivaa$HFresh)]

usethis::use_data(belegg_ssb, overwrite = TRUE, internal = FALSE)
