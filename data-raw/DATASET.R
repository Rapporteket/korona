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
