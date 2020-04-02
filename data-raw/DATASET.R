## code to prepare `DATASET` dataset goes here


# ReshNivaa <- read.table('data-raw/EnhetsnivaaerResh.csv', sep=';',
#                         stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
ReshNivaa <- read.table(system.file(file.path('extdata', 'EnhetsnivaaerResh.csv'), package = 'korona'), sep=';',
                        stringsAsFactors=FALSE, header=T, fileEncoding = 'latin1')

usethis::use_data(ReshNivaa, overwrite = TRUE, internal = TRUE)

#IndBeskr <- readxl::read_excel("data-raw/Indikatorbeskrivelser.xlsx")
#usethis::use_data(IndBeskr, overwrite = TRUE)
