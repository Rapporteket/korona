#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(magrittr)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(sship)
library(intensivberedskap)
library(korona)

## Forsikre om at reshNivaa blir lest inn med korrekt encoding:
# ReshNivaa <- read.table(system.file(file.path('extdata', 'EnhetsnivaaerResh.csv'), package = 'korona'), sep=';',
#                         stringsAsFactors=FALSE, header=T, fileEncoding = 'latin1')
# usethis::use_data(ReshNivaa, overwrite = TRUE, internal = FALSE)
# ###################################################################

shiny::addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

regTitle <- paste0('Koronaregistreringer, pandemi 2020',
                   ifelse(context=='QA', 'QA',''))

#---------Hente data------------
  #Mange av variablene på ut-skjema er med i inn-dumpen
  #Variabler fra utskjema som er med i innskjema i datadump er fra ferdigstilte utregistereringer

  KoroDataRaa <- rapbase::loadStagingData("korona", "KoroDataRaa") #Benyttes i appen
  if (isFALSE(KoroDataRaa)) {
    KoroDataRaa <-  KoronaDataSQL(koble=1)
    rapbase::saveStagingData("korona", "KoroDataRaa", KoroDataRaa)
  }

KoroDataOpph <- rapbase::loadStagingData("korona", "KoroDataOpph")
if (isFALSE(KoroDataOpph)) {
   KoroDataOpph <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 0)
   rapbase::saveStagingData("korona", "KoroDataOpph", KoroDataOpph)
 }

 BeredData <- rapbase::loadStagingData("korona", "BeredData")
 if (isFALSE(BeredData)) {
   BeredDataRaa <- intensivberedskap::NIRberedskDataSQL()
   BeredData <- intensivberedskap::NIRPreprosessBeredsk(RegData = BeredDataRaa, aggPers = 1)
   rapbase::saveStagingData("korona", "BeredData", BeredData)
 }

 KoroData <- rapbase::loadStagingData("korona", "KoroData")
 if (isFALSE(KoroData)) {
   KoroData <- KoronaPreprosesser(RegData = KoroDataRaa, aggPers = 1)
 KoroData <- merge(KoroData,
                   BeredData,
                   all.x = T,
                   all.y = F,
                   suffixes = c("", "Bered"),
                   by = 'PersonId')
 KoroData  <- KoroData %>%
   dplyr::mutate(BeredPas = ifelse(is.na(PasientIDBered), 0, 1))
rapbase::saveStagingData("korona", "KoroData", KoroData)
}

#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------

#Definere utvalgsinnhold
rhfNavn <- c('Alle', as.character(sort(unique(KoroData$RHF))))
hfNavn <- c('Alle', sort(unique(KoroData$HF))) #KoroData$HF, index.return=T)
enhetsNavn <- rhfNavn
dum <- unique(KoroData[,c('HF', "HFresh")])
HFreshValg <- dum$HFresh
names(HFreshValg) <- dum$HF
HFreshValg <- HFreshValg[order(dum$HF)]

#updateTextInput(session, inputId, label = NULL, value = NULL). Hvis input skal endres som følge av et annet input.
startDato <- as.Date('2020-03-01') #min(KoroData$InnDato, na.rm = T)
sluttDato <- Sys.Date()

aarsakInnValg <- c(
  "Ja, minst siste opphold" = 2,
  "Ja, alle opphold"=1,
  "Ja, minst ett opph" = 3,
  "Alle registrerte"=0,
  "Nei, ingen opphold" = 4)
  #original variabel: c("Ja"=1, "Alle"=9, "Nei"=2)


#last modul(er)
source(system.file("shinyApps/korona/R/resultatmodul.R", package = "korona"), encoding = 'UTF-8')

ui <- tagList(
  navbarPage(id='hovedark',
             title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                         regTitle),
             windowTitle = regTitle,
             theme = "rap/bootstrap.css",


#-------------Startside--------------
             tabPanel("Oversikt",
                      useShinyjs(),
                      sidebarPanel(id = 'brukervalgStartside',
                                   width = 3,
                                   uiOutput('KoroRappTxt'),
                                   downloadButton(outputId = 'KoroRapp.pdf', label='Last ned Koronarapport', class = "butt"),
                                   tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                                   br(),
                                   br(),
                                   h3('Gjør filtreringer/utvalg:'),

                                   selectInput(inputId = "valgtEnhet", label="Velg enhet",
                                               choices = 'Alle'
                                   ),
                                    selectInput(inputId = "aarsakInn", label="Covid-19 hovedårsak til innleggelse?",
                                               choices = aarsakInnValg
                                    ),
                                   dateRangeInput(inputId = "valgtDato", label = "Tidsperiode",
                                                  start = startDato, end = Sys.Date(),
                                                  separator="t.o.m.", language="nb"),

                                   selectInput(inputId = "skjemastatusInn", label="Skjemastatus, inklusjon",
                                               choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                   ),
                                   selectInput(inputId = "dodSh", label="Utskrevne, tilstand",
                                               choices = c("Ikke valgt"=9,"Levende og døde"=3,  "Død"=2, "Levende"=1)
                                   ),
                                   selectInput(inputId = "erMann", label="Kjønn",
                                               choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                   ),
                                   h4('Kun for risikofaktorer:'),
                                   sliderInput(inputId="alder", label = "Alder",
                                               min = 0, max = 110,
                                               value = c(0, 110),
                                               step = 10
                                   ),
                                   br(),
                                   actionButton("tilbakestillValg", label="Tilbakestill valg"),
                                   br(),
                                   selectInput(inputId = "bildeformatAldKj",
                                               label = "Velg format for nedlasting av figur",
                                               choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))

                      ),
                      mainPanel(width = 9,
                                appNavbarUserWidget(user = uiOutput("appUserName"),
                                                    organization = uiOutput("appOrgName"),
                                                    addUserInfo = TRUE),
                                tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                                uiOutput('manglerRegResh'),
                                h3('Resultater fra pandemiregistrering, korona.'),
                                uiOutput('antFlereForl'),
                                h5('Merk at resultatene kan inkludere ufullstendige registreringer'),
                                h4('Sidene er organisert i faner. Mer detaljert informasjon fra registreringer i
                                   pandemiregisteret finnes under fanen "Resultater".'),
                                #h5('Siden er under utvikling... ', style = "color:red"),
                                br(),
                                fluidRow(
                                  column(width = 6,
                                         h3('Status nå'),
                                         uiOutput('utvalgNaa'),
                                         tableOutput('statusNaaShTab'),
                                         #h6('Flere variabler?', style = "color:red"),
                                         hr(),
                                         h4('WALL OF SHAME'),
                                         column(width=2,
                                         tableOutput('skjemaInnKladdTab')),
                                         column(width=2, offset=5,
                                                tableOutput('skjemaUtKladdTab')                                                )
                                  ),
                                  column(width=5, offset=1,
                                         uiOutput('tittelFerdigeReg'),
                                         uiOutput('utvalgFerdigeReg'),
                                         tableOutput('tabFerdigeReg')
                                  )),

                                fluidRow(
                                  #column(width=4,
                                h3('Antall ny-innlagte siste 10 dager'),
                                h5('Overføringer telles ikke med'),
                                uiOutput('utvalgAntOpph'),
                                tableOutput('tabAntOpph')
                                  ),
                                fluidRow(#column(width=5, offset = 3,
                                       br(),
                                       h3('Antall utskrevne siste 10 dager'),
                                       uiOutput('utvalgAntUtskr'),
                                       br(),
                                       tableOutput('tabAntUtskr')
                                ),
                                br(),
                                fluidRow(
                                  column(width=3,
                                         h3('Risikofaktorer'),
                                         uiOutput('utvalgRisiko'),
                                         tableOutput('tabRisikofaktorer')),
                                  column(width=5, offset=1,
                                         h3('Aldersfordeling'),
                                         plotOutput("FigurAldersfordeling", height="auto"),
                                         downloadButton('LastNedFigAldKj', label='Last ned figur'),
                                         h5('Velg figurformat i nedtrekksmeny i venstre panel'),
                                         downloadButton("lastNedAldKj", "Last ned tabell")
                                  ))
                      ) #main
             ), #tab Startside

#-----------Resultater-------------------------------------
             tabPanel("Resultater",
                      #tags$style(HTML(".tabbable > .nav > li > a {background-color: #DBDBDB;  color:black; width: 300PX;}")),
                      tabsetPanel(

                        tabPanel("Ant. pasienter",
                                 koronaresultater_UI("resultater_id")
                        ),
                        tabPanel("Ant. opphold",
                                 sidebarPanel(
                                   selectInput(inputId = 'enhetsNivaaOpph', label='Velg enhetsnivå',
                                               choices = c("Sykehus"='ShNavn',
                                                           'HF' = 'HF',
                                                           'RHF' = 'RHF')
                                   ),
                                   selectInput(inputId = 'tidsenhetOpph', label='Velg tidsenhet',
                                               selected = 'Mnd',
                                               choices = c("Måned"='Mnd',
                                                           'Kvartal' = 'Kvartal',
                                                           'År' = 'Aar')
                                 ),
                                 dateInput(inputId = 'tilDatoOpph', label = 'Velg sluttdato',
                                           min = '2020-03-15', max = Sys.Date()),
                                 sliderInput(inputId = 'antTidsenhOpph', label = 'Velg antall hele måneder/kvartal/år forut for "sluttdato"',
                                             value = 6, step = 1,
                                             min = 1, max = 13)
                                 ),
                                 mainPanel(
                                   h2('Antall opphold i valgt tidsperiode'),
                                   tableOutput('tabAntOpphEnhTid')
                                 )
                        ),
                        tabPanel("Belegg",
                                 koronabelegg_UI("koronabelegg_id")
                        ),
    #---------Fordelinger-------------
                        tabPanel(p('Fordelinger',
                                   title='Figurer/tabeller for de fleste opplysninger registrert i
                  inlusjons- eller utskrivingsskjema'),
                                 value = 'Fordelinger',
                                 sidebarPanel(id = 'brukervalgRes',
                                              width = 3,
                                              br(),
                                              h3('Velg variabel/tema og filtreringer i data'),
                                              #conditionalPanel(condition = "input.ark == 'Fordelinger' ",
                                              selectInput(inputId = 'valgtVarFord', label='Velg variabel',
                                                          selected = 'regForsinkelseInn',
                                                          choices = c("Alder"='alder',
                                                                      'Covid-19 hovedårsak til innleggelse?' = 'aarsakInn4kat',
                                                                      'Demografi' = 'demografi',
                                                                      "Liggetid"='liggetid',
                                                                      'Risikofaktorer, innleggelse'='risikoInn',
                                                                      'Antibiotika, innleggelse'='antibiotikaInn',
                                                                      'Antibiotika, utskriving'='antibiotikaUt',
                                                                      'Registreringsforsinkelse, inn' = 'regForsinkelseInn',
                                                                      'Registreringsforsinkelse, ut' = 'regForsinkelseUt',
                                                                      'Respirasjonssvikt, innleggelse' = 'respSviktInn',
                                                                      'Respirasjonssvikt på sykehus' = 'respSviktUt',
                                                                      'Sirkulasjonssvikt, innleggelse' = 'sirkSviktInn',
                                                                      'Sirkulasjonssvikt på sykehus' = 'sirkSviktUt',
                                                                      'Tilstand ved innleggelse' = 'tilstandInn'
                                                          )
                                              ),
                                              selectInput(inputId = "enhetsUtvalgFord", label="Velg enhetsnivå",
                                                          choices = c('Valgt enhet mot resten'=1, 'Hele landet'=0, 'Valgt enhet'=2)
                                              ),
                                              selectInput(inputId = "valgtEnhetRes", label="Velg enhet",
                                                          choices = 'Alle'
                                              ),
                                              dateRangeInput(inputId = "valgtDatoRes", label = "Tidsperiode",
                                                             start = startDato, end = Sys.Date(),
                                                             separator="t.o.m.", language="nb"),
                                              selectInput(inputId = "aarsakInnRes", label="Covid-19 hovedårsak til innleggelse?",
                                                          choices = aarsakInnValg
                                              ),
                                              selectInput(inputId = "skjemastatusInnRes", label="Skjemastatus, inklusjon",
                                                          choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                                              ),
                                              selectInput(inputId = "dodShRes", label="Utskrevne, tilstand",
                                                          choices = c("Ikke valgt"=9,"Levende og døde"=3,  "Død"=2, "Levende"=1)
                                              ),
                                              selectInput(inputId = "beredPasRes", label="Intensivpasient?",
                                                          choices = c("Alle pasienter"=9, "Ja"=1, "Nei"=0)
                                              ),
                                              br(),selectInput(inputId = "erMannRes", label="Kjønn",
                                                          choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                                              ),
                                              selectInput(inputId = "bildeformatFord",
                                                          label = "Velg format for nedlasting av figur",
                                                          choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                                              br(),
                                              actionButton("tilbakestillValgRes", label="Tilbakestill valg")

                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel(
                                       'Figur',
                                       plotOutput('fordelinger', height="auto"),
                                       downloadButton('LastNedFigFord', label='Velg format (til venstre) og last ned figur')
                                       ),
                                     tabPanel(
                                       'Tabell',
                                       uiOutput("tittelFord"),
                                       tableOutput('fordelingTab'),
                                       downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell')
                                     )
                                   )
                                 ) #mainPanel
                        ), #Fordelinger
    #------------------Andeler-----------------
    tabPanel(p('Utvikling over tid (andeler)',
               title='Resultater som kan vises som andel per tidsenhet'),
             value = 'Andeler',
             sidebarPanel(id = 'brukervalgAndel',
                          width = 3,
                          br(),
                          h3('Velg variabel/tema og filtreringer i data'),
                          #conditionalPanel(condition = "input.ark == 'Fordelinger' ",
                          selectInput(inputId = 'valgtVarAndel', label='Velg variabel',
                                      selected = 'regForsinkelseInn',
                                      choices = c("Alder under 18 år"='alder_u18',
                                                  "Alder under 40 år"='alder_u40',
                                                  "Alder over 60 år"='alder_o60',
                                                  "Alder over 80 år"='alder_o80',
                                                  "Isolert ved ankomst" = 'isolertInn',
                                                  'Intensivpasient' = 'beredPas',
                                                  'Døde' = 'dodSh'
                                      )
                          ),
                          dateRangeInput(inputId = "valgtDatoAndel", label = "Tidsperiode",
                                         start = startDato, end = Sys.Date(),
                                         separator="t.o.m.", language="nb"),
                          selectInput(inputId = "aarsakInnAndel", label="Covid-19 hovedårsak til innleggelse?",
                                      choices = aarsakInnValg
                          ),
                          selectInput(inputId = "dodShAndel", label="Utskrevne, tilstand",
                                      choices = c("Ikke valgt"=9,"Levende og døde"=3,  "Død"=2, "Levende"=1)
                          ),
                          selectInput(inputId = "beredPasAndel", label="Intensivpasient?",
                                      choices = c("Alle pasienter"=9, "Ja"=1, "Nei"=0)
                          ),
                          selectInput(inputId = "erMannAndel", label="Kjønn",
                                           choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                          ),
                          selectInput(inputId = "bildeformatAndel",
                                      label = "Velg format for nedlasting av figur",
                                      choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')
                                      ),
                          selectInput(inputId = "tidsenhetAndel", label="Velg tidsenhet",
                                      choices = rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                                                      'Kvartal'='Kvartal', 'Måned'='Mnd'))),
                          br(),
                          actionButton("tilbakestillValgAndel", label="Tilbakestill valg"),
                          br()
                          #p(em('Følgende utvalg gjelder bare figuren som viser utvikling over tid')),
                          #selectInput(inputId = 'enhetsUtvalgAndel', label='Egen enhet og/eller landet',
                          #            choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
             ),
             mainPanel(
               #tabsetPanel(
                 #tabPanel(
                   'Figur',
                   plotOutput('andelTid', height="auto"),
                   downloadButton('LastNedFigAndelTid', label='Velg format (til venstre) og last ned figur')
             ) #main
    ) #tabset & Andeler
)), #tabset og Resultater


#----------Datakvalitet-------------------------
tabPanel('Datakvalitet',
  tabsetPanel(
    tabPanel('Manglende ut-skjema',
         h3('Innleggelsesskjema som mangler utskrivning'),
         downloadButton(outputId = 'lastNed_innManglerUt', label='Last ned tabell'),
         tableOutput('innManglerUtTab')
         ),
    tabPanel('Dobbeltregistrering av inn-skjema',
             h3('Pasienter som har to innleggelsesskjema med like innleggelsestidspunkt (<30 min.) '),
             downloadButton(outputId = 'lastNed_dblInn', label='Last ned tabell'),
             tableOutput('dblInn')
             )

)), #Datakvalitet
#---------Intensivregistreringer--------------------------------
             tabPanel(p('Intensivpasienter',
                        title='Resultater fra koronaregistrering i intensivregisteret'),
                      value = 'Intensiv',

                      sidebarPanel(id = 'intensiv',
                                   width = 3,
                                   br(),
                                   br(),
                                   h3('Gjør filtreringer/utvalg:'),

                                   selectInput(inputId = "bekrInt", label="Bekreftet/Mistenkt",
                                               choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                                   )
                      ),
                      mainPanel(width = 9,
                                h3('Resultater fra koronaregistrering på INTENSIVavdelinger.'),
                                h4('Mer detaljerte resultater fra intensivavdlingene
                               finnes på Rapporteket-NIR-Beredskap og på Rapporteket-Intensiv'),
                                #h4('Husk at andre tilgangsnivåer/resh enn i Rapporteket-Beredskap', style = "color:red"),
                                #h5('Siden er under utvikling... ', style = "color:red"),
                                br(),
                                fluidRow(
                                  column(width = 4,
                                         h4('Opphold uten registrert ut-tid fra intensiv'), #, align='center'),
                                         uiOutput('utvalgIntensivNaa'),
                                         tableOutput('tabIntensivNaa')
                                  ),
                                  column(width=5, offset=1,
                                         uiOutput('tittelFerdigeRegInt'), #Ta med utvalg i tittel?
                                         uiOutput('utvalgFerdigeRegInt'),
                                         tableOutput('tabFerdigeRegInt')
                                  )),

                                h3('Antall intensivopphold'),
                                h5('Innleggelser siste to uker, samt totalt'),
                                uiOutput('utvalgAntRegInt'),
                                tableOutput('tabAntRegInt'),
                                br(),
                                fluidRow(
                                  column(width=3,
                                         h3('Risikofaktorer, intensiv'),
                                         uiOutput('utvalgRisikoInt'),
                                         tableOutput('tabRisikofaktorerInt')),
                                  column(width=5, offset=1,
                                         h3('Aldersfordeling, intensiv'),
                                         uiOutput('utvalgAlderInt'),
                                         tableOutput('tabAlderInt')
                                  ))
                      )
             ), #Intensiv-side

#-----------Abonnement ny--------------------------------
tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av rapporter på e-post'),
         value = 'Abonnement',
         sidebarLayout(
           sidebarPanel(width = 3,
                        selectInput("subscriptionRep", "Dokument:", c("Koronarapport")), #Evt legg til influensarapport
                        selectInput("subscriptionFreq", "Frekvens:",
                                    list(Månedlig="Månedlig-month",
                                         Ukentlig="Ukentlig-week",
                                         Daglig="Daglig-DSTday"),
                                    selected = "Ukentlig-week"),
                        actionButton("subscribe", "Bestill!",icon = shiny::icon("paper-plane")),
                        br(),
                        br(),
                        br()
           ),
           mainPanel(
             h4('NB: Abonnementet løper til det sies opp. '),
             uiOutput("subscriptionContent")
           )
         )
), #tab abonnement


#----------Registeradministrasjon----------------------------------
tabPanel(p("Registeradm",
           title='Side som bare vises for Marianne S., Eivind, Eirik og Reidar'),
         value = 'Registeradm',
         sidebarLayout(
           sidebarPanel(width = 4,
                        h4('Last ned data'),
                        br(),
                        downloadButton(outputId = 'lastNed_dataPandemiRaa',
                                       label='Last ned ubesudlede pandemidata', class = "butt"),
                        downloadButton(outputId = 'lastNed_dataPandemiPas',
                                       label='Last ned pandemidata, pasientaggregert', class = "butt"),
                        br(),
                        br(),
                        br(),
                        h4('Data til FHI'),
                        selectInput("hvilkeFilerTilFHI", "Data:", c("Pandemi, beredskap og influensa" = "DataFHIPanBeredInflu", #c("Pandemi og beredskap" = "DataFHIPanBered",
                                                                    "Testfil" = "Testfil")),
                        actionButton("bestillDataTilFHI", "Bestill data til FHI"),
                        br(),
                        downloadButton(outputId = 'lastNed_filstiDataNHN',
                                       label='Send filer til NHN og last ned filsti', class = "butt"),
                        #),
                        br(),
                        br(),
                        br(),
                        h4('Lage abonnementslister for utsendinger'),
                        uiOutput("reportUts"),
                        uiOutput("freqUts"),
                        uiOutput("HFreshUts"),
                        uiOutput("rolleUts"),
                        h5('E-postmottagere legges inn en og en. Trykk legg til e-postmottager for hver gang.
                           Når du har lagt til alle, trykker du på lag utsending. '),
                                   textInput("email", "Epostmottakere:"),
                                   uiOutput("editEmail"),
                                   htmlOutput("recipients"),
                                   tags$hr(),
                                   uiOutput("makeDispatchment") #utsending
                      ),
                      mainPanel(
                        uiOutput("dispatchmentContent")
                      )
                    )
         ), #tab registeradm.
    shiny::tabPanel(
      "Eksport",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::exportUCInput("koronaExport")
        ),
        shiny::mainPanel(
          rapbase::exportGuideUI("koronaExportGuide")
        )
      )
    )

  ) # navbarPage
) # tagList
#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Last inn data

  #-----------Div serveroppstart------------------
  if (context %in% c('QA', 'PRODUCTION')){
    rapbase::appLogger(session = session, msg = "Starter Pandemi-app")}

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 100082) # 100089

  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavnDummy')
  output$brukernavn <- renderText(brukernavn)

  finnesEgenResh <- reshID %in% unique(KoroData$HFresh)
  egetHF <- 'ReshUreg'
  egetRHF <- 'ReshUreg'
  if (finnesEgenResh) {
    indReshEgen <- match(reshID, KoroData$HFresh) #Her skal benyttes HF-resh
    egetRHF <- as.character(KoroData$RHF[indReshEgen])
    egetHF <- as.character(KoroData$HF[indReshEgen])
  }

  #Filtreringsnivå for data:
  egetEnhetsNivaa <- switch(rolle, SC = 'RHF', LC = 'RHF', LU = 'HF')
  egenEnhet <- switch(rolle, SC='Alle', LC=egetRHF, LU=egetHF) #For LU vil reshID benyttes

  #observe({
    if (rolle != 'SC') {
    shinyjs::hide(id = 'KoroRappInt.pdf')
    shinyjs::hide(id = 'KoroRappTxtInt')
    }
    if (!(brukernavn %in% c('lenaro', 'aed0903unn', 'kevin.thon'))){
      shinyjs::hide(id = 'bestillDataTilFHI')
      shinyjs::hide(id = 'hvilkeFilerTilFHI')
      shinyjs::hide(id = 'lastNed_filstiDataNHN')
    }

  if (!(brukernavn %in% c('lenaro', 'aed0903unn', 'kevin.thon',
                          'eabu', 'MarianneSaevik', 'eivh', 'anif', 'helkri'))) {
    hideTab(inputId = "hovedark", target = "Registeradm")
  }
  #})

  # SC kan velge blant RHF, Resten kan bare velge EGEN ENHET/ALLE
  enhetsvalg <- if (rolle=='SC'){c('Alle', rhfNavn)} else {c(egenEnhet,'Alle')}
  #if (rolle != 'SC') {
  updateSelectInput(session, "valgtEnhet",
                    choices = enhetsvalg)
  updateSelectInput(session, "valgtEnhetRes",
                    choices = enhetsvalg)

  #Telle pasienter med flere forløp
  KoroDataOpph$Dato <- as.Date(KoroDataOpph$FormDate)
  PasFlere <- KoroDataOpph %>% group_by(PasientID) %>%
    summarise(.groups = 'drop',
              InnNr0 = ifelse(Dato-min(Dato)>90, 2, 1))
  antPasFlereForlAlle <- sum(PasFlere$InnNr0>1)

  PasFlere <- KoroDataOpph %>% dplyr::filter(ArsakInnleggelse==1) %>%
    group_by(PasientID) %>%
    summarise(.groups = 'drop',
              InnNr0 = ifelse(Dato-min(Dato)>90, 2, 1))
  antPasFlereForl <- sum(PasFlere$InnNr0>1)

  output$antFlereForl <- renderUI(h5(HTML(paste0('Resultatene er stort sett basert på antall pasienter. Det betyr at alle opphold
  for overflyttede eller reinnlagte pasienter er aggregerte til ett forløp per pasient.
  Det er ikke tatt hensyn til at en pasient kan ha flere Covid-forløp.
Per i dag er det på landsbasis ', antPasFlereForlAlle, ' som har mer enn ett forløp
og ', antPasFlereForl, ' av disse har mer enn ett forløp med Covid-19 som hovedårsak.'))))



  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle, ', bruker: ', brukernavn,
                                           '<br> ReshID: ', reshID) )}
  #,'<br> Org: ', egenOrg) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session, callerPkg = "korona")
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })


  #-------- Laste ned Samlerapporter------------
  observe({
    #valgtEnhet <- ifelse(rolle == 'LU', egetRHF, as.character(input$valgtEnhet))
    output$KoroRapp.pdf <- downloadHandler(
      filename = function(){
        paste0('KoronaRapport', Sys.time(), '.pdf')},
      content = function(file){
        henteSamlerapporterKorona(file, rnwFil="KoronaRapport.Rnw",
                                  rolle = rolle,
                                  valgtEnhet = egenEnhet,
                                  enhetsNivaa = egetEnhetsNivaa,
                                  reshID = reshID
        ) #Vurder å ta med tidsinndeling eller startdato
      }
    )
  })

  output$KoroRappTxt <- renderUI(tagList(
    h3(HTML('Koronarapport med samling av resultater')),
     h5(HTML('Koronarapporten kan man få regelmessig tilsendt på e-post.
                     Gå til fanen "Abonnement" for å bestille dette'))
    ))

  #----------Dæsjbord, Korona----------------------------

  observeEvent(input$tilbakestillValg, shinyjs::reset("brukervalgStartside"))
  observeEvent(input$tilbakestillValgRes, shinyjs::reset("brukervalgRes"))
  observeEvent(input$tilbakestillValgAndel, shinyjs::reset("brukervalgAndel"))

  output$manglerRegResh <- renderUI(tagList(
    if (finnesEgenResh | rolle=='SC') {''} else {
           h2(HTML('Ingen registreringer på innlogget ReshID'), style = "color:red")}))

  observe({

#Antall innleggelser
    AntTab <- antallTidEnhTab(RegData=KoroData, tilgangsNivaa=rolle,
                              valgtEnhet= egenEnhet, #nivå avgjort av rolle
                              tidsenhet='dag',
                              aarsakInn = as.numeric(input$aarsakInn),
                              skjemastatusInn=as.numeric(input$skjemastatusInn),
                              erMann=as.numeric(input$erMann))
    UtData <- KoronaUtvalg(RegData=KoroData,
                           enhetsNivaa=egetEnhetsNivaa, valgtEnhet=egenEnhet,
                           aarsakInn = as.numeric(input$aarsakInn),
                           skjemastatusInn=as.numeric(input$skjemastatusInn),
                           erMann=as.numeric(input$erMann)
    )

    txt <- if(dim(UtData$RegData)[1]>2) {
      paste0('For hele tidsperioden er gjennomsnittsalderen <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
             round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn.
              Antall døde: ', sum(UtData$RegData$StatusVedUtskriving==2, na.rm=T))
    } else {''}

    output$utvalgAntOpph <- renderUI({
      UtTekst <- tagList(
        h5(HTML(paste0(AntTab$utvalgTxt, '<br />'))),
        h4(HTML(paste0(txt, '<br />')))

      )})

    output$tabAntOpph <- renderTable({
      Nrad <- nrow(AntTab$Tab)
      AntTab$Tab[(Nrad-10):Nrad,]}, rownames = T, digits=0, spacing="xs"
    )
#Antall utskrevne
    AntUtskr <- antallTidUtskrevne(RegData=KoroData, tilgangsNivaa=rolle,
                                             valgtEnhet= egenEnhet, #enhetsnivå avgjort av rolle
                                             tidsenhet='dag',
                                             aarsakInn = as.numeric(input$aarsakInn),
                                             skjemastatusInn=as.numeric(input$skjemastatusInn),
                                             erMann=as.numeric(input$erMann))
    output$tabAntUtskr <- renderTable({
      Nrad <- nrow(AntUtskr$Tab)
      AntUtskr$Tab[(Nrad-10):Nrad,]}, rownames = T, digits=0, spacing="xs")
    output$utvalgAntUtskr <- renderUI(h5(HTML(paste0(AntTab$utvalgTxt, '<br />'))))

    #Tab status nå
    statusNaaTab <- statusNaaTab(RegData=KoroData, enhetsNivaa=egetEnhetsNivaa, #
                                 valgtEnhet=input$valgtEnhet,
                                 aarsakInn = as.numeric(input$aarsakInn))
    output$statusNaaShTab <- renderTable({statusNaaTab$Tab}, rownames = T, digits=0, spacing="xs")
    output$utvalgNaa <- renderUI({h5(HTML(paste0(statusNaaTab$utvalgTxt, '<br />'))) })

    #Skjema i kladd
    KoroDataEget <- KoronaUtvalg(RegData=KoronaPreprosesser(KoroDataRaa, aggPers = 0),
                                 valgtEnhet = egenEnhet, enhetsNivaa = egetEnhetsNivaa)$RegData
    indKladdEget <- which(KoroDataEget$FormStatus==1)
    if (length(indKladdEget)>0) {
    AntKladdShus <- table(KoroDataEget$ShNavn[indKladdEget], dnn= 'Inkl.skjema i kladd')
    output$skjemaInnKladdTab <-
      if (length(AntKladdShus) > 1) {AntKladdShus <-  xtable::xtable(sort(AntKladdShus, decreasing = T))
      renderTable({AntKladdShus}, rownames = T, digits=0, spacing="xs")
      } else {AntKladdShus <-  as.data.frame(AntKladdShus)
      names(AntKladdShus) <- c('', 'Inkl.skjema i kladd')
      renderTable({AntKladdShus}, rownames = F, digits=0, spacing="xs")}
    } else {
      output$skjemaInnKladdTab <- renderText('Alle inklusjonsskjema ferdigstilt!')}

    indKladdUtEget <- which(KoroDataEget$FormStatusUt==1)
    if (length(indKladdUtEget)>0) {
      AntKladdUtShus <- table(KoroDataEget$ShNavn[indKladdUtEget], dnn= 'Ut.skjema i kladd')
      output$skjemaUtKladdTab <-
        if (length(AntKladdUtShus) > 1) {
          AntKladdUtShus <-  xtable::xtable(sort(AntKladdUtShus, decreasing = T))
          renderTable({AntKladdUtShus}, rownames = T, digits=0, spacing="xs")
        } else {
          AntKladdUtShus <-  as.data.frame(AntKladdUtShus)
        names(AntKladdUtShus) <- c('', 'Ut.skjema i kladd')
        renderTable({AntKladdUtShus}, rownames = F, digits=0, spacing="xs")}
    } else {output$skjemaUtKladdTab <- renderText('Alle utskrivingsskjema ferdigstilt!')}


    #Tab ferdigstilte
    TabFerdig <- FerdigeRegTab(RegData=KoroData,
                               aarsakInn = as.numeric(input$aarsakInn),
                               valgtEnhet=input$valgtEnhet,
                               datoFra=input$valgtDato[1],
                               datoTil=input$valgtDato[2],
                               enhetsNivaa = egetEnhetsNivaa,
                               dodSh=as.numeric(input$dodSh),
                               erMann=as.numeric(input$erMann))

    output$tabFerdigeReg <- if (TabFerdig$Ntest>4){
      renderTable({TabFerdig$Tab}, rownames = T, digits=0, spacing="xs")} else {
        renderText('Få registreringer (N<5)')}
    output$utvalgFerdigeReg <- renderUI({h5(HTML(paste0(TabFerdig$utvalgTxt, '<br />'))) })
    output$tittelFerdigeReg <- renderUI(
      h3(paste0('Utskrevne pasienter (', TabFerdig$Ntest, ' forløp)')))


    #Tab risiko
    RisikoTab <- RisikoInnTab(RegData=KoroData,
                              valgtEnhet= input$valgtEnhet,
                              enhetsNivaa = egetEnhetsNivaa,
                              datoFra=input$valgtDato[1],
                              datoTil=input$valgtDato[2],
                              skjemastatusInn=as.numeric(input$skjemastatusInn),
                              dodSh=as.numeric(input$dodSh),
                              aarsakInn = as.numeric(input$aarsakInn),
                              erMann=as.numeric(input$erMann),
                              minald=as.numeric(input$alder[1]),
                              maxald=as.numeric(input$alder[2]))


    output$tabRisikofaktorer <- if (RisikoTab$Ntest>2){
      renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
        renderText('Få registreringer (N<3)')}
    output$utvalgRisiko <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
    })

  })

  ############ Kevin start ######################
  output$FigurAldersfordeling <-
    renderPlot({korona::AlderKjFig(RegData=KoroData,
                                   valgtEnhet= input$valgtEnhet,
                                   enhetsNivaa = egetEnhetsNivaa,
                                   datoFra=input$valgtDato[1],
                                   datoTil=input$valgtDato[2],
                                   dodSh=as.numeric(input$dodSh),
                                   aarsakInn = as.numeric(input$aarsakInn),
                                   skjemastatusInn=as.numeric(input$skjemastatusInn)
                                   )
    }, width = 500, height = 500)

  output$LastNedFigAldKj <- downloadHandler(
    filename = function(){
      paste0('FigurAldKj_', Sys.time(), '.', input$bildeformatAldKj)
    },
    content = function(file){
      korona::AlderKjFig(RegData=KoroData,
                         valgtEnhet= input$valgtEnhet,
                         enhetsNivaa = egetEnhetsNivaa,
                         datoFra=input$valgtDato[1],
                         datoTil=input$valgtDato[2],
                         dodSh=as.numeric(input$dodSh),
                         aarsakInn = as.numeric(input$aarsakInn),
                         skjemastatusInn=as.numeric(input$skjemastatusInn),
                         outfile = file)
    }
  )


  output$lastNedAldKj <- downloadHandler(
    filename = function(){
      paste0('AldKjTabell', Sys.time(), '.csv')
    },

    content = function(file){
      Tabell <- korona::AlderKjFig(RegData=KoroData,
                                   valgtEnhet= input$valgtEnhet,
                                   enhetsNivaa = egetEnhetsNivaa,
                                   datoFra=input$valgtDato[1],
                                   datoTil=input$valgtDato[2],
                                   dodSh=as.numeric(input$dodSh),
                                   aarsakInn = as.numeric(input$aarsakInn),
                                   skjemastatusInn=as.numeric(input$skjemastatusInn),
                                   outfile = file)
      write.csv2(Tabell, file, row.names = F, fileEncoding = 'latin1')
    }
  )

  callModule(koronaresultater, "resultater_id", KoroData = KoroData, KoroDataOpph=KoroDataOpph, rolle=rolle, enhetsvalg=enhetsvalg,
             egetEnhetsNivaa=egetEnhetsNivaa, egenEnhet=egenEnhet, hvdsession = session)


  callModule(koronabelegg, "koronabelegg_id", KoroData = KoroData, rolle=rolle, reshID=reshID,
             egetEnhetsNivaa=egetEnhetsNivaa, egenEnhet=egenEnhet, hvdsession = session)

  ########## Kevin slutt ##################

  #-----------------------------Resultater---------------------------------


  output$tabAntOpphEnhTid <- renderTable( #xtable::xtable(
    tabAntOpphEnhTid(RegData=KoroDataOpph,
                     datoTil=input$tilDatoOpph,
                     enhetsNivaa = input$enhetsNivaaOpph,
                     tidsenhet = input$tidsenhetOpph,
                     antTidsenh=input$antTidsenhOpph), rownames = T, digits = 0
  )

  #------------Fordelinger---------------------

  output$fordelinger <- renderPlot({
    KoronaFigAndeler(RegData=KoroData,
                     valgtVar=input$valgtVarFord,
                     valgtEnhet = input$valgtEnhetRes, #egenEnhet,  #
                     enhetsNivaa=egetEnhetsNivaa,
                     datoFra=input$valgtDatoRes[1],
                     datoTil=input$valgtDatoRes[2],
                     enhetsUtvalg = as.numeric(input$enhetsUtvalgFord),
                     dodSh=as.numeric(input$dodShRes),
                     aarsakInn = as.numeric(input$aarsakInnRes),
                     erMann=as.numeric(input$erMannRes),
                     beredPas = as.numeric(input$beredPasRes),
                     skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                     kjemastatusUt=as.numeric(input$skjemastatusUtRes),
                     session = session)
  }, height=700, width=700 #height = function() {session$clientData$output_fordelinger_width}
  )
  output$LastNedFigFord <- downloadHandler(
    filename = function(){
      paste0('FordelingsFigur', valgtVar=input$valgtVarFord, '_', Sys.time(), '.', input$bildeformatFord)
    },

    content = function(file){
      KoronaFigAndeler(RegData=KoroData,
                       valgtVar=input$valgtVarFord,
                       valgtEnhet = input$valgtEnhetRes,
                       datoFra=input$valgtDatoRes[1],
                       datoTil=input$valgtDatoRes[2],
                       enhetsNivaa= egetEnhetsNivaa,
                       enhetsUtvalg = as.numeric(input$enhetsUtvalgFord),
                       dodSh=as.numeric(input$dodShRes),
                       aarsakInn = as.numeric(input$aarsakInnRes),
                       erMann=as.numeric(input$erMannRes),
                       skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                       kjemastatusUt=as.numeric(input$skjemastatusUtRes),
                       session = session,
                       outfile = file)
    }
  )

  observe({
    #print(paste0('FigurAndelTid', valgtVar=input$valgtVarAndel, '_', Sys.Date(), '.', input$bildeformatAndel))
    UtDataFord <- KoronaFigAndeler(RegData=KoroData,
                                   valgtVar=input$valgtVarFord,
                                   valgtEnhet = input$valgtEnhetRes,
                                   datoFra=input$valgtDatoRes[1],
                                   datoTil=input$valgtDatoRes[2],
                                   enhetsNivaa= egetEnhetsNivaa,
                                   enhetsUtvalg = as.numeric(input$enhetsUtvalgFord),
                                   dodSh=as.numeric(input$dodShRes),
                                   aarsakInn = as.numeric(input$aarsakInnRes),
                                   erMann=as.numeric(input$erMannRes),
                                   skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                   kjemastatusUt=as.numeric(input$skjemastatusUtRes),
                                   session = session)


    tab <- lagTabavFigFord(UtDataFraFig = UtDataFord)


    output$tittelFord <- renderUI({
      tagList(
        h3(HTML(paste(UtDataFord$tittel, sep='<br />'))),
        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
      )}) #, align='center'
    output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
      #       kable_styling("hover", full_width = F)
      antKol <- ncol(tab)
      kableExtra::kable(tab, format = 'html'
                        , full_width=F
                        , digits = c(0,0,1,0,0,1)[1:antKol]
      ) %>%
        add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
        column_spec(column = 1, width_min = '7em') %>%
        column_spec(column = 2:(ncol(tab)+1), width = '7em') %>%
        row_spec(0, bold = T)
    }

    output$lastNed_tabFord <- downloadHandler(
      filename = function(){
        paste0(input$valgtVarFord, '_fordeling.csv')
      },
      content = function(file, filename){
        write.csv2(tab, file, row.names = T, na = '')
      })
  }) #observe

  #------------Andeler---------------------
  #----------AndelTid------------
  output$andelTid <- renderPlot({
    KoronaFigAndelTid(RegData=KoroData,
                     valgtVar=input$valgtVarAndel,
                     valgtEnhet = input$valgtEnhetAndel,
                     enhetsNivaa=egetEnhetsNivaa,
                     datoFra=input$valgtDatoAndel[1],
                     datoTil=input$valgtDatoAndel[2],
                     enhetsUtvalg = as.numeric(input$enhetsUtvalgAndel),
                     dodSh=as.numeric(input$dodShAndel),
                     aarsakInn = as.numeric(input$aarsakInnAndel),
                     erMann=as.numeric(input$erMannAndel),
                     beredPas = as.numeric(input$beredPasAndel),
                     tidsenhet=input$tidsenhetAndel,
                     session = session)
  }, height = 300, width = 1000 #height = function() {session$clientData$output_fordelinger_width}
  )



    output$LastNedFigAndelTid <- downloadHandler(
      filename = function(){
        paste0('FigurAndelTid', valgtVar=input$valgtVarAndel, '_', Sys.time(), '.', input$bildeformatAndel)
      },

      content = function(file){
        KoronaFigAndelTid(RegData=KoroData,
                          valgtVar=input$valgtVarAndel,
                          valgtEnhet = input$valgtEnhetAndel,
                          enhetsNivaa=egetEnhetsNivaa,
                          datoFra=input$valgtDatoAndel[1],
                          datoTil=input$valgtDatoAndel[2],
                          enhetsUtvalg = as.numeric(input$enhetsUtvalgAndel),
                          dodSh=as.numeric(input$dodShAndel),
                          aarsakInn = as.numeric(input$aarsakInnAndel),
                          erMann=as.numeric(input$erMannAndel),
                          beredPas = as.numeric(input$beredPasAndel),
                          tidsenhet=input$tidsenhetAndel,
                          session = session,
                          outfile = file)
      }
    )

#  }) #observe
  #----------Datakvalitet-------------------------
  innManglerUtTab <- innManglerUt(RegData=KoroDataRaa, valgtEnhet=egenEnhet, enhetsNivaa=egetEnhetsNivaa)
  output$innManglerUtTab <- renderTable(innManglerUtTab)

  output$lastNed_innManglerUt <- downloadHandler(
    filename = function(){
      paste0('ManglerUtSkjema.csv')
    },
    content = function(file, filename){
      write.csv2(innManglerUtTab, file, row.names = F, na = '')
    })

  TabDblInn <- PasMdblReg(RegData=KoroDataRaa, tidsavvik = 30)
  output$dblInn <- renderTable(TabDblInn)

  output$lastNed_dblInn <- downloadHandler(
    filename = function(){
      paste0('ToInnskjema.csv')
    },
    content = function(file, filename){
      write.csv2(TabDblInn, file, row.names = F, na = '')
    })


  #Antall opphold
  output$tabOpphHF <- renderTable({
      if (rolle == 'LU') {KoroDataOpph <- KoroDataOpph[which(KoroDataOpph$RHF == egetRHF), ]}
      OpphHF <- KoroDataOpph %>% dplyr::group_by(RHF, HFkort) %>% dplyr::summarise(Antall = n(), .groups = 'keep')
      colnames(OpphHF) <- c('RHF', 'HF', 'Antall')
      OpphHF
    }, rownames = F, digits = 0)



  #-------------Intensivregistreringer------------------------

  observe({

    AntTab <- intensivberedskap::TabTidEnhet(RegData=BeredData, tidsenhet='dag', #valgtRHF= 'Alle',
                                             bekr=as.numeric(input$bekrInt)
    )
    UtData <- NIRUtvalgBeredsk(RegData=BeredData,
                               bekr=as.numeric(input$bekrInt)
    )
    utvalg <- UtData$utvalgTxt
    txt <- if(AntTab$Ntest>2) {
      paste0('Gjennomsnittsalderen er <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
             round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn.')
    } else {''}
    output$utvalgAntRegInt <- renderUI({
      UtTekst <- tagList(
        h5(HTML(paste0(utvalg, '<br />'))),
        h4(HTML(paste0(txt, '<br />')))

      )})
    Nrader <- dim(AntTab$Tab)[1]
    output$tabAntRegInt <- renderTable({AntTab$Tab[(Nrader-14):Nrader, ]}, rownames = T, digits=0, spacing="xs"
    )


    #Tab status nå
    statusNaaIntTab <- intensivberedskap::statusECMOrespTab(RegData=BeredData,
                                                            bekr=as.numeric(input$bekrInt))
    output$tabIntensivNaa <- renderTable({statusNaaIntTab$Tab}, rownames = T, digits=0, spacing="xs")
    output$utvalgIntensivNaa <- renderUI({h5(HTML(paste0(statusNaaIntTab$utvalgTxt, '<br />'))) })

    #Tab ferdigstilte
    TabFerdigInt <- intensivberedskap::oppsumFerdigeRegTab(RegData=BeredData,
                                                           bekr = as.numeric(input$bekrInt))

    output$tabFerdigeRegInt <- if (TabFerdigInt$Ntest>2){
      renderTable({TabFerdigInt$Tab}, rownames = T, digits=0, spacing="xs")} else {
        renderText('Få registreringer (N<3)')}

    output$utvalgFerdigeRegInt <- renderUI({h5(HTML(paste0(TabFerdigInt$utvalgTxt, '<br />'))) })
    output$tittelFerdigeRegInt <- renderUI(
      h4(paste0('Fullførte registreringer, intensiv (', TabFerdigInt$Ntest, ' forløp)')))

    #Registreringer i limbo:
    output$RegIlimboInt <- renderUI({
      finnBurdeFerdig <- function(RegData) {sum((!(is.na(RegData$DateDischargedIntensive)) & (RegData$FormStatus!=2)))}
      AntBurdeFerdig <- paste0(finnBurdeFerdig(BeredData), ' skjema for hele landet')
      h5(HTML(paste0('&nbsp;&nbsp;&nbsp;', AntBurdeFerdig, '<br />')))
    })


    #Tab risiko
    RisikoTab <- intensivberedskap::RisikofaktorerTab(RegData=BeredData, #tidsenhet='Totalt',
                                                      bekr=as.numeric(input$bekrInt))

    output$tabRisikofaktorerInt <- if (RisikoTab$Ntest>2){
      renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
        renderText('Få registreringer (N<3)')}
    output$utvalgRisikoInt <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
    })

    TabAlder <- intensivberedskap::TabAlder(RegData=BeredData,
                                            bekr=as.numeric(input$bekrInt)
    )
    output$tabAlderInt<- renderTable({xtable::xtable(TabAlder$Tab)}, rownames = T, digits=0, spacing="xs")
    output$utvalgAlderInt <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })
  })

  #------------------ Abonnement ----------------------------------------------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  subscription <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session, type = "subscription"))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$tab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = 6))),
    rownames = FALSE
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userFullName <- rapbase::getUserFullName(session)
    userEmail <- rapbase::getUserEmail(session)
    if (length(subscription$tab) == 0) {
      p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userFullName,
                 ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })


  ## nye abonnement
  observeEvent(input$subscribe, { #MÅ HA
    owner <- rapbase::getUserName(session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
    email <- rapbase::getUserEmail(session)

    if (input$subscriptionRep == "Koronarapport") {
      synopsis <- "Rapporteket-Pandemi: Koronarapport"
      rnwFil <- "KoronaRapport.Rnw" #Navn på fila
      }
    fun <- "abonnementKorona"
    paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtEnhet", "enhetsNivaa", 'rolle')
    paramValues <- c(rnwFil, brukernavn, reshID, egenEnhet, egetEnhetsNivaa, rolle) #, as.character(input$valgtEnhetabb))
    # test <- abonnementKorona(rnwFil="KoronaRapport.Rnw", brukernavn='tullebukk',
    #                          reshID=100082) #, valgtEnhet=egenEnhet, enhetsNivaa='RHF', rolle='SC')

    rapbase::createAutoReport(synopsis = synopsis, package = 'korona',
                              type = "subscription",
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval,
                              intervalName = intervalName)

    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
  })

  #----- Utsending ------
  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  dispatchment <- reactiveValues(
    tab = rapbase::makeAutoReportTab(session = session, type = "dispatchment", includeReportId = TRUE),
    koblRoller = matrix(NA, ncol=2, dimnames=list(NULL, c('id', 'Rolle') )),
    report = "Koronarapport",
    freq = "Månedlig-month",
    email = vector()
  )

  #observe({
  alleAutorapporter <- rapbase::readAutoReportData()
  egneUts <-  rapbase::filterAutoRep(
    rapbase::filterAutoRep(alleAutorapporter, by = 'package', pass = 'korona'),
    by = 'type', pass = 'dispatchment')

  if (length(names(egneUts))!=0) {
    ider <- names(egneUts)
    roller <- vector()
    for (k in 1:length(ider)) {
       roller <- c(roller, egneUts[[k]]$params$rolle)
    }
  dispatchment$koblRoller <- cbind(id = ider,
                      Rolle = roller)
  }
  ## observér og foreta endringer mens applikasjonen kjører
  observeEvent(input$addEmail, {
    dispatchment$email <- c(dispatchment$email, input$email)
  })
  observeEvent(input$delEmail, {
    dispatchment$email <-
      dispatchment$email[!dispatchment$email == input$email]
  })
  observeEvent (input$dispatch, {
    package <- "korona"
    type <- "dispatchment"
    owner <- rapbase::getUserName(session)
    ownerName <- rapbase::getUserFullName(session)
    interval <- strsplit(input$dispatchmentFreq, "-")[[1]][2]
    intervalName <- strsplit(input$dispatchmentFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- dispatchment$email


    if (input$dispatchmentRep == "Koronarapport") {
      synopsis <- "Rapporteket-Pandemi: Koronarapport"
      fun <- "abonnementKorona"
      rnwFil <- "KoronaRapport.Rnw" #Navn på fila
      rolleUts <- input$dispatchmentRole
      egetEnhetsNivaaUts <- switch(rolleUts, SC = 'RHF', LC = 'RHF', LU = 'HF')
      reshIDuts <- input$dispatchmentResh
      organization <- reshIDuts #rapbase::getUserReshId(session)
      indReshUts <- match(reshIDuts, KoroData$HFresh) #Her skal benyttes HF-resh
      egenEnhetUts <- switch(rolleUts, SC='Alle', #switch(rolle, SC='Alle',
                          LC=as.character(KoroData$RHF[indReshUts]),
                          LU=as.character(KoroData$HF[indReshUts]))
      paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtEnhet", "enhetsNivaa", 'rolle')
      paramValues <- c(rnwFil, brukernavn, reshIDuts, egenEnhetUts, egetEnhetsNivaaUts, rolleUts)
    }

    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              ownerName = ownerName,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    dispatchment$tab <- rapbase::makeAutoReportTab(session, type = "dispatchment", includeReportId = TRUE)

    alleAutorapporter <- rapbase::readAutoReportData()
    egneUts <-  rapbase::filterAutoRep(
      rapbase::filterAutoRep(alleAutorapporter, by = 'package', pass = 'korona'),
      by = 'type', pass = 'dispatchment')

    ider <- names(egneUts)
    roller <- vector()
    for (k in 1:length(ider)) {
      roller <- c(roller, egneUts[[k]]$params$rolle)
    }
    dispatchment$koblRoller <- cbind(id = ider,
                                     Rolle = roller)


    dispatchment$email <- vector()
  })


  ## ui: velg rapport
  output$reportUts <- renderUI({
    selectInput("dispatchmentRep", "Rapport:",
                c("Koronarapport"),
                selected = dispatchment$report)
  })
  ## ui: velg rolle
  output$rolleUts <- renderUI({
    selectInput("dispatchmentRole", "Rolle/nivå:",
                c("LU", "LC", "SC"),
                selected = dispatchment$rolle)
  })
  ## ui: velg HF
  output$HFreshUts <- renderUI({
    selectInput("dispatchmentResh", "HF-tilhørighet:",
                HFreshValg,
                selected = dispatchment$HFresh)
  })

  ## ui: velg frekvens
  output$freqUts <- renderUI({
    selectInput("dispatchmentFreq", "Frekvens:",
                list(Årlig = "Årlig-year",
                      Kvartalsvis = "Kvartalsvis-quarter",
                      Månedlig = "Månedlig-month",
                      Ukentlig = "Ukentlig-week",
                      Daglig = "Daglig-DSTday"),
                selected = dispatchment$freq)
  })

  ## ui: legg til gyldig- og slett epost
  output$editEmail <- renderUI({
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
               input$email)) {
      tags$p("Angi mottaker over")
    } else {
      if (input$email %in% dispatchment$email) {
        actionButton("delEmail", "Slett epostmottaker",
                     icon = shiny::icon("trash"))
      } else {
        actionButton("addEmail", "Legg til epostmottaker",
                     icon = shiny::icon("pencil"))
      }
    }
  })

  ## ui: vis valgte mottakere
  output$recipients <- renderText(paste(dispatchment$email, sep = "<br>"))

  ## ui: lag ny utsending
  output$makeDispatchment <- renderUI({
    if (length(dispatchment$email) == 0) {
      NULL
    } else {
      actionButton("dispatch", "Lag utsending",
                   icon = shiny::icon("save"))
    }
  })

  ## lag tabell over gjeldende status for utsending - MÅ TA HØYDE FOR AT IKKE FINNES NOEN

  output$activeDispatchments <- DT::renderDataTable(
    if (length(dispatchment$tab) != 0) { #(!is.na(dispatchment$koblRoller[1])) {
    merge(as.data.frame(dispatchment$tab), as.data.frame(dispatchment$koblRoller), by = 'id',
          sort=F, all.x=T, all.y=F)[ ,c("Ansvarlig", "Rapport", "Datakilde", "Rolle", "Mottaker",
                                        "Periode", "Utløp", "Neste", "Endre", "Slett")]} else NULL,
    server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 'tp', ordning = FALSE), #, columnDefs = list(list(visible = FALSE, targets = 9))
                   rownames = FALSE

    )



  ## ui: lag side som viser status for utsending, også når det ikke finnes noen
  output$dispatchmentContent <- renderUI({
    if (length(dispatchment$tab) == 0) {
      p("Det finnes ingen utsendinger")
    } else {
      tagList(
        h4("Aktive utsendinger:"),
        h5("NB: Når du trykker på knappen for å gjøre endringer i ei utsending,
           slettes utsendinga fra lista og legger seg inn i skjemaet til venstre
           slik at du f.eks. kan legge til/slette e-postmottagere og endre frekvens.
           Pass på at du får riktig enhet/rolle når du oppdaterer!"),
        DT::dataTableOutput("activeDispatchments")
      )
    }
  })

  # Rediger eksisterende auto rapport (alle typer)
  observeEvent(input$edit_button, {
    repId <- strsplit(input$edit_button, "_")[[1]][2]
    rep <- rapbase::readAutoReportData()[[repId]]
    if (rep$type == "subscription") {#abonnement

    }
    if (rep$type == "dispatchment") { #utsending
      dispatchment$freq <- paste0(rep$intervalName, "-", rep$interval)
      dispatchment$email <- rep$email
      rapbase::deleteAutoReport(repId)
      dispatchment$tab <-
        rapbase::makeAutoReportTab(session, type = "dispatchment", includeReportId = TRUE)
      dispatchment$report <- rep$synopsis
    }
   })


  # Slett eksisterende auto rapport (alle typer)
  observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(repId)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment", includeReportId = TRUE)
  })



#-------Registeradministrasjon------------------------

  output$lastNed_dataPandemiRaa <- downloadHandler(
    filename = function(){
      paste0('DataPandemiRaa.', Sys.Date(), '.csv')
    },
    content = function(file, filename){
      write.csv2(KoroDataRaa, file, row.names = F, na = '')
    })

  output$lastNed_dataPandemiPas <- downloadHandler(
    filename = function(){
      paste0('DataPandemiPas', Sys.Date(), '.csv')
    },
    content = function(file, filename){
      write.csv2(KoroData, file, row.names = F, na = '')
    })

  #Send filer til FHI:
    output$lastNed_filstiDataNHN <- downloadHandler(
      filename = function(){
        paste0('Filsti', Sys.time(), '.csv')},
      content = function(file, filename){
        Filsti <- sendDataFilerFHI(zipFilNavn=input$hvilkeFilerTilFHI) #brukernavn = brukernavn)
        write.csv2(x=Filsti, file, row.names = F, na = '') #x - r-objektet
  })

    #Abonnement, filer til FHI
    observeEvent(input$bestillDataTilFHI, { #MÅ HA
      owner <- rapbase::getUserName(session)
      organization <- rapbase::getUserReshId(session)
      email <- rapbase::getUserEmail(session)
      interval <- "DSTday"
      intervalName <- "Daglig"
      runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
      paramNames = c('zipFilNavn', 'brukernavn')
      paramValues = c(input$hvilkeFilerTilFHI, brukernavn)
      rapbase::createAutoReport(synopsis = paste0('Sendt til FHI: ',input$hvilkeFilerTilFHI),
                                package = 'korona',
                                fun = "sendDataFilerFHI",
                                paramNames = paramNames,
                                paramValues = paramValues,
                                owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear,
                                interval = interval,
                                intervalName = intervalName)

      #rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      subscription$tab <-
        rapbase::makeAutoReportTab(session, type = "subscription")

    })

    # Eksport
    registryName <- "korona"
    ## brukerkontroller
    rapbase::exportUCServer("koronaExport", registryName)
    ## veileding
    rapbase::exportGuideServer("koronaExportGuide", registryName)



}
# Run the application
shinyApp(ui = ui, server = server)

