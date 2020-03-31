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
library(rapbase)
library(intensivberedskap)
library(korona)

shiny::addResourcePath('rap', system.file('www', package='rapbase'))
context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION")

regTitle <- ifelse(paaServer,
                   paste0('Koronaregistreringer, pandemi 2020 ',ifelse(context=='QA', 'QA','')),
                   'FIKTIVE data')

#---------Hente data------------
#NB Henter nå koronadata fra intensivregisteret !!!!!!!!!
library(intensivberedskap)
if (paaServer) {
  qCoro <- 'SELECT *  from ReadinessFormDataContract'
  KoroData <- rapbase::LoadRegData(registryName= "nir", query=qCoro, dbType="mysql")
  KoroDataInt <- intensivberedskap::NIRberedskDataSQL()
  #repLogger(session = session, 'Hentet alle data fra intensivregisteret')
} else {
  KoroData <- read.table('A:/Pandemi/Pandemiskjema2020-03-31.csv', sep=';',
                         stringsAsFactors=FALSE, header=T, encoding = 'UTF-8')
} #hente data

KoroData <- KoronaPreprosesser(RegData = KoroData)
KoroDataInt <- intensivberedskap::NIRPreprosessBeredsk(RegData=KoroDataInt)

#-----Definere utvalgsinnhold og evt. parametre som er statiske i appen----------


#Definere utvalgsinnhold
rhfNavn <- c('Alle', as.character(sort(unique(KoroData$RHF))))
hfNavn <- sort(unique(KoroData$HF)) #, index.return=T)
#updateTextInput(session, inputId, label = NULL, value = NULL). Hvis input skal endres som flge av et annet input.
#enhetsNivaa <- c('Alle', 'RHF', 'HF')
#names(enhetsNivaa) <- c('RHF', 'HF')

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
                # h3('Koronarapport med samling av resultater'),
                 # h5('Koronarapporten kan man få regelmessig tilsendt på e-post.
                 #    Gå til fanen "Abonnement" for å bestille dette.'),
                 downloadButton(outputId = 'KoroRapp.pdf', label='Last ned Koronarapport', class = "butt"),
                 tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                 br(),
              br(),
              h3('Gjør filtreringer/utvalg:'),
              #br(),

              # conditionalPanel(condition = "input.hovedark == 'Nøkkeltall' || input.ark == 'Ant. opphold'",
              #                  dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
              #                            value = Sys.Date(), max = Sys.Date())
              # ),
#MÅ HA ET VALG SOM ENDRER SEG AVHENGIG AV ROLLE, DVS. Velg RHF/HF/ingen valg?
              selectInput(inputId = "valgtEnhet", label="Velg RHF",
                          choices = rhfNavn
              ),
# selectInput(inputId = 'enhetsGruppe', label='Enhetgruppe',
#             choices = c("RHF"=1, "HF"=2, "Sykehus"=3)
# ),

              #br(),
              selectInput(inputId = "bekr", label="Bekreftet/Mistenkt",
                          choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
              ),
              selectInput(inputId = "skjemastatus", label="Skjemastatus",
                          choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
              ),
              # selectInput(inputId = "dodInt", label="Tilstand ut fra intensiv",
              #             choices = c("Alle"=9, "Død"=1, "Levende"=0)
              # ),
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
              actionButton("tilbakestillValg", label="Tilbakestill valg")

              # dateRangeInput(inputId = 'datovalg', start = startDato, end = idag,
              #                label = "Tidsperiode", separator="t.o.m.", language="nb" #)
              # ),
              ),
             mainPanel(width = 9,
                       shinyalert::useShinyalert(),
                       appNavbarUserWidget(user = uiOutput("appUserName"),
                                           organization = uiOutput("appOrgName"),
                                           addUserInfo = TRUE),
                       tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),

                       h3('Resultater fra pandemiregistrering, korona.'),
                       h4('Merk at resultatene er basert på til dels ikke-fullstendige registreringer'),
                       h5('Siden er under utvikling... ', style = "color:red"),
                      br(),
                      fluidRow(
                        column(width = 4,
                               h4('Innlagte på sykehus nå'),
                               uiOutput('statusNaaShTab'),
                               br(),
                               h4('Opphold uten ferdigstilt innleggelsesskjema innen 24t'), #, align='center'),
                               h5('Kommer...'),
                        ),
                      column(width=5, offset=1,
                             uiOutput('tittelFerdigeReg'),
                             uiOutput('utvalgFerdigeReg'),
                             tableOutput('tabFerdigeReg')
                      )),

                      h3('Antall sykehusopphold'),
                      uiOutput('utvalgAntOpph'),
                      tableOutput('tabAntOpph'),
                      br(),
                      fluidRow(
                        column(width=3,
                               h3('Risikofaktorer'),
                               uiOutput('utvalgRisiko'),
                               tableOutput('tabRisikofaktorer')),
                        column(width=5, offset=1,
                               h3('Aldersfordeling'),
                               uiOutput('utvalgAlder'),
                               tableOutput('tabAlder')
                               ))
             ) #main
    ), #tab Startside

#---------Intensivregistreringer--------------------------------
tabPanel(p('Intensivpasienter',
           title='Resultater fra koronaregistrering i intensivregisteret'),
         value = 'Intensiv',

                  sidebarPanel(id = 'intensiv',
                               width = 3,
                               uiOutput('KoroRappTxtInt'),
                               h3('Koronarapport fra intensivregisteret'),
                               h5('Koronarapporten kan man få regelmessig tilsendt på e-post.
                                   Gå til fanen "Abonnement" for å bestille dette.'),
                               downloadButton(outputId = 'KoroRappInt.pdf',
                                              label=HTML('Last ned Koronarapport <br /> for intensivopphold'), class = "butt"),
                               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
                               br(),
                               br(),
                               h3('Gjør filtreringer/utvalg:'),
                               #br(),

                               selectInput(inputId = "bekrInt", label="Bekreftet/Mistenkt",
                                           choices = c("Alle"=9, "Bekreftet"=1, "Mistenkt"=0)
                               ),
                   ),
                  mainPanel(width = 9,
                            h3('Resultater fra koronaregistrering på INTENSIVavdelinger.'),
                            h4('Mer detaljerte resultater fra intensivavdlingene
                               finnes på Rapporteket-NIR-Beredskap'),
                            h4('Husk at andre tilgangsnivåer/resh enn i Rapporteket-Beredskap', style = "color:red"),
                            h5('Siden er under utvikling... ', style = "color:red"),
                            br(),
                            fluidRow(
                              column(width = 4,
                                     h4('Opphold uten registrert ut-tid fra intensiv'), #, align='center'),
                                     uiOutput('utvalgIntensivNaa'),
                                     tableOutput('tabIntensivNaa'),
                                     # br(),
                                     # h4('Opphold registrert som utskrevet, uten ferdigstilt skjema:'),
                                     # uiOutput('RegIlimbo')
                              ),
                              column(width=5, offset=1,
                                     uiOutput('tittelFerdigeRegInt'), #Ta med utvalg i tittel?
                                     uiOutput('utvalgFerdigeRegInt'),
                                     tableOutput('tabFerdigeRegInt')
                              )),

                            h3('Antall intensivopphold'),
                            h5('Innleggelser siste to uker, samt totalt siden 10.mars'),
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

#-----------Abonnement--------------------------------
    tabPanel(p("Abonnement",
             title='Bestill automatisk utsending av rapporter på e-post'),
             value = 'Abonnement',
             sidebarLayout(
               sidebarPanel(width = 3,
                 selectInput("subscriptionRep", "Dokument:", c("Koronarapport")), #Evt legg til intensivrapport
                 selectInput("subscriptionFreq", "Frekvens:",
                             list(Månedlig="Månedlig-month",
                                   Ukentlig="Ukentlig-week",
                                   Daglig="Daglig-DSTday"),
                             selected = "Ukentlig-week"),
                 selectInput(inputId = "valgtEnhetabb", label="Velg RHF",
                             choices = rhfNavn
                 ),
                 actionButton("subscribe", "Bestill!")
               ),
                mainPanel(
               h4('NB: Abonnementet løper til det sies opp. '),
               uiOutput("subscriptionContent")
                )
             )
    ) #tab abonnement


  ) # navbarPage
) # tagList
#----------Slutt ui-del--------------


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Last inn data

  #-----------Div serveroppstart------------------
  if (context %in% c('QA', 'PRODUCTION')){
    raplog::appLogger(session = session, msg = "Starter Pandemi-app")}

  reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 0)

  rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')
  brukernavn <- ifelse(paaServer, rapbase::getUserName(shinySession=session), 'brukernavnDummy')

  finnesEgenResh <- reshID %in% unique(KoroData$ReshId)
  if (finnesEgenResh) {
    indReshEgen <- match(reshID, KoroData$ReshId) #Her skal benyttes HF-resh
    egetShNavn <- as.character(KoroData$ShNavn[indReshEgen])
    egetRHF <- as.character(KoroData$RHF[indReshEgen])
    egetHF <- as.character(KoroData$HF[indReshEgen])
  }

  egenEnhet <- switch(rolle, SC='Alle', LC=egetRHF, LU=egetHF)
  #Filtreringsnivå for data:
  enhetsNivaaRolle <- switch(rolle, SC = 'RHF', LC = 'RHF', LU = 'HF')

  # observe({if ((rolle != 'SC') & !(finnesEgenResh)) {
  #   shinyjs::hide(id = 'KoroRappInt.pdf')
  #   shinyjs::hide(id = 'KoroRappTxtInt')
  #   shinyjs::hide(id = 'KoroRapp.pdf')
  #   shinyjs::hide(id = 'KoroRappTxt')
  #   hideTab(inputId = "hovedark", target = "Abonnement")
  # }
  #})

  # SC KAN VELGE BLANT ALLE RHF, DE ANDRE KAN BARE VELGE EGEN ENHET/ALLE
  if (rolle != 'SC') {
    updateSelectInput(session, "valgtEnhet",
                      choices = unique(c('Alle', ifelse(egetRHF=='Ukjent', 'Alle',
                                                        egetRHF))))
                                  #KoroData$RHF[match(reshID, KoroData$ReshId)]))
    updateSelectInput(session, "valgtEnhetabb",
                        choices = egetRHF) #unique(c('Alle', egetRHF)))
                                    #KoroData$RHF[match(reshID, KoroData$ReshId)]))
    }


  # widget
  if (paaServer) {
    output$appUserName <- renderText(rapbase::getUserFullName(session))
    output$appOrgName <- renderText(paste0('rolle: ', rolle,
                                           '<br> ReshID: ', reshID) )}
                                           #,'<br> Org: ', egenOrg) )}

  # User info in widget
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })



  #-------- Laste ned Samlerapporter------------
  observe({
  valgtEnhet <- ifelse(rolle == 'LU', egetRHF, as.character(input$valgtEnhet))
  output$KoroRapp.pdf <- downloadHandler(
      filename = function(){
      paste0('KoronaRapport', Sys.time(), '.pdf')},
    content = function(file){
      henteSamlerapporterBered(file, rnwFil="KoroRapp.Rnw",
                               #rolle = rolle,
                               valgtEnhet = valgtEnhet, #as.character(input$valgtEnhet),
                          reshID = reshID) #Vurder å ta med tidsinndeling eller startdato
    }
  )
})

  output$KoroRappTxt <- renderUI(tagList(
    h3(HTML('Koronarapport med samling av resultater')),
    h5(HTML('Koronarapporten kan man få regelmessig tilsendt på e-post.
                    Gå til fanen "Abonnement" for å bestille dette')))
  )

  #----------Tabeller, Korona----------------------------

  observeEvent(input$tilbakestillValg, shinyjs::reset("brukervalgStartside"))

observe({

    AntTab <- antallTidEnhTab(RegData=KoroData, tilgangsNivaa=rolle,
                      valgtEnhet= egenEnhet, #nivå avgjort av rolle
                      tidsenhet='dag',
                      skjemastatus=as.numeric(input$skjemastatus),
                      bekr=as.numeric(input$bekr),
                      erMann=as.numeric(input$erMann)
                      )
#NB: Per nå henger ikke UtData (mangler filtrering på enhet) og AntTab sammen
      UtData <- KoronaUtvalg(RegData=KoroData,
                             enhetsNivaa=enhetsNivaaRolle, valgtEnhet=egenEnhet,
                                 skjemastatus=as.numeric(input$skjemastatus),
                                 bekr=as.numeric(input$bekr),
                                 erMann=as.numeric(input$erMann)
      )

  txt <- if(dim(UtData$RegData)[1]>2) {
    paste0('Gjennomsnittsalderen er <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
                round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn. Antall døde: ',
           sum(UtData$RegData$DischargedIntensivStatus==1))
    } else {''}

  output$utvalgAntOpph <- renderUI({
    UtTekst <- tagList(
      h5(HTML(paste0(AntTab$utvalgTxt, '<br />'))),
      h4(HTML(paste0(txt, '<br />')))

    )})

  output$tabAntOpph <- renderTable({AntTab$Tab}, rownames = T, digits=0, spacing="xs"
  )

#Tab status nå
  statusNaaTab <- statusNaaTab(RegData=KoroData, enhetsNivaa=enhetsNivaa, #valgtEnhet=input$valgtEnhet,
                                    erMann=as.numeric(input$erMann),
                                    bekr=as.numeric(input$bekr))
  output$statusNaaShTab <- renderTable({statusNaaTab$Tab}, rownames = T, digits=0, spacing="xs")
  output$utvalgNaa <- renderUI({h5(HTML(paste0(statusNaaTab$utvalgTxt, '<br />'))) })

  #Tab ferdigstilte
  TabFerdig <- FerdigeRegInnTab(RegData=KoroData,
                                     #valgtEnhet=input$valgtEnhet,
                                   bekr = as.numeric(input$bekr),
                                     erMann=as.numeric(input$erMann))

  output$tabFerdigeReg <- if (TabFerdig$Ntest>2){
    renderTable({TabFerdig$Tab}, rownames = T, digits=0, spacing="xs")} else {
      renderText('Få registreringer (N<3)')}

  output$utvalgFerdigeReg <- renderUI({h5(HTML(paste0(TabFerdig$utvalgTxt, '<br />'))) })
  output$tittelFerdigeReg <- renderUI(
    h4(paste0('Fullførte registreringer (', TabFerdig$Ntest, ' skjema)')))


  #Tab risiko
  RisikoTab <- RisikoInnTab(RegData=KoroData, tidsenhet='Totalt',
                                 valgtEnhet= input$valgtEnhet,
                                 skjemastatus=as.numeric(input$skjemastatus),
                                 bekr=as.numeric(input$bekr),
                                 #dodInt=as.numeric(input$dodInt),
                                 erMann=as.numeric(input$erMann),
                                 minald=as.numeric(input$alder[1]),
                                 maxald=as.numeric(input$alder[2]))


    output$tabRisikofaktorer <- if (RisikoTab$Ntest>2){
      renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
        renderText('Få registreringer (N<3)')}
    output$utvalgRisiko <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
                         })

    TabAlder <- AlderTab(RegData=KoroData,
                         valgtEnhet= egetRHF, #input$valgtEnhet,
                         #dodInt=as.numeric(input$dodInt),
                         erMann=as.numeric(input$erMann),
                         bekr=as.numeric(input$bekr),
                         skjemastatus=as.numeric(input$skjemastatus)
    )
    output$tabAlder<- renderTable({xtable::xtable(TabAlder$Tab)}, rownames = T, digits=0, spacing="xs")
    output$utvalgAlder <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })


})

#-------------Intensivregistreringer------------------------

observe({

  AntTab <- intensivberedskap::TabTidEnhet(RegData=KoroDataInt, tidsenhet='dag', #valgtRHF= 'Alle',
                        bekr=as.numeric(input$bekr)
  )
  UtData <- NIRUtvalgBeredsk(RegData=KoroDataInt,
                             bekr=as.numeric(input$bekr)
  )
  utvalg <- UtData$utvalgTxt
  txt <- if(AntTab$Ntest>2) {
    paste0('Gjennomsnittsalderen er <b>', round(mean(UtData$RegData$Alder, na.rm = T)), '</b> år og ',
           round(100*mean(UtData$RegData$erMann, na.rm = T)), '% er menn. Antall døde: ',
           sum(UtData$RegData$DischargedIntensivStatus==1))
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
  statusNaaIntTab <- intensivberedskap::statusECMOrespTab(RegData=KoroDataInt,
                                    bekr=as.numeric(input$bekrInt))
  output$tabIntensivNaa <- renderTable({statusNaaIntTab$Tab}, rownames = T, digits=0, spacing="xs")
  output$utvalgIntensivNaa <- renderUI({h5(HTML(paste0(statusNaaIntTab$utvalgTxt, '<br />'))) })

  #Tab ferdigstilte
  TabFerdigInt <- intensivberedskap::oppsumFerdigeRegTab(RegData=KoroDataInt,
                                   bekr = as.numeric(input$bekrInt))

  output$tabFerdigeRegInt <- if (TabFerdigInt$Ntest>2){
    renderTable({TabFerdigInt$Tab}, rownames = T, digits=0, spacing="xs")} else {
      renderText('Få registreringer (N<3)')}

  output$utvalgFerdigeRegInt <- renderUI({h5(HTML(paste0(TabFerdigInt$utvalgTxt, '<br />'))) })
  output$tittelFerdigeRegInt <- renderUI(
    h4(paste0('Fullførte registreringer, intensiv (', TabFerdigInt$Ntest, ' skjema)')))

  #Registreringer i limbo:
  output$RegIlimboInt <- renderUI({
    finnBurdeFerdig <- function(RegData) {sum((!(is.na(RegData$DateDischargedIntensive)) & (RegData$FormStatus!=2)))}
    AntBurdeFerdig <- paste0(finnBurdeFerdig(KoroDataInt), ' skjema for hele landet')
    h5(HTML(paste0('&nbsp;&nbsp;&nbsp;', AntBurdeFerdig, '<br />')))
  })


  #Tab risiko
  RisikoTab <- intensivberedskap::RisikofaktorerTab(RegData=KoroDataInt, tidsenhet='Totalt',
                                 bekr=as.numeric(input$bekr))

  output$tabRisikofaktorerInt <- if (RisikoTab$Ntest>2){
    renderTable(RisikoTab$Tab, rownames = T, digits=0, spacing="xs") } else {
      renderText('Få registreringer (N<3)')}
  output$utvalgRisikoInt <- renderUI({h5(HTML(paste0(RisikoTab$utvalgTxt, '<br />'))) #tagList()
  })

  TabAlder <- intensivberedskap::TabAlder(RegData=KoroDataInt,
                       bekr=as.numeric(input$bekr)
  )
  output$tabAlderInt<- renderTable({xtable::xtable(TabAlder$Tab)}, rownames = T, digits=0, spacing="xs")
  output$utvalgAlderInt <- renderUI({h5(HTML(paste0(TabAlder$utvalgTxt, '<br />'))) })
})

  #------------- Abonnement----------------
    #------------------ Abonnement ----------------------------------------------
    ## reaktive verdier for å holde rede på endringer som skjer mens
    ## applikasjonen kjører
    rv <- reactiveValues(
      subscriptionTab = rapbase::makeUserSubscriptionTab(session))
    ## lag tabell over gjeldende status for abonnement
    output$activeSubscriptions <- DT::renderDataTable(
      rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
      rownames = FALSE, options = list(dom = 't')
    )

    ## lag side som viser status for abonnement, også når det ikke finnes noen
    output$subscriptionContent <- renderUI({
      fullName <- rapbase::getUserFullName(session)
      if (length(rv$subscriptionTab) == 0) {
        p(paste("Ingen aktive abonnement for", fullName))
      } else {
        tagList(
          p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                  rapbase::getUserEmail(session), ":")),
          DT::dataTableOutput("activeSubscriptions")
        )
      }
    })


    ## nye abonnement
    observeEvent (input$subscribe, { #MÅ HA
      #package <- "intensiv"
      owner <- rapbase::getUserName(session)
      interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
      intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
      organization <- rapbase::getUserReshId(session)
      runDayOfYear <- rapbase::makeRunDayOfYearSequence(
        interval = interval
      )
      email <- rapbase::getUserEmail(session)
      if (input$subscriptionRep == "Koronarapport") {
        synopsis <- "Rapporteket-Pandemi: Koronarapport"
        rnwFil <- "KoronaRapp.Rnw" #Navn på fila
      }
      fun <- "abonnementKorona"
      paramNames <- c('rnwFil', 'brukernavn', "reshID", "valgtEnhet")

      paramValues <- c(rnwFil, brukernavn, reshID, as.character(input$valgtEnhetabb))

      #test <- abonnementBeredsk(rnwFil="BeredskapKorona.Rnw", brukernavn='tullebukk',
      #                       reshID=105460)

      rapbase::createAutoReport(synopsis = synopsis, package = 'korona',
                                fun = fun, paramNames = paramNames,
                                paramValues = paramValues, owner = owner,
                                email = email, organization = organization,
                                runDayOfYear = runDayOfYear, interval = interval,
                                intervalName = intervalName)
      rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    })

    ## slett eksisterende abonnement
    observeEvent(input$del_button, {
      selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
      rapbase::deleteAutoReport(selectedRepId)
      rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
    })
}
# Run the application
shinyApp(ui = ui, server = server)

