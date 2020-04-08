koronaresultater_UI <- function(id){
  ns <- shiny::NS(id)


  shiny::sidebarLayout(
    shiny::sidebarPanel(id = ns('brukervalgRes'),

                        width = 3,
                        br(),
                        h3('Velg variabel/tema og filtreringer i data'),
                        conditionalPanel(condition = paste0("input['", ns("resultater"), "'] == 'Fordelinger'"),
                                         selectInput(inputId = ns('valgtVarFord'), label='Velg variabel',
                                                     choices = c("Alder"='alder',
                                                                 "Kommer: Liggetid"='liggetid',
                                                                 'Kommer: Risikotiltander'='risiko',
                                                                 'Kommer: sirkulasjonssvikt' = 'sirkSvikt',
                                                                 'Kommer: respirasjonssvikt' = 'sirkSvikt',
                                                                 'Kommer: Antibiotikaordinasjon' = 'antibiotika',
                                                                 'Kommer: nyre/sirk/respsvikt, inn(+forvirring)/ut',
                                                                 'Kommer: grad av sirksvikt, inn/ut',
                                                                 'Kommer: grad av respsvikt, inn/ut',
                                                                 'Kommer: Demografi og epidemYrke' = 'yrke',
                                                                 'Kommer: sanns. smittested' = 'smittested',
                                                                 'Kommer: fylker' = 'fylker')
                                         ),
                                         selectInput(inputId = ns("enhetsUtvalgFord"), label="Velg enhetsnivå",
                                                     choices = c('Valgt enhet mot resten'=1, 'Hele landet'=0, 'Egen enhet'=2)
                                         )),
                        conditionalPanel(condition = paste0("input['", ns("resultater"), "'] == 'Tellinger'"),
                                         selectInput(inputId = ns('valgtVar'), label='Velg variabel',
                                                     choices = c('Antall registreringer'='antreg',
                                                                 'Antall døde'='antdod',
                                                                 'Antall utskrivinger'= 'antut',
                                                                 'Antall inneliggende'='antinn')
                                         )),
                        selectInput(inputId = ns("valgtEnhetRes"), label="Velg enhet",
                                    choices = 'Alle'
                        ),
                        selectInput(inputId = ns("skjemastatusInnRes"), label="Skjemastatus, inklusjon",
                                    choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                        ),
                        selectInput(inputId = ns("aarsakInnRes"), label="Covid-19 hovedårsak til innleggelse?",
                                    choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                        ),
                        selectInput(inputId = ns("dodShRes"), label="Utskrevne, tilstand",
                                    choices = c("Ikke valgt"=9,"Levende og døde"=3,  "Død"=2, "Levende"=1)
                        ),
                        selectInput(inputId = ns("erMannRes"), label="Kjønn",
                                    choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                        ),
                        br(),
                        actionButton(inputId = ns("tilbakestillValgRes"), label="Tilbakestill valg"
                        )
    ),
    mainPanel(
      tabsetPanel(id=ns("resultater"),
                  tabPanel('Antall registreringer',
                           value = 'Tellinger',
                           br(),
                           h2('Her kommer figur og tabell med antall registreringer'),
                           br(),
                           h3('Antall registreringer'),
                           h3('Ant. døde - utskrivingsdag'),
                           h3('Antall utskrivinger - utskrivingsdag'),
                           h3('Antall inneliggende'),
                           br(),
                           plotOutput(ns("FigurTidEnhet"), height="auto"),
                           # downloadButton(ns("LastNedFig"), label = 'Last ned figur'),
                           br(),
                           br(),
                           DT::DTOutput(ns("tabTidEnhet_DT")),
                           downloadButton(ns("lastNed"), "Last ned tabell")
                  ),
                  tabPanel(p('Fordelinger',
                             title='Figurer/tabeller for de fleste opplysninger registrert i
                  inlusjons- eller utskrivingsskjema'),
                           value = 'Fordelinger',
                           br(),
                           h2('Fordelingsfigurer, inkl. nedlastbare tabeller'),
                           h3('?Vise fordelingsfigurer bare for ferdigstilte skjema'),
                           plotOutput(ns('fordelinger'))
                  )
      )
    )
  )
}


koronaresultater <- function(input, output, session, KoroData, rolle, enhetsvalg, egetEnhetsNivaa, egenEnhet, hvdsession){

  observeEvent(input$tilbakestillValgRes, {
    shinyjs::reset("brukervalgRes")
  })

  # enhetsvalg <- if (rolle=='SC'){c('Alle', rhfNavn)} else {c(egenEnhet,'Alle')}
  updateSelectInput(session, "valgtEnhetRes", choices = enhetsvalg)


  AntTab <- function() {
    AntTab <- antallTidEnhTab(RegData=KoroData, tilgangsNivaa=rolle,
                              valgtEnhet= egenEnhet, #nivå avgjort av rolle
                              tidsenhet='dag',
                              aarsakInn = as.numeric(input$aarsakInnRes),
                              skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                              erMann=as.numeric(input$erMannRes))
    ant_skjema <- AntTab$Tab_tidy
    ant_skjema[-dim(ant_skjema)[1], ] <- ant_skjema[rev(1:(dim(ant_skjema)[1]-1)), ]
    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    AntTab$ant_skjema <- ant_skjema
    AntTab$sketch <- sketch
    AntTab
  }

  output$tabTidEnhet_DT = DT::renderDT(
    DT::datatable(AntTab()$ant_skjema[-dim(AntTab()$ant_skjema)[1], ],
                  container = AntTab()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )

  output$FigurTidEnhet <- renderPlot({
    AntTab <- AntTab()
    if (rolle != 'SC') {
      AntTab$Tab_tidy <- AntTab$Tab_tidy[, -(dim(AntTab$Tab_tidy)[2]-1)]
    }
    korona::FigTidEnhet(AntTab)
  }, width = 700, height = 700)


  # output$LastNedFig <- downloadHandler(
  #   filename = function(){
  #     paste0('KoronaFigur', Sys.time(), '.', input$bildeformat)
  #   },
  #
  #   content = function(file){
  #     AntTab <- AntTab()
  #     if (rolle != 'SC') {
  #       AntTab$Tab_tidy <- AntTab$Tab_tidy[, -(dim(AntTab$Tab_tidy)[2]-1)]
  #     }
  #     korona::FigTidEnhet(AntTab, outfile = file)
  #   }
  # )

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0('KoronaTabell', Sys.time(), '.csv')
    },

    content = function(file){
      Tabell1 <- AntTab()$Tab_tidy
      write.csv2(Tabell1, file, row.names = F, fileEncoding = 'latin1')
    }
  )

  ####### Fordelingsfigurer #########################################################################

  output$fordelinger <- renderPlot({
    KoronaFigAndeler(RegData=KoroData,
                     valgtVar=input$valgtVarFord,
                     valgtEnhet = input$valgtEnhetRes,  #input$valgtEnhetFord,
                     enhetsNivaa=egetEnhetsNivaa,
                     enhetsUtvalg = as.numeric(input$enhetsUtvalgFord),
                     dodSh=as.numeric(input$dodShRes),
                     aarsakInn = as.numeric(input$aarsakInnRes),
                     erMann=as.numeric(input$erMannRes),
                     skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                     kjemastatusUt=as.numeric(input$skjemastatusUtRes),
                     session = hvdsession)
  }, height=700, width=700 #height = function() {session$clientData$output_fordelinger_width}
  )
}
