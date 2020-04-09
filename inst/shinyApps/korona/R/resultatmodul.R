koronaresultater_UI <- function(id){
  ns <- shiny::NS(id)


  shiny::sidebarLayout(
    shiny::sidebarPanel(id = ns('brukervalgRes'),

                        width = 3,
                        br(),
                        h3('Velg variabel/tema og filtreringer i data'),

                        #conditionalPanel(condition = paste0("input['", ns("resultater"), "'] == 'Tellinger'"),
                                         selectInput(inputId = ns('valgtVar'), label='Velg variabel',
                                                     choices = c('Antall innleggelser'='antreg',
                                                                 'Antall døde'='antdod',
                                                                 'Antall utskrivinger'= 'antut',
                                                                 'Antall inneliggende'='antinn')
                                         ),
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
      #tabsetPanel(id=ns("resultater"),
                  # tabPanel('Antall registreringer',
                  #          value = 'Tellinger',
                  #          br(),
                           h2('Tellinger:'),
                           h4('Antall registreringer, inneliggende, utskrivinger, døde'),
                           h3('NB:Siden er under utvikling!', style = "color:red"),
                           br(),
                           plotOutput(ns("FigurTidEnhet"), height="auto"),
                           # downloadButton(ns("LastNedFig"), label = 'Last ned figur'),
                           br(),
                           br(),
                           DT::DTOutput(ns("tabTidEnhet_DT")),
                           downloadButton(ns("lastNed"), "Last ned tabell")
                  #)

      #)
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
    AntTab <- switch(input$valgtVar,
                     'antreg'= antallTidEnhTab(RegData=KoroData, tilgangsNivaa=rolle,
                              valgtEnhet= egenEnhet, #nivå avgjort av rolle
                              tidsenhet='dag',
                              aarsakInn = as.numeric(input$aarsakInnRes),
                              skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                              erMann=as.numeric(input$erMannRes)),
                     'antdod'= antallTidAvdode(RegData=KoroData, tilgangsNivaa=rolle,
                                     valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                     tidsenhet='dag',
                                     aarsakInn = as.numeric(input$aarsakInnRes),
                                     skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                     erMann=as.numeric(input$erMannRes)),
                     'antut'=antallTidUtskrevne(RegData=KoroData, tilgangsNivaa=rolle,
                                             valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                             tidsenhet='dag',
                                             aarsakInn = as.numeric(input$aarsakInnRes),
                                             skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                             erMann=as.numeric(input$erMannRes))
    )
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

}
