koronaresultater_UI <- function(id){
  ns <- shiny::NS(id)


  shiny::sidebarLayout(
    shiny::sidebarPanel(id = ns('brukervalgRes'),

                        width = 3,
                        br(),
                        h3('Velg variabel/tema og filtreringer i data'),

                        selectInput(inputId = ns('valgtVar'), label='Velg variabel',
                                    choices = c('Antall innleggelser'='antreg',
                                                'Antall døde'='antdod',
                                                'Antall utskrivinger'= 'antut',
                                                'Antall inneliggende'='antinn')
                        ),
                        selectInput(inputId = ns("velgTidsenhet"), label="Velg tidsenhet",
                                    choices = c("Dag"="dag", "Uke"="uke", "Måned"="maaned")),
                        selectInput(inputId = ns("velgAntVisning"), label="Velg antall dager",
                                    choices = c(10, 20, 30, 50, 100, 200), selected = 30),
                        selectInput(inputId = ns("aarsakInnRes"), label="Covid-19 hovedårsak til innleggelse?",
                                    choices = c("Ja"=1, "Alle"=9, "Nei"=2)
                        ),
                        selectInput(inputId = ns("skjemastatusInnRes"), label="Skjemastatus, inklusjon",
                                    choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                        ),
                        selectInput(inputId = ns("erMannRes"), label="Kjønn",
                                    choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                        ),
                        br(),
                        actionButton(inputId = ns("tilbakestillValgRes"), label="Tilbakestill valg"
                        )
    ),
    mainPanel(
      h2('Tellinger:'),
      h4('Merk at i figur/tabell over antall døde så benyttes skjemadato på utskrivingsskjema i de tilfeller det ikke
                              finnes utskrivingsdato. Dette kan skje når man inkluderer registreringer i kladd.'),
      # h3('NB:Siden er under utvikling!', style = "color:red"),
      br(),
      plotOutput(ns("FigurTidEnhet"), height="auto"),
      # downloadButton(ns("LastNedFig"), label = 'Last ned figur'),
      br(),
      br(),
      # DT::DTOutput(ns("tabTidEnhet_DT")),
      tableOutput(ns("tabTidEnhet_plain")),
      downloadButton(ns("lastNed"), "Last ned tabell")
    )
  )
}


koronaresultater <- function(input, output, session, KoroData, rolle, enhetsvalg, egetEnhetsNivaa, egenEnhet, hvdsession){

  observeEvent(input$tilbakestillValgRes, {
    shinyjs::reset("brukervalgRes")
  })

  # enhetsvalg <- if (rolle=='SC'){c('Alle', rhfNavn)} else {c(egenEnhet,'Alle')}
  updateSelectInput(session, "valgtEnhetRes", choices = enhetsvalg)

  observe(
    switch (input$velgTidsenhet,
            "dag" = updateSelectInput(session, "velgAntVisning", label="Velg antall dager",
                                      choices = c(10, 20, 30, 50, 100, 200), selected = 30),
            "uke" = updateSelectInput(session, "velgAntVisning", label="Velg antall uker",
                                      choices = c(4, 8, 12, 20, 40, 100), selected = 8),
            "maaned" = updateSelectInput(session, "velgAntVisning", label="Velg antall måneder",
                                         choices = c(2, 4, 8, 12, 20), selected = 4)
    )
  )

  datoFra <- reactive(datoFra <- switch (input$velgTidsenhet,
                                         "dag" = Sys.Date() - days(as.numeric(input$velgAntVisning)-1),
                                         "uke" = floor_date(Sys.Date() - weeks(as.numeric(input$velgAntVisning)-1), unit = 'week', week_start = 1),
                                         "maaned" = floor_date(Sys.Date() - months(as.numeric(input$velgAntVisning)-1), unit = 'month')
  )
  )


  AntTab <- function() {
    AntTab <- switch(input$valgtVar,
                     'antreg'= antallTidEnhTab(RegData=KoroData, tilgangsNivaa=rolle,
                                               valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                               tidsenhet=input$velgTidsenhet,
                                               datoFra=datoFra(),
                                               aarsakInn = as.numeric(input$aarsakInnRes),
                                               skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                               erMann=as.numeric(input$erMannRes)),
                     'antdod'= antallTidAvdode(RegData=KoroData, tilgangsNivaa=rolle,
                                               valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                               tidsenhet=input$velgTidsenhet,
                                               datoFra=datoFra(),
                                               aarsakInn = as.numeric(input$aarsakInnRes),
                                               skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                               erMann=as.numeric(input$erMannRes)),
                     'antut'=antallTidUtskrevne(RegData=KoroData, tilgangsNivaa=rolle,
                                                valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                                tidsenhet=input$velgTidsenhet,
                                                datoFra=datoFra(),
                                                aarsakInn = as.numeric(input$aarsakInnRes),
                                                skjemastatusInn=as.numeric(input$skjemastatusInnRes),
                                                erMann=as.numeric(input$erMannRes)),
                     'antinn'= antallTidInneliggende(RegData=KoroData, tilgangsNivaa=rolle,
                                                     valgtEnhet= egenEnhet, #nivå avgjort av rolle
                                                     tidsenhet=input$velgTidsenhet,
                                                     datoFra=datoFra(),
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

  # output$tabTidEnhet_DT = DT::renderDT(
  #   DT::datatable(AntTab()$ant_skjema[-dim(AntTab()$ant_skjema)[1], ],
  #                 container = AntTab()$sketch,
  #                 rownames = F,
  #                 options = list(pageLength = 40)
  #   )
  # )

  output$tabTidEnhet_plain = renderTable(
    AntTab()$ant_skjema,
    digits = 0
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


koronabelegg_UI <- function(id){
  ns <- shiny::NS(id)


  shiny::sidebarLayout(
    shiny::sidebarPanel(id = ns('brukervalgBelegg'),

                        width = 3,
                        br(),
                        h3('Velg variabel/tema og filtreringer i data'),

                        selectInput(inputId = ns("aarsakInn"), label="Covid-19 hovedårsak til innleggelse?",
                                    choices = c("Alle"=9, "Ja"=1, "Nei"=2)
                        ),
                        selectInput(inputId = ns("skjemastatusInn"), label="Skjemastatus, inklusjon",
                                    choices = c("Alle"=9, "Ferdistilt"=2, "Kladd"=1)
                        ),
                        selectInput(inputId = ns("erMann"), label="Kjønn",
                                    choices = c("Begge"=9, "Menn"=1, "Kvinner"=0)
                        ),
                        br(),
                        actionButton(inputId = ns("tilbakestillValg"), label="Tilbakestill valg"
                        )
    ),
    mainPanel(

      h2('Estimert belegg sykehussenger'),
      h4('Tallene er basert på SSB sine tall på døgnplasser i somatikken per HF fra 2018, og beregnes som antall inneliggende delt på antall døgnplasser.'),
      # h3('Merk at ', style = "color:red"),
      br(),

      DT::DTOutput(ns("tabBelegg_DT")),

    )
  )
}


koronabelegg <- function(input, output, session, KoroData, rolle, reshID, egetEnhetsNivaa, egenEnhet, hvdsession){

  observeEvent(input$tilbakestillValgRes, {
    shinyjs::reset("brukervalgBelegg")
  })

  AntTab <- function() {
    AntTab <- antallTidBelegg(RegData=KoroData, tilgangsNivaa=rolle,
                              valgtEnhet= egenEnhet, #nivå avgjort av rolle
                              tidsenhet='dag', reshID = reshID,
                              aarsakInn = as.numeric(input$aarsakInn),
                              skjemastatusInn=as.numeric(input$skjemastatusInn),
                              erMann=as.numeric(input$erMann))
    ant_skjema <- AntTab$belegg_anslag_txt
    ant_skjema[-dim(ant_skjema)[1], ] <- ant_skjema[rev(1:(dim(ant_skjema)[1]-1)), ]
    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Dognplasser' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    AntTab$ant_skjema <- ant_skjema
    AntTab$sketch <- sketch
    AntTab
  }

  output$tabBelegg_DT = DT::renderDT(
    DT::datatable(AntTab()$ant_skjema[-dim(AntTab()$ant_skjema)[1], ],
                  container = AntTab()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )

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


