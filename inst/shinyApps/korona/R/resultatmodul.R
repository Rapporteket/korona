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
                 br(),
                 h2('Her kommer figur og tabell med antall registreringer'),
                 br(),
                 h3('Antall registreringer'),
                 h3('Ant. døde - utskrivingsdag'),
                 h3('Antall utskrivinger - utskrivingsdag'),
                 h3('Antall inneliggende')

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


koronaresultater <- function(input, output, session, KoroData, enhetsvalg, egetEnhetsNivaa, hvdsession){

  observeEvent(input$tilbakestillValgRes, {
    shinyjs::reset("brukervalgRes")
  })

  # enhetsvalg <- if (rolle=='SC'){c('Alle', rhfNavn)} else {c(egenEnhet,'Alle')}
  updateSelectInput(session, "valgtEnhetRes", choices = enhetsvalg)


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
