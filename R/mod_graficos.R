mod_graficos_UI <- function(id,x) {
  ns <- NS(id)
  ano_atual <- as.numeric(format(Sys.Date(), "%Y"))
  tagList(
    fluidRow(
     
  #column(12,h1("Indicadores gerais")),
  
  column(12,mod_total_de_documentos_UI("total_de_documentos")),

       column(12,
         fluidRow(
           column(6,textInput("textoBuscaInput", label=NULL, placeholder = "Filtre os documentos pelos indicadores abaixo",width = "100%")),
           column(3,selectInput("camposInput", label=NULL, choices = c("Todos os campos"="AllFields", "Título"="Title", "Autor"="Author","Assunto"="Subject"),width="100%")),
           column(3,actionButton("buscarButton", "Buscar",width="100%"))
           )
         ),
   
  column(12, uiOutput(ns("resultadosDaBuscaTextoOutput"))),
  
             box(
               title = "Título da fonte", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("titulo_fonteTopInput"),"Termos exibidos", min=1, max=30, 10,width="30%")),
               column(12,addSpinner(plotlyOutput(ns("titulo_fontePlotlyOutput"),height="312px"),spin="folding-cube",color="green")),
             ),
  
             box(
               title = "Instituições brasileiras com mais documentos", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("instituicoesTopInput"),"Termos exibidos",min=1, max=25, 10,width="30%")),
               column(12,addSpinner(plotlyOutput(ns("instituicoesPlotlyOutput"),height="300px"),spin="folding-cube",color="green")),
               height = "450px"
             ),
             
             box(
               title = "Documentos por tipo de acesso", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,addSpinner(plotlyOutput(ns("tipoAcessoPlotlyOutput"),height="350px"),spin="folding-cube",color="green")),
               tags$i(h5("As áreas no gráfico não são perfeitamente proporcionais aos valores de cada entrada",align="center")),
               height = "450px"
             ),
             
             box(
               title = "Documentos por tipo", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,addSpinner(plotlyOutput(ns("tipoDocumentoPlotlyOutput"),height="350px"),spin="folding-cube",color="green")),
               tags$i(h5("As áreas no gráfico não são perfeitamente proporcionais aos valores de cada entrada",align="center")),
               height = "450px"
             ),
  
             box(
               title = "Documentos por tipo e ano", width = 12, solidHeader = TRUE, #status = "primary",
               column(12,sliderInput(ns("tipo_docSliderInput"),"Ano de publicação",min = 1980, max = ano_atual, value = c(1980, 2021),width="30%",sep="")),
               column(12,addSpinner(plotlyOutput(ns("tipo_anoPlotlyOutput"),height="350px"),spin="folding-cube",color="green")),
             ),
             
             box(
               title = "Documentos por área do conhecimento do CNPq", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("subjectTopInput"),"Termos exibidos", 10, min = 1, max=25, width = "30%")),
               column(12,addSpinner(plotlyOutput(ns("subject_cnpqPlotlyOutput")),spin="folding-cube",color="green"))
             ),

             box(
               title = "Documentos por programa de pós-graduação - PPG", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("programTopInput"),"Termos exibidos", min = 1, max = 25, 10, width="30%")),
               column(12,addSpinner(plotlyOutput(ns("programPlotlyOutput")),spin="folding-cube",color="green"))
             ),
             
             box(
               title = "Autores com mais documentos", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("authorTopInput"),"Termos exibidos", min = 1, max = 30, 10, width="30%")),
               column(12,addSpinner(plotlyOutput(ns("authorPlotOutput"),height="437px"),spin="folding-cube",color="green"))
             ),

             box(
               title = "Documentos por idioma", width = 6, solidHeader = TRUE, #status = "primary",
               column(12,addSpinner(plotlyOutput(ns("idiomaPlotlyOutput"),height="475px"),spin="folding-cube",color="green")),
               tags$i(h5("As áreas no gráfico não são perfeitamente proporcionais aos valores de cada entrada",align="center"))
             ),
  
             box(
               title = "Pesquisadores com mais orientações", width = 12, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("pesquisadorTopInput"),"Termos exibidos", min=1, max=30, 10,width="30%")),
               column(12,addSpinner(plotlyOutput(ns("pesquisadorPlotlyOutput")),spin="folding-cube",color="green")),
             ),
             box(
               title = "Documentos por palavra-chave", width = 12, solidHeader = TRUE, #status = "primary",
               column(12,numericInput(ns("wordCloudTopInput"),"Termos exibidos", 30, min=1, max=30, width="30%")),
               column(12,addSpinner(wordcloud2Output(ns("palavraChavePlotOutput"), height="400px", width="auto"),spin="folding-cube",color="green"))
  )
    )
  )
}

mod_graficos_server <- function(id, base) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$authorPlotOutput <- renderPlotly({ renderAuthorPlot(oasisbrBuscaUser(),input$authorTopInput) })
      
      output$palavraChavePlotOutput <- renderWordcloud2({ wordCloudPlot(oasisbrBuscaUser(),input$wordCloudTopInput) })
      
      output$subject_cnpqPlotlyOutput <- renderPlotly({ renderSubjectPlot(oasisbrBuscaUser(),input$subjectTopInput)})
      
      output$publishDatePlotlyOutput <- renderPlotly({renderAnoPublicacaoPlot(oasisbrBuscaUser(),input$anoPublicacaoSliderInput[1],input$anoPublicacaoSliderInput[2])})
      
      output$programPlotlyOutput <- renderPlotly({renderProgramPlot(oasisbrBuscaUser(),input$programTopInput)})
      
      output$idiomaPlotlyOutput <- renderPlotly({renderIdiomaPlot(oasisbrBuscaUser())})
      
      output$tipoDocumentoPlotlyOutput <- renderPlotly({renderTipoDocumentoPlot(oasisbrBuscaUser())})
      
      output$tipoAcessoPlotlyOutput <- renderPlotly({renderTipoAcessoPlot(oasisbrBuscaUser())})
      
      output$pesquisadorPlotlyOutput <- renderPlotly({render_pesquisadorPlot(oasisbrBuscaUser(),input$pesquisadorTopInput)})
      
      output$instituicoesPlotlyOutput <- renderPlotly({render_instituicoesPlot(oasisbrBuscaUser(),input$instituicoesTopInput)})
      
      output$titulo_fontePlotlyOutput <- renderPlotly({render_titulo_fonte_Plot(oasisbrBuscaUser(),input$titulo_fonteTopInput)})
      
      output$tipo_anoPlotlyOutput <- renderPlotly({render_tipodoc_ano_plot(x=oasisbrBuscaUser_tipodoc_ano(),min_ano=input$tipo_docSliderInput[1],max_ano=input$tipo_docSliderInput[2])})
      
      # busca_tipo_ano_reativo <- reactive({
      #   
      #   x <- input$textoBuscaI
      #   
      # })
      
      # observeEvent(event_data("plotly_click",source="teste"), {
      # barData = event_data("plotly_click")
      # showModal(modalDialog(title = "Bar info", "Teste"))
      #  })
      
    }
  )
}