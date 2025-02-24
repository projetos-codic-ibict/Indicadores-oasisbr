mod_graficos_evolucao_UI <- function(id) {
  ns <- NS(id)
  ano_atual <- as.numeric(format(Sys.Date(), "%Y"))
  tagList(
    
    fluidRow(
      sliderInput("ano_evolucao_input",
                  label = "Ano",
                  min = 2017, max = ano_atual, value = c(2017, ano_atual), sep=""),
      box(title = "Número de documentos por mês", width = 12, solidHeader = TRUE, status = "primary",
        column(12,addSpinner(plotlyOutput(ns("evolucao_documentos_PlotlyOutput"),height="300px"),spin="folding-cube",color="green")),height = "450px"
      ),
      
      box(title = "Número de fontes por mês", width = 12, solidHeader = TRUE, status = "primary",
          column(12,addSpinner(plotlyOutput(ns("evolucao_fontes_PlotlyOutput"),height="300px"),spin="folding-cube",color="green")),height = "450px"
      ))
  
  )
}

mod_graficos_evolucao_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$evolucao_documentos_PlotlyOutput <- renderPlotly({ indicadores_evolucao_plotly })
      
      output$evolucao_fontes_PlotlyOutput <- renderPlotly({ indicadores_evolucao_fontes_plotly })
      
    }
  )
}


