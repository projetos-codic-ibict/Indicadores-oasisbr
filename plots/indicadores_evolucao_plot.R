oasisbrEvolucao <- tryCatch(
  {
    # Formatar a data no formato "dia/mes/ano"
    data_atual <- Sys.Date()
    data_formatada <- format(data_atual, "%d/%m/%Y")
    # Montar a URL corretamente
    url <- paste0("https://api-oasisbr.ibict.br/api/v1/evolution-indicators?init=01/01/2017&end=", data_formatada)
    # url <- paste0("http://172.16.17.19:3000/api/v1/evolution-indicators?init=01/01/2017&end=", data_formatada)

    oasisbrEvolucao <- fromJSON(url)

    print("========================")
    print("Indicadores de evolução:")
    print("STATUS: ONLINE")
    print("========================")
    return(oasisbrEvolucao)
  },
  error = function(e) {
    print("========================")
    print("Indicadores de evolução:")
    print("STATUS: ERRO!")
    print("========================")
    return(null)
  }
)


shiny::validate(need(is.null(oasisbrEvolucao) == FALSE, paste("Erro.")))


## Cria novo dataframe com 'content'
content <- oasisbrEvolucao


## Transforma coluna 'createdAt' para formato de data.
content$createdAt <- as.Date(content$createdAt)


## Gráfico
indicadores_evolucao_plotly <<- ggplotly(
  ggplot(content) +
    aes(
      x = createdAt,
      y = numberOfDocuments,
      colour = sourceType
    ) +
    geom_line(size = 0.5) +
    # geom_point(size=1)+
    scale_color_hue(direction = 1) +
    labs(
      x = "Ano",
      y = "Total de documentos",
      title = "",
      subtitle = "Subtítulo",
      caption = "Caption",
      color = "Tipo de fonte"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal()
) %>%
  layout(font = t) %>%
  config(displayModeBar = F)


####


indicadores_evolucao_fontes_plotly <<- ggplotly(
  ggplot(content) +
    aes(
      x = createdAt,
      y = numberOfNetworks,
      colour = sourceType
    ) +
    geom_line(size = 0.5) +
    # geom_point(size=0.52)+
    scale_color_hue(direction = 1) +
    labs(
      x = "Ano",
      y = "Total de fontes",
      title = "",
      subtitle = "Subtítulo",
      caption = "Caption",
      color = "Tipo de fonte"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal()
) %>%
  layout(font = t) %>%
  config(displayModeBar = F)
