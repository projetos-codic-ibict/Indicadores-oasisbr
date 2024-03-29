
oasisbrEvolucao <- tryCatch(
  {
    oasisbrEvolucao <- fromJSON("http://localhost:3000/api/v1/evolution-indicators?init=10/10/2010&end=06/12/2022")
    print("========================")
    print("Indicadores de evolução:")
    print("STATUS: ONLINE")
    print("========================")
    return(oasisbrEvolucao)

  },
  error = function(e){
    
    oasisbrEvolucao <- fromJSON("data/indicadores_de_evolucao.json")
    print("========================")
    print("Indicadores de evolução:")
    print("STATUS: ERRO!")
    print("========================")
    return(oasisbrEvolucao)

  }
)


shiny::validate(need(is.null(oasisbrEvolucao)==FALSE, paste("Erro.")))


## Cria novo dataframe com 'content'
content <- oasisbrEvolucao$content


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
    #geom_point(size=1)+
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
  
) %>% layout(font=t) %>% config(displayModeBar = F)


####


indicadores_evolucao_fontes_plotly <<- ggplotly(
  
  ggplot(content) +
    aes(
      x = createdAt,
      y = numberOfNetworks,
      colour = sourceType
    ) +
    geom_line(size = 0.5) +
    #geom_point(size=0.52)+
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
  
) %>% layout(font=t) %>% config(displayModeBar = F)


