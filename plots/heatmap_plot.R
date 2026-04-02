render_heatmap_plot <- function(x, min, max, n_instituicoes) {
  x <- x$facet_counts$facet_pivot$`publishDate,network_acronym_str`


  for (i in 1:nrow(x)) {
    y <- as.integer(x$value[i])
    z <- data.frame(x$pivot[i])

    ano <- y
    instituicao <- z$value
    quantidade <- z$count


    if (i == 1) {
      heatmap_data <- data.frame(instituicao, ano, quantidade)
    } else {
      heatmap_data_i <- data.frame(instituicao, ano, quantidade)
      heatmap_data <- rbind(heatmap_data, heatmap_data_i)
    }
  }


  # Cria coluna com totais por linha e total por coluna
  heatmap_data <- heatmap_data %>%
    group_by(ano) %>%
    mutate(totalAno = sum(quantidade)) %>%
    group_by(instituicao) %>%
    mutate(totalInstituicao = sum(quantidade))


  # Reordena
  heatmap_data$instituicao <- reorder(heatmap_data$instituicao, heatmap_data$totalInstituicao)

  # Cria variaveis com valor máximo e mínimo para ano de publicação
  ano_minimo <- min(heatmap_data$ano)
  ano_maximo <- max(heatmap_data$ano)

  # Top instituicoes
  top_instituicoes <- unique(heatmap_data[, c("instituicao", "totalInstituicao")])
  top_instituicoes <- head(top_instituicoes$instituicao, n = n_instituicoes)

  # Subseta DF com heatmap
  heatmap_data <- subset(heatmap_data, instituicao %in% top_instituicoes & ano %in% c(min:max))

  # Subseta DF com instituicoes unicas
  # heatmap_data <- subset(heatmap_data, instituicao %in% instituicoes_unicas)


  heatmap <- ggplot(heatmap_data, aes(ano, instituicao)) +
    theme_bw() +
    geom_tile(aes(fill = quantidade), colour = "white") +
    # scale_fill_distiller(palette = "YlGnBu", direction = 1)+
    scale_fill_gradient(low = "lightblue", high = "red", space = "Lab", na.value = "white") +
    labs(
      title = "Heatmap de publicações por ano\n",
      x = "Ano de publicação",
      y = "Nome da Instituição"
    )


  heatmap_plotly <- ggplotly(heatmap)
  ggplotly(heatmap)
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
render_tipodoc_ano_plot <- function(x, min_ano, max_ano) {
  x <- x$facet_counts$facet_pivot$`publishDate,format`

  req(!is.null(x))
  req(nrow(x) > 0)

  data <- data.frame()

  for (i in seq_len(nrow(x))) {
    y <- as.integer(x$value[i])
    z <- data.frame(x$pivot[[i]])

    if (nrow(z) == 0) next

    ano <- y
    formato <- z$value
    quantidade <- z$count

    data_i <- data.frame(formato, ano, quantidade)
    data <- rbind(data, data_i)
  }


  if (nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "Nenhum dado encontrado."))
  }

  data <- data %>%
    group_by(ano) %>% mutate(totalAno = sum(quantidade)) %>%
    group_by(formato) %>% mutate(total_formato = sum(quantidade))

  traducao_tipoDocumento <- data.frame(
    valuePor = c("Artigo","Dissertação","Tese","Trabalho de conclusão de curso",
                 "Relatório","Capítulo de livro","Livro","NA","Artigo de conferência",
                 "Artigo (review)","Outros","Conjunto de dados","Artigo (working paper)"),
    value = c("article","masterThesis","doctoralThesis","bachelorThesis","report",
              "bookPart","book","<NA>","conferenceObject","review","other","dataset","workingPaper")
  )

  data <- left_join(data, traducao_tipoDocumento, by = c("formato" = "value"))

  data <- subset(data, ano %in% c(min_ano:max_ano))

  if (nrow(data) == 0) {
    return(plotly_empty() %>% layout(title = "Nenhum dado para o período selecionado."))
  }

  grafico <- ggplot(data) +
    aes(x = ano, y = quantidade, colour = valuePor) +
    geom_line(size = 0.5) +
    scale_color_hue(direction = 1) +
    theme_minimal()

  ggplotly(grafico) %>%
    layout(
      font = t,
      legend = list(title = list(text = "<b> Tipo de documento </b>")),
      margin = list(l = 50, b = 55),
      hoverlabel = list(bgcolor = "white")
    ) %>%
    config(displayModeBar = F)
}

# data2 <- data %>% group_by(ano,formato) %>% mutate(total_acumulado = cumsum(quantidade))
# esquisser(data2)
