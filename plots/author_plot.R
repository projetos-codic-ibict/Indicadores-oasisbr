renderAuthorPlot <- function(x,y) {

## Validação para busca sem registros
shiny::validate(need(x$resultCount>0, paste("A sua busca não corresponde a nenhum registro.")))  

author_facet <- x$facets$author_facet
#author_facet


## Validação para informação vazia.
shiny::validate(need(is.null(author_facet)==FALSE, paste("Não existem informações sobre esse(s) registro(s).")))

## Validação para informação vazia.
shiny::validate(need((y>0 & y<=30), paste("O número de termo exibidos precisa estar entre 0 e 30.")))


## Ordena coluna 'count'
author_facet <- author_facet[with(author_facet, order(-count)),]

## Retira registro 'sem informação' da coluna 'value'
author_facet <- author_facet[author_facet$value!='sem informação',]

## Adiciona % do total
author_facet <- author_facet %>% mutate(pctTotal=count/x$resultCount)

## Seleciona top 10
author_facet <- head(author_facet, n=y)

## Gráfico de top 10 Autore(a)s

authorPlot <- ggplot(author_facet) +
  aes(x = reorder(toupper(value), count), group = value, weight = count, 
      text=paste('<b style="font-family: Lato !important; align=left; font-size:14px; font-weight:400; color:gray">Autor(a):</b>',
                 '<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 color: black">',value,"</b>",
                 "<br><br>",
                 '<b style="font-family: Lato !important; align=left; font-size:14px font-weight:400; color:gray">Total de documentos:</b>',
                 '<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 color: black">',comma(count),"</b>",
                 "<br><br>"#,
                 #'<b style="font-family: Lato !important; align=left; font-size:14px font-weight:400; color:gray"">% do total:</b>',
                 #'<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 color: black">',scales::percent(pctTotal),"</b>"
      )
      #text=paste("Autor(a):",value,"<br>","Quantidade",comma(count))
      ) +
  geom_bar(fill = "#76B865") +
  
  scale_y_continuous(labels = scales::comma)+
  labs(x = "",
      #x = "<b style='color:gray'>Autor(a)</b><br><br><b style='color:white'>.", 
       y = "<b style='color:gray; font-size:14px'>Total de documentos", title = NULL) +
  
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14L)) +
  coord_flip()

authorPlot <- ggplotly(authorPlot, tooltip="text")

authorPlot %>%
  
  layout(font=t, 
         margin = list(l=50,b = 55),
         hoverlabel=list(bgcolor="white")
  ) %>% config(displayModeBar = F) 
  



}
