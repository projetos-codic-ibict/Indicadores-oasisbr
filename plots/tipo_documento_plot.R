renderTipoDocumentoPlot <- function(x) {

## Tipo de documento
### Cria subconjunto
#x <- busca_oasisbr(lookfor="varíola do macaco")
tipoDocumento_facet <- x$facets$format

shiny::validate(need(is.null(tipoDocumento_facet)==FALSE, paste("A sua busca não corresponde a nenhum registro.")))

traducao_tipoDocumento <- data.frame(valuePor = c("Artigo","Dissertação","Tese","Trabalho de conclusão de curso","Relatório","Capítulo de livro","Livro","NA","Artigo de conferência","Artigo (review)","Outros","Conjunto de dados","Artigo (working paper)"),
                                     value = c("article","masterThesis","doctoralThesis","bachelorThesis","report","bookPart","book","<NA>","conferenceObject","review","other","dataset","workingPaper"))

tipoDocumento_facet <- left_join(tipoDocumento_facet, traducao_tipoDocumento,by="value")

## Ordena coluna 'count'
tipoDocumento_facet <- tipoDocumento_facet[with(tipoDocumento_facet, order(-count)),]

## Retira registro 'sem informação' da coluna 'value'
tipoDocumento_facet <- tipoDocumento_facet[tipoDocumento_facet$translated!='sem informação',]

## Adiciona % do total
tipoDocumento_facet <- tipoDocumento_facet %>% mutate(pctTotal=count/x$resultCount)

## Cria vetor com cores
colors <- c("#F8C300","#76B865", "#EA6A47")

## Treemap Idiomas

tipoDocumentoPlotly <- plot_ly(tipoDocumento_facet, labels = ~toupper(valuePor), 
                               marker = list(colors = colors),
                               texttemplate=paste('<b style="font-family: Lato !important; align=left; font-size:20px; font-weight:400;">',scales::comma(tipoDocumento_facet$count),'<br><b style="font-size:16px;">',tipoDocumento_facet$valuePor),
                               values = ~count, 
                               parents = ~NA, 
                               type = 'treemap',
                               #hovertext = ~ paste0(valuePor, ":", scales::comma(count),"<br>% do total:",scales::percent(pctTotal)),
                               hovertext = paste('<b style="font-family: Lato !important; align=left; font-size:14px; font-weight:400;">Tipo de documento:</b>',
                                                 '<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 ">',tipoDocumento_facet$valuePor,"</b>",
                                                 "<br><br>",
                                                 '<b style="font-family: Lato !important; align=left; font-size:14px font-weight:400; ">Total de documentos:</b>',
                                                 '<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 ">',scales::comma(tipoDocumento_facet$count),"</b>",
                                                 "<br><br>",
                                                 '<b style="font-family: Lato !important; align=left; font-size:14px font-weight:400; "">% do total:</b>',
                                                 '<b style="font-family: Lato !important; align=left; font-size:16px; font-weight:600 ">',scales::percent(tipoDocumento_facet$pctTotal),"</b>"
                               ),
                               hoverinfo = "text"
                              )

tipoDocumentoPlotly %>% 
  layout(font=t, xaxis=list(tickvals=~count)) %>% config(displayModeBar = F) 
  

}
