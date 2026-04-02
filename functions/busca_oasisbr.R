## Funcões para download de busca feitas pelo usuário

################################################################################
# Função de busca para criar json com busca feita pelo usuario utilizando a API
busca_oasisbr <- function(url="https://oasisbr.ibict.br/vufind/api/v1/search?",
                          lookfor,
                          type="AllFields",
                          sort="relevance",
                          facet_parameters="&facet[]=author_facet&facet[]=dc.subject.por.fl_str_mv&facet[]=eu_rights_str_mv&facet[]=dc.publisher.program.fl_str_mv&facet[]=dc.subject.cnpq.fl_str_mv&facet[]=publishDate&facet[]=language&facet[]=format&facet[]=institution&facet[]=dc.contributor.advisor1.fl_str_mv&facet[]=network_name_str")
  {
  
  query <- paste(url,"lookfor=",URLencode(lookfor),"&type=",type,"&sort=",sort,facet_parameters,sep="")
  print(query)
  x <- fromJSON(query)
  
  return(x)
}
################################################################################



################################################################################
# Função de busca para criar heatmap utilizando a funcao 'facet.pivot' do solr
busca_oasisbr_heatmap <- function(url="http://172.16.17.42:8080/solr/biblio/select?facet.field=institution&facet=on&indent=on&facet.pivot=publishDate,network_acronym_str",
                                  q="*:*",
                                  rows="1",
                                  wt="json") {
  query <- paste(url,"&q=",URLencode(q),"&rows=",rows,"&wt=",wt,sep="")
  print(query)
  x <- fromJSON(query)
  
  return(x)
}
################################################################################


################################################################################
# Função de busca para tipo de documento por ano utilizando a funcao 'facet.pivot' do solr
busca_oasisbr_tipodoc_ano <- function(url="http://172.16.17.42:8080/solr/biblio/select?facet.field=institution&facet=on&indent=on&facet.pivot=publishDate,format",
                                  q="*:*",
                                  rows="1",
                                  wt="json") {
  query <- paste(url,"&q=",URLencode(q),"&rows=",rows,"&wt=",wt,sep="")
  #print(query)
  x <- fromJSON(query)
  
  return(x)
}
################################################################################