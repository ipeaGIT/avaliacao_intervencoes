options(java.parameters = '-Xmx10G')
library(r5r)


create_network <- function(sigla_muni) {
  
  # antes
  r5r::setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes/", sigla_muni))
  
  # depois
  r5r::setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois/", sigla_muni))
  
}



