options(java.parameters = '-Xmx10G')
library(r5r)


create_network <- function(sigla_muni) {
  
  
  # antes
  path_antes <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes/", sigla_muni)
  r5r::setup_r5(data_path = path_antes, use_elevation = TRUE, overwrite = TRUE)
  
  # depois
  path_depois <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois/", sigla_muni)
  r5r::setup_r5(data_path = path_depois, use_elevation = TRUE, overwrite = TRUE)
  
}



create_network("for")


