options(java.parameters = '-Xmx10G')
library(r5r)


create_network <- function(sigla_muni) {
  
  
  # antes
  path_antes <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes/", sigla_muni)
  files_remove <- dir(path_antes, pattern = "*.dat|*.mapdb|*.mapdb.p", full.names = TRUE)
  purrr::walk(files_remove, file.remove)
  r5r::setup_r5(data_path = path_antes)
  
  # depois
  path_depois <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois/", sigla_muni)
  files_remove <- dir(path_depois, pattern = "*.dat|*.mapdb|*.mapdb.p", full.names = TRUE)
  purrr::walk(files_remove, file.remove)
  r5r::setup_r5(data_path = path_depois)
  
}



