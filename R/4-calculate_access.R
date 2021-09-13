library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(readr)



calcular_acess_muni <- function(sigla_muni, modo_acesso) {
  
  
  # status message
  message('Woking on city ', sigla_muni)
  
  # 1) Abrir tttmatrix ---------------------------------------------------
  
  
  ttmatrix_median <- read_rds(sprintf("../../data/avaliacao_intervencoes/%s/ttmatrix/ttmatrix_%s_%s.rds",
                                      sigla_muni, sigla_muni, modo_acesso))
  
  
  
  
  # 2) Agregar dados de uso do solo à ttmatrix --------------------------
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_09_%s.rds", "2019", sigla_muni, "2019")
  
  
  
  
  
  # Abrir oportunidades com hexagonos
  hexagonos_sf <- readr::read_rds(dir_hex) 
  
  
  # hexagonos_sf %>% filter(id_hex %in% c("8980104e373ffff", "89801045a17ffff")) %>% st_sf() %>% mapview::mapView()
  # 8980104e373ffff
  # library(mapview)
  # teste_centroid_origin <- hexagonos_sf %>% filter(id_hex %in% c("8980104e373ffff")) %>% st_centroid() %>% select(id = id_hex)
  # teste_centroid_dest <- hexagonos_sf %>% filter(id_hex %in% c("89801045a17ffff")) %>% st_centroid() %>% select(id = id_hex)
  # r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes/", sigla_muni), verbose = FALSE)
  # snap <- r5r::find_snap(r5r_core, teste_centroid_origin, mode = c("WALK"))  %>% st_as_sf(crs = 4326, coords = c("lon", "lat"))
  # snap <- r5r::find_snap(r5r_core, teste_centroid_dest, mode = c("WALK"))  %>% st_as_sf(crs = 4326, coords = c("lon", "lat"))
  # 
  # mapview(teste_centroid) + mapview(snap)
  # detailed <- r5r::detailed_itineraries(r5r_core, origins = teste_centroid_origin, destinations = teste_centroid_dest,
  #                                       mode = c("WALK", "TRANSIT"),
  #                                       max_trip_duration = 200)
  
  
  # Filtrar apenas colunas com info demograficas na origem
  hex_orig <- hexagonos_sf %>% dplyr::select(id_hex, 
                                             # variaveis de populacao - cor
                                             pop_total, cor_branca, cor_amarela, cor_indigena, cor_negra, 
                                             # variaveis de populacao - idade
                                             # matches("idade_"),
                                             # variaveis de renda
                                             renda_total, renda_capita, quintil, decil) %>% setDT()
  
  # Filtrar apenas colunas com info de uso do solo no destino
  hex_dest <- setDT(hexagonos_sf)[, .(id_hex, empregos_total, empregos_baixa, empregos_media, empregos_alta,  
                                      saude_total, saude_baixa, saude_media, saude_alta,
                                      edu_total, edu_infantil, edu_fundamental, edu_medio
                                      # mat_total, mat_infantil, mat_fundamental, mat_medio
  )]
  
  
  # Merge dados de origem na matrix de tempo de viagem
  ttmatrix <- ttmatrix_median[hex_orig, on = c("origin" = "id_hex"),  
                              c('pop_total','cor_branca','cor_amarela','cor_indigena','cor_negra',
                                # "idade_0a5", "idade_6a14", "idade_15a18", "idade_19a24",    
                                # "idade_25a39", "idade_40a69", "idade_70",
                                'renda_total','renda_capita','quintil','decil') :=
                                list(i.pop_total, i.cor_branca, i.cor_amarela, i.cor_indigena, i.cor_negra, 
                                     # i.idade_0a5, i.idade_6a14, i.idade_15a18, i.idade_19a24,    
                                     # i.idade_25a39, i.idade_40a69, i.idade_70,    
                                     i.renda_total, i.renda_capita, i.quintil, i.decil)]
  
  # Merge dados de destino na matrix de tempo de viagem
  ttmatrix <- ttmatrix[hex_dest, on = c("destination" = "id_hex"),  
                       c("empregos_total", "empregos_baixa","empregos_media","empregos_alta",
                         "saude_total", "saude_baixa", "saude_media", "saude_alta",
                         "edu_total","edu_infantil","edu_fundamental","edu_medio"
                         # "mat_total", "mat_infantil", "mat_fundamental", "mat_medio"
                       ) :=
                         list(i.empregos_total, i.empregos_baixa,i.empregos_media,i.empregos_alta,
                              i.saude_total, i.saude_baixa, i.saude_media, i.saude_alta,
                              i.edu_total,i.edu_infantil,i.edu_fundamental,i.edu_medio
                              # i.mat_total, i.mat_infantil, i.mat_fundamental, i.mat_medio
                         )]    
  
  
  
  
  # Dicionario de variaveis:
  # Acessibilidade:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
  # - TMI = Acessibilidade de Tempo Mínimo à Oportunidade
  # Atividades:
  # - PT ~ "pop_total"
  # - PB ~ "cor_branca"
  # - PA ~ "cor_amarela"
  # - PI ~ "cor_indigena"
  # - PN ~ "cor_negra"
  # - TT ~ "empregos_total"
  # - TQ ~ "empregos_match_quintil"
  # - TD ~ "empregos_match_decil"
  # - ST ~ "saude_total"
  # - SB ~ "saude_baixa"
  # - SM ~ "saude_media"
  # - SA ~ "saude_alta"
  # - ET ~ "edu_total"
  # - EI ~ "edu_infantil"
  # - EF ~ "edu_fundamental"
  # - EM ~ "edu_medio"
  # - EI ~ "edu_infantil"
  
  
  # 3) Calcular acessibilidade cumulativa ativa ----------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  acess_cma <- "CMA"
  atividade_cma <- c("TT", #"TQ", "TD", 
                     "ST", "SB", "SM", "SA", 
                     "ET", "EI", "EF", "EM"
                     # "MT", "MI", "MF", "MM"
  )
  # criar dummy para tt
  tt <- c(1, 2, 3, 4)
  
  # tipo
  # tipo <- c("A", "D")
  
  grid_cma <- expand.grid(acess_cma, atividade_cma, tt, stringsAsFactors = FALSE) %>%
    rename(acess_sigla = Var1, atividade_sigla = Var2, tt_sigla = Var3) %>%
    # adicionar colunas de time threshold  para cada um dos modos
    mutate(tt_tp = case_when(
      tt_sigla == 1 ~ 30,
      tt_sigla == 2 ~ 60,
      tt_sigla == 3 ~ 90,
      tt_sigla == 4 ~ 120
    )) %>%
    mutate(junto_tp = paste0(acess_sigla, atividade_sigla, tt_tp)) %>%
    mutate(atividade_nome = case_when(atividade_sigla == "TT" ~ "empregos_total",
                                      atividade_sigla == "TQ" ~ "empregos_match_quintil",
                                      atividade_sigla == "TD" ~ "empregos_match_decil",
                                      atividade_sigla == "ST" ~ "saude_total",
                                      atividade_sigla == "SB" ~ "saude_baixa",
                                      atividade_sigla == "SM" ~ "saude_media",
                                      atividade_sigla == "SA" ~ "saude_alta",
                                      atividade_sigla == "ET" ~ "edu_total",
                                      atividade_sigla == "EF" ~ "edu_fundamental",
                                      atividade_sigla == "EM" ~ "edu_medio",
                                      atividade_sigla == "EI" ~ "edu_infantil",
                                      atividade_sigla == "MT" ~ "mat_total",
                                      atividade_sigla == "MF" ~ "mat_fundamental",
                                      atividade_sigla == "MM" ~ "mat_medio",
                                      atividade_sigla == "MI" ~ "mat_infantil")) %>% setDT()
  
  
  # gerar o codigo
  # para tp
  codigo_cma_a <- c(
    
    sprintf("%s = (sum(%s[which(ttime_antes <= %s)], na.rm = T))", 
            grid_cma$junto_tp, 
            grid_cma$atividade_nome, 
            grid_cma$tt_tp
    )
    
  )
  
  codigo_cma_d <- c(
    
    sprintf("%s = (sum(%s[which(ttime_depois <= %s)], na.rm = T))", 
            grid_cma$junto_tp, 
            grid_cma$atividade_nome, 
            grid_cma$tt_tp
    )
    
  )
  
  
  # dar nomes às variaveis
  to_make_cma_a <-    setNames(codigo_cma_a,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_a))
  to_make_cma_d <-    setNames(codigo_cma_d,    sub('^([[:alnum:]]*) =.*', '\\1', codigo_cma_d))
  
  
  
  # rodar funcao para calcular access
  acess_cma_a <- ttmatrix[, 
                        lapply(to_make_cma_a, function(x) eval(parse(text = x)))
                        , by=.(city, mode, origin, quintil, decil, pop_total)]
  acess_cma_a[, tipo := "antes"]
  
  
  acess_cma_d <- ttmatrix[, 
                        lapply(to_make_cma_d, function(x) eval(parse(text = x)))
                        , by=.(city, mode, origin, quintil, decil, pop_total)]
  acess_cma_d[, tipo := "depois"]
  
  
  # juntar
  acess_cma <- rbind(acess_cma_a, acess_cma_d)
  
  # identificar modo
  acess_cma[, modo_acesso := modo_acesso]
  
  
  
  
  
  # Transformar para sf
  acess_sf <- merge(acess_cma, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                    by.x = "origin",
                    by.y = "id_hex",
                    all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  
  
  # 8) Salvar output --------------------------------------
  
  path_out <- sprintf("../../data/avaliacao_intervencoes/%s/output_access/acess_%s_%s.rds", sigla_muni, sigla_muni, tolower(modo_acesso))
  write_rds(acess_sf, path_out)
  
  
}



calcular_acess_muni("for", "WALK")
calcular_acess_muni("for", "BICYCLE")

calcular_acess_muni("goi", "WALK")
