options(java.parameters = '-Xmx10G')
library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
source("../acesso_oport/R/fun/setup.R")


# first, lets create the points -------------------------------------------

create_points_r5 <- function(sigla_muni) {
  
  dir_hex <- sprintf("../../data/acesso_oport/hex_agregados/%s/hex_agregado_%s_09_%s.rds", "2019", sigla_muni, "2019")
  
  
  hex_centroides <- readr::read_rds(dir_hex) %>%
    # Tirar hexagonos sem atividade
    filter(!(pop_total == 0 & 
               renda_total == 0 & 
               empregos_total == 0 & 
               saude_total == 0 & 
               edu_total == 0)) %>%
    dplyr::select(id_hex) %>%
    st_centroid() %>%
    sfc_as_cols(names = c("X","Y")) # funcao (dentro de stup.R) que transforma sf em data.frame com lat/long em colunas separadas
  
  
  # salvar centroids
  dir_output <- sprintf("../../data/avaliacao_intervencoes/r5/points/points_%s_09_2019.csv", sigla_muni)
  data.table::fwrite(hex_centroides, dir_output)
}

create_points_r5("for")
create_points_r5("goi")






# departure <- "02-10-2019 06:00:00"

calculate_ttmatrix <- function(sigla_muni, modo_acesso, departure = "02-03-2020 06:00:00") {
  
  points <- fread(sprintf("../../data/avaliacao_intervencoes/r5/points/points_%s_09_2019.csv", sigla_muni)) %>% dplyr::select(id = id_hex, lat = Y, lon = X)
  mode <- c(modo_acesso, "TRANSIT")
  max_walk_dist <- 3000   # meters
  max_trip_duration <- 180 # minutes
  departure_datetime <- as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S")
  
  
  # 1 - ttmatrix - antes ----------------------------------------------------
  
  
  
  r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes", sigla_muni), verbose = FALSE)
  
  # 3.1) calculate a travel time matrix
  ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                             origins = points,
                             destinations = points,
                             mode = mode,
                             departure_datetime = departure_datetime,
                             time_window = 120,
                             max_walk_dist = max_walk_dist,
                             max_trip_duration = max_trip_duration)
  
  
  
  
  # 2 - ttmatrix - depois ----------------------------------------------------
  
  r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois", sigla_muni), verbose = TRUE)
  
  ttm2 <- travel_time_matrix(r5r_core = r5r_core,
                             origins = points,
                             destinations = points,
                             mode = mode,
                             departure_datetime = departure_datetime,
                             time_window = 120,
                             max_walk_dist = max_walk_dist,
                             max_trip_duration = max_trip_duration)
  
  # juntar matrizes
  
  ttm <- full_join(ttm1, ttm2,
                   by = c("fromId", "toId"),
                   suffix = c("_antes", "_depois"))
  
  
  # rename
  ttm <- ttm %>% 
    mutate(city = sigla_muni, mode = "tp") %>%
    dplyr::select(city, mode, origin = fromId, destination = toId, ttime_antes = travel_time_antes, ttime_depois = travel_time_depois)
  
  # setDT(ttm)[, dif := ttime_depois - ttime_antes] %>% View()
  
  # save
  readr::write_rds(ttm, sprintf("../../data/avaliacao_intervencoes/%s/ttmatrix/ttmatrix_%s_%s.rds", sigla_muni, sigla_muni, tolower(modo_acesso)))
  
}


calculate_ttmatrix("for", "WALK", departure = "02-01-2020 06:00:00")

calculate_ttmatrix("goi", "WALK", departure = "02-10-2019 06:00:00")
