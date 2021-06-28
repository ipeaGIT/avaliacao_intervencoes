options(java.parameters = '-Xmx10G')
library(r5r)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')

calculate_ttmatrix <- function(sigla_muni) {
  
  points <- fread(sprintf("../../data/avaliacao_intervencoes/r5/points/points_%s_09_2019.csv", sigla_muni)) %>% select(id = id_hex, lat = Y, lon = X)
  mode <- c("WALK", "TRANSIT")
  max_walk_dist <- 3000   # meters
  max_trip_duration <- 120 # minutes
  departure_datetime <- as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S")
  
  r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes/", sigla_muni), verbose = FALSE)
  
  # 3.1) calculate a travel time matrix
  ttm1 <- travel_time_matrix(r5r_core = r5r_core,
                            origins = points,
                            destinations = points,
                            mode = mode,
                            departure_datetime = departure_datetime,
                            time_window = 2,
                            max_walk_dist = max_walk_dist,
                            max_trip_duration = max_trip_duration)
  
  
  r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois/", sigla_muni), verbose = TRUE)
  
  ttm2 <- travel_time_matrix(r5r_core = r5r_core,
                            origins = points,
                            destinations = points,
                            mode = mode,
                            departure_datetime = departure_datetime,
                            time_window = 2,
                            max_walk_dist = max_walk_dist,
                            max_trip_duration = max_trip_duration)
  
  ttm <- full_join(ttm1, ttm2,
                   by = c("fromId", "toId"),
                   suffix = c("_antes", "_depois"))
  
  setDT(ttm)[, dif := travel_time_antes - travel_time_depois]

  }