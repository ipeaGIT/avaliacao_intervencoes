
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
source("R/fun/break_line_points.R")
source("R/fun/crop_line_btw_points.R")



# 1) Fortaleza - metro linha leste ex-ante ---------------------------------------------------

gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs_for_metrofor_2018-11_mod.zip"


# open tables - they are in a spreadsheet
headways_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                          sheet = "headways_df", skip = 11) %>% setDT()



ttime_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "ttime_df", skip = 11) %>% setDT()


stops_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "stops_df", skip = 11) %>% setDT()


routes_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                        sheet = "routes_df", skip = 11) %>% setDT()


# open lines
line_shape <- st_read("../../data-raw/avaliacao_intervencoes/for/linha_leste_kaue_gearth.gpkg") %>%
  # identify route and direction
  mutate(route_id = "LL", 
         direction_id = 0) %>%
  select(route_id, direction_id)

# a linha leste so tem um shape - o de ida
# para pegar o shape de volta, eh necessario reverter a ordem do shape para que os pontos resultantes
# estejam no sentido certo
# depois, juntar os arquivos e colocar o sentido
line_shape <- rbind(line_shape, st_reverse(line_shape)) %>%
  mutate(direction_id = c(0, 1))


# stops_sf <- st_as_sf(stops_df, coords = c("stop_lon", "stop_lat"),
#                      # crs SIRGAS 2000 / UTM zone 24S
#                      crs = 31984
# ) %>%
#   st_transform(4326)
# 
# mapview(stops_sf)



# apply function to create the new gtfs and merge to the original one
purrr::walk(list.files("R/fun", full.names = TRUE), source)

a <- create_merge_gtfs(gtfs = gtfs,
                       headways_df = headways_df,
                       ttime_df = ttime_df,
                       stops_df = stops_df,
                       routes_df = routes_df,
                       line_shape = line_shape,
                       service_id = "weekdays",
                       stops_crs = 31984
)

# export gtfs
gtfstools::write_gtfs(a, path = "../../data/avaliacao_intervencoes/for/gtfs_for_metrofor_2018-11_mod_depois.zip")











# 2) Goiania -  ---------------------------------------------------



gtfs <- "../../data-raw/gtfs/goi/2019/gtfs_goi_rmtc_2019-10.zip"

gsheet <- "https://docs.google.com/spreadsheets/d/143Q6JuMsfvfYIruB4RzopnnJ22nOrD7fyKnQEgAjCaU/edit#gid=1999921628"


# open tables - they are in a spreadsheet
headways_df <- read_sheet(gsheet,
                          sheet = "headways_df", skip = 11) %>% setDT()



ttime_df <- read_sheet(gsheet,
                       sheet = "ttime_df", skip = 11) %>% setDT()


stops_df <- read_sheet(gsheet,
                       sheet = "stops_df", skip = 11) %>% setDT()


routes_df <- read_sheet(gsheet,
                        sheet = "routes_df", skip = 11) %>% setDT()


# 006 -> T001 -  ENS19B
# 007 -> T002 -  ENS19B
# 013 -> ENS19A -  ENS40

# open lines
line_shape <- st_read("../../data-raw/avaliacao_intervencoes/goi/Corredor_BRT.gpkg") %>%
  st_cast("POINT") %>%
  slice(1:152) %>%
  group_by(Name) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

points_routes <- list(
  points_006 = stops_df %>%
    filter(stop_id %in% c("T001", "ENS19B")) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    mutate(route_id = "BRT_006"),
  points_007 = stops_df %>%
    filter(stop_id %in% c("T002", "ENS19B")) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    mutate(route_id = "BRT_007"),
  points_013 = stops_df %>%
    filter(stop_id %in% c("ENS19A", "T006")) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    mutate(route_id = "BRT_013"))

# temos que quebrar o shape para cada linha
break_routes_goi <- function(stops, line) {
  
  a <- crop_line_btw_points(line = line, points = stops)
  a <- a %>%   mutate(route_id = unique(stops$route_id), 
                      direction_id = 1) %>%
    select(route_id, direction_id)
  
  # invert
  a0 <- st_reverse(a) %>% mutate(direction_id = 0)
  
  # bind
  a_fim <- rbind(a0, a)
  
}


line_shape_routes <- lapply(points_routes, FUN = break_routes_goi, line_shape) %>%
  rbindlist() %>% st_sf()


# a <- st_read(("../../data-raw/avaliacao_intervencoes/goi/goi_paradas_brt_nomeados.gpkg")) %>% st_zm()

# oi %>% filter(route_id == "BRT_013") %>% mapview() + a


# apply function to create the new gtfs and merge to the original one
purrr::walk(list.files("R/fun", full.names = TRUE), source)

a <- create_merge_gtfs(gtfs = gtfs,
                       headways_df = headways_df,
                       ttime_df = ttime_df,
                       stops_df = stops_df,
                       routes_df = routes_df,
                       line_shape = line_shape_routes,
                       service_id = "weekdays",
                       stops_crs = 31984
)

# lines to be deleated

# export gtfs
gtfstools::write_gtfs(a, path = "../../data/avaliacao_intervencoes/for/gtfs_for_metrofor_2018-11_mod_depois.zip")


