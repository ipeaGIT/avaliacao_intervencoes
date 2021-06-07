
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')



# 1) Fortaleza - metro linha leste ex-ante ---------------------------------------------------

gtfs_path <- "../../data-raw/avaliacao_intervencoes/fortaleza/gtfs_for_metrofor_2021-01.zip"


# open tables - they are in a spreadsheet
headways_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                          sheet = "headways_df", skip = 7) %>% setDT()



ttime_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "ttime_df", skip = 7) %>% setDT()


stops_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "stops_df", skip = 7) %>% setDT()


routes_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                        sheet = "routes_df", skip = 7) %>% setDT()


# open lines
line_shape <- st_read("../../data-raw/avaliacao_intervencoes/fortaleza/linha_leste_kaue_gearth.gpkg") %>%
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

a <- create_merge_gtfs(gtfs = gtfs_path,
                       headways_df = headways_df,
                       ttime_df = ttime_df,
                       stops_df = stops_df,
                       routes_df = routes_df,
                       line_shape = line_shape,
                       service_id = "weekdays",
                       stops_crs = 31984
)

# export gtfs
gtfstools::write_gtfs(a, path = "../../data/avaliacao_intervencoes/fortaleza/gtfs_for_metrofor_2021-01_new.zip")




# 2) Goiania -  ---------------------------------------------------





