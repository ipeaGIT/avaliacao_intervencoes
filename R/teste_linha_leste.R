
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')



# open metrofor gtfs --------------------------------------------------------------------------

gtfs <- "C:/Users/kaue/Downloads/gtfs_for_metrofor_2021-01.zip"

# spreadshhet here
# https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing

headways_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                          sheet = "headways_df") %>% setDT()



ttime_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "ttime_df") %>% setDT()




stops_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "stops_df") %>% setDT()


routes_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "routes_df") %>% setDT()


line_shape <- st_read("C:/Users/kaue/Documents/linha_leste_kaue_gearth.gpkg") %>%
  mutate(route_id = "LL", 
         direction_id = 0) %>%
  select(route_id, direction_id)

# teste
line_shape <- rbind(line_shape, st_reverse(line_shape)) %>%
  mutate(direction_id = c(0, 1))





# stops_sf <- st_as_sf(stops_df, coords = c("stop_lon", "stop_lat"),
#                      # crs SIRGAS 2000 / UTM zone 24S
#                      crs = 31984
# ) %>%
#   st_transform(4326)
# 
# mapview(stops_sf)

stops_crs <- 31984

create_stoptimes_by_line <- function(gtfs, 
                                     headways_df, ttime_df, stops_df, routes_df,
                                     line_shape, line_both_directions = TRUE,
                                     service_id = c("weekdays", "weekends"),
                                     agency_id = c("guess"),
                                     stops_crs) {
  
  
  # open original gtfs
  gtfs_original <- gtfstools::read_gtfs(gtfs,
                                        files = c("agency", "calendar"))
  
  # select service_id based on the calendar choice
  if (calendar_days == "weekdays") {
    
    # filter only weekdays
    service_id1 <- gtfs_original$calendar[monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1]$service_id
    
    if (length(service_id1) == 0) {
      
      stop("There is no service_id for all weekday trips/nIf you want one, we advise you to create manually")
      
    }
    
    # get the first one
    service_id <- unique(service_id)[1]
    
  } else if (calendar_days == "weekends") {
    
    # filter only weekends
    service_id1 <- gtfs_original$calendar[saturday == 1 & sunday == 1]$service_id
    
    if (length(service_id1) == 0) {
      
      stop("There is no service_id for weekends trips/nIf you want one, we advise you to create manually")
      
    }
    
  }
  
  
  # check if all stops located in the ttime_df are in the stops_df
  stops_check <- setdiff(c(ttime_df$stop_id_start, ttime_df$stop_id_end), stops_df$stop_id)
  
  if(length(stops_check) > 0) {
    
    stop("The following stop_id are located in your ttime_df but are not listed in your stops_df:\n",
         paste0(stops_check, collapse = ", "), "\n",
         "Please add these stop_id to your stops_df")
    
  }
  
  # check if all shapes located in the headways_df and ttime_df are in the line_shape
  shapes_headways_check <- setdiff(c(paste0(headways_df$route_id, "_", headways_df$direction_id)), 
                                   paste0(line_shape$route_id, '_', line_shape$direction_id))
  
  if(length(shapes_headways_check) > 0) {
    
    stop("The following shape_id are located in your headways_df but are not listed in your line_shape:\n",
         paste0(shapes_headways_check, collapse = ", "), "\n",
         "Please add these shape_id to your line_shape")
    
  }
  
  
  
  shapes_ttime_check <- setdiff(c(paste0(ttime_df$route_id, "_", ttime_df$direction_id)), 
                                paste0(line_shape$route_id, '_', line_shape$direction_id))
  
  if(length(shapes_ttime_check) > 0) {
    
    stop("The following shape_id are located in your ttime_df but are not listed in your line_shape:\n",
         paste0(shapes_ttime_check, collapse = ", "), "\n",
         "Please add these shape_id to your line_shape")
    
  }
  
  
  
  
  # 1. Calculate number of trips each hour ------------------------------------------------------
  headways_df[, n_trips := floor((hour_end-hour_start)*60/headway), by = .(route_id, direction_id)]
  # change start_time to ITime
  headways_df[, start_trip := as.ITime(paste0(hour_start, ":00:00"))]
  
  
  # 2. Create dummy df with each trip start --------------------------------------------------------  
  
  # create temporary df with the trips based on its start hour of frequencies
  df <- data.table(
    # trip_id = rep(headways_df$trip_id, headways_df$n_trips),
    route_id = rep(headways_df$route_id, headways_df$n_trips),
    direction_id = rep(headways_df$direction_id, headways_df$n_trips),
    start_trip = rep(headways_df$start_trip, headways_df$n_trips),
    headway = rep(headways_df$headway * 60, headways_df$n_trips))
  
  # tag each interval, which are the trips with unique headways
  df[, interval := rleid(start_trip)]
  
  # cum sum of headway of each interval of unique headways
  df[, headway_cum := cumsum(c(0, headway[-.N])), by = .(route_id, interval, direction_id)]
  
  # apply cumulative headways to each start trip - now we have corrects start trips
  df[, start_trip := start_trip + headway_cum]
  df[, start_trip := as.ITime(start_trip)]
  
  # identify trip_id
  df[, trip_id := paste0(route_id, "-", direction_id, "-", rleid(start_trip)),
     by = .(route_id, direction_id)]
  
  # 3. Generate new stop_times based on the information created on the dummy df --------------------
  # Assembly new stop_times
  
  # ttime_df[, ':='(stop_sequence_start = seq_len(.N),
  #                 stop_sequence_end = seq_len(.N) + 1,
  #                 nstops = .N + 1),
  #          
  #          by = .(route_id, direction_id)]
  
  
  # ttime_df_long <- ttime_df %>%
  #   group_by(route_id, direction_id) %>%
  #   mutate(group  = seq_len(n())) %>%
  #   ungroup() %>%
  #   pivot_longer(cols = stop_id_start:stop_id_end,
  #                names_to = "teste",
  #                values_to = "stop_id") %>%
  #   group_by(route_id, direction_id, group) %>%
  #   summarise(stop_id = first(stop_id),
  #             ttime = first(ttime)) %>%
  #   ungroup()
  
  
  # transform from wide to long stop_times style
  ttime_df_long <-  ttime_df[, .(stop_id = rep(unique(c(stop_id_start, stop_id_end)), each = 1),
                                 ttime = c(0, ttime)
  ),
  by = .(route_id, direction_id) ]
  
  # add stop_sequence
  ttime_df_long[, stop_sequence := 1:.N, by = .(route_id, direction_id)]
  
  # bring nstops to df
  df_v1 <- merge(df, ttime_df_long,
                 by = c("route_id", "direction_id"),
                 sort = FALSE,
                 allow.cartesian=TRUE)
  
  # df[, .(arrival_time = rep(start_trip, each = nstops)
  #        # stop_sequence = seq_along(.N)), 
  # ),
  # 
  # 
  # by = .(route_id, direction_id)]
  
  
  
  
  
  # # create temporary df with the stop_times with correct start_trip but not with correct time between stops
  # df_v1 <- rbind(
  #   
  #   # ida
  #   data.table(
  #     # trip_id      = rep(stop_times_teste_sub$trip_id, nrow(df)),
  #     direction_id = 0,
  #     arrival_time = rep(df[direction_id == 0]$start_trip,  each = nstops),
  #     headway      = rep(c(0, ttime_df[direction_id == 0]$ttime), times = nrow(df[direction_id == 0]))),
  #   
  #   # volta
  #   data.table(
  #     # trip_id      = rep(stop_times_teste_sub$trip_id, nrow(df)),
  #     direction_id = 1,
  #     arrival_time = rep(df[direction_id == 1]$start_trip,  each = nstops),
  #     headway      = rep(c(0, ttime_df[direction_id == 1]$ttime), times = nrow(df[direction_id == 1])))
  #   
  # )
  # 
  # # create unique id for each new trip
  # df_v1[, trip_id := paste0(unique(headways_df$route_id), "_", direction_id, "_", rleid(arrival_time)),
  #       by = direction_id]
  # # create stop_sequence
  # df_v1[, stop_sequence := 1:.N, by = trip_id]
  
  # add delay to each stop
  # df_v1[, departure_time_novo := as.numeric(arrival_time) + delay]
  
  # cum sum of headway between each stop, by the trip_id
  df_v1[, ttime_cum := cumsum(ttime), by = trip_id]
  
  # sum cumulative headway to create a corrected departure_time
  df_v1[, arrival_time := as.numeric(start_trip) + headway_cum]
  # departure_time is arrival_time plus delay
  # df_v1[, departure_time_novo := arrival_time_novo + delay]
  
  # convert them back to itime
  df_v1[, arrival_time := as.ITime(arrival_time)]
  df_v1[, departure_time := as.ITime(arrival_time)]
  
  # # create stops with stop_sequence
  # stops_df1 <- 
  #   rbind(
  #     
  #     data.table(
  #       direction_id = 0,
  #       stop_id = unique(c(ttime_df[direction_id == 0]$stop_id_start, 
  #                          ttime_df[direction_id == 0]$stop_id_end)),
  #       stop_sequence = 1:nstops),
  #     
  #     data.table(
  #       direction_id = 1,
  #       stop_id = unique(c(ttime_df[direction_id == 1]$stop_id_start, 
  #                          ttime_df[direction_id == 1]$stop_id_end)),
  #       stop_sequence = 1:nstops)
  #     
  #   )
  # 
  # # bring stopid
  # df_fim <- left_join(df_v1, stops_df1[, .(direction_id, stop_sequence, stop_id)],
  #                     by = c("stop_sequence", "direction_id")) %>% setDT()
  
  # organize columns
  stop_times <- df_v1 %>%
    select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)
  
  setDT(stop_times)[, ':='(arrival_time = as.character(arrival_time),
                           departure_time = as.character(departure_time))]
  
  
  # generate new shapes -------------------------------------------------------------------------
  
  # colunas: shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled
  source("teste_linha_leste_shapes.R")
  shapes <- line_to_shapes(line_shape)
  
  # assemble shapes
  
  # when the same shape is used both for inbound (0) and outbound (0) trips,
  # we assume that the input shape is the inbound one and that the outbound
  # one will be the reverse of the inbound
  # what will define if the shape is inbound or outbound is the sequence of the shape points
  # if (line_both_directions) {
  #   
  #   shapes <- rbind(
  #     
  #     # ida
  #     data.table(shape_id = paste0(unique(headways_df$route_id), "_0"),
  #                shape_pt_lat = shapes$lat,
  #                shape_pt_lon = shapes$lon,
  #                shape_pt_sequence = shapes$shape_pt_sequence,
  #                shape_dist_traveled = shapes$shape_dist_traveled
  #     ),
  #     # volta
  #     data.table(shape_id = paste0(unique(headways_df$route_id), "_1"),
  #                shape_pt_lat = rev(shapes$lat),
  #                shape_pt_lon = rev(shapes$lon),
  #                shape_pt_sequence = shapes$shape_pt_sequence,
  #                shape_dist_traveled = rev(shapes$shape_dist_traveled)
  #     )
  #     
  #     
  #   )
  #   
  # } 
  
  
  
  # genarate new stops --------------------------------------------------------------------------
  # colunas: stop_id, stop_code, stop_name, stop_lat, stop_lon, location_type
  
  stops <- stops_df %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = stops_crs) %>%
    st_transform(crs = 4326) %>%
    sfc_as_cols()
  
  stops <- stops %>%
    mutate(location_type = 0) %>%
    select(stop_id, stop_name, stop_lat = lat, stop_lon = lon, location_type) %>%
    setDT()
  
  
  
  # check stops and shapes ----------------------------------------------------------------------
  
  
  
  
  # generate new routes -------------------------------------------------------------------------
  # colunas: route_id, agency_id, route_short_name, route_long_name, route_type
  
  # routes <- data.table(
  #   route_id = unique(headways_df$route_id),
  #   agency_id = unique(gtfs_original$agency_id)[1],
  #   route_short_name = c("LL"),
  #   route_long_name = c("Metro Linha Leste"),
  #   route_type = c(1)
  # ) 
  
  routes <- routes_df[, agency_id := unique(gtfs_original$agency$agency_id)[1]]
  
  
  
  
  # generate new trips --------------------------------------------------------------------------
  # colunas: route_id, service_id, trip_id, direction_id, shape_id
  
  # trips <- data.table(route_id = unique(headways_df$route_id),
  #                         service_id = 4,
  #                         trip_id = unique(df_fim_go$trip_id),
  #                         direction_id = unique(headways_df$direction_id))
  
  
  
  trips <- distinct(df_v1, trip_id, .keep_all = TRUE) %>% 
    mutate(route_id = unique(headways_df$route_id),
           service_id = service_id1) %>%
    select(route_id, service_id, trip_id, direction_id) %>%
    # odentofy shape_id
    mutate(shape_id = paste0(route_id, "_", direction_id))
  
  
  
  
  
  
  # generate new calendar -----------------------------------------------------------------------
  # colunas: service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday, start_date, end_date
  
  
  
  # combine new gtfs files ----------------------------------------------------------------------
  
  gtfs_new <- list(stop_times = stop_times, 
                   stops = stops, 
                   routes = routes, 
                   trips = trips, 
                   shapes = shapes)
  
  # teste
  class(gtfs_new) <- 'dt_gtfs'
  
  gtfs_merge <- gtfstools::merge_gtfs(
    gtfs,
    gtfs_new
  )
  
  return(gtfs_merge)
  
}



