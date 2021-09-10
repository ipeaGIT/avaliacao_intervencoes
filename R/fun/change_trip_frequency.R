# 
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(Hmisc)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
source("R/fun/break_line_points.R")
source("R/fun/crop_line_btw_points.R")
# 
# 
# 
# 1) Fortaleza - metro linha leste ex-ante ---------------------------------------------------

gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs_for_metrofor_2021-01.zip"
gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs_for_etufor_2019-10.zip"
# lines to be changed frequency
gtfs <- read_gtfs(gtfs)

# identify lines to change frequency
routes_metrofor <- gtfs$routes %>% select(route_id, route_long_name)

# # table with new frequency
# metrofor_new_frequency <- data.frame(
#   route_id = c(8, 6, 7),
#   frequency = c(7.5, 6, 20)
# )

# route_id1 <- "026"; new_freq <- 4.6

change_trip_frequency <- function(gtfs, route_id1, new_freq) {
  
  # route_id1 <- 8
  # new_freq <- 7.5
  
  new_freq_sec <- new_freq * 60
  
  # get trips
  trips <- gtfs$trips
  # ATENCAO: para o caso de Fortaleza ETUFOR, a informacao de direction_id teve que ser
  # extraida a partir do trip_id
  trips[, direction_id := fifelse(stringr::str_sub(trip_id, -1, -1) == "I", 0, 1)]
  # get stoptimes
  stop_times <- gtfs$stop_times
  
  # bring route_id to stop_times
  stop_times_route <- merge(stop_times, trips[, .(trip_id, route_id, direction_id, service_id)], sort = FALSE)
  
  # filter route_id
  stop_times_route <- stop_times_route[route_id == route_id1]
  
  # filter only weekdays trips
  service_id_weekdays <- gtfs$calendar[monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1]
  stop_times_route <- stop_times_route[service_id %in% service_id_weekdays]
  
  # get start of each trip
  stop_times_starts <- stop_times_route[, .(arrival_time = arrival_time[1],
                                            n_stops = .N,
                                            ttime_trip = (last(as.ITime(arrival_time)) - first(as.ITime(arrival_time)))/60), 
                                        by = .(trip_id, direction_id)]
  setorder(stop_times_starts, direction_id, arrival_time)
  
  # select the first trip after 6am (correction is made only after morning peak)
  # stop_times_starts[arrival_time := as.ITime(arrival_time)]
  stop_times_starts <- stop_times_starts[arrival_time >= "06:00:00"]
  
  # get the first trip for inbound and outbound
  trips_starts <- stop_times_starts[, .(arrival_time = arrival_time[1], 
                                        trip_id = trip_id[1],
                                        n_stops = n_stops[1]), 
                                    by = direction_id]
  trips_starts[, arrival_time := as.ITime(arrival_time)]
  
  # replicate this by the new trip frequency
  trips_starts_new <- trips_starts[, .(arrival_time = seq(
    from = arrival_time, 
    to = as.ITime("10:00:00"),
    by = as.ITime(new_freq_sec)),
    n_stops = n_stops[1]
    # trip_id = trip_id[1]
  ),
  by = direction_id]
  
  
  # identify which trips we will replicate to get the travel times from
  stop_times_starts[, arrival_time := as.ITime(arrival_time)]
  setnames(stop_times_starts, "arrival_time", "arrival_time_reference")
  
  setkey(trips_starts_new, direction_id, arrival_time)
  setkey(stop_times_starts, direction_id, arrival_time_reference)
  
  a <- stop_times_starts[trips_starts_new, roll = "nearest"]
  
  # bring the entire set of stop_sequence and arrival_time from the stop_times
  # filter only trips of reference
  stop_times_reference <- stop_times_route[trip_id %in% a$trip_id]
  stop_times_reference[, arrival_time := ifelse(arrival_time == "", NA, arrival_time)]
  stop_times_reference[, arrival_time := as.ITime(arrival_time)]
  stop_times_reference[, arrival_time_reference := arrival_time[1], by = .(trip_id, direction_id)]
  # interpolate NAs
  stop_times_reference[, arrival_time := zoo::na.approx(arrival_time), by = .(trip_id, direction_id)]
  stop_times_reference[, arrival_time := as.ITime(arrival_time)]
  stop_times_reference[, ttime := arrival_time - lag(arrival_time), by = .(trip_id, direction_id)]
  stop_times_reference[, ttime := ifelse(is.na(ttime), 0, ttime)]
  stop_times_reference[, ttime := cumsum(ttime), by = .(trip_id, direction_id)]
  
  b <- merge(a,
             stop_times_reference,
             by = c("trip_id", "direction_id"),
             allow.cartesian = TRUE,
             sort = FALSE)

  
    
  # create trip_id
  trips_starts_new[, trip_id := paste0(trip_id, "_", rleid(arrival_time_reference.x)),
                   by = .(direction_id)]
  
  # replicate this by the number of stops to recreate stop times
  stop_times_new <- trips_starts_new[,
                                     .(arrival_time1 = rep(arrival_time1, n_stops)),
                                     by = .(trip_id, direction_id)]
  
  # create stop_sequence and trip_number
  stop_times_new[, ':='(stop_sequence = 1:.N
  ), by = .(trip_id, direction_id)]
  
  
  # get sequence of stops for the first trip
  stop_times_first <- stop_times_route[trip_id %in% trips_starts$trip_id]
  stop_times_first[, arrival_time := as.ITime(arrival_time)]
  stop_times_first[, ttime := arrival_time - lag(arrival_time), by = direction_id]
  stop_times_first[, ttime := ifelse(is.na(ttime), 0, ttime)]
  stop_times_first[, ttime := cumsum(ttime), by = direction_id]
  # select vars
  stop_times_first <- stop_times_first[, .(ttime, stop_sequence, stop_id, route_id, direction_id)]
  
  # join with the new stop_times
  stop_times_new_ttime <- merge(
    stop_times_new,
    stop_times_first,
    by = c("direction_id", "stop_sequence"),
    sort = FALSE
  )
  
  # cumsum to get new arrival_time
  stop_times_new_ttime[, arrival_time2 := arrival_time1 + as.ITime(ttime)]
  
  # organize
  stop_times_new_ttime <- stop_times_new_ttime %>%
    mutate(arrival_time = arrival_time2) %>%
    mutate(arrival_time = as.character(arrival_time)) %>%
    mutate(departure_time = arrival_time) %>%
    select(trip_id, direction_id, arrival_time, departure_time, stop_sequence, stop_id, route_id)
  
  # to bind them together, fist delete trips between 06h and 10h
  stop_times_starts1 <- stop_times_starts[arrival_time %between% c("06:00:00", "10:00:00")]
  stop_times_bind <- stop_times[trip_id %nin% stop_times_starts1$trip_id]
  
  # bind!!!!!!!!!!!!!!!!!
  stop_times_end <- rbind(stop_times_bind,
                          stop_times_new_ttime,
                          fill = TRUE)
  
  # identify first departure of each trip
  stop_times_end[, arrival_time_first := arrival_time[1], by = trip_id ]
  
  # stop_times_end <- stop_times_end %>%
  #   group_by(trip_id) %>%
  #   arrange(direction_id, arrival_time)
  setorder(stop_times_end, direction_id, arrival_time_first)
  
  
  
  # merge this new stop_times with the complete stop_times
  stop_times_complete <- rbind(
    stop_times[trip_id %nin% stop_times_route$trip_id],
    stop_times_end,
    fill = TRUE
  )
  
  stop_times_complete <- stop_times_complete %>% select(-route_id, -arrival_time_first, -direction_id)
  
  # setup new trips ---------------------------------------------------------
  trips_new <- distinct( stop_times_new_ttime, trip_id, direction_id, route_id)
  
  
  
  # get other vars
  trips_vars <- distinct(trips, route_id, direction_id, .keep_all = TRUE) %>% select(-trip_id)
  trips_new <- left_join(trips_new,
                         trips_vars,
                         by = c("route_id", "direction_id"))
  
  trips_complete <- rbind(
    trips[trip_id %nin% stop_times_starts1$trip_id],
    trips_new,
    fill = TRUE
    
  )
  
  
  
  # combine new gtfs files ----------------------------------------------------------------------
  
  gtfs$stop_times <- NULL
  gtfs$trips <- NULL
  
  # create a named list with the new gtfs files
  gtfs_new <- c(gtfs,
                list(stop_times = stop_times_complete,
                     trips      = trips_complete))
  
  # we need to identify it as 'dt_gtfs' class so it can match with the original gtfs
  class(gtfs_new) <- c('dt_gtfs', 'gtfs')
  
  return(gtfs_new)
  
  # # merge the new gtfs with the original gtfs
  # gtfs_merge <- gtfstools::merge_gtfs(
  #   gtfs_original,
  #   gtfs_new
  # )
  # 
  # class(gtfs_merge) <- c('dt_gtfs', 'gtfs')
  # return(gtfs_merge)
  
}


