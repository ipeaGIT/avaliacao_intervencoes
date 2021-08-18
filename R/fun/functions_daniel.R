bike_ttm <- function(graph, points_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  bike_speed <- 12
  bike_max_distance <- 5
  
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = "BICYCLE",
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 3,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  return(ttm)
  
}


transit_ttm <- function(graph, points_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = 180L,
    max_walk_dist = 3000,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  return(ttm)
  
}

# Primeira limitação: como setar o horário de partida da viagem após a primeira
# milha de bicicleta?
# Outra: provavelmente teria que usar o bike_parks_path antes e depois porque as
# novas estações vão ter bicicletários
bfm_ttm <- function(graph, points_path, bike_parks_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  bike_parks <- fread(bike_parks_path)
  bike_speed <- 12
  bike_max_distance <- 5
  
  first_mile_ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = bike_parks,
    mode = "BICYCLE",
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 3,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  rest_ttm <- travel_time_matrix(
    r5r_core,
    origins = bike_parks,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = 180L,
    max_walk_dist = 1500,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # join both tables together, calculate total travel time and keep only the
  # fastest trip between two points
  
  ttm <- merge(
    first_mile_ttm,
    rest_ttm,
    by.x = "toId",
    by.y = "fromId",
    all.y = TRUE,
    allow.cartesian = TRUE
  )
  
  setnames(
    ttm,
    old = c("fromId", "toId", "toId.y", "travel_time.x", "travel_time.y"),
    new = c("fromId", "intermediateId", "toId", "first_mile_time", "rest_time")
  )
  
  ttm[, travel_time := first_mile_time + rest_time]
  ttm <- ttm[
    ttm[, .I[travel_time == min(travel_time)], by = .(fromId, toId)]$V1
  ]
  
  return(ttm)
  
}


join_ttms <- function(bike_matrix,
                      transit_matrix,
                      bfm_matrix,
                      points_path) {
  
  points <- fread(points_path, select = "id_hex")
  setnames(points, "id")
  
  ttm <- setDT(expand.grid(toId = points$id, fromId = points$id))
  
  ttm[bike_matrix, on = c("toId", "fromId"), bike_time := i.travel_time]
  ttm[transit_matrix, on = c("toId", "fromId"), transit_time := i.travel_time]
  ttm[bfm_matrix, on = c("toId", "fromId"), bfm_time := i.travel_time]
  
  return(ttm)
  
}


exploratory_report <- function(ttm,
                               scenario,
                               bike_parks_path,
                               grid_path,
                               exploratory_skeleton) {
  
  bike_parks <- fread(bike_parks_path)
  
  random_points <- data.table(
    hex_name = c("aldeota", "parangaba", "luciano_cavalcanti", "carlito_pamplona"),
    lon = c(-38.499719, -38.562038, -38.487955, -38.558178),
    lat = c(-3.740666, -3.777242, -3.776920, -3.717195)
  )
  random_points <- st_as_sf(random_points, coords = c("lon", "lat"), crs = 4326)
  
  grid <- readRDS(grid_path)
  
  # suppress "attribute variables are assumed constant" warning
  suppressWarnings(
    relevant_ids <- st_intersection(grid, random_points)$id_hex
  )
  
  setDT(random_points)[, hex_id := relevant_ids]
  
  ttm <- ttm[fromId %in% relevant_ids]
  ttm[setDT(random_points), on = c(fromId = "hex_id"), hex_name := i.hex_name]
  
  # save exploratory analysis report to new folder 
  
  if (!dir.exists("./results")) dir.create("./results")
  
  filename <- normalizePath(
    paste0("./results/exploratory_analysis_", scenario, ".html")
  )
  
  rmarkdown::render(
    exploratory_skeleton,
    output_file = filename,
    params = list(
      ttm = ttm,
      scenario = scenario,
      bike_parks = bike_parks,
      grid = grid,
      random_points = random_points
    ),
    quiet = TRUE
  )
  
}


analyse_scenarios <- function(ttms, grid_path, analysis_skeleton) {
  
  grid <- setDT(readRDS(grid_path))
  
  accessibility <- lapply(
    ttms,
    function(dt) {
      dt <- dt[
        grid,
        on = c(toId = "id_hex"),
        `:=`(
          total_jobs = i.empregos_total,
          total_edu = i.edu_total,
          basic_health = i.saude_baixa
        )
      ]
      
      calculate_accessibility(dt, c("total_jobs", "total_edu", "basic_health"))
    }
  )
  
  # create threed different reports, one for job accessibility, the other for
  # education accessibility and the last for health accessibility
  
  if (!dir.exists("./results")) dir.create("./results")
  
  vapply(
    c("jobs", "health", "edu"),
    FUN.VALUE = character(1),
    FUN = function(type) {
      
      filename <- normalizePath(
        paste0("./results/scenario_analysis_", type, ".html")
      )
      
      # don't pass the whole accessibility dataset to the report, only relevant
      # columns
      
      relevant_cols <- c(
        "fromId",
        paste0(c("all_modes_", "transit_bike_", "only_transit_"), type)
      )
      smaller_accessibility <- lapply(
        accessibility,
        function(dt) {
          dt <- dt[, ..relevant_cols]
          dt <- setnames(dt, c("id", "all_modes", "transit_bike", "only_transit"))
        }
      )
      
      rmarkdown::render(
        analysis_skeleton,
        output_file = filename,
        params = list(
          access = smaller_accessibility,
          grid = grid,
          type = type
        ),
        quiet = TRUE
      )
      
    }
  )
  
}

calculate_accessibility <- function(ttm, opportunities) {
  
  only_transit_access <- ttm[
    transit_time <= 60,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  
  transit_bike_access <- ttm[
    transit_time <= 60 | bike_time <= 60,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  
  all_modes_access <- ttm[
    transit_time <= 60 | bike_time <= 60 | bfm_time <= 60,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  setnames(
    all_modes_access,
    c("fromId", "all_modes_jobs", "all_modes_edu", "all_modes_health")
  )
  
  all_modes_access[
    transit_bike_access,
    on = "fromId",
    `:=`(
      transit_bike_jobs = i.total_jobs,
      transit_bike_edu = i.total_edu,
      transit_bike_health = i.basic_health
    )
  ]
  all_modes_access[
    only_transit_access,
    on = "fromId",
    `:=`(
      only_transit_jobs = i.total_jobs,
      only_transit_edu = i.total_edu,
      only_transit_health = i.basic_health
    )
  ]
  
}