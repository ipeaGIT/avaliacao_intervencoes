# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
bike_ttm <- function(city, scenario, graph, points_path) {
  
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
    time_window = 1L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 2,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_bike.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}

# city <- tar_read(both_cities)[1]
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
transit_ttm <- function(city, scenario, graph, points_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  departure <- ifelse(
    city == "for",
    "02-03-2020 06:00:00",
    "02-10-2019 06:00:00"
  )
  
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = 180L,
    max_walk_dist = 1000,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_transit.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
# bike_parks_path <- tar_read(bike_parks_path)[1]
bfm_ttm <- function(city, scenario, graph, points_path, bike_parks_path) {
  
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
    time_window = 1L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 2,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # the first mile leg can take anywhere from 0 to 25 minutes in this case.
  # we then have to calculate 26 different remaining matrices, each one of them
  # with a different departure time (so 6:00, 6:01, ..., 6:24, 6:25).
  # we also need to subtract the first mile travel time from the max trip
  # duration and from the time window to keep their consistency.
  # finally, when merging the first mile and the remaining matrix we need to use
  # both the ids and the first mile duration as our indices - so we join not
  # only the right ids, but the trips that start at the correct time.
  
  # TODO: implement list of matrices
  
  remaining_ttm <- lapply(
    0:25,
    function(i) {
      departure_datetime <- as.POSIXct(
        "02-03-2020 06:00:00",
        format = "%d-%m-%Y %H:%M:%S"
      )
      departure_datetime <- departure_datetime + 60 * i
      max_trip_duration <- 180L - i
      
      travel_time_matrix(
        r5r_core,
        origins = bike_parks,
        destinations = points,
        mode = c("WALK", "TRANSIT"),
        departure_datetime = departure_datetime,
        time_window = 1L,
        max_trip_duration = max_trip_duration,
        max_walk_dist = 1000,
        n_threads = getOption("R5R_THREADS"),
        verbose = FALSE
      )
    }
  )
  names(remaining_ttm) <- 0:25
  remaining_ttm <- rbindlist(remaining_ttm, idcol = "departure_minute")
  remaining_ttm[, departure_minute := as.integer(departure_minute)]
  
  # join both tables together, calculate total travel time and keep only the
  # fastest trip between two points
  
  ttm <- merge(
    first_mile_ttm,
    remaining_ttm,
    by.x = c("toId", "travel_time"),
    by.y = c("fromId", "departure_minute"),
    allow.cartesian = TRUE
  )
  
  setnames(
    ttm,
    old = c("toId", "travel_time", "fromId", "toId.y", "travel_time.y"),
    new = c("intermediateId", "first_mile_time", "fromId", "toId", "remn_time")
  )
  
  ttm[, travel_time := first_mile_time + remn_time]
  ttm <- ttm[
    ttm[, .I[travel_time == min(travel_time)], by = .(fromId, toId)]$V1
  ]
  
  # there may be many trips between the same two points whose travel time equals
  # to the minimum travel time (e.g. imagine that you can get from point A to
  # point B using 3 stops as intermediate bike-first-mile stations and two of
  # those trips have the same total travel time, which is lower than the third).
  # therefore we need to filter 'ttm' to keep only one entry for each pair,
  # otherwise we will double (triple, quadruple, ...) count the opportunities
  # when estimating the accessibility.
  
  ttm <- ttm[ttm[, .I[1], by = .(fromId, toId)]$V1]
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_bike_first_mile.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# bike_matrix_path <- tar_read(bike_matrix)[1]
# transit_matrix_path <- tar_read(transit_matrix)[1]
# bfm_matrix_path <- tar_read(bike_first_mile_matrix)[1]
# points_path <- tar_read(points_path)[1]
join_ttms <- function(city,
                      scenario,
                      bike_matrix_path,
                      transit_matrix_path,
                      bfm_matrix_path,
                      points_path) {
  
  bike_matrix <- readRDS(bike_matrix_path)
  transit_matrix <- readRDS(transit_matrix_path)
  bfm_matrix <- readRDS(bfm_matrix_path)
  
  points <- fread(points_path, select = "id_hex")
  setnames(points, "id")
  
  ttm <- setDT(expand.grid(toId = points$id, fromId = points$id))
  
  ttm[bike_matrix, on = c("toId", "fromId"), bike_time := i.travel_time]
  ttm[transit_matrix, on = c("toId", "fromId"), transit_time := i.travel_time]
  ttm[bfm_matrix, on = c("toId", "fromId"), bfm_time := i.travel_time]
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_full.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# ttm_path <- tar_read(full_matrix)[1]
# scenario <- tar_read(before_after)[1]
# bike_parks_path <- tar_read(bike_parks_path)[1]
# grid_path <- tar_read(grid_path)[1]
# exploratory_skeleton <- tar_read(exploratory_skeleton)
exploratory_report <- function(city,
                               ttm_path,
                               scenario,
                               bike_parks_path,
                               grid_path,
                               exploratory_skeleton) {
  
  ttm <- readRDS(ttm_path)
  bike_parks <- fread(bike_parks_path)
  
  random_points <- data.table(
    hex_name = c("aldeota", "parangaba", "luciano_cavalcanti", "carlito_pamplona", "ponto_estranho", "ponto_ao_lado", "ponto_abaixo"),
    lon = c(-38.499719, -38.562038, -38.487955, -38.558178, -38.54842, -38.55137, -38.54847),
    lat = c(-3.740666, -3.777242, -3.776920, -3.717195, -3.708078, -3.706314, -3.711516)
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
  
  report_dir <- file.path("../../data/avaliacao_intervencoes", city, "reports")
  if (!dir.exists(report_dir)) dir.create(report_dir)
  
  filename <- normalizePath(
    file.path(report_dir, paste0("exploratory_analysis_", scenario, ".html"))
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


# # for_goi case
#
# city <- tar_read(both_cities)[1]
# scenario <- tar_read(before_after)[1]
# ttm_path <- tar_read(transit_matrix)[1]
# grid_path <- tar_read(grid_path)[1]
# opportunities <- c("total_jobs", "total_edu", "basic_health")
#
# # for special case
#
# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# ttm_path <- tar_read(full_matrix)[1]
# grid_path <- tar_read(grid_path)[1]
# opportunities <- c("total_jobs", "total_edu", "basic_health")
calculate_accessibility <- function(city,
                                    scenario,
                                    ttm_path,
                                    grid_path,
                                    opportunities = c(
                                      "total_jobs",
                                      "total_edu",
                                      "basic_health"
                                    )) {
  
  ttm <- readRDS(ttm_path)
  grid <- setDT(readRDS(grid_path))
  
  ttm <- ttm[
    grid,
    on = c(toId = "id_hex"),
    `:=`(
      total_jobs = i.empregos_total,
      total_edu = i.edu_total,
      basic_health = i.saude_baixa
    )
  ]
  
  # if ttm$bfm_time is NULL then this is the transit-only matrix. therefore,
  # only the transit-only bits of the accessibility calculation will be
  # conducted.
  # otherwise we are dealing with fortaleza-specific analysis that also include
  # bike-only and bike first mile scenarios
  
  setnames(ttm, old = "travel_time", new = "transit_time", skip_absent = TRUE)
  
  transit_access <- ttm[
    transit_time <= 60,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  
  access <- data.table(fromId = unique(ttm$fromId))
  access[
    transit_access,
    on = "fromId",
    `:=`(
      only_transit_CMATT60 = i.total_jobs,
      only_transit_CMAET60 = i.total_edu,
      only_transit_CMASB60 = i.basic_health
    )
  ]
  
  if (!is.null(ttm$bfm_time)) {
    
    only_bike_access <- ttm[
      bike_time <= 60,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      only_bike_access,
      on = "fromId",
      `:=`(
        only_bike_CMATT60 = i.total_jobs,
        only_bike_CMAET60 = i.total_edu,
        only_bike_CMASB60 = i.basic_health
      )
    ]
    
    only_bfm_access <- ttm[
      bfm_time <= 60,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      only_bfm_access,
      on = "fromId",
      `:=`(
        only_bfm_CMATT60 = i.total_jobs,
        only_bfm_CMAET60 = i.total_edu,
        only_bfm_CMASB60 = i.basic_health
      )
    ]
    
    transit_bike_access <- ttm[
      transit_time <= 60 | bike_time <= 60,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      transit_bike_access,
      on = "fromId",
      `:=`(
        transit_bike_CMATT60 = i.total_jobs,
        transit_bike_CMAET60 = i.total_edu,
        transit_bike_CMASB60 = i.basic_health
      )
    ]
    
    all_modes_access <- ttm[
      transit_time <= 60 | bike_time <= 60 | bfm_time <= 60,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      all_modes_access,
      on = "fromId",
      `:=`(
        all_modes_CMATT60 = i.total_jobs,
        all_modes_CMAET60 = i.total_edu,
        all_modes_CMASB60 = i.basic_health
      )
    ]
    
  }
  
  # save object and return path
  # change name of file depending if it's only transit or all modes access.
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "output_access"
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- ifelse(
    is.null(ttm$bfm_time),
    file.path(dir_path, paste0("transit_access_", scenario, ".rds")),
    file.path(dir_path, paste0("full_access_", scenario, ".rds"))
  )
  saveRDS(access, file_path)
  
  return(file_path)
  
}


# city <- "goi"
# access_paths <- tar_read(access_metadata)$access_file[3:4]
# method <- "absolute"
calculate_access_diff <- function(city,
                                  access_paths,
                                  method = c("absolute", "relative")) {
  
  method <- method[1]
  access <- lapply(access_paths, readRDS)
  
  # retrieve the name of the columns with accessibility data to calculate diff
  # between the two datasets programatically
  # if method == "relative" then substitute NaN to 0s (they shouldn't matter
  # much because there are very few places with 0 accessibility and they all
  # have 0 accessibility in both scenarios)
  
  access_cols <- setdiff(names(access[[1]]), "fromId")
  setnames(access[[1]], old = access_cols, new = paste0(access_cols, "_antes"))
  setnames(access[[2]], old = access_cols, new = paste0(access_cols, "_depois"))
  
  access_diff <- access[[1]][access[[2]], on = "fromId"]
  
  if (method == "absolute") {
    
    diff_expression <- paste0(
      "`:=`(",
      paste(
        access_cols,
        "=",
        paste0(access_cols, "_depois"),
        "-",
        paste0(access_cols, "_antes"),
        collapse = ", "
      ),
      ")"
    )
    
    access_diff[, eval(parse(text = diff_expression))]
    
  } else if (method == "relative") {
    
    diff_expression <- paste0(
      "`:=`(",
      paste(
        access_cols,
        "= (",
        paste0(access_cols, "_depois"),
        "-",
        paste0(access_cols, "_antes"),
        ") /",
        paste0(access_cols, "_antes"),
        collapse = ", "
      ),
      ")"
    )
    
    access_diff[, eval(parse(text = diff_expression))]
    
    for (col in access_cols) {
      data.table::set(
        access_diff,
        i = which(is.nan(access_diff[[col]])),
        j = col,
        value = 0
      )
    }
    
  }
  
  cols_to_keep <- c("fromId", access_cols)
  access_diff <- access_diff[, ..cols_to_keep]
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "output_access"
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- ifelse(
    any(grepl("only_bfm", names(access_diff))),
    file.path(dir_path, paste0("full_access_diff_", method, ".rds")),
    file.path(dir_path, paste0("transit_access_diff_", method, ".rds"))
  )
  saveRDS(access_diff, file_path)
  
  return(file_path)
  
}


# access_paths <- list(tar_read(accessibility_antes), tar_read(accessibility_depois))
# access_diff_path <- tar_read(accessibility_diff_abs)
# grid_path <- tar_read(grid_path)[1]
# analysis_skeleton <- tar_read(analysis_skeleton)
# type <- "CMATT60"
analyse_scenarios <- function(access_paths,
                              access_diff_path,
                              grid_path,
                              analysis_skeleton) {
  
  access <- lapply(access_paths, readRDS)
  access_diff <- readRDS(access_diff_path)
  grid <- setDT(readRDS(grid_path))
  
  names(access) <- c("antes", "depois")
  access <- rbindlist(access, idcol = "scenario")
  
  # create three different reports, one for job accessibility, the other for
  # education accessibility and the last for health accessibility
  
  report_dir <- "../../data/avaliacao_intervencoes/for/reports"
  if (!dir.exists(report_dir)) dir.create(report_dir)
  
  vapply(
    c("CMATT60", "CMAET60", "CMASB60"),
    FUN.VALUE = character(1),
    FUN = function(type) {
      
      filename <- normalizePath(
        file.path(report_dir, paste0("scenario_analysis_", type, ".html"))
      )
      
      # don't pass the whole accessibility dataset to the report, only relevant
      # columns
      
      relevant_cols <- c(
        "scenario",
        "fromId",
        paste0(
          c(
            "all_modes_",
            "transit_bike_",
            "only_transit_",
            "only_bike_",
            "only_bfm_"
          ),
          type
        )
      )
      relevant_cols_diff <- setdiff(relevant_cols, "scenario")
      
      smaller_access <- access[, ..relevant_cols]
      smaller_access <- setnames(
        smaller_access,
        c("scenario", "id", "all_modes", "transit_bike", "only_transit", "only_bike", "only_bfm")
      )
      
      smaller_access_diff <- access_diff[, ..relevant_cols_diff]
      smaller_access_diff <- setnames(
        smaller_access_diff,
        c("id", "all_modes", "transit_bike", "only_transit", "only_bike", "only_bfm")
      )
      
      rmarkdown::render(
        analysis_skeleton,
        output_file = filename,
        params = list(
          access = smaller_access,
          access_diff = smaller_access_diff,
          grid = grid,
          type = type
        ),
        quiet = TRUE
      )
      
    }
  )
  
}


# city <- tar_read(both_cities)[1]
# access_diff_abs <- tar_read(transit_access_diff_abs)[1]
# access_diff_rel <- tar_read(transit_access_diff_rel)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT60"
create_boxplots <- function(city, access_diff_abs, access_diff_rel, grid_path) {
  
  access_diff_abs <- readRDS(access_diff_abs)
  access_diff_rel <- readRDS(access_diff_rel)
  grid <- setDT(readRDS(grid_path))
  
  access_diff <- rbind(
    abs = access_diff_abs,
    rel = access_diff_rel,
    idcol = "type"
  )
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access_diff <- access_diff[decil > 0]
  
  measures <- c("CMATT60", "CMAET60", "CMASB60")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      if (city == "for") {
        
        abs_ceiling <- fcase(
          measure == "CMATT60", 70000,
          measure == "CMAET60", 30,
          measure == "CMASB60", 12
        )
        rel_ceiling <- fcase(
          measure == "CMATT60", 0.2,
          measure == "CMAET60", 0.12,
          measure == "CMASB60", 0.2
        )
        
      } else if (city == "goi") {
        
        abs_ceiling <- fcase(
          measure == "CMATT60", 30000,
          measure == "CMAET60", 8,
          measure == "CMASB60", 5
        )
        rel_ceiling <- fcase(
          measure == "CMATT60", 0.1,
          measure == "CMAET60", 0.05,
          measure == "CMASB60", 0.05
        )
        
      }
      
      boxplot_theme <- theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_markdown(),
          legend.position = "bottom",
          legend.text.align = 0.5
        )
      
      # absolute difference
      
      palma_abs <- calculate_palma(access_diff[type == "abs"], relevant_var)
      
      abs_plot <- ggplot(access_diff[type == "abs"]) +
        geom_boxplot(
          aes(
            as.factor(decil),
            get(relevant_var),
            weight = pop,
            color = as.factor(decil)
          ),
          outlier.size = 1.5,
          outlier.alpha = 0.5,
          show.legend = FALSE
        ) +
        scale_colour_brewer(palette = "RdBu") +
        guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
        labs(
          y = "Ganho absoluto de acess.",
          x = NULL,
          subtitle = paste0("**Razao de Palma**: ", palma_abs)
        ) +
        coord_cartesian(ylim = c(0, abs_ceiling)) +
        boxplot_theme
      
      # relative difference
      
      palma_rel <- calculate_palma(access_diff[type == "rel"], relevant_var)
      
      rel_plot <- ggplot(access_diff[type == "rel"]) +
        geom_boxplot(
          aes(
            as.factor(decil),
            get(relevant_var),
            weight = pop,
            color = as.factor(decil)
          ),
          outlier.size = 1.5,
          outlier.alpha = 0.5
        ) +
        scale_colour_brewer(
          palette = "RdBu",
          labels = c("1\nmais pobres", 2:9, "10\nmais ricos"),
          name = "Decil de renda"
        ) +
        scale_y_continuous(labels = scales::percent) +
        guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
        labs(
          y = "Ganho relativo de acess.",
          x = NULL,
          subtitle = paste0("**Razao de Palma**: ", palma_rel)
        ) +
        coord_cartesian(ylim = c(0, rel_ceiling)) +
        boxplot_theme
      
      # join them, save and return the result so the target follows the file
      
      plot <- (abs_plot / rel_plot) +
        plot_layout(heights = c(1, 1))
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "boxplot_difference")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 13,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


calculate_palma <- function(access_diff, relevant_var) {
  
  richest_10 <- access_diff[decil == 10]
  poorest_40 <- access_diff[decil >= 1 & decil <= 4]
  
  palma <- weighted.mean(richest_10[[relevant_var]], w = richest_10$pop) /
    weighted.mean(poorest_40[[relevant_var]], w = poorest_40$pop)
  palma <- format(palma, digits = 4)
  
}


# city <- "for"
# access_paths <- tar_read(access_metadata)$access_file[1:2]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT60"
create_dist_maps <- function(city, access_paths, grid_path) {
  
  access <- lapply(access_paths, readRDS)
  names(access) <- c("Antes", "Depois")
  grid <- setDT(readRDS(grid_path))
  
  # join accessibility difference datasets to create a faceted chart
  
  access <- rbindlist(access, idcol = "type")
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
  # download city and transit routes shapes, depending on the city
  
  city_code <- ifelse(city == "for", 2304400, 5208707)
  city_shape <- geobr::read_municipality(city_code)
  
  if (city == "for") {
    
    gtfs <-  gtfstools::read_gtfs(
      "../../data/avaliacao_intervencoes/r5/graph/for_depois/gtfs_for_metrofor_2021-01_depois.zip"
    )
    desired_trips <- gtfs$trips[
      gtfs$routes,
      on = "route_id",
      `:=`(route_id = i.route_id, route_long_name = i.route_long_name)
    ]
    desired_trips <- desired_trips[
      desired_trips[, .I[1], by = route_long_name]$V1
    ]
    desired_trips <- desired_trips$trip_id
    
    transit_shapes <- gtfstools::get_trip_geometry(
      gtfs,
      trip_id = desired_trips
    )
    transit_shapes <- setDT(transit_shapes)[
      !(trip_id == "LL-0-1" & origin_file == "stop_times")
    ]
    
  } else if (city == "goi") {
    
    gtfs <- gtfstools::read_gtfs(
      "../../data/avaliacao_intervencoes/r5/graph/goi_depois/gtfs_goi_rmtc_2019-10_depois.zip"
    )
    
    desired_trips <- gtfs$trips[
      gtfs$routes,
      on = "route_id",
      `:=`(route_id = i.route_id)
    ]
    desired_trips <- desired_trips[grepl("BRT", route_id)]
    desired_trips <- desired_trips[desired_trips[, .I[1], by = shape_id]$V1]
    desired_trips <- desired_trips$trip_id
    
    transit_shapes <- gtfstools::get_trip_geometry(
      gtfs,
      trip_id = desired_trips,
      file = "shapes"
    )
    
  }
  
  # generate figures for each of our desired variables

  measures <- c("CMATT60", "CMAET60", "CMASB60")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      label_func <- if (grepl("TT", measure)) {
        scales::label_number(
          accuracy = 1,
          scale = 1/1000,
          suffix = "k",
          big.mark = ","
        )
      } else {
        scales::label_number()
      }
      
      plot <- ggplot() +
        geom_sf(
          data = st_sf(access),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        facet_wrap(~ type) +
        scale_fill_viridis_c(
          name = "Acessibilidade por\ntransporte publico",
          option = "inferno",
          label = label_func,
          n.breaks = 4
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text = element_blank(),
          panel.grid = element_blank()
        )
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "map_distribution")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 9,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- "for"
# access_diff_abs <- tar_read(transit_access_diff_abs)[1]
# access_diff_rel <- tar_read(transit_access_diff_rel)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT60"
create_diff_maps <- function(city, access_diff_abs, access_diff_rel, grid_path) {
  
  access_diff_abs <- readRDS(access_diff_abs)
  access_diff_rel <- readRDS(access_diff_rel)
  grid <- setDT(readRDS(grid_path))
  
  access_diff <- rbind(
    abs = access_diff_abs,
    rel = access_diff_rel,
    idcol = "type"
  )
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  # access_diff <- access_diff[decil > 0]
  
  # download city and transit routes shapes, depending on the city
  
  city_code <- ifelse(city == "for", 2304400, 5208707)
  city_shape <- geobr::read_municipality(city_code)
  
  if (city == "for") {
    
    gtfs <-  gtfstools::read_gtfs(
      "../../data/avaliacao_intervencoes/r5/graph/for_depois/gtfs_for_metrofor_2021-01_depois.zip"
    )
    desired_trips <- gtfs$trips[
      gtfs$routes,
      on = "route_id",
      `:=`(route_id = i.route_id, route_long_name = i.route_long_name)
    ]
    desired_trips <- desired_trips[
      desired_trips[, .I[1], by = route_long_name]$V1
    ]
    desired_trips <- desired_trips$trip_id
    
    transit_shapes <- gtfstools::get_trip_geometry(
      gtfs,
      trip_id = desired_trips
    )
    transit_shapes <- setDT(transit_shapes)[
      !(trip_id == "LL-0-1" & origin_file == "stop_times")
    ]
    
  } else if (city == "goi") {
    
    gtfs <- gtfstools::read_gtfs(
      "../../data/avaliacao_intervencoes/r5/graph/goi_depois/gtfs_goi_rmtc_2019-10_depois.zip"
    )
    
    desired_trips <- gtfs$trips[
      gtfs$routes,
      on = "route_id",
      `:=`(route_id = i.route_id)
    ]
    desired_trips <- desired_trips[grepl("BRT", route_id)]
    desired_trips <- desired_trips[desired_trips[, .I[1], by = shape_id]$V1]
    desired_trips <- desired_trips$trip_id
    
    transit_shapes <- gtfstools::get_trip_geometry(
      gtfs,
      trip_id = desired_trips,
      file = "shapes"
    )
    
  }
  
  # generate figures depending on the variable
  
  measures <- c("CMATT60", "CMAET60", "CMASB60")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      diff_map_theme <- theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(hjust = 0.5)
        )
      
      label_func <- if (grepl("TT", measure)) {
        scales::label_number(
          accuracy = 1,
          scale = 1/1000,
          suffix = "k",
          big.mark = ","
        )
      } else {
        scales::label_number()
      }
      
      # legend bar depends on the city, because goi has negative differences and
      # for doesn't
      
      if (city == "for") {
        pal <- "PuBu"
        lim_abs <- NULL
        lim_rel <- NULL
      } else if (city == "goi") {
        pal <- "RdBu"
        
        max_diff_abs <- max(
          access_diff[type == "abs"][[relevant_var]],
          na.rm = TRUE
        )
        lim_abs <- c(-1, 1) * max_diff_abs
        
        max_diff_rel <- max(
          access_diff[type == "rel"][[relevant_var]],
          na.rm = TRUE
        )
        lim_rel <- c(-1, 1) * max_diff_rel
      }
      
      # absolute difference
      
      abs_plot <- ggplot() +
        geom_sf(
          data = st_sf(access_diff[type == "abs"]),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        scale_fill_distiller(
          name = "Acessibilidade",
          palette = pal,
          label = label_func,
          #n.breaks = 4,
          direction = 1,
          limits = lim_abs
        ) +
        labs(subtitle = "Dif. absoluta") +
        diff_map_theme
      
      # relative difference
      
      rel_plot <- ggplot() +
        geom_sf(
          data = st_sf(access_diff[type == "rel"]),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        scale_fill_distiller(
          name = "Acessibilidade",
          palette = pal,
          #n.breaks = 4,
          direction = 1,
          labels = scales::percent_format(),
          limits = lim_rel
        ) +
        labs(subtitle = "Dif. relativa") +
        diff_map_theme
      
      # join them using patchwork, save the result and return the path
      
      plot <- abs_plot | rel_plot
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "map_difference")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 9,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}