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
create_accessibility_data <- function(city,
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
  
  # if 'ttm' is the pure transit matrix change the column travel_time to
  # transit_time, so the same bits of code can be used to calculate the "full"
  # and the "transit-only" access
  
  setnames(ttm, old = "travel_time", new = "transit_time", skip_absent = TRUE)
  
  access <- lapply(1:60, calculate_accessibility, ttm, opportunities)
  access <- rbindlist(access, idcol = "travel_time")
  
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


calculate_accessibility <- function(threshold, ttm, opportunities) {
  
  access <- data.table(fromId = unique(ttm$fromId))
  
  transit_access <- ttm[
    transit_time <= threshold,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  access[
    transit_access,
    on = "fromId",
    `:=`(
      only_transit_CMATT60 = i.total_jobs,
      only_transit_CMAET60 = i.total_edu,
      only_transit_CMASB60 = i.basic_health
    )
  ]
  
  # if ttm$bfm_time is NULL then this is the transit-only matrix. therefore,
  # only the transit-only bits of the accessibility calculation will be
  # conducted.
  # otherwise we are dealing with fortaleza-specific analysis that also include
  # bike-only and bike first mile scenarios
  
  if (!is.null(ttm$bfm_time)) {
    
    only_bike_access <- ttm[
      bike_time <= threshold,
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
      bfm_time <= threshold,
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
      transit_time <= threshold | bike_time <= threshold,
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
      transit_time <= threshold | bike_time <= threshold | bfm_time <= threshold,
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
  
  return(access)
  
}


# city <- "for"
# access_paths <- tar_read(access_metadata)$access_file[1:2]
# method <- "relative"
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
    
    diff_expression_log <- paste0(
      "`:=`(",
      paste(
        paste0(access_cols, "_log"),
        "= log(",
        paste0(access_cols, "_depois"),
        "/",
        paste0(access_cols, "_antes"),
        ")",
        collapse = ", "
      ),
      ")"
    )
    
    access_diff[, eval(parse(text = diff_expression))]
    access_diff[, eval(parse(text = diff_expression_log))]
    
    access_cols <- c(access_cols, paste0(access_cols, "_log"))
    
    for (col in access_cols) {
      data.table::set(
        access_diff,
        i = which(is.nan(access_diff[[col]])),
        j = col,
        value = 0
      )
      
      # calculating log difference may introduce some Inf and -Inf
      # substitute them with the man and min finite numbers
      
      max_finite <- max(access_diff[[col]][is.finite(access_diff[[col]])])
      min_finite <- min(access_diff[[col]][is.finite(access_diff[[col]])])
      
      data.table::set(
        access_diff,
        i = which(is.infinite(access_diff[[col]]) & access_diff[[col]] > 0),
        j = col,
        value = max_finite
      )
      
      data.table::set(
        access_diff,
        i = which(is.infinite(access_diff[[col]]) & access_diff[[col]] < 0),
        j = col,
        value = min_finite
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