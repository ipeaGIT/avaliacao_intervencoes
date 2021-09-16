# city <- tar_read(both_cities)[2]
# access_diff_abs <- tar_read(transit_access_diff_abs)[2]
# access_diff_rel <- tar_read(transit_access_diff_rel)[2]
# grid_path <- tar_read(grid_path)[2]
# measure <- "CMAET60"
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
      
      # ggsave() raises a warning in the CMAET60 case related to the non
      # uniqueness of the quantile regression solution. nothing to really worry
      # about
      
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
  palma <- format(palma, digits = 2, nsmall = 2)
  
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
  
  # download basemap and city and transit routes shapes, depending on the city
  # using 2018 data to generate the basemap of goiania city only, not MR 
  
  basemap <- readRDS(
    paste0(
      "../../data/acesso_oport/maptiles_crop/2018/mapbox/maptile_crop_mapbox_",
      city,
      "_2018.rds"
    )
  )
  
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
      !(trip_id == "LL-0.1-1" & origin_file == "stop_times")
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
  
  # transform sf objects' crs to 3857 so they became "compatible" with the
  # basemap raster
  
  access <- st_transform(st_sf(access), 3857)
  transit_shapes <- st_transform(st_sf(transit_shapes), 3857)
  city_shape <- st_transform(city_shape, 3857)
  
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
        geom_raster(data = basemap, aes(x, y, fill = hex)) +
        coord_equal() +
        scale_fill_identity() +
        ggnewscale::new_scale_fill() +
        geom_sf(
          data = access,
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = transit_shapes,
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
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11) # same size as legend title
        )
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "map_distribution")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      if (city == "for") {
        plot_height <- 9
      } else {
        plot_height <- 10
      }
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = plot_height,
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
create_diff_maps <- function(city,
                             access_diff_abs,
                             access_diff_rel,
                             grid_path) {
  
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
  
  # download basemap and city and transit routes shapes, depending on the city
  # using 2018 data to generate the basemap of goiania city only, not MR 
  
  basemap <- readRDS(
    paste0(
      "../../data/acesso_oport/maptiles_crop/2018/mapbox/maptile_crop_mapbox_",
      city,
      "_2018.rds"
    )
  )
  
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
      !(trip_id == "LL-0.1-1" & origin_file == "stop_times")
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
          axis.title = element_blank(),
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
        pal <- "Greens"
        lim_abs <- NULL
        lim_rel <- NULL
      } else if (city == "goi") {
        pal <- "RdYlGn"
        
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
      
      # transform sf objects' crs to 3857 so they became "compatible" with the
      # basemap raster
      
      access_diff <- st_transform(st_sf(access_diff), 3857)
      transit_shapes <- st_transform(st_sf(transit_shapes), 3857)
      city_shape <- st_transform(city_shape, 3857)
      
      # absolute difference
      
      abs_plot <- ggplot() +
        geom_raster(data = basemap, aes(x, y, fill = hex)) +
        coord_equal() +
        scale_fill_identity() +
        ggnewscale::new_scale_fill() +
        geom_sf(
          data = access_diff[access_diff$type == "abs", ],
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
          name = "Diferenca de\nacessibilidade",
          palette = pal,
          label = label_func,
          n.breaks = 4,
          direction = 1,
          limits = lim_abs
        ) +
        labs(subtitle = "Absoluta") +
        diff_map_theme
      
      # relative difference
      
      rel_plot <- ggplot() +
        geom_raster(data = basemap, aes(x, y, fill = hex)) +
        coord_equal() +
        scale_fill_identity() +
        ggnewscale::new_scale_fill() +
        geom_sf(
          data = access_diff[access_diff$type == "rel", ],
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = transit_shapes,
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        scale_fill_distiller(
          name = "Diferenca de\nacessibilidade",
          palette = pal,
          n.breaks = 4,
          direction = 1,
          labels = scales::percent_format(),
          limits = lim_rel
        ) +
        labs(subtitle = "Relativa") +
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
      
      if (city == "for") {
        plot_height <- 9
      } else {
        plot_height <- 10
      }
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = plot_height,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- tar_read(only_for)
# access_paths <- tar_read(full_access)
# access_diff_path <- tar_read(full_access_diff_abs)
# grid_path <- tar_read(grid_path)[1]
# analysis_skeleton <- tar_read(analysis_skeleton)
# type <- "CMATT60"
analyse_scenarios <- function(city,
                              access_paths,
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


# city <- tar_read(only_for)
# access_paths <- tar_read(full_access)
# access_diff_abs_path <- tar_read(full_access_diff_abs)
# access_diff_rel_path <- tar_read(full_access_diff_rel)
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT60"
plot_summary <- function(city,
                         access_paths,
                         access_diff_abs_path,
                         access_diff_rel_path,
                         grid_path) {
  
  access <- lapply(access_paths, readRDS)
  access_diff_abs <- readRDS(access_diff_abs_path)
  access_diff_rel <- readRDS(access_diff_rel_path)
  grid <- setDT(readRDS(grid_path))
  
  names(access) <- c("Antes", "Depois")
  access <- rbindlist(access, idcol = "scenario")
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
  # download basemap and city and transit routes shapes
  
  basemap <- readRDS(
    paste0(
      "../../data/acesso_oport/maptiles_crop/2018/mapbox/maptile_crop_mapbox_",
      city,
      "_2018.rds"
    )
  )

  city_shape <- geobr::read_municipality(2304400)
    
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
    !(trip_id == "LL-0.1-1" & origin_file == "stop_times")
  ]
  
  # transform sf objects' crs to 3857 so they became "compatible" with the
  # basemap raster
  
  access <- st_transform(st_sf(access), 3857)
  transit_shapes <- st_transform(st_sf(transit_shapes), 3857)
  city_shape <- st_transform(city_shape, 3857)
  
  # three different plots
  
  measures <- c("CMATT60", "CMAET60", "CMASB60")
  
  vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("all_modes_", measure)
      
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
      
      # first plot - "normal" accessibility distribution
      
      map_dist <- ggplot() +
        # geom_raster(data = basemap, aes(x, y, fill = hex)) +
        # coord_equal() +
        # scale_fill_identity() +
        # ggnewscale::new_scale_fill() +
        geom_sf(
          data = access,
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = transit_shapes,
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        facet_wrap(~ scenario) +
        scale_fill_viridis_c(
          name = NULL,
          option = "inferno",
          label = label_func,
          n.breaks = 4
        ) +
        theme_minimal() +
        labs(y = "Acessibilidade") +
        theme(
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank()
          #,
          #strip.text = element_text(size = 11) # same size as legend title
        )
      
      # save the result and return the path
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "summary_bfm_bike_transit")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        final_plot,
        width = 16,
        height = 16,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}