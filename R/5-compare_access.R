# city <- tar_read(both_cities)[2]
# access_diff_abs <- tar_read(transit_access_diff_abs)[2]
# access_diff_rel <- tar_read(transit_access_diff_rel)[2]
# grid_path <- tar_read(grid_path)[2]
# measure <- "CMAET"
# travel_time <- 60
create_boxplots <- function(city,
                            access_diff_abs,
                            access_diff_rel,
                            grid_path,
                            travel_time) {
  
  access_diff_abs <- readRDS(access_diff_abs)
  access_diff_rel <- readRDS(access_diff_rel)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  access_diff <- rbind(
    abs = access_diff_abs,
    rel = access_diff_rel,
    idcol = "type",
    fill = TRUE
  )
  access_diff <- access_diff[travel_time == get("travel_time", envir = env)]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access_diff <- access_diff[decil > 0]
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      if (city == "for") {
        
        abs_limit <- fcase(
          measure == "CMATT", 100000,
          measure == "CMAET", 80,
          measure == "CMASB", 30
        )
        rel_limit <- fcase(
          measure == "CMATT", 0.7,
          measure == "CMAET", 0.5,
          measure == "CMASB", 0.6
        )
        
      } else if (city == "goi") {
        
        abs_limit <- fcase(
          measure == "CMATT", 50000,
          measure == "CMAET", 8,
          measure == "CMASB", 5
        )
        rel_limit <- fcase(
          measure == "CMATT", 0.2,
          measure == "CMAET", 0.05,
          measure == "CMASB", 0.05
        )
        
      }
      
      # function to create the boxplot chart
      
      boxplot_maker <- function(method) {
        
        access_diff_data <- access_diff[type == method]
        
        boxplot_theme <- theme_minimal() +
          theme(
            axis.text.x = element_blank(),
            panel.grid = element_blank(),
            plot.subtitle = element_markdown(),
            legend.position = "bottom",
            legend.text.align = 0.5
          )
        
        palma <- calculate_palma(access_diff_data, relevant_var)
        
        label <- ifelse(method == "abs", scales::number, scales::percent)
        
        limit <- ifelse(method == "abs", abs_limit, rel_limit)
        show_legend <- ifelse(method == "abs", FALSE, TRUE)
        y_lab <- ifelse(
          method == "abs",
          "Ganho absoluto de acess.",
          "Ganho relativo de acess."
        )
        
        plot <- ggplot(access_diff_data) +
          geom_segment(
            aes(
              x = 0.5, y = 0,
              xend = 10.5, yend = 0
            ),
            color = "gray85"
          ) +
          geom_boxplot(
            aes(
              as.factor(decil),
              get(relevant_var),
              weight = pop,
              color = as.factor(decil)
            ),
            outlier.size = 1.5,
            outlier.alpha = 0.5,
            show.legend = show_legend
          ) +
          scale_colour_brewer(
            palette = "BrBG",
            labels = c("1\nmais pobres", 2:9, "10\nmais ricos"),
            name = "Decil de renda"
          ) +
          scale_y_continuous(labels = label) +
          scale_x_discrete(limits = factor(1:10)) +
          guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
          labs(
            y = y_lab,
            x = NULL,
            subtitle = paste0("**Razao de Palma**: ", palma)
          ) +
          coord_cartesian(ylim = c(-limit, limit)) +
          boxplot_theme
        
      }
      
      abs_plot <- boxplot_maker("abs")
      rel_plot <- boxplot_maker("rel")
      
      # join plots, save and return the result so the target follows the file
      
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
      
      # ggsave() raises a warning in the CMAET case related to the non
      # uniqueness of the quantile regression solution. nothing to really worry
      # about
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
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


calculate_palma <- function(access, relevant_var) {
  
  richest_10 <- access[decil == 10]
  poorest_40 <- access[decil >= 1 & decil <= 4]
  
  numerator <- weighted.mean(
    richest_10[[relevant_var]],
    w = richest_10$pop,
    na.rm = TRUE
  )
  
  denominator <- weighted.mean(
    poorest_40[[relevant_var]],
    w = poorest_40$pop,
    na.rm = TRUE
  )
  
  palma <- numerator / denominator
  
  return(palma)
  
}


# city <- "for"
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenario <- tar_read(access_metadata)$scenario[1:3]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
create_dist_maps <- function(city,
                             access_paths,
                             scenario,
                             grid_path,
                             travel_time) {
  
  access <- lapply(access_paths, readRDS)
  names(access) <- scenario
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  # dont include counterfactual scenario in this plot
  
  access$contrafactual <- NULL
  names(access) <- c("Antes", "Depois")
  
  # join accessibility difference datasets to create a faceted chart and filter
  # to keep only relevant travel_time
  
  access <- rbindlist(access, idcol = "type")
  access <- access[travel_time == get("travel_time", envir = env)]
  
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
    
    # the 'before' scenario must not include the new subway line.
    # bind 'transit_shapes' into itself and add column to make sure the lines
    # are adequately faceted
    
    n_trips <- length(unique(transit_shapes$trip_id))
    
    transit_shapes <- rbind(
      transit_shapes,
      transit_shapes[origin_file != "shapes"]
    )
    transit_shapes[
      ,
      type := c(
        rep("Depois", n_trips),
        rep("Antes", n_trips - 1)
      )
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
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
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
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
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
# access_diff_path <- tar_read(transit_access_diff)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
create_diff_maps <- function(city,
                             access_diff_path,
                             grid_path,
                             travel_time) {
  
  access_diff <- readRDS(access_diff_path)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  access_diff <- access_diff[type == "abs"]
  access_diff <- access_diff[travel_time == get("travel_time", envir = env)]
  access_diff[
    ,
    scenario := factor(
      scenario,
      levels = c("depois", "contrafactual"),
      labels = c("Previsto", "Contrafactual")
    )
  ]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
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
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      # truncate difference values, based on each measure's value
      
      max_value <- fcase(
        measure == "CMATT", 100000,
        measure == "CMAET", 50,
        measure == "CMASB", 15
      )
      
      access_diff[
        ,
        eval(relevant_var) := fifelse(
          get(relevant_var) > max_value,
          max_value,
          fifelse(
            get(relevant_var) < -max_value,
            -max_value,
            get(relevant_var)
          )
        )
      ]
      
      # legend-guide related objects
      
      breaks <- c(-max_value, 0, max_value)
      labels <- if (grepl("TT", measure)) {
        c(paste0("< ", breaks[1]/1000, "k"), 0, paste0("> ", breaks[3]/1000, "k"))
      } else {
        c(paste0("< ", breaks[1]), 0, paste0("> ", breaks[3]))
      }
      
      # plot settings
      
      plot <- ggplot() +
        geom_sf(
          data = st_sf(access_diff),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        facet_wrap(~ scenario) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        scale_fill_distiller(
          name = "Diferença de\nacessibilidade",
          palette = "RdBu",
          direction = 1,
          n.breaks = 4,
          limits = c(-max_value, max_value),
          breaks = breaks,
          labels = labels
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(vjust = 1),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11)
        )
      
      # save the result and return the path
      
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
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
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
# access_diff_abs_path <- tar_read(full_access_diff_abs)
# access_diff_rel_path <- tar_read(full_access_diff_rel)
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
plot_summary <- function(city,
                         access_paths,
                         access_diff_abs_path,
                         access_diff_rel_path,
                         grid_path,
                         travel_time) {
  
  access <- lapply(access_paths, readRDS)
  access_diff_abs <- readRDS(access_diff_abs_path)
  access_diff_rel <- readRDS(access_diff_rel_path)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  names(access) <- c("Antes", "Depois")
  access <- rbindlist(access, idcol = "scenario")
  access <- access[travel_time == get("travel_time", envir = env)]
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
  access_diff_abs <- access_diff_abs[travel_time == get("travel_time", envir = env)]
  access_diff_abs[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil, pop = i.pop_total)
  ]
  
  access_diff_rel <- access_diff_rel[travel_time == get("travel_time", envir = env)]
  access_diff_rel[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil, pop = i.pop_total)
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
  
  # the 'before' scenario must not include the new subway line.
  # bind 'transit_shapes' into itself and add column to make sure the lines
  # are adequately faceted
  
  n_trips <- length(unique(transit_shapes$trip_id))
  
  transit_shapes <- rbind(
    transit_shapes,
    transit_shapes[origin_file != "shapes"]
  )
  transit_shapes[
    ,
    scenario := c(
      rep("Depois", n_trips),
      rep("Antes", n_trips - 1)
    )
  ]
  
  # transform sf objects' crs to 3857 so they became "compatible" with the
  # basemap raster
  
  access <- st_transform(st_sf(access), 3857)
  access_diff_abs <- setDT(st_transform(st_sf(access_diff_abs), 3857))
  access_diff_rel <- setDT(st_transform(st_sf(access_diff_rel), 3857))
  transit_shapes <- st_transform(st_sf(transit_shapes), 3857)
  city_shape <- st_transform(city_shape, 3857)
  
  # three different plots
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
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
      
      # first row - "normal" accessibility distribution
      
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
          panel.grid = element_blank(),
          strip.text = element_text(size = 10)
        )
      
      row_one <- cowplot::plot_grid(
        map_dist, 
        NULL,
        nrow = 1,
        rel_widths = c(1, 0.1)
      )
      
      # function to generate rows with a difference map on the left and a
      # difference boxplot on the right
      
      # access_diff <- access_diff_rel
      # method <- "rel"
      diff_row_generator <- function(access_diff, method) {
        
        diff_map_theme <- theme_minimal() +
          theme(
            axis.text = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.box.spacing = unit(0, "pt")
          )
        
        boxplot_theme <- theme_minimal() +
          theme(
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            plot.subtitle = element_markdown(),
            legend.position = "bottom",
            legend.title = element_blank()
          )
        
        # objects that depend on the method
        
        relevant_var_treated <- paste0(relevant_var, "_treated")
        
        access_diff[
          get(relevant_var) > 1,
          eval(relevant_var_treated) := 1
        ]
        access_diff[
          get(relevant_var) < -0.5,
          eval(relevant_var_treated) := -0.5
        ]
        access_diff[
          get(relevant_var) >= -0.5 & get(relevant_var) <= 1,
          eval(relevant_var_treated) := get(relevant_var)
        ]
        
        title <- ifelse(method == "abs", "Dif. absoluta", "Dif. relativa")
        diff_var <- ifelse(method == "abs", relevant_var, relevant_var_treated)
        
        map_label <- label_func
        if (method == "rel") map_label <- c("<50%", "0%", "50%", ">100%")
        
        values <- NULL
        if (method == "rel") values <- scales::rescale(c(-0.5, 0, 1))
        
        # map's legend-guide related objects
        
        max_diff<- max(
          abs(access_diff[[diff_var]]),
          na.rm = TRUE
        )
        lim <- c(-1, 1) * max_diff
        
        if (method == "rel") lim <- c(-0.5, 1.0)
        
        # map
        
        map_diff <- ggplot() +
          # geom_raster(data = basemap, aes(x, y, fill = hex)) +
          # coord_equal() +
          # scale_fill_identity() +
          # ggnewscale::new_scale_fill() +
          geom_sf(
            data = st_sf(access_diff),
            aes(fill = get(diff_var)),
            color = NA
          ) +
          geom_sf(
            data = transit_shapes,
            size = 0.5,
            alpha = 0.7
          ) +
          geom_sf(data = city_shape, fill = NA) +
          scale_fill_distiller(
            name = NULL,
            palette = "RdBu",
            labels = map_label,
            values = values,
            n.breaks = 4,
            direction = 1,
            limits = lim
          ) +
          labs(y = title) +
          diff_map_theme
        
        # boxplot
        
        ceiling <- fcase(
          measure == "CMATT" && method == "abs", 100000,
          measure == "CMAET" && method == "abs", 80,
          measure == "CMASB" && method == "abs", 30,
          measure == "CMATT" && method == "rel", 0.7,
          measure == "CMAET" && method == "rel", 0.5,
          measure == "CMASB" && method == "rel", 0.6
        )
        label <- ifelse(method == "abs", scales::number, scales::percent)
        
        palma <- calculate_palma(access_diff, relevant_var)
        
        access_diff <- access_diff[decil > 0, ]
        
        boxplot_diff <- ggplot(access_diff) +
          geom_segment(
            aes(
              x = 0.5, y = 0,
              xend = 10.5, yend = 0
            ),
            color = "gray85"
          ) +
          geom_boxplot(
            aes(
              as.factor(decil),
              get(relevant_var),
              weight = pop,
              color = as.factor(decil)
            ),
            outlier.size = 1.5,
            outlier.alpha = 0.5,
            show.legend = TRUE
          ) +
          scale_colour_brewer(palette = "BrBG") +
          scale_y_continuous(labels = label) +
          scale_x_discrete(limits = factor(1:10)) +
          guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
          labs(
            y = NULL,
            x = "Decil de renda",
            subtitle = paste0("**Razao de Palma**: ", palma)
          ) +
          coord_cartesian(ylim = c(-ceiling, ceiling)) +
          boxplot_theme
        
        cowplot::plot_grid(
          map_diff,
          boxplot_diff,
          nrow = 1,
          rel_widths = c(0.7, 1)
        )
        
      }
      
      # second row - absolute difference map and boxplot
      
      row_two <- diff_row_generator(access_diff_abs, "abs")
      
      # third row - relative difference map and boxplot
      
      row_three <- diff_row_generator(access_diff_rel, "rel")
      
      # join all three rows in a single plot
      
      final_plot <- cowplot::plot_grid(
        row_one,
        row_two,
        row_three,
        ncol = 1,
        rel_heights = c(1, 1.2, 1.2),
        labels = c("A)", "B)", "C)")
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
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        final_plot,
        width = 16,
        height = 19.5,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- tar_read(both_cities)[1]
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenarios <- tar_read(access_metadata)$scenario[1:3]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
compare_palma <- function(city, access_paths, scenarios, grid_path) {
  
  grid <- setDT(readRDS(grid_path))
  
  names(access_paths) <- scenarios
  access <- lapply(access_paths, readRDS)
  access <- rbindlist(access, idcol = "scenario")
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access <- access[decil > 0]
  
  measures <- c("CMATT", "CMAET", "CMASB")
  relevant_vars <- paste0("only_transit_", measures)
  
  # nest dataframes of accessibility distribution for each scenario and travel
  # time to calculate the palma ratio of each case
  
  access <- access[
    ,
    .(dist = list(.SD)),
    by = .(scenario, travel_time),
    .SDcols = c(relevant_vars, "pop", "decil")
  ]
  
  # calculate the palma ratio for each relevant_var
  
  palma_expr <- paste0(
    "palma_", measures,
    "=",
    "vapply(dist, function(dt) calculate_palma(dt,'", relevant_vars,
    "'), numeric(1))",
    collapse = ", "
  )
  palma_expr <- paste0("`:=`(", palma_expr, ")")
  
  access[, eval(parse(text = palma_expr))]
  
  # remove the 'dist' column, otherwise the data.table is kindle clunky
  
  access[, dist := NULL]
  
  # generate one file for each measure
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      palma_var <- paste0("palma_", measure)
      
      plot_theme <- theme_minimal() +
        theme(
          panel.grid.minor = element_blank()
        )
      
      plot <- ggplot(access) +
        geom_line(aes(travel_time, get(palma_var), color = scenario)) +
        plot_theme
      
      # save and return the result so the target follows the file
      
      dir_path <- file.path(
        "../../data/avaliacao_intervencoes",
        city,
        "figures"
      )
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "palma_comparison")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 6,
        units = "cm"
      )
    }
  )
  
}
