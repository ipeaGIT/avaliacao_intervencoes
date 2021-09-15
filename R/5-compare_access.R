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


# source("../acesso_oport/R/fun/setup.R")
# library(ggnewscale)
# library(BAMMtools) # fast calculation of jenks natural breaks
# 
# theme_mapa <- function(base_size) {
#   
#   theme_void() %+replace%
#     
#     theme(
#       legend.position = "bottom",
#       plot.margin=unit(c(0,0,0,0),"mm"),
#       legend.key.width=unit(1.0,"line"),
#       legend.key.height = unit(0.1,"cm"),
#       # legend.text=element_text(size=rel(0.5)),
#       # legend.title=element_text(size=rel(0.7)),
#       # legend.text=element_text(size=unit(7, "cm")),
#       # legend.title=element_blank(),
#       # plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
#       
#       
#     )
# }
# 
# 
# # sigla_muni <- "goi"; modo_acesso <- "WALK"
# # sigla_muni <- "for"; modo_acesso <- "WALK"
# 
# compare_access <- function(sigla_muni, modo_acesso) {
#   
#   city_code <- munis_list$munis_df[abrev_muni == sigla_muni]$code_muni
#   
#   
# 
# 
#   # selecionar o shape de intervencao da cidade - para mapa -----------------
# 
#   if (sigla_muni == "for") {
#   # abrir shape da linha
#   linha_leste_shape <- st_read("../../data-raw/avaliacao_intervencoes/for/linha_leste_kaue_gearth.gpkg")
#   # abrir outras linhas
#   gtfs <- gtfstools::read_gtfs("../../data/avaliacao_intervencoes/r5/graph/for_depois/gtfs_for_metrofor_2021-01_depois.zip")
#   linhas_shape <- gtfstools::get_trip_geometry(gtfs) %>%
#     left_join(gtfs$trips %>%  dplyr::select(trip_id, route_id), by = "trip_id") %>%
#     left_join(gtfs$routes %>% dplyr::select(route_id, route_long_name), by = "route_id") %>%
#     filter(!(route_id == "LL" & origin_file == "stop_times")) %>%
#     count(route_long_name)
#   
#   # paradas <- gtfs$stops %>% 
#   #   mutate(linha = ifelse(stop_id %in% c("XDS", "CMT", "NVT", "PAP"), "Linha Leste", "Outra")) %>%
#   #   select(stop_name, linha, stop_lon, stop_lat) %>% 
#   #   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
#   # 
#   # st_write(linhas_shape, "linhas_metrofor_fortaleza.gpkg")
#   # st_write(paradas, "paradas_metrofor_fortaleza.gpkg")
#   
#   
#   # rename
#   linhas_shape <- linhas_shape %>%
#     mutate(route_long_name1 = case_when(route_long_name == "Metro Linha Leste" ~ "Linha Leste",
#                                         route_long_name == "VLT Parangaba Papicu" ~ "VLT",
#                                         TRUE ~ route_long_name ))
#   
#   } else if (sigla_muni == "goi") {
#     
#     gtfs <- gtfstools::read_gtfs("../../data/avaliacao_intervencoes/r5/graph/goi_depois/gtfs_goi_rmtc_2019-10_depois.zip")
#     
#     linhas_shape <- gtfstools::get_trip_geometry(gtfs, file = c("shapes")) %>% setDT()
#     linhas_shape <- merge(linhas_shape, gtfs$trips[, .(trip_id, route_id)], by = "trip_id")
#     linhas_shape <- merge(linhas_shape, gtfs$routes[, .(route_id, route_long_name)], by = "route_id")
#     linhas_shape <- linhas_shape[route_id %like% "BRT"]
#     linhas_shape <- distinct(linhas_shape, route_id, geometry) %>% st_sf()
#       
#     
#     
#   }
#   
#   
#   # abrir city limits
#   city_shape <- geobr::read_municipality(city_code)
#   
#   
#   # abrir access
#   access <- read_rds(sprintf("../../data/avaliacao_intervencoes/%s/output_access/acess_%s_%s.rds", sigla_muni, sigla_muni, modo_acesso))
#   
#   
#   # compare each indicator
#   access_comp <- setDT(access)
#   access_comp <- access_comp[!is.na(quintil)]
#   access_comp <- access_comp %>% dplyr::select(city, origin, pop_total, quintil, decil, tipo, geometry, CMATT30:CMAEM120) %>%
#     gather("ind", "valor", CMATT30:CMAEM120) %>%
#     spread(tipo, valor) %>%
#     # calculate abs diffs
#     mutate(dif_abs = depois - antes,
#            dif_rel = log(depois/antes)) %>%
#            # dif_rel = round((depois-antes)/antes, 2)) %>%
#     st_sf(crs = 4326)
#   
#   
#   
#   # variaveiss <- "CMATT60"
#   # variaveiss <- "CMASB60"
#   # variaveiss <- "CMASA60" checar os zeros desse indicador
#   # TODO: 
#   # - criar graficos (boxplot + pontos) com desigualdades
#   # - colocar as outras linhas de alta capacidade nos mapas
#   # - mapas: antes e depois (em cima) e dif abs e dif rel (em baixo)
#   # - calcular indicador gravitacional
#   # - colocar porcentagem nos valores relativos
#   
#   library(scales)
#   
#   ks <- function (x) { scales::label_number(accuracy = 1,
#                                             scale = 1/1000,
#                                             suffix = "k",
#                                             big.mark = ",")(x)
#     
#   }
#   
#   
#   
#   # variaveiss <- "CMATT60"
#   # variaveiss <- "CMAET60"
#   # variaveiss <- "CMASM60"
#   
#   fazer_plots_acess_comp <- function(variaveiss) {
#     
#     
#     go <- access_comp %>%
#       filter(quintil != 0) %>%
#       filter(ind == variaveiss)
#     # mutate(dif_abs = ifelse(dif_abs < 0, 0, dif_abs),
#     #        dif_rel = ifelse(dif_rel < 0, 0, dif_rel))
#     
#     # classficiar os hex com 0 antes e 0 depois
#     go <- go %>%
#       mutate(dif_rel = ifelse(antes == 0 & depois == 0, 0, dif_rel))
#     
#     dif_abs_trun <- max(abs(quantile(go$dif_abs, 0.05)), abs(quantile(go$dif_abs, 0.97)))
#     dif_rel_trun <- max(abs(quantile(go$dif_rel[go$dif_rel != 0], 0.05, na.rm = TRUE)),
#                         abs(quantile(go$dif_rel[go$dif_rel != 0], 0.95, na.rm = TRUE)))
# 
#     go <- go %>%
#       mutate(dif_abs_tc = ifelse(dif_abs < -dif_abs_trun, -dif_abs_trun,
#                              ifelse(dif_abs > dif_abs_trun, dif_abs_trun, dif_abs))) %>%
#       mutate(dif_rel_tc = ifelse(dif_rel < -dif_rel_trun, -dif_rel_trun,
#                              ifelse(dif_rel > dif_rel_trun, dif_rel_trun, dif_rel)))
# 
#     # trazer paradas
#     gtfs_stops_antes <- gtfstools::read_gtfs("../../data-raw/gtfs/goi/2019/gtfs_goi_rmtc_2019-10.zip", files = "stops")[[1]]
#     gtfs_stops_antes <- gtfs_stops_antes %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
#     gtfs_stops_depois <- gtfstools::read_gtfs("../../data/avaliacao_intervencoes/r5/graph/goi_depois/gtfs_goi_rmtc_2019-10_depois.zip", files = "stops")[[1]]
#     gtfs_stops_depois<- gtfs_stops_depois[stop_id %like% "ENS"]
#     gtfs_stops_depois <- gtfs_stops_depois %>% st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
# 
# 
#     mapview(go, zcol = "dif_abs_tc", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL) + gtfs_stops_depois + linhas_shape
#     # 
#     # acess_teste <- go %>% filter(origin %in% c("89a8c0ccd5bffff", "89a8c0cc897ffff"))
#     # 
#     # 
#     # mapview(go, zcol = "antes") + gtfs_stops_depois
#     # mapview(acess_teste, zcol = "antes")
#     # mapview(acess_teste, zcol = "depois") + gtfs_stops_depois
#     
#     
#     # go %>%
#     #   filter(dif_rel >= 0.02) %>%
#     #   pull(dif_abs) %>% mean()
#     # 
#     # go %>%
#     #   filter(dif_rel >= 0.02) %>%
#     #   pull(dif_rel) %>% mean()
#     # 
#     # hex_ben2 <- go %>%
#     #   filter(dif_rel >= 0.02) %>% pull(origin)
#     # 
#     # a <- unique(c(hex_ben1, hex_ben2))
#     # 
#     # go %>% filter(origin %in% a) %>% pull(pop_total) %>% sum()
#     # 
#     # %>%
#     #   
#     #   pull(pop_total) %>% sum()
#     
#     library(ggtext)
#     
#     # abrir tiles
#     # abrir tiles
#     map_tiles <- read_rds(sprintf("../../data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_%s_2019.rds", sigla_muni))
#     
#     
#     # transform map elements to UTM
#     linhas_shape_map <- st_transform(linhas_shape, 3857)
#     
#     
#     
#     city_shape_map <- st_transform(city_shape, 3857)
#     go_map <- st_transform(go, 3857)
#     
#     # limits for each indicator
#     limits_ind <- go %>%
#       st_set_geometry(NULL) %>%
#       group_by(ind) %>%
#       summarise(dif_abs = max(abs(dif_abs), na.rm = TRUE),
#                 dif_rel = max(abs(dif_rel), na.rm = TRUE),
#                 dif_abs_tc = max(abs(dif_abs_tc), na.rm = TRUE),
#                 dif_rel_tc = max(abs(dif_rel_tc), na.rm = TRUE)
#                 ) %>% setDT()
#     
#     # library(BAMMtools)
#     # library(stringi)
#     # go_map <- jenks_natural(go_map, "dif_abs", 8) %>% st_sf()
#     
#     # create fun
#     create_map_acess <- function(var) {
#       
#       
# 
#       
#       ggplot()+
#         # geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
#         # coord_equal() +
#         # scale_fill_identity()+
#         # # nova escala
#         # new_scale_fill() +
#         geom_sf(data = go_map, aes(fill = {{var}}), color = NA)+
#         geom_sf(data = linhas_shape_map, size = 0.5, alpha = 0.5)+
#         # geom_sf(data = linhas_shape, aes(color = route_long_name1), size = 0.5, alpha = 0.7)+
#         geom_sf(data= city_shape_map, fill = NA)+
#         theme_mapa()
#     }
#     
#     
#     
#     
#     labelss <- if (grepl("CMATT", variaveiss)) ks else label_number(accuracy = 1) 
#       
#     
#     
#     # map with access antes
#     map1 <- create_map_acess(antes) +
#       scale_fill_viridis_c(labels = labelss, option = "inferno"
#                            )+
#       labs(title = "Acessibilidade TP ***antes***",
#            # subtitle = variaveiss,
#            fill = "")
#     
#     
#     # map with acess depois
#     map2 <- create_map_acess(depois)+
#       scale_fill_viridis_c(labels = labelss, option = "inferno")+
#       labs(title = "Acessibilidade TP ***depois***",
#            # subtitle = variaveiss,
#            fill = "")
#     
#     map3 <- create_map_acess(dif_abs)+
#       scale_fill_distiller(palette = "PuBu", direction = 1, labels = labelss
#                            # limits = c(-1,1)*limits_ind$dif_abs_tc
#                            # breaks = c(-30000, 0, 30000),
#                            # labels = c("-30 mil", 0, "30 mil")
#       )+
#       labs(title = "Diferença ***absoluta***",
#            # subtitle = variaveiss,
#            fill = "")
#     
#     
#     
#     
#     # boxplot(go$dif_abs)
#     # boxplot(go$dif_rel)
#     # mapview(go, zcol = "dif_abs")
#     
#     map4 <- create_map_acess(dif_rel)+
#       scale_fill_distiller(palette = "PuBu", direction = 1
#                            # limits = c(-1,1)*limits_ind$dif_rel_tc
#                            # breaks = c(-30000, 0, 30000),
#                            # labels = c("-30 mil", 0, "30 mil")
#                            , label = label_percent(accuracy = 1)
#       )+
#       labs(title = "Diferença ***relativa***",
#            # subtitle = variaveiss,
#            fill = "")
#     
#     # boxplot(go_map$dif_abs)
#     
#     
#     # arrange plot
#     plot_conditions <- 
#       # map2 + map3 + map4 &
#       # (map1 | map2) / (map3 |map4) +
#       (map1 | map2) +
#       # plot_layout(nrow = 2) & 
#       # plot_layout(nrow = 2, heights = c(3, 1)) & 
#       plot_layout(heights = c(1, 1)) &
#       theme_mapa() +
#       theme(plot.title = element_markdown(size = 9),
#             plot.subtitle = element_text(size = 6),
#             legend.text = element_text(size = 7),
#             legend.key.width = unit(0.8, "cm"))
#     
#     # out
#     filename <- sprintf("figures/%s/access_conditions/map1_conditions_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
#     ggsave(plot = plot_conditions, filename = paste0(filename, ".png"),
#            height = 10, width = 16,
#            # height = 14, width = 16,
#            units = "cm", device = "png", dpi = 300)
#     
#     plot_comparison <- 
#       # map2 + map3 + map4 &
#       # (map1 | map2) / (map3 |map4) +
#       (map3 | map4) +
#       # plot_layout(nrow = 2) & 
#       # plot_layout(nrow = 2, heights = c(3, 1)) & 
#       plot_layout(heights = c(1, 1)) &
#       theme_mapa() +
#       theme(plot.title = element_markdown(size = 9),
#             plot.subtitle = element_text(size = 6),
#             legend.text = element_text(size = 7),
#             legend.key.width = unit(0.8, "cm"))
#     
#     # out
#     filename <- sprintf("figures/%s/access_comparison/map2_comparisonn_%s_%s_%s", sigla_muni, sigla_muni, modo_acesso, variaveiss)
#     ggsave(plot = plot_comparison, filename = paste0(filename, ".png"),
#            height = 10, width = 16,
#            # height = 14, width = 16,
#            units = "cm", device = "png", dpi = 300)
#     
#     
#     
#     
#     # plot access inequalities --------------------------------------------------------------
#     
#     # 1) access inequalities boxplot 1
#     go_long <- go %>%
#       st_set_geometry(NULL) %>%
#       pivot_longer(cols = dif_abs:dif_rel,
#                    names_to = "tipo_indicador",
#                    values_to = "valor_indicador"
#       )
#     # 
#     # # change label
#     # go_long <- go_long %>%
#     #   mutate(valor_indicador1 = ifelse(tipo_indicador == "dif_rel", 
#     #                                    paste0(valor_indicador * 100, " %"), valor_indicador))
#     
#     # calcular palma ratio
#     acess_palma <- go_long %>%
#       dplyr::select(city, decil, pop_total, tipo_indicador, valor_indicador) %>%
#       # pegar so decis 4 e 9
#       filter(decil %in% c(1, 2, 3, 4, 10)) %>%
#       # definir ricos e pobres
#       mutate(classe = ifelse(decil %in% c(1, 2, 3, 4), "pobre", "rico")) %>%
#       group_by(city, classe, tipo_indicador) %>%
#       summarise(acess_media = weighted.mean(valor_indicador, pop_total)) %>%
#       ungroup() %>%
#       spread(classe, acess_media) %>%
#       # calcular palma ratio
#       group_by(city, tipo_indicador) %>%
#       mutate(palma_ratio = rico/pobre) %>%
#       ungroup()
#     
#     # definir valor pra truncar eixo y
#     valor_trunc_abs <- ifelse(variaveiss == "CMATT60", 20000, 10)
#     valor_trunc_rel <- ifelse(variaveiss == "CMATT60", 0.1, 0.1)
#     
#     
#     boxplot_inequalities11 <- ggplot()+
#       geom_boxplot(data = go, 
#                    aes(x = as.factor(decil), y = dif_abs, weight = pop_total, color = as.factor(decil)), 
#                    outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05),
#                    show.legend = FALSE) +
#       # coord_flip()+
#       # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
#       # scale_y_continuous(label = percent)+
#       # scale_y_continuous(labels = function(x) ifelse(x <= 2, paste0(x * 10, "%"), x))+
#       scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
#       theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
#       guides(color=guide_legend(nrow=1)) +
#       labs(y = "Ganho absoluto de acess.", x = "",
#            subtitle = sprintf("**Razão de Palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_abs")$palma_ratio, 2)))+
#       coord_cartesian(ylim = c(NA, valor_trunc_abs)) +
#       theme( 
#         panel.grid.minor = element_blank()
#         ,strip.text = element_markdown(size = 7, face ="bold")
#         ,legend.text = element_text(size = 5)
#         , legend.position = "bottom"
#         , axis.text.x = element_blank()
#         , axis.text.y = element_text(size = 6),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7, face="bold"),
#         legend.title = element_text(size = 7)
#         
#       )
#     
#     boxplot_inequalities12 <- ggplot()+
#       geom_boxplot(data = go, 
#                    aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
#                    outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
#       # coord_flip()+
#       # facet_wrap(~tipo_indicador, scale = "free_y", nrow = 2)+
#       # scale_y_continuous(label = percent)+
#       scale_y_continuous(labels = percent)+
#       scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
#       theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
#       guides(color=guide_legend(nrow=1)) +
#       labs(y = "Ganho relativo de acess.", x = "",
#            subtitle = sprintf("**Razão de palma**: %s", round(subset(acess_palma, tipo_indicador == "dif_rel")$palma_ratio, 2)))+
#       coord_cartesian(ylim = c(NA, valor_trunc_rel)) +
#       theme( 
#         panel.grid.minor = element_blank()
#         ,strip.text = element_markdown(size = 5, face ="bold")
#         ,legend.text = element_text(size = 5)
#         , legend.position = "bottom"
#         , axis.text.x = element_blank()
#         , axis.text.y = element_text(size = 6),
#         axis.title.x = element_text(size = 7),
#         axis.title.y = element_text(size = 7, face="bold"),
#         legend.title = element_text(size = 7)
#         
#       )
#     
#     # juntar
#     
#     boxplot_inequalities1 <- 
#       # map2 + map3 + map4 &
#       # (map1 | map2) / (map3 |map4) +
#       (boxplot_inequalities11 / boxplot_inequalities12) +
#       # plot_layout(nrow = 2) & 
#       # plot_layout(nrow = 2, heights = c(3, 1)) & 
#       plot_layout(heights = c(1, 1)) &
#       theme(plot.title = element_markdown(size = 7),
#             plot.subtitle = element_markdown(size = 8),
#             legend.text = element_text(size = 6),
#             plot.margin=unit(c(0,0,0,0),"mm"))
#     
#     
#     ggsave(plot = boxplot_inequalities1, 
#            filename = sprintf("figures/%s/access_inequalities/fig1_ineq1_%s_%s_%s.png", 
#                               sigla_muni, sigla_muni, modo_acesso, variaveiss), 
#            height = 10, width = 16, units = "cm", device = "png")
#     
#     
#     
#     # 
#     # # 2) access inequalities boxplot 2
#     # # teste: pegar locais que tiveram pelo menos 2% de melhoria no acesso
#     # boxplot_inequalities2 <- go %>%
#     #   filter(dif_rel >= 0.02) %>%
#     #   ggplot()+
#     #   geom_boxplot(aes(x = as.factor(decil), y = dif_rel, weight = pop_total, color = as.factor(decil)), 
#     #                outlier.size = 1.5, outlier.alpha = 0.5, outlier.colour=rgb(.5,.5,.5, alpha=0.05)) +
#     #   # coord_flip()+
#     #   facet_wrap(~ind, scale = "free_y", nrow = 3)+
#     #   scale_y_continuous(label = percent)+
#     #   theme_ipsum_rc(grid = "X", base_family = 'Helvetica')+
#     #   scale_colour_brewer(palette = "RdBu", labels=c('D1 Pobres', paste0('D', c(2:9)), 'D10 ricos'), name='Decil de renda') +
#     #   
#     #   coord_cartesian(ylim = c(0, NA))+
#     #   geom_hline(yintercept = 0)+
#     #   guides(color=guide_legend(nrow=1)) +
#     #   theme( 
#     #     panel.grid.minor = element_blank()
#     #     ,strip.text = element_text(size = 5, face ="bold")
#     #     ,legend.text = element_text(size = 5)
#     #     , legend.position = "bottom"
#     #     , axis.text.x = element_blank()
#     #     , axis.text.y = element_text(size = 5),
#     #     axis.title.x = element_text(size = 5),
#     #     axis.title.y = element_text(size = 5, face="bold"),
#     #     legend.title = element_text(size = 5)
#     #     
#     #   )
#     # 
#     # 
#     # ggsave(plot = boxplot_inequalities2, 
#     #        filename = sprintf("figures/%s/access_inequalities/fig1_ineq2_%s.png", sigla_muni, sigla_muni), 
#     #        height = 12, width = 16, units = "cm", device = "png")
#     # 
#     # 
#     # 
#     # 
#     # # 3) access inequalities scatterplot
#     # 
#     # baseplot1 <- theme_minimal() +
#     #   theme( 
#     #     axis.text.y  = element_text(face="bold", size=9)
#     #     ,axis.text.x  = element_text(face="bold", size=9)
#     #     ,panel.grid.minor = element_blank()
#     #     ,strip.text = element_text(size = 11, face ="bold")
#     #     ,legend.text = element_text(size = 11)
#     #   )
#     # 
#     # scatterplot_inequalities <- ggplot()+
#     #   geom_point(data = go, 
#     #              aes(x = antes, y = depois, color = as.factor(quintil), size = pop_total/100),
#     #              alpha = 0.4)+
#     #   geom_abline()+
#     #   scale_color_brewer(palette = "RdBu")+
#     #   # coord_flip()+
#     #   # theme_ipsum_rc(base_family = 'Helvetica')+
#     #   facet_wrap(~ind, scale = "free")+
#     #   labs(size = "Pop. total, milhares",
#     #        color = "Quintil de renda")+
#     #   baseplot1+
#     #   guides(color = guide_legend(nrow = 1, title.position = "top", label.position = "bottom", override.aes = list(size=5))) +
#     #   # guides(size = guide_legend(nrow = 1,  label.position = "bottom")) +
#     #   theme(
#     #     # Legends
#     #     legend.position="bottom", # horz vert
#     #     legend.direction='horizontal',
#     #     legend.box='horizontal',
#     #     legend.title=element_text(size=8),
#     #     legend.text=element_text(size=7),
#     #     legend.key.size = unit(.2, "cm"),
#     #     legend.text.align=0.5,
#     #     # axis
#     #     axis.ticks=element_blank())
#     # 
#     # 
#     # 
#     # 
#     # 
#     # ggsave(plot = scatterplot_inequalities, 
#     #        filename = sprintf("figures/%s/access_inequalities/fig2_scatterplot_%s.png", sigla_muni, sigla_muni), 
#     #        height = 12, width = 16, units = "cm", device = "png")
#     
#     
#   }
#   
#   # variaveis <- grep(pattern = "TT|ET|EI|EF|EM|ST|SB|SM|SA", x = colnames(access), value = TRUE)
#   variaveis <- c("CMATT60", "CMAET60", "CMASB60")
#   
#   lapply(variaveis, fazer_plots_acess_comp)
#   
#   
#   
# }
# 
# 
# 
# 
# 
# # # apply function
# # compare_access('for', "WALK")
# # compare_access('for', "BICYCLE")
# # 
# # compare_access('goi', "WALK")
