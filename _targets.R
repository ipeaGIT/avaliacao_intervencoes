options(
  java.parameters = "-Xmx32G",
  R5R_THREADS = 10
)

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(data.table)
  library(r5r)
  library(sf)
  library(ggplot2)
  library(ggtext)
  library(patchwork)
  library(dplyr)
})

source("R/fun/functions_daniel.R")

list(
  tar_target(
    only_for,
    "for"
  ),
  tar_target(
    both_cities,
    c("for", "goi")
  ),
  tar_target(
    before_after,
    c("antes", "depois")
  ),
  tar_target(
    points_path,
    paste0(
      "../../data/avaliacao_intervencoes/r5/points/points_",
      both_cities,
      "_09_2019.csv"
    ),
    format = "file",
    pattern = map(both_cities)
  ),
  tar_target(
    grid_path,
    paste0(
      "../../data/acesso_oport/hex_agregados/2019/hex_agregado_",
      both_cities,
      "_09_2019.rds"
    ),
    format = "file",
    pattern = map(both_cities)
  ),
  tar_target(
    graph,
    paste0(
      "../../data/avaliacao_intervencoes/r5/graph/",
      both_cities, "_", before_after
    ),
    format = "file",
    pattern = cross(both_cities, before_after)
  ),
  tar_target(
    transit_matrix,
    transit_ttm(both_cities, before_after, graph, points_path),
    format = "file",
    pattern = map(
      graph,
      cross(map(both_cities, points_path), before_after)
    )
  ),
  tar_target(
    transit_access,
    calculate_accessibility(
      both_cities,
      before_after,
      transit_matrix,
      grid_path
    ),
    format = "file",
    pattern = map(
      transit_matrix,
      cross(map(both_cities, grid_path), before_after)
    )
  ),
  tar_target(
    access_metadata,
    tar_group(
      group_by(
        tidyr::nesting(
          access_file = transit_access,
          tidyr::crossing(city = both_cities, scenario = before_after)
        ),       
        city
      )
    ),
    iteration = "group"
  ),
  tar_target(
    transit_access_diff_abs,
    calculate_access_diff(
      access_metadata$city[1],
      access_metadata$access_file,
      method = "absolute"
    ),
    pattern = map(access_metadata),
    format = "file"
  ),
  tar_target(
    transit_access_diff_rel,
    calculate_access_diff(
      access_metadata$city[1],
      access_metadata$access_file,
      method = "relative"
    ),
    pattern = map(access_metadata),
    format = "file"
  ),
  tar_target(
    distribution_maps,
    create_dist_maps(
      access_metadata$city[1],
      access_metadata$access_file,
      grid_path
    ),
    pattern = map(access_metadata, grid_path),
    format = "file"
  )
  
  
  
  
  
  
  # tar_target(
  #   points_path,
  #   "../../data/avaliacao_intervencoes/r5/points/points_for_09_2019.csv",
  #   format = "file"
  # ),
  # tar_target(
  #   grid_path,
  #   "../../data/acesso_oport/hex_agregados/2019/hex_agregado_for_09_2019.rds",
  #   format = "file"
  # ),
  # tar_target(
  #   exploratory_skeleton,
  #   "rmarkdown/exploratory_skeleton.Rmd",
  #   format = "file"
  # ),
  # tar_target(
  #   analysis_skeleton,
  #   "rmarkdown/scenario_analysis.Rmd",
  #   format = "file"
  # ),
  # tar_map(
  #   unlist = FALSE,
  #   values = list(scenario = c("antes", "depois")),
  #   tar_target(
  #     graph,
  #     paste0("../../data/avaliacao_intervencoes/r5/graph/for_", scenario),
  #     format = "file"
  #   ),
  #   tar_target(
  #     bike_parks_path,
  #     paste0(
  #       "../../data/avaliacao_intervencoes/r5/points/bike_parks_for_",
  #       scenario,
  #       ".csv"
  #     ),
  #     format = "file"
  #   ),
  #   tar_target(
  #     bike_matrix,
  #     bike_ttm(scenario, graph, points_path),
  #     format = "file"
  #   ),
  #   tar_target(
  #     transit_matrix,
  #     transit_ttm(scenario, graph, points_path),
  #     format = "file"
  #   ),
  #   tar_target(
  #     bike_first_mile_matrix,
  #     bfm_ttm(scenario, graph, points_path, bike_parks_path),
  #     format = "file"
  #   ),
  #   tar_target(
  #     full_matrix,
  #     join_ttms(
  #       scenario,
  #       bike_matrix,
  #       transit_matrix,
  #       bike_first_mile_matrix,
  #       points_path
  #     ),
  #     format = "file"
  #   ),
  #   tar_target(
  #     exploratory_analysis,
  #     exploratory_report(
  #       full_matrix,
  #       scenario,
  #       bike_parks_path,
  #       grid_path,
  #       exploratory_skeleton
  #     ),
  #     format = "file"
  #   ),
  #   tar_target(
  #     accessibility,
  #     calculate_accessibility(
  #       scenario,
  #       full_matrix,
  #       grid_path
  #     ),
  #     format = "file"
  #   )
  # ),
  # tar_target(
  #   accessibility_diff_abs,
  #   calculate_access_diff(
  #     c(accessibility_antes, accessibility_depois),
  #     method = "absolute"
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   accessibility_diff_rel,
  #   calculate_access_diff(
  #     c(accessibility_antes, accessibility_depois),
  #     method = "relative"
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   scenario_analysis,
  #   analyse_scenarios(
  #     list(accessibility_antes, accessibility_depois),
  #     accessibility_diff_abs,
  #     grid_path,
  #     analysis_skeleton
  #   ),
  #   format = "file"
  # ),
  # tar_target(
  #   boxplot_charts,
  #   create_boxplots(accessibility_diff_abs, accessibility_diff_rel, grid_path),
  #   format = "file"
  # ),
  # tar_target(
  #   distribution_maps,
  #   create_dist_maps(accessibility_antes, accessibility_depois, grid_path),
  #   format = "file"
  # ),
  # tar_target(
  #   difference_maps,
  #   create_diff_maps(accessibility_diff_abs, accessibility_diff_rel, grid_path),
  #   format = "file"
  # )
)
