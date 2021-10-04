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

source("R/3.2-calculate_ttmatrix.R")
source("R/4-calculate_access.R")
source("R/5-compare_access.R")

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
    exploratory_skeleton_file,
    "rmarkdown/exploratory_skeleton.Rmd"
  ),
  tar_target(
    tt_thresholds,
    c(15, 30, 45, 60)
  ),
  tar_target(
    exploratory_skeleton,
    exploratory_skeleton_file,
    pattern = map(exploratory_skeleton_file),
    format = "file"
  ),
  tar_target(
    analysis_skeleton_file,
    "rmarkdown/scenario_analysis.Rmd"
  ),
  tar_target(
    analysis_skeleton,
    analysis_skeleton_file,
    pattern = map(analysis_skeleton_file),
    format = "file"
  ),
  tar_target(
    bike_parks_path,
    paste0(
      "../../data/avaliacao_intervencoes/r5/points/bike_parks_",
      only_for, "_", before_after,
      ".csv"
    ),
    pattern = cross(only_for, before_after),
    format = "file"
  ),
  tar_target(
    grid_path,
    paste0(
      "../../data/acesso_oport/hex_agregados/2019/hex_agregado_",
      both_cities,
      "_09_2019.rds"
    ),
    pattern = map(both_cities),
    format = "file"
  ),
  tar_target(
    points_path,
    create_points_r5(both_cities, grid_path),
    pattern = map(both_cities, grid_path),
    format = "file"
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
    create_accessibility_data(
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
          access_df_hash = vapply(
            transit_access,
            FUN.VALUE = character(1),
            FUN = function(i) {
              df <- readRDS(i)
              digest::digest(df, algo = "md5")
            }
          ),
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
      grid_path,
      tt_thresholds
    ),
    pattern = cross(map(access_metadata, grid_path), tt_thresholds),
    format = "file"
  ),
  tar_target(
    difference_maps,
    create_diff_maps(
      both_cities, 
      transit_access_diff_abs,
      transit_access_diff_rel,
      grid_path
    ),
    pattern = map(
      both_cities, transit_access_diff_abs, transit_access_diff_rel, grid_path
    ),
    format = "file"
  ),
  tar_target(
    difference_boxplot,
    create_boxplots(
      both_cities, 
      transit_access_diff_abs,
      transit_access_diff_rel,
      grid_path
    ),
    pattern = map(
      both_cities, transit_access_diff_abs, transit_access_diff_rel, grid_path
    ),
    format = "file"
  ),
  tar_target(
    bike_matrix,
    bike_ttm(only_for, before_after, graph, points_path),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(before_after, head(graph, 2))
    ),
    format = "file"
  ),
  tar_target(
    bike_first_mile_matrix,
    bfm_ttm(only_for, before_after, graph, points_path, bike_parks_path),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(before_after, head(graph, 2), bike_parks_path)
    ),
    format = "file"
  ),
  tar_target(
    full_matrix,
    join_ttms(
      only_for,
      before_after,
      bike_matrix,
      transit_matrix,
      bike_first_mile_matrix,
      points_path
    ),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(
        before_after,
        bike_matrix,
        head(transit_matrix, 2),
        bike_first_mile_matrix
      )
    ),
    format = "file"
  ),
  tar_target(
    full_access,
    create_accessibility_data(
      only_for,
      before_after,
      full_matrix,
      grid_path
    ),
    pattern = map(
      full_matrix,
      cross(map(only_for, head(grid_path, 1)), before_after)
    ),
    format = "file"
  ),
  tar_target(
    exploratory_analysis,
    exploratory_report(
      only_for,
      full_matrix,
      before_after,
      bike_parks_path,
      grid_path,
      exploratory_skeleton
    ),
    pattern = map(
      full_matrix,
      cross(
        map(only_for, head(grid_path, 1), exploratory_skeleton),
        map(before_after, bike_parks_path)
      )
    ),
    format = "file"
  ),
  tar_target(
    full_access_diff_abs,
    calculate_access_diff(
      only_for,
      full_access,
      method = "absolute"
    ),
    format = "file"
  ),
  tar_target(
    full_access_diff_rel,
    calculate_access_diff(
      only_for,
      full_access,
      method = "relative"
    ),
    format = "file"
  ),
  tar_target(
    scenario_analysis,
    analyse_scenarios(
      only_for,
      full_access,
      full_access_diff_abs,
      grid_path,
      analysis_skeleton
    ),
    pattern = head(grid_path, 1),
    format = "file"
  ),
  tar_target(
    all_modes_summary,
    plot_summary(
      only_for,
      full_access,
      full_access_diff_abs,
      full_access_diff_rel,
      grid_path
    ),
    pattern = head(grid_path, 1),
    format = "file"
  )
)
