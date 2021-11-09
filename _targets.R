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

source("R/3.2-calculate_ttmatrix.R", encoding = "UTF-8")
source("R/4-calculate_access.R", encoding = "UTF-8")
source("R/5-compare_access.R", encoding = "UTF-8")

list(
  tar_target(
    only_for,
    "for"
  ),
  tar_target(
    both_cities,
    c("for")
  ),
  tar_target(
    scenarios,
    c("antes", "contrafactual", "depois")
  ),
  tar_target(
    exploratory_skeleton_file,
    "rmarkdown/exploratory_skeleton.Rmd"
  ),
  tar_target(
    tt_thresholds,
    c(60)
  ),
  tar_target(
    exploratory_skeleton,
    exploratory_skeleton_file,
    pattern = map(exploratory_skeleton_file),
    format = "file"
  ),
  tar_target(
    bike_parks_path,
    paste0(
      "../../data/avaliacao_intervencoes/r5/points/bike_parks_",
      only_for, "_", scenarios,
      ".csv"
    ),
    pattern = cross(only_for, scenarios),
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
      both_cities, "_", scenarios
    ),
    format = "file",
    pattern = cross(both_cities, scenarios)
  ),
  tar_target(
    transit_matrix,
    transit_ttm(both_cities, scenarios, graph, points_path),
    format = "file",
    pattern = map(
      graph,
      cross(map(both_cities, points_path), scenarios)
    )
  ),
  tar_target(
    transit_access,
    create_accessibility_data(
      both_cities,
      scenarios,
      transit_matrix,
      grid_path
    ),
    format = "file",
    pattern = map(
      transit_matrix,
      cross(map(both_cities, grid_path), scenarios)
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
          tidyr::crossing(city = both_cities, scenario = scenarios)
        ),
        city
      )
    ),
    iteration = "group"
  ),
  tar_target(
    transit_access_diff,
    calculate_access_diff(
      access_metadata$city[1],
      access_metadata$access_file,
      access_metadata$scenario
    ),
    pattern = map(access_metadata),
    format = "file"
  ),
  tar_target(
    distribution_maps,
    create_dist_maps(
      access_metadata$city[1],
      access_metadata$access_file,
      access_metadata$scenario,
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
      transit_access_diff,
      grid_path,
      tt_thresholds
    ),
    pattern = cross(
      map(
        both_cities, transit_access_diff, grid_path
      ),
      tt_thresholds
    ),
    format = "file"
  ),
  tar_target(
    difference_boxplot,
    create_boxplots(
      both_cities,
      transit_access_diff,
      grid_path,
      tt_thresholds
    ),
    pattern = cross(
      map(
        both_cities, transit_access_diff, grid_path
      ),
      tt_thresholds
    ),
    format = "file"
  ),
  tar_target(
    palma_bars,
    create_palma_bars(
      both_cities,
      access_metadata$access_file,
      access_metadata$scenario,
      grid_path,
      tt_thresholds
    ),
    pattern = cross(
      map(both_cities, access_metadata, grid_path),
      tt_thresholds
    ),
    format = "file"
  ),
  tar_target(
    palma_comparison,
    compare_palma(
      both_cities,
      access_metadata$access_file,
      access_metadata$scenario,
      grid_path
    ),
    pattern = map(both_cities, access_metadata, grid_path),
    format = "file"
  ),
  tar_target(
    access_gains_comparison,
    compare_gains(
      both_cities,
      transit_access_diff,
      grid_path
    ),
    pattern = map(both_cities, transit_access_diff, grid_path),
    format = "file"
  ),
  tar_target(
    avg_access_gains,
    calculate_avg_gains(
      both_cities,
      transit_access_diff,
      grid_path
    ),
    pattern = map(both_cities, transit_access_diff, grid_path)
  ),
  tar_target(
    bike_matrix,
    bike_ttm(only_for, scenarios, graph, points_path),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(scenarios, head(graph, 3))
    ),
    format = "file"
  ),
  tar_target(
    bike_first_mile_matrix,
    bfm_ttm(only_for, scenarios, graph, points_path, bike_parks_path),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(scenarios, head(graph, 3), bike_parks_path)
    ),
    format = "file"
  ),
  tar_target(
    full_matrix,
    join_ttms(
      only_for,
      scenarios,
      bike_matrix,
      transit_matrix,
      bike_first_mile_matrix,
      points_path
    ),
    pattern = cross(
      map(only_for, head(points_path, 1)),
      map(
        scenarios,
        bike_matrix,
        head(transit_matrix, 3),
        bike_first_mile_matrix
      )
    ),
    format = "file"
  ),
  tar_target(
    full_access,
    create_accessibility_data(
      only_for,
      scenarios,
      full_matrix,
      grid_path
    ),
    pattern = map(
      full_matrix,
      cross(map(only_for, head(grid_path, 1)), scenarios)
    ),
    format = "file"
  ),
  tar_target(
    exploratory_analysis,
    exploratory_report(
      only_for,
      full_matrix,
      scenarios,
      bike_parks_path,
      grid_path,
      exploratory_skeleton
    ),
    pattern = map(
      full_matrix,
      cross(
        map(only_for, head(grid_path, 1), exploratory_skeleton),
        map(scenarios, bike_parks_path)
      )
    ),
    format = "file"
  ),
  tar_target(
    full_access_diff,
    calculate_access_diff(
      only_for,
      full_access,
      scenarios
    ),
    format = "file"
  ),
  tar_target(
    all_modes_summary,
    plot_summary(
      only_for,
      scenarios,
      full_access,
      full_access_diff,
      grid_path,
      tt_thresholds
    ),
    pattern = cross(head(grid_path, 1), tt_thresholds),
    format = "file"
  )
)
