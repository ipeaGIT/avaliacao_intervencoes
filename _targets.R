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
})

source("R/fun/functions_daniel.R")

list(
  tar_target(
    points_path,
    "../../data/avaliacao_intervencoes/r5/points/points_for_09_2019.csv",
    format = "file"
  ),
  tar_target(
    bike_parks_path,
    "../../data/avaliacao_intervencoes/r5/points/bike_parks_for.csv",
    format = "file"
  ),
  tar_target(
    grid_path,
    "../../data/acesso_oport/hex_agregados/2019/hex_agregado_for_09_2019.rds",
    format = "file"
  ),
  tar_target(
    exploratory_skeleton,
    "inst/rmarkdown/exploratory_skeleton.Rmd",
    format = "file"
  ),
  tar_target(
    analysis_skeleton,
    "inst/rmarkdown/scenario_analysis.Rmd",
    format = "file"
  ),
  tar_map(
    unlist = FALSE,
    values = list(scenario = c("antes", "depois")),
    tar_target(
      graph,
      paste0("../../data/avaliacao_intervencoes/r5/graph/for_", scenario),
      format = "file"
    ),
    tar_target(
      bike_matrix,
      bike_ttm(scenario, graph, points_path),
      format = "file"
    ),
    tar_target(
      transit_matrix,
      transit_ttm(scenario, graph, points_path),
      format = "file"
    ),
    tar_target(
      bike_first_mile_matrix,
      bfm_ttm(scenario, graph, points_path, bike_parks_path),
      format = "file"
    ),
    tar_target(
      full_matrix,
      join_ttms(
        scenario,
        bike_matrix,
        transit_matrix,
        bike_first_mile_matrix,
        points_path
      ),
      format = "file"
    ),
    tar_target(
      exploratory_analysis,
      exploratory_report(
        full_matrix,
        scenario,
        bike_parks_path,
        grid_path,
        exploratory_skeleton
      ),
      format = "file"
    ),
    tar_target(
      accessibility,
      calculate_accessibility(
        scenario,
        full_matrix,
        grid_path
      ),
      format = "file"
    )
  ),
  tar_target(
    accessibility_diff_abs,
    calculate_access_diff(
      c(accessibility_antes, accessibility_depois),
      method = "absolute"
    ),
    format = "file"
  ),
  tar_target(
    accessibility_diff_rel,
    calculate_access_diff(
      c(accessibility_antes, accessibility_depois),
      method = "relative"
    ),
    format = "file"
  ),
  tar_target(
    scenario_analysis,
    analyse_scenarios(
      list(accessibility_antes, accessibility_depois),
      accessibility_diff_abs,
      grid_path,
      analysis_skeleton
    ),
    format = "file"
  )
)
