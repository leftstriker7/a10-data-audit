# 01_pull_data.R
# Downloads the public ESPN data (via hoopR) for every game involving an
# Atlantic 10 team in one season and caches it in data/.
#
# Run from the project root:  Rscript scripts/01_pull_data.R

suppressPackageStartupMessages({
  library(hoopR)
  library(dplyr)
})

SEASON <- 2026 # hoopR labels a season by the year it ends: 2026 = 2025-26

# ESPN team ids for the 14 Atlantic 10 members in 2025-26
A10_TEAMS <- c(
  "Davidson" = 2166, "Dayton" = 2168, "Duquesne" = 2184, "Fordham" = 2230,
  "George Mason" = 2244, "George Washington" = 45, "La Salle" = 2325,
  "Loyola Chicago" = 2350, "Rhode Island" = 227, "Richmond" = 257,
  "Saint Joseph's" = 2603, "Saint Louis" = 139, "St. Bonaventure" = 179,
  "VCU" = 2670
)

dir.create("data", showWarnings = FALSE)

team_box <- load_mbb_team_box(seasons = SEASON)
game_ids <- team_box |>
  filter(team_id %in% A10_TEAMS) |>
  distinct(game_id) |>
  pull(game_id)

team_box <- team_box |> filter(game_id %in% game_ids)
player_box <- load_mbb_player_box(seasons = SEASON) |> filter(game_id %in% game_ids)
pbp <- load_mbb_pbp(seasons = SEASON) |> filter(game_id %in% game_ids)

saveRDS(team_box, "data/team_box.rds")
saveRDS(player_box, "data/player_box.rds")
saveRDS(pbp, "data/pbp.rds")
saveRDS(list(season = SEASON, teams = A10_TEAMS, pulled = Sys.Date()), "data/meta.rds")

cat(sprintf(
  "Saved %d games: %d team box rows, %d player box rows, %d play-by-play events\n",
  length(game_ids), nrow(team_box), nrow(player_box), nrow(pbp)
))
