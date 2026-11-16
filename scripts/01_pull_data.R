# 01_pull_data.R
# Downloads the public ESPN data (via hoopR) for every game involving an
# Atlantic 10 team in one season and caches it in data/<season>/.
#
# Run from the project root:  Rscript scripts/01_pull_data.R [season]
# hoopR labels a season by the year it ends: 2026 = 2025-26 (the default).

suppressPackageStartupMessages({
  library(hoopR)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
SEASON <- if (length(args) > 0) as.integer(args[1]) else 2026L

# ESPN team ids for the Atlantic 10 members
A10_TEAMS <- c(
  "Davidson" = 2166, "Dayton" = 2168, "Duquesne" = 2184, "Fordham" = 2230,
  "George Mason" = 2244, "George Washington" = 45, "La Salle" = 2325,
  "Loyola Chicago" = 2350, "Rhode Island" = 227, "Richmond" = 257,
  "Saint Joseph's" = 2603, "Saint Louis" = 139, "St. Bonaventure" = 179,
  "VCU" = 2670
)
# UMass left for the MAC after 2024-25
if (SEASON <= 2025) A10_TEAMS <- c(A10_TEAMS, "UMass" = 113)

dir <- file.path("data", SEASON)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

team_box <- load_mbb_team_box(seasons = SEASON)

# stop if a team id does not resolve to the expected school
found <- team_box |> distinct(team_id, team_location) |> filter(team_id %in% A10_TEAMS)
stopifnot(nrow(found) == length(A10_TEAMS))
names(A10_TEAMS) <- found$team_location[match(A10_TEAMS, found$team_id)]

game_ids <- team_box |>
  filter(team_id %in% A10_TEAMS) |>
  distinct(game_id) |>
  pull(game_id)

team_box <- team_box |> filter(game_id %in% game_ids)
player_box <- load_mbb_player_box(seasons = SEASON) |> filter(game_id %in% game_ids)
pbp <- load_mbb_pbp(seasons = SEASON) |> filter(game_id %in% game_ids)

saveRDS(team_box, file.path(dir, "team_box.rds"))
saveRDS(player_box, file.path(dir, "player_box.rds"))
saveRDS(pbp, file.path(dir, "pbp.rds"))
saveRDS(
  list(
    season = SEASON, label = sprintf("%d-%02d", SEASON - 1L, SEASON %% 100L),
    teams = A10_TEAMS, pulled = Sys.Date()
  ),
  file.path(dir, "meta.rds")
)

cat(sprintf(
  "Season %d: saved %d games, %d team box rows, %d player box rows, %d play-by-play events\n",
  SEASON, length(game_ids), nrow(team_box), nrow(player_box), nrow(pbp)
))
cat("Teams:", paste(names(A10_TEAMS), collapse = ", "), "\n")
