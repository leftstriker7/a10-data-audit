# 03_compare_seasons.R
# Tests whether the assist gap by host arena repeats from one season to the
# next. Run after 01 and 02 have been run for both seasons.
#
# Run from the project root:  Rscript scripts/03_compare_seasons.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

SEASONS <- c(2025L, 2026L)

host_rates <- function(season) {
  a <- readRDS(file.path("output", season, "audit.rds"))
  team_chk <- a$all_checks |> filter(check == "Play-by-play vs team box")

  # Only games whose play-by-play accounts for every point, so that a missing
  # stretch of the feed is not counted as an assist problem.
  complete <- team_chk |>
    filter(stat == "pts") |>
    group_by(game_id) |>
    summarise(ok = all(diff == 0), .groups = "drop") |>
    filter(ok) |>
    pull(game_id)

  team_chk |>
    filter(stat == "ast", game_id %in% complete) |>
    left_join(a$games |> select(game_id, host = home), by = "game_id") |>
    filter(host %in% names(a$meta$teams)) |>
    group_by(host) |>
    summarise(
      team_games = n(), team_games_off = sum(diff != 0),
      share_off = team_games_off / team_games,
      assists_missing = sum(box - pbp), .groups = "drop"
    ) |>
    mutate(season = season, label = a$meta$label)
}

by_host <- bind_rows(lapply(SEASONS, host_rates))

wide <- by_host |>
  select(host, season, team_games, team_games_off, share_off) |>
  pivot_wider(names_from = season, values_from = c(team_games, team_games_off, share_off))

both <- wide |> filter(!is.na(share_off_2025), !is.na(share_off_2026))
rank_cor <- cor(both$share_off_2025, both$share_off_2026, method = "spearman")

overall <- by_host |>
  group_by(season, label) |>
  summarise(
    team_games = sum(team_games), team_games_off = sum(team_games_off),
    share_off = team_games_off / team_games, .groups = "drop"
  )

# The play-by-play wording changed in 2025-26 ("makes" / "misses" in place of
# "made" / "missed") in some games. Are those games any worse for assists?
wording <- function(season) {
  a <- readRDS(file.path("output", season, "audit.rds"))
  fmt <- readRDS(file.path("data", season, "pbp.rds")) |>
    group_by(game_id) |>
    summarise(new_wording = any(grepl(" makes | misses ", text)), .groups = "drop")
  a$all_checks |>
    filter(check == "Play-by-play vs team box", stat == "ast") |>
    inner_join(fmt, by = "game_id") |>
    group_by(new_wording) |>
    summarise(games = n_distinct(game_id), team_games = n(), share_off = mean(diff != 0), .groups = "drop") |>
    mutate(season = season)
}
by_wording <- bind_rows(lapply(SEASONS, wording))

write_csv(by_host, "output/assist_host_by_season.csv")
saveRDS(
  list(
    by_host = by_host, wide = wide, overall = overall, rank_cor = rank_cor,
    hosts_compared = nrow(both), by_wording = by_wording
  ),
  "output/season_comparison.rds"
)

print(as.data.frame(wide |> arrange(desc(share_off_2026))), digits = 2)
print(as.data.frame(overall), digits = 3)
cat(sprintf("Rank correlation across %d hosts: %.2f\n", nrow(both), rank_cor))
print(as.data.frame(by_wording), digits = 3)
