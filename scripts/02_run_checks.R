# 02_run_checks.R
# Reconciles three views of the same games against each other:
#   play-by-play events  vs  team box score  vs  player box score
# and writes every disagreement to output/.
#
# Run from the project root:  Rscript scripts/02_run_checks.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

pbp <- readRDS("data/pbp.rds")
team_box <- readRDS("data/team_box.rds")
player_box <- readRDS("data/player_box.rds")
meta <- readRDS("data/meta.rds")

SHOTS <- c("JumpShot", "LayUpShot", "DunkShot", "TipShot")

STAT_LABELS <- c(
  pts = "Points", fgm = "Field goals made", fga = "Field goals attempted",
  tpm = "Threes made", tpa = "Threes attempted", ftm = "Free throws made",
  fta = "Free throws attempted", orb = "Offensive rebounds",
  drb = "Defensive rebounds", ast = "Assists", stl = "Steals", blk = "Blocks",
  tov = "Turnovers", pf = "Fouls", min = "Minutes"
)

# ---- Game index -------------------------------------------------------------

games <- team_box |>
  mutate(team_score = as.numeric(team_score)) |>
  group_by(game_id) |>
  summarise(
    game_date = first(game_date),
    home = team_location[team_home_away == "home"][1],
    away = team_location[team_home_away == "away"][1],
    home_score = team_score[team_home_away == "home"][1],
    away_score = team_score[team_home_away == "away"][1],
    conference_game = all(team_id %in% meta$teams),
    .groups = "drop"
  ) |>
  left_join(
    pbp |> group_by(game_id) |> summarise(periods = max(period_number), events = n()),
    by = "game_id"
  ) |>
  mutate(
    overtimes = pmax(periods - 2, 0),
    matchup = sprintf("%s %d, %s %d", away, away_score, home, home_score)
  )

teams <- team_box |> distinct(team_id, team = team_location)

# ---- Play-by-play totals ----------------------------------------------------
# One row per game / team / stat, counted from the events.

classify <- function(d) {
  d |> mutate(
    is_shot = type_text %in% SHOTS,
    is_ft = type_text == "MadeFreeThrow", # ESPN uses this type for makes and misses
    made = scoring_play
  )
}

count_stats <- function(d) {
  d |> summarise(
    pts = sum(score_value[made]),
    fgm = sum(is_shot & made),
    fga = sum(is_shot),
    tpm = sum(is_shot & made & score_value == 3),
    tpa = sum(is_shot & score_value == 3),
    ftm = sum(is_ft & made),
    fta = sum(is_ft),
    orb = sum(type_text == "Offensive Rebound"),
    drb = sum(type_text == "Defensive Rebound"),
    stl = sum(type_text == "Steal"),
    blk = sum(type_text == "Block Shot"),
    tov = sum(type_text == "Lost Ball Turnover"),
    # box score fouls include technicals, so the play-by-play count must too
    pf = sum(type_text %in% c("PersonalFoul", "Technical Foul")),
    .groups = "drop"
  )
}

ev <- classify(pbp)

# assists are credited to the passer, who is the second athlete on a made shot
assists_team <- ev |>
  filter(is_shot, made, grepl("assist", text, ignore.case = TRUE)) |>
  count(game_id, team_id, name = "ast")
assists_player <- ev |>
  filter(is_shot, made, !is.na(athlete_id_2)) |>
  count(game_id, team_id, athlete_id = athlete_id_2, name = "ast")

pbp_team <- ev |>
  filter(!is.na(team_id)) |>
  group_by(game_id, team_id) |>
  count_stats() |>
  left_join(assists_team, by = c("game_id", "team_id")) |>
  mutate(ast = coalesce(ast, 0L)) |>
  pivot_longer(-c(game_id, team_id), names_to = "stat", values_to = "pbp")

pbp_player <- ev |>
  filter(!is.na(team_id), !is.na(athlete_id_1)) |>
  group_by(game_id, team_id, athlete_id = athlete_id_1) |>
  count_stats() |>
  full_join(assists_player, by = c("game_id", "team_id", "athlete_id")) |>
  mutate(across(-c(game_id, team_id, athlete_id), ~ coalesce(as.numeric(.x), 0))) |>
  pivot_longer(-c(game_id, team_id, athlete_id), names_to = "stat", values_to = "pbp")

# ---- Box score totals -------------------------------------------------------

box_team_wide <- team_box |>
  transmute(
    game_id, team_id,
    pts = team_score, fgm = field_goals_made, fga = field_goals_attempted,
    tpm = three_point_field_goals_made, tpa = three_point_field_goals_attempted,
    ftm = free_throws_made, fta = free_throws_attempted,
    orb = offensive_rebounds, drb = defensive_rebounds, trb = total_rebounds,
    ast = assists, stl = steals, blk = blocks,
    tov = total_turnovers, team_tov = team_turnovers, pf = fouls,
    opp_id = opponent_team_id, opp_pts = opponent_team_score
  ) |>
  mutate(across(-c(game_id, team_id, opp_id), as.numeric))

box_team <- box_team_wide |>
  select(game_id, team_id, pts:drb, ast:tov, pf) |>
  pivot_longer(-c(game_id, team_id), names_to = "stat", values_to = "box")

played <- player_box |> filter(!did_not_play)

box_player_wide <- played |>
  transmute(
    game_id, team_id, athlete_id,
    player = athlete_display_name,
    pts = points, fgm = field_goals_made, fga = field_goals_attempted,
    tpm = three_point_field_goals_made, tpa = three_point_field_goals_attempted,
    ftm = free_throws_made, fta = free_throws_attempted,
    orb = offensive_rebounds, drb = defensive_rebounds,
    ast = assists, stl = steals, blk = blocks, tov = turnovers, pf = fouls,
    min = minutes
  ) |>
  mutate(across(pts:min, as.numeric))

box_player <- box_player_wide |>
  select(-min) |>
  pivot_longer(pts:pf, names_to = "stat", values_to = "box")

# ---- Check 1: play-by-play vs team box --------------------------------------

chk_team <- box_team |>
  full_join(pbp_team, by = c("game_id", "team_id", "stat")) |>
  mutate(check = "Play-by-play vs team box", player = NA_character_)

# ---- Check 2: play-by-play vs player box ------------------------------------
# Rebounds and turnovers with no athlete are team stats, so they are excluded
# here and covered by check 1.

chk_player <- box_player |>
  full_join(pbp_player, by = c("game_id", "team_id", "athlete_id", "stat")) |>
  group_by(game_id, team_id, athlete_id) |>
  mutate(player = first(na.omit(player))) |>
  ungroup() |>
  mutate(
    in_box = !is.na(box),
    box = coalesce(box, 0), pbp = coalesce(pbp, 0),
    player = coalesce(player, paste("Unknown athlete", athlete_id)),
    check = "Play-by-play vs player box"
  )

# ---- Check 3: player box sums vs team box -----------------------------------

player_sums <- box_player_wide |>
  group_by(game_id, team_id) |>
  summarise(across(pts:min, ~ sum(.x, na.rm = TRUE)), .groups = "drop")

chk_sum <- box_team_wide |>
  # individual turnovers exclude team turnovers; individual rebounds exclude
  # team rebounds, so rebounds are not compared here
  mutate(tov = tov - team_tov) |>
  select(game_id, team_id, pts, fgm, fga, tpm, tpa, ftm, fta, ast, stl, blk, tov, pf) |>
  pivot_longer(-c(game_id, team_id), names_to = "stat", values_to = "box") |>
  inner_join(
    player_sums |>
      select(-min, -orb, -drb) |>
      pivot_longer(-c(game_id, team_id), names_to = "stat", values_to = "pbp"),
    by = c("game_id", "team_id", "stat")
  ) |>
  mutate(check = "Player box sum vs team box", player = NA_character_)

# ---- Check 4: minutes -------------------------------------------------------
# Five players on the floor: 200 minutes in regulation plus 25 per overtime.

chk_min <- player_sums |>
  select(game_id, team_id, pbp = min) |>
  inner_join(games |> select(game_id, overtimes), by = "game_id") |>
  mutate(
    box = 200 + 25 * overtimes, stat = "min",
    check = "Player minutes vs game length", player = NA_character_
  ) |>
  select(-overtimes)

# ---- Check 5: box score arithmetic ------------------------------------------

chk_arith <- bind_rows(
  box_team_wide |> transmute(game_id, team_id, stat = "pts", box = pts,
    pbp = 2 * (fgm - tpm) + 3 * tpm + ftm, rule = "Points = 2s + 3s + free throws"),
  box_team_wide |> transmute(game_id, team_id, stat = "orb", box = trb,
    pbp = orb + drb, rule = "Total rebounds = offensive + defensive"),
  box_team_wide |>
    inner_join(box_team_wide |> select(game_id, opp_id = team_id, other_pts = pts),
      by = c("game_id", "opp_id")) |>
    transmute(game_id, team_id, stat = "pts", box = opp_pts, pbp = other_pts,
      rule = "Opponent score matches the opponent's own row")
) |>
  mutate(check = "Box score arithmetic", player = NA_character_)

# ---- Check 6: event integrity -----------------------------------------------

ordered <- pbp |>
  arrange(game_id, sequence_number) |>
  group_by(game_id) |>
  mutate(
    secs = clock_minutes * 60 + clock_seconds,
    clock_back = period_number == lag(period_number) & secs > lag(secs),
    period_back = period_number < lag(period_number)
  ) |>
  ungroup()

integrity <- games |>
  select(game_id) |>
  left_join(
    ordered |> group_by(game_id) |> summarise(
      out_of_order = sum(clock_back | period_back, na.rm = TRUE),
      has_end_event = any(type_text == "End Game"),
      .groups = "drop"
    ),
    by = "game_id"
  ) |>
  left_join(
    ev |> filter(is_shot | is_ft, is.na(athlete_id_1)) |> count(game_id, name = "shots_no_shooter"),
    by = "game_id"
  ) |>
  left_join(
    ev |> filter(type_text %in% c("Steal", "Block Shot", "PersonalFoul"), is.na(athlete_id_1)) |>
      count(game_id, name = "events_no_player"),
    by = "game_id"
  ) |>
  mutate(
    no_pbp = is.na(has_end_event),
    across(c(out_of_order, shots_no_shooter, events_no_player), ~ coalesce(.x, 0L)),
    has_end_event = coalesce(has_end_event, FALSE)
  )

# ---- Combine ----------------------------------------------------------------

all_checks <- bind_rows(
  chk_team, chk_player |> select(-in_box, -athlete_id), chk_sum, chk_min, chk_arith
) |>
  mutate(box = coalesce(box, 0), pbp = coalesce(pbp, 0), diff = pbp - box)

# Explain the patterns that have a known cause; leave the rest for a person.
explain <- function(check, stat, diff, rule) {
  case_when(
    check == "Box score arithmetic" ~ rule,
    check == "Player minutes vs game length" ~ "Player minutes do not add up to the game length",
    check == "Player box sum vs team box" & stat == "pf" & diff == -1 ~
      "Team line has a foul charged to no player (typically a bench technical)",
    check == "Player box sum vs team box" ~ "Player lines do not add up to the team line",
    stat == "ast" & diff < 0 ~ "Assist in the box score has no matching play-by-play event",
    stat == "ast" & diff > 0 ~ "Play-by-play credits an assist the box score does not",
    stat %in% c("fga", "tpa", "fta") & diff < 0 ~ "Missed shot missing from the play-by-play",
    stat %in% c("orb", "drb") ~ "Rebound logged differently in the two sources",
    diff < 0 ~ "Event in the box score is missing from the play-by-play",
    TRUE ~ "Play-by-play has an event the box score does not"
  )
}

discrepancies <- all_checks |>
  filter(diff != 0) |>
  left_join(teams, by = "team_id") |>
  left_join(games |> select(game_id, game_date, matchup, home, conference_game), by = "game_id") |>
  mutate(
    stat_label = STAT_LABELS[stat],
    likely_cause = explain(check, stat, diff, rule)
  ) |>
  # In the checks that do not involve play-by-play, the compared value sits in
  # the pbp column; name the columns for what they hold.
  transmute(
    game_id, game_date, matchup, home_team = home, conference_game, check, team, player,
    stat = stat_label, expected = box, found = pbp, diff, likely_cause
  ) |>
  arrange(game_date, game_id, check, team, player)

check_summary <- all_checks |>
  group_by(check) |>
  summarise(
    comparisons = n(), mismatches = sum(diff != 0),
    match_rate = 1 - mismatches / comparisons,
    games_affected = n_distinct(game_id[diff != 0]), .groups = "drop"
  )

stat_summary <- all_checks |>
  filter(check == "Play-by-play vs team box") |>
  group_by(stat) |>
  summarise(
    comparisons = n(), mismatches = sum(diff != 0),
    match_rate = 1 - mismatches / comparisons,
    total_abs_diff = sum(abs(diff)), box_total = sum(box), pbp_total = sum(pbp),
    .groups = "drop"
  ) |>
  mutate(stat = STAT_LABELS[stat]) |>
  arrange(match_rate)

game_summary <- games |>
  left_join(
    discrepancies |> count(game_id, check) |>
      pivot_wider(names_from = check, values_from = n, values_fill = 0),
    by = "game_id"
  ) |>
  left_join(discrepancies |> count(game_id, name = "mismatches"), by = "game_id") |>
  left_join(integrity, by = "game_id") |>
  mutate(across(where(is.numeric), ~ coalesce(.x, 0)), clean = mismatches == 0) |>
  arrange(desc(mismatches))

# ---- Assists: where the gaps are --------------------------------------------

assist_rows <- all_checks |>
  filter(check == "Play-by-play vs team box", stat == "ast") |>
  left_join(games |> select(game_id, home), by = "game_id")

# by host arena, for A-10 hosts (two team-games per game hosted)
assist_by_host <- assist_rows |>
  group_by(host = home) |>
  summarise(
    games_hosted = n_distinct(game_id), team_games = n(),
    team_games_off = sum(diff != 0), share_off = team_games_off / team_games,
    assists_missing = sum(box - pbp), .groups = "drop"
  ) |>
  filter(host %in% names(meta$teams)) |>
  arrange(desc(share_off))

# by team: assists per game from each source, and the conference rank each gives
assist_by_team <- assist_rows |>
  filter(team_id %in% meta$teams) |>
  left_join(teams, by = "team_id") |>
  group_by(team) |>
  summarise(games = n(), box = sum(box), pbp = sum(pbp), .groups = "drop") |>
  mutate(
    box_per_game = box / games, pbp_per_game = pbp / games,
    missing = box - pbp, pct_missing = missing / box,
    rank_box = min_rank(desc(box_per_game)), rank_pbp = min_rank(desc(pbp_per_game))
  ) |>
  arrange(rank_box)

# ---- Write ------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
write_csv(discrepancies, "output/discrepancies.csv")
write_csv(check_summary, "output/check_summary.csv")
write_csv(stat_summary, "output/stat_summary.csv")
write_csv(game_summary, "output/game_summary.csv")
saveRDS(
  list(
    meta = meta, games = games, discrepancies = discrepancies,
    check_summary = check_summary, stat_summary = stat_summary,
    game_summary = game_summary, integrity = integrity, all_checks = all_checks,
    assist_by_host = assist_by_host, assist_by_team = assist_by_team
  ),
  "output/audit.rds"
)

cat(sprintf(
  "%d games, %d comparisons, %d mismatches, %d games fully clean\n",
  nrow(games), nrow(all_checks), nrow(discrepancies), sum(game_summary$clean)
))
print(as.data.frame(check_summary))
print(as.data.frame(stat_summary))
cat(sprintf(
  "Integrity: %d games with out-of-order sequence numbers (%d events), %d shots with no shooter, %d games with no play-by-play\n",
  sum(integrity$out_of_order > 0), sum(integrity$out_of_order),
  sum(integrity$shots_no_shooter), sum(integrity$no_pbp)
))
