# Atlantic 10 data audit, 2025-26

Every college basketball game produces three records of the same events: the
play-by-play log, the team box score and the player box score. This project
counts each one against the others for every game an Atlantic 10 team played in
2025-26 and lists every place they disagree. The season is a parameter (2026 =
2025-26, the default; 2025 = 2024-25).

**Result:** 335 games, 115,846 comparisons, 660 mismatches. Scoring reconciles
exactly. Assists do not: play-by-play assists match the box score in 79% of
team-games. The gap is uneven across host arenas, but a second season
(2024-25) shows the pattern does not repeat from year to year, so it is not a
fixed arena habit. The full write-up is at
<https://leftstriker7.github.io/a10-data-audit/>.

## What is here

| File | What it does |
| --- | --- |
| `scripts/01_pull_data.R` | Downloads public ESPN data with `hoopR` and caches it in `data/<season>/` |
| `scripts/02_run_checks.R` | Runs the checks and writes the results to `output/<season>/` |
| `scripts/03_compare_seasons.R` | Tests whether the assist gap by arena repeats across seasons |
| `report.Rmd` | One-page write-up of the findings |
| `app.R` | Shiny app for browsing mismatches by game, team, stat and check |
| `output/2026/discrepancies.csv` | Every mismatch, one row each |

## The checks

1. **Play-by-play vs team box:** 14 stats per team per game.
2. **Play-by-play vs player box:** the same stats per player.
3. **Player box sum vs team box:** player lines must add up to the team line.
4. **Player minutes vs game length:** 200 team minutes plus 25 per overtime.
5. **Box score arithmetic:** points, rebounds and opponent score must be
   internally consistent.
6. **Event order:** events sorted by `sequence_number` must run forward in time.

## Run it

Needs R 4.1 or later and these packages: `hoopR`, `dplyr`, `tidyr`, `readr`,
`ggplot2`, `shiny`, `bslib`, `DT`, `rmarkdown`.

```sh
for season in 2025 2026; do
  Rscript scripts/01_pull_data.R $season
  Rscript scripts/02_run_checks.R $season
done
Rscript scripts/03_compare_seasons.R
Rscript -e 'rmarkdown::render("report.Rmd", output_dir = "output")'
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

Rendering the report needs pandoc. Opening `report.Rmd` in RStudio and clicking
Knit uses RStudio's own copy.

## Limits

This is public ESPN data, so the specific errors may not match another
provider's feed. A mismatch shows that two records disagree, not which one is
right. The scores in this play-by-play were rebuilt by `hoopR`, so the running
score is not tested independently.
