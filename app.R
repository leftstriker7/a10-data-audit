# app.R
# Browse the audit results: which games reconcile, which do not, and why.
#
# Run from the project root:  Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
# (run scripts/01_pull_data.R and scripts/02_run_checks.R first)

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

a <- readRDS("output/2026/audit.rds")
d <- a$discrepancies
gs <- a$game_summary
cs <- a$check_summary
ss <- a$stat_summary

ACCENT <- "#1f5f8b"
WARN <- "#c8553d"
pct <- function(x, digits = 1) paste0(formatC(100 * x, format = "f", digits = digits), "%")
num <- function(x) format(x, big.mark = ",")

all_teams <- sort(unique(d$team))
a10 <- sort(names(a$meta$teams))

game_table <- gs |>
  transmute(
    game_id, Date = game_date, Game = matchup,
    `A-10 game` = ifelse(conference_game, "Yes", "No"),
    Mismatches = mismatches,
    `Out-of-order events` = out_of_order,
    `Shots with no shooter` = shots_no_shooter
  )

tile <- function(title, value, note) {
  value_box(title = title, value = value, p(note), theme = "light")
}

ui <- page_navbar(
  title = "Atlantic 10 data audit, 2025-26",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = ACCENT),
  fillable = FALSE,
  nav_panel(
    "Overview",
    layout_columns(
      tile("Games audited", num(nrow(gs)), paste(num(sum(gs$events)), "play-by-play events")),
      tile("Comparisons", num(sum(cs$comparisons)), "play-by-play, team box and player box"),
      tile("Mismatches", num(nrow(d)), paste(pct(nrow(d) / sum(cs$comparisons), 2), "of comparisons")),
      tile("Fully clean games", num(sum(gs$clean)), paste("of", num(nrow(gs))))
    ),
    layout_columns(
      card(card_header("Play-by-play vs team box: match rate by stat"), plotOutput("stat_plot", height = 380)),
      card(card_header("Assist mismatches by host arena"), plotOutput("host_plot", height = 380))
    ),
    card(card_header("Results by check"), tableOutput("check_table"))
  ),
  nav_panel(
    "Games",
    p("Select a game to see every mismatch in it."),
    card(DTOutput("games")),
    card(card_header(textOutput("game_title")), DTOutput("game_detail"))
  ),
  nav_panel(
    "All mismatches",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      selectInput("f_check", "Check", c("All", cs$check)),
      selectInput("f_stat", "Stat", c("All", sort(unique(d$stat)))),
      selectInput("f_team", "Team", c("All", all_teams)),
      selectInput("f_scope", "Games", c("All games", "A-10 games only"))
    ),
    downloadButton("download", "Download as CSV", class = "btn-primary mb-3"),
    card(DTOutput("mismatches"))
  ),
  nav_panel(
    "Assists",
    p(
      "Assists are the least reliable stat in the play-by-play. These tables show",
      "where the gaps are and what they do to a team's numbers."
    ),
    layout_columns(
      card(card_header("By host arena"), tableOutput("host_table")),
      card(card_header("Assists per game: box score vs play-by-play"), tableOutput("team_table"))
    )
  )
)

server <- function(input, output, session) {
  output$stat_plot <- renderPlot({
    ss |>
      mutate(stat = reorder(stat, -match_rate), flag = stat == "Assists") |>
      ggplot(aes(match_rate, stat, fill = flag)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = pct(match_rate)), hjust = -0.15, size = 4.2) +
      scale_fill_manual(values = c(`TRUE` = WARN, `FALSE` = ACCENT), guide = "none") +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1.1), breaks = seq(0, 1, .25)) +
      labs(x = "Share of team-games that match", y = NULL) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
  })

  output$host_plot <- renderPlot({
    a$assist_by_host |>
      mutate(host = reorder(host, share_off)) |>
      ggplot(aes(share_off, host)) +
      geom_col(width = 0.7, fill = ACCENT) +
      geom_text(aes(label = pct(share_off, 0)), hjust = -0.15, size = 4.2) +
      scale_x_continuous(labels = scales::percent, limits = c(0, max(a$assist_by_host$share_off) * 1.15)) +
      labs(x = "Share of team-games with an assist mismatch", y = NULL) +
      theme_minimal(base_size = 15) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
  })

  output$check_table <- renderTable(
    cs |> transmute(
      Check = check, Comparisons = num(comparisons), Mismatches = num(mismatches),
      `Match rate` = pct(match_rate, 2), `Games affected` = games_affected
    ),
    align = "lrrrr", width = "100%"
  )

  output$games <- renderDT(
    datatable(
      game_table |> select(-game_id),
      selection = list(mode = "single", selected = 1),
      rownames = FALSE, filter = "top",
      options = list(pageLength = 10, order = list(list(3, "desc")))
    )
  )

  selected_game <- reactive({
    i <- input$games_rows_selected
    if (length(i) == 0) NULL else game_table[i, ]
  })

  output$game_title <- renderText({
    g <- selected_game()
    if (is.null(g)) "No game selected" else sprintf("%s (%s): %d mismatches", g$Game, g$Date, g$Mismatches)
  })

  output$game_detail <- renderDT({
    g <- selected_game()
    req(g)
    datatable(
      d |>
        filter(game_id == g$game_id) |>
        transmute(
          Check = check, Team = team, Player = coalesce(player, "Team total"), Stat = stat,
          Expected = expected, Found = found, Difference = diff, `Likely cause` = likely_cause
        ),
      rownames = FALSE, options = list(pageLength = 15, dom = "tp")
    )
  })

  filtered <- reactive({
    x <- d
    if (input$f_check != "All") x <- x |> filter(check == input$f_check)
    if (input$f_stat != "All") x <- x |> filter(stat == input$f_stat)
    if (input$f_team != "All") x <- x |> filter(team == input$f_team)
    if (input$f_scope == "A-10 games only") x <- x |> filter(conference_game)
    x
  })

  output$mismatches <- renderDT(
    datatable(
      filtered() |> transmute(
        Date = game_date, Game = matchup, Check = check, Team = team,
        Player = coalesce(player, "Team total"), Stat = stat,
        Expected = expected, Found = found, Difference = diff, `Likely cause` = likely_cause
      ),
      rownames = FALSE, options = list(pageLength = 25)
    )
  )

  output$download <- downloadHandler(
    filename = "a10_mismatches.csv",
    content = function(file) readr::write_csv(filtered(), file)
  )

  output$host_table <- renderTable(
    a$assist_by_host |> transmute(
      Host = host, `Games hosted` = games_hosted,
      `Team-games with a mismatch` = sprintf("%d of %d", team_games_off, team_games),
      Share = pct(share_off, 0), `Net assists missing` = assists_missing
    ),
    align = "lrrrr", digits = 0, width = "100%"
  )

  output$team_table <- renderTable(
    a$assist_by_team |> transmute(
      Team = team, Games = games,
      `Box score` = formatC(box_per_game, format = "f", digits = 1),
      `Play-by-play` = formatC(pbp_per_game, format = "f", digits = 1),
      `Missing` = pct(pct_missing),
      `Rank (box)` = rank_box, `Rank (play-by-play)` = rank_pbp
    ),
    align = "lrrrrrr", digits = 0, width = "100%"
  )
}

shinyApp(ui, server)
