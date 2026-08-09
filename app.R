
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# ============================================================
# FOOTBALL RANKING SHINY APP
# ============================================================

PLAYERS_FILE <- "players.csv"
MATCHES_FILE <- "matches.csv"

# ----------------------------
# Rating rules
# ----------------------------

participation_bonus <- 11

stage_rules <- data.frame(
  stage = c("Group Stage", "Round of 16", "Quarterfinal", "Semifinal", "Final"),
  win_base = c(10, 12, 14, 16, 20),
  loss_base = c(10, 9, 8, 7, 6),
  stringsAsFactors = FALSE
)

gap_adjustment <- function(gap) {
  if (gap <= 20) return(0)
  if (gap <= 40) return(0.30)
  if (gap <= 60) return(0.50)
  if (gap <= 80) return(0.70)
  if (gap <= 100) return(1.00)
  return(1.50)
}

draw_bonus <- function(gap) {
  if (gap <= 20) return(0)
  if (gap <= 40) return(1)
  if (gap <= 60) return(2)
  if (gap <= 80) return(3)
  if (gap <= 100) return(4)
  return(5)
}

# ----------------------------
# Initial player database
# These are the current values after the first recorded cup.
# ----------------------------

default_players <- tibble::tribble(
  ~player, ~base_rating, ~base_matches, ~base_wins, ~base_losses, ~base_gf, ~base_ga,
  "Naser", 373, 4, 4, 0, 29, 17,
  "Tanha", 347, 4, 3, 1, 34, 25,
  "Rezgar", 330, 3, 2, 1, 16, 11,
  "Abolfazl", 318, 2, 1, 1, 11, 17,
  "Sepehri", 303, 1, 0, 1, 6, 7,
  "Sia", 303, 1, 0, 1, 3, 6,
  "Tohid", 303, 1, 0, 1, 8, 12,
  "Firoozi", 303, 1, 0, 1, 3, 7,
  "Ali Af", 302, 1, 0, 1, 2, 4,
  "Ghajar", 302, 1, 0, 1, 3, 6,
  "Mortazavi", 302, 1, 0, 1, 4, 7,
  "Vahid", 300, 0, 0, 0, 0, 0,
  "Ahmad", 300, 0, 0, 0, 0, 0,
  "Mori", 300, 0, 0, 0, 0, 0,
  "M Fadaei", 300, 0, 0, 0, 0, 0,
  "Mhdi", 300, 0, 0, 0, 0, 0,
  "A Aa", 300, 0, 0, 0, 0, 0,
  "Reza Shokri", 300, 0, 0, 0, 0, 0,
  "Mojtaba Elyasi", 300, 0, 0, 0, 0, 0,
  "Seyed Behdad Arshahi", 300, 0, 0, 0, 0, 0,
  "Mehdi Mameshli", 300, 0, 0, 0, 0, 0
)

empty_matches <- tibble(
  match_id = integer(),
  date = as.Date(character()),
  cup = character(),
  stage = character(),
  player_a = character(),
  goals_a = integer(),
  player_b = character(),
  goals_b = integer(),
  rating_a_before = numeric(),
  rating_b_before = numeric(),
  participation_a = numeric(),
  participation_b = numeric(),
  rating_change_a = numeric(),
  rating_change_b = numeric(),
  rating_a_after = numeric(),
  rating_b_after = numeric()
)

# ----------------------------
# File helpers
# ----------------------------

ensure_files <- function() {
  if (!file.exists(PLAYERS_FILE)) {
    write_csv(default_players, PLAYERS_FILE)
  }
  if (!file.exists(MATCHES_FILE)) {
    write_csv(empty_matches, MATCHES_FILE)
  }
}

load_players <- function() {
  read_csv(PLAYERS_FILE, show_col_types = FALSE)
}

load_matches <- function() {
  x <- read_csv(MATCHES_FILE, show_col_types = FALSE)
  if (nrow(x) > 0) x$date <- as.Date(x$date)
  x
}

save_matches <- function(x) {
  write_csv(x, MATCHES_FILE)
}

ensure_files()

# ----------------------------
# Core calculations
# ----------------------------

player_rating_before_new_match <- function(player_name, players, matches) {
  base <- players %>%
    filter(player == player_name) %>%
    pull(base_rating)

  if (length(base) == 0) return(300)

  deltas <- matches %>%
    filter(player_a == player_name | player_b == player_name) %>%
    mutate(
      delta = ifelse(player_a == player_name, rating_change_a, rating_change_b),
      participation = ifelse(player_a == player_name, participation_a, participation_b)
    )

  base + sum(deltas$delta, na.rm = TRUE) + sum(deltas$participation, na.rm = TRUE)
}

already_joined_cup <- function(player_name, cup_name, matches) {
  any(
    matches$cup == cup_name &
      (matches$player_a == player_name | matches$player_b == player_name)
  )
}

calculate_match <- function(player_a, goals_a, player_b, goals_b, stage, cup, players, matches) {

  rating_a_base <- player_rating_before_new_match(player_a, players, matches)
  rating_b_base <- player_rating_before_new_match(player_b, players, matches)

  participation_a <- ifelse(already_joined_cup(player_a, cup, matches), 0, participation_bonus)
  participation_b <- ifelse(already_joined_cup(player_b, cup, matches), 0, participation_bonus)

  rating_a_before <- rating_a_base + participation_a
  rating_b_before <- rating_b_base + participation_b

  gap <- abs(rating_a_before - rating_b_before)
  adj <- gap_adjustment(gap)

  rule <- stage_rules %>% filter(stage == !!stage)
  win_base <- rule$win_base[1]
  loss_base <- rule$loss_base[1]

  if (goals_a > goals_b) {
    # A wins
    if (rating_a_before <= rating_b_before) {
      change_a <- round(win_base * (1 + adj))
    } else {
      change_a <- round(win_base / (1 + adj))
    }

    # B loses
    if (rating_b_before >= rating_a_before) {
      change_b <- -round(loss_base * (1 + adj))
    } else {
      change_b <- -round(loss_base / (1 + adj))
    }

  } else if (goals_b > goals_a) {
    # B wins
    if (rating_b_before <= rating_a_before) {
      change_b <- round(win_base * (1 + adj))
    } else {
      change_b <- round(win_base / (1 + adj))
    }

    # A loses
    if (rating_a_before >= rating_b_before) {
      change_a <- -round(loss_base * (1 + adj))
    } else {
      change_a <- -round(loss_base / (1 + adj))
    }

  } else {
    # Draw
    pts <- draw_bonus(gap)

    if (rating_a_before == rating_b_before) {
      change_a <- 0
      change_b <- 0
    } else if (rating_a_before < rating_b_before) {
      change_a <- pts
      change_b <- -pts
    } else {
      change_a <- -pts
      change_b <- pts
    }
  }

  tibble(
    rating_a_before = rating_a_before,
    rating_b_before = rating_b_before,
    participation_a = participation_a,
    participation_b = participation_b,
    rating_change_a = change_a,
    rating_change_b = change_b,
    rating_a_after = rating_a_before + change_a,
    rating_b_after = rating_b_before + change_b
  )
}

build_ranking <- function(players, matches) {

  current <- lapply(players$player, function(p) {

    pm <- matches %>% filter(player_a == p | player_b == p)

    rating_change <- if (nrow(pm) == 0) 0 else
      sum(ifelse(pm$player_a == p, pm$rating_change_a, pm$rating_change_b), na.rm = TRUE)

    participation <- if (nrow(pm) == 0) 0 else
      sum(ifelse(pm$player_a == p, pm$participation_a, pm$participation_b), na.rm = TRUE)

    wins <- if (nrow(pm) == 0) 0 else
      sum(
        (pm$player_a == p & pm$goals_a > pm$goals_b) |
          (pm$player_b == p & pm$goals_b > pm$goals_a)
      )

    losses <- if (nrow(pm) == 0) 0 else
      sum(
        (pm$player_a == p & pm$goals_a < pm$goals_b) |
          (pm$player_b == p & pm$goals_b < pm$goals_a)
      )

    gf <- if (nrow(pm) == 0) 0 else
      sum(ifelse(pm$player_a == p, pm$goals_a, pm$goals_b))

    ga <- if (nrow(pm) == 0) 0 else
      sum(ifelse(pm$player_a == p, pm$goals_b, pm$goals_a))

    base <- players %>% filter(player == p)

    tibble(
      player = p,
      rating = base$base_rating + rating_change + participation,
      matches = base$base_matches + nrow(pm),
      wins = base$base_wins + wins,
      losses = base$base_losses + losses,
      goals_for = base$base_gf + gf,
      goals_against = base$base_ga + ga
    )
  }) %>% bind_rows()

  current %>%
    mutate(
      goal_difference = goals_for - goals_against,
      win_rate = ifelse(matches == 0, 0, 100 * wins / matches)
    ) %>%
    arrange(desc(rating), desc(wins), desc(goal_difference), player) %>%
    mutate(rank = row_number()) %>%
    select(rank, everything())
}

player_history <- function(player_name, players, matches) {

  base_rating <- players %>%
    filter(player == player_name) %>%
    pull(base_rating)

  pm <- matches %>%
    filter(player_a == player_name | player_b == player_name) %>%
    arrange(match_id)

  history <- tibble(
    game = 0,
    label = "Baseline",
    rating = base_rating
  )

  if (nrow(pm) == 0) return(history)

  current <- base_rating

  for (i in seq_len(nrow(pm))) {
    row <- pm[i, ]

    participation <- ifelse(
      row$player_a == player_name,
      row$participation_a,
      row$participation_b
    )

    delta <- ifelse(
      row$player_a == player_name,
      row$rating_change_a,
      row$rating_change_b
    )

    current <- current + participation + delta

    opponent <- ifelse(
      row$player_a == player_name,
      row$player_b,
      row$player_a
    )

    history <- bind_rows(
      history,
      tibble(
        game = i,
        label = paste0(row$cup, " vs ", opponent),
        rating = current
      )
    )
  }

  history
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#f4f6f8; }
      .navbar { margin-bottom:20px; }
      .cardx {
        background:white; border-radius:12px; padding:18px;
        box-shadow:0 2px 10px rgba(0,0,0,.08); margin-bottom:18px;
      }
      .metric {
        font-size:28px; font-weight:700; margin-top:4px;
      }
      .metric-label {
        color:#6c757d; font-size:13px; text-transform:uppercase;
      }
      .profile-title {
        font-size:30px; font-weight:800;
      }
      .rating-big {
        font-size:42px; font-weight:800;
      }
      .dataTables_wrapper { background:white; padding:12px; border-radius:12px; }
    "))
  ),

  navbarPage(
    title = "Football Rating",
    id = "main_tabs",

    tabPanel(
      "Ranking",
      fluidRow(
        column(
          12,
          div(
            class = "cardx",
            h3("Live Ranking"),
            p("Click a player row to open the full player profile."),
            DTOutput("ranking_table")
          )
        )
      )
    ),

    tabPanel(
      "Add Match",
      fluidRow(
        column(
          5,
          div(
            class = "cardx",
            textInput("cup", "Cup ID", placeholder = "Example: CUP-02"),
            selectInput("stage", "Stage", choices = stage_rules$stage),
            selectInput("player_a", "Player A", choices = NULL),
            numericInput("goals_a", "Goals A", 0, min = 0),
            selectInput("player_b", "Player B", choices = NULL),
            numericInput("goals_b", "Goals B", 0, min = 0),
            actionButton("add_match", "Save Match", class = "btn-primary")
          )
        ),
        column(
          7,
          div(
            class = "cardx",
            h3("Latest Matches"),
            DTOutput("matches_table")
          )
        )
      )
    ),

    tabPanel(
      "Player Profile",
      fluidRow(
        column(
          12,
          div(
            class = "cardx",
            selectInput("profile_player", "Player", choices = NULL),
            uiOutput("profile_header")
          )
        )
      ),

      fluidRow(
        column(2, div(class="cardx", div(class="metric-label","Current Rating"), div(class="metric", textOutput("m_rating")))),
        column(2, div(class="cardx", div(class="metric-label","Rank"), div(class="metric", textOutput("m_rank")))),
        column(2, div(class="cardx", div(class="metric-label","Matches"), div(class="metric", textOutput("m_matches")))),
        column(2, div(class="cardx", div(class="metric-label","Wins"), div(class="metric", textOutput("m_wins")))),
        column(2, div(class="cardx", div(class="metric-label","Win Rate"), div(class="metric", textOutput("m_winrate")))),
        column(2, div(class="cardx", div(class="metric-label","Goal Diff"), div(class="metric", textOutput("m_gd"))))
      ),

      fluidRow(
        column(
          8,
          div(
            class = "cardx",
            h3("Rating Progress"),
            plotOutput("rating_chart", height = "380px")
          )
        ),
        column(
          4,
          div(
            class = "cardx",
            h3("Performance"),
            plotOutput("performance_chart", height = "380px")
          )
        )
      ),

      fluidRow(
        column(
          12,
          div(
            class = "cardx",
            h3("Match History"),
            DTOutput("player_matches")
          )
        )
      )
    ),

    tabPanel(
      "Rules",
      fluidRow(
        column(
          7,
          div(
            class = "cardx",
            h3("Rating Rules"),
            tags$ul(
              tags$li("+11 participation bonus once per player per Cup ID."),
              tags$li("Higher stages give more points for a win."),
              tags$li("Higher stages have a lower base loss penalty."),
              tags$li("Beating a stronger player gives more rating."),
              tags$li("Losing to a stronger player costs less rating."),
              tags$li("Beating a weaker player gives less rating."),
              tags$li("Losing to a weaker player costs more rating.")
            )
          )
        ),
        column(
          5,
          div(
            class = "cardx",
            h3("Stage Values"),
            tableOutput("rules_table")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  players <- reactiveVal(load_players())
  matches <- reactiveVal(load_matches())

  ranking <- reactive({
    build_ranking(players(), matches())
  })

  observe({
    p <- players()$player
    updateSelectInput(session, "player_a", choices = p)
    updateSelectInput(session, "player_b", choices = p, selected = if (length(p) > 1) p[2] else p[1])
    updateSelectInput(session, "profile_player", choices = p)
  })

  output$ranking_table <- renderDT({
    datatable(
      ranking() %>%
        mutate(win_rate = paste0(round(win_rate, 1), "%")),
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = "tip",
        order = list(list(0, "asc"))
      )
    )
  })

  # Clicking a row in Ranking opens that player's profile automatically.
  observeEvent(input$ranking_table_rows_selected, {
    idx <- input$ranking_table_rows_selected
    if (length(idx) == 1) {
      selected_player <- ranking()$player[idx]
      updateSelectInput(session, "profile_player", selected = selected_player)
      updateTabsetPanel(session, "main_tabs", selected = "Player Profile")
    }
  })

  observeEvent(input$add_match, {

    req(input$cup, input$stage, input$player_a, input$player_b)

    if (trimws(input$cup) == "") {
      showNotification("Cup ID is required.", type = "error")
      return()
    }

    if (input$player_a == input$player_b) {
      showNotification("Player A and Player B must be different.", type = "error")
      return()
    }

    m <- matches()
    p <- players()

    calc <- calculate_match(
      player_a = input$player_a,
      goals_a = input$goals_a,
      player_b = input$player_b,
      goals_b = input$goals_b,
      stage = input$stage,
      cup = trimws(input$cup),
      players = p,
      matches = m
    )

    new_id <- ifelse(nrow(m) == 0, 1, max(m$match_id, na.rm = TRUE) + 1)

    new_match <- tibble(
      match_id = new_id,
      date = Sys.Date(),
      cup = trimws(input$cup),
      stage = input$stage,
      player_a = input$player_a,
      goals_a = as.integer(input$goals_a),
      player_b = input$player_b,
      goals_b = as.integer(input$goals_b),
      rating_a_before = calc$rating_a_before,
      rating_b_before = calc$rating_b_before,
      participation_a = calc$participation_a,
      participation_b = calc$participation_b,
      rating_change_a = calc$rating_change_a,
      rating_change_b = calc$rating_change_b,
      rating_a_after = calc$rating_a_after,
      rating_b_after = calc$rating_b_after
    )

    updated <- bind_rows(m, new_match)
    save_matches(updated)
    matches(updated)

    showNotification("Match saved and ranking updated.", type = "message")
  })

  output$matches_table <- renderDT({
    m <- matches()

    if (nrow(m) == 0) return(datatable(data.frame(Message = "No new matches yet."), rownames = FALSE))

    datatable(
      m %>%
        arrange(desc(match_id)) %>%
        transmute(
          Date = date,
          Cup = cup,
          Stage = stage,
          `Player A` = player_a,
          Score = paste(goals_a, "-", goals_b),
          `Player B` = player_b,
          `A Change` = paste0(ifelse(rating_change_a > 0, "+", ""), rating_change_a),
          `B Change` = paste0(ifelse(rating_change_b > 0, "+", ""), rating_change_b)
        ),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip")
    )
  })

  selected_stats <- reactive({
    req(input$profile_player)
    ranking() %>% filter(player == input$profile_player)
  })

  output$profile_header <- renderUI({
    s <- selected_stats()

    div(
      class = "profile-title",
      s$player,
      tags$span(
        style = "font-size:16px;color:#6c757d;margin-left:14px;",
        paste0("#", s$rank, " • Rating ", s$rating)
      )
    )
  })

  output$m_rating <- renderText(selected_stats()$rating)
  output$m_rank <- renderText(paste0("#", selected_stats()$rank))
  output$m_matches <- renderText(selected_stats()$matches)
  output$m_wins <- renderText(selected_stats()$wins)
  output$m_winrate <- renderText(paste0(round(selected_stats()$win_rate, 1), "%"))
  output$m_gd <- renderText({
    x <- selected_stats()$goal_difference
    ifelse(x > 0, paste0("+", x), x)
  })

  output$rating_chart <- renderPlot({
    req(input$profile_player)

    h <- player_history(input$profile_player, players(), matches())

    ggplot(h, aes(x = game, y = rating)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.8) +
      geom_hline(
        yintercept = players() %>% filter(player == input$profile_player) %>% pull(base_rating),
        linetype = 2,
        alpha = .45
      ) +
      scale_x_continuous(breaks = h$game) +
      labs(
        x = "Match",
        y = "Rating",
        title = paste(input$profile_player, "Rating History")
      ) +
      theme_minimal(base_size = 13)
  })

  output$performance_chart <- renderPlot({
    s <- selected_stats()

    d <- tibble(
      result = c("Wins", "Losses", "Other"),
      count = c(
        s$wins,
        s$losses,
        max(0, s$matches - s$wins - s$losses)
      )
    )

    ggplot(d, aes(x = result, y = count)) +
      geom_col(width = .65) +
      geom_text(aes(label = count), vjust = -0.35, size = 5) +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = NULL, y = "Matches") +
      theme_minimal(base_size = 13)
  })

  output$player_matches <- renderDT({
    req(input$profile_player)

    p <- input$profile_player

    m <- matches() %>%
      filter(player_a == p | player_b == p) %>%
      arrange(desc(match_id))

    if (nrow(m) == 0) {
      return(datatable(data.frame(Message = "No new match history after baseline."), rownames = FALSE))
    }

    history <- m %>%
      mutate(
        Opponent = ifelse(player_a == p, player_b, player_a),
        GF = ifelse(player_a == p, goals_a, goals_b),
        GA = ifelse(player_a == p, goals_b, goals_a),
        Result = case_when(
          GF > GA ~ "Win",
          GF < GA ~ "Loss",
          TRUE ~ "Draw"
        ),
        `Rating Change` = ifelse(player_a == p, rating_change_a, rating_change_b),
        `Rating After` = ifelse(player_a == p, rating_a_after, rating_b_after)
      ) %>%
      transmute(
        Date = date,
        Cup = cup,
        Stage = stage,
        Opponent,
        Score = paste(GF, "-", GA),
        Result,
        `Rating Change` = ifelse(`Rating Change` > 0,
                                 paste0("+", `Rating Change`),
                                 as.character(`Rating Change`)),
        `Rating After`
      )

    datatable(
      history,
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tip")
    )
  })

  output$rules_table <- renderTable({
    stage_rules
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
