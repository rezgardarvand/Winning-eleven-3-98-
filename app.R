
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(httr2)
library(jsonlite)
library(digest)

# ============================================================
# WEB FOOTBALL RANKING
# Public ranking + admin-only match entry
# Persistent storage: Dropbox CSV
# ============================================================

# ----------------------------
# REQUIRED ENVIRONMENT VARIABLES
# ----------------------------
# ADMIN_PASSWORD_HASH : SHA-256 hash of your admin password
# DROPBOX_TOKEN       : Dropbox API access token
#
# Optional:
# DROPBOX_MATCHES_PATH : default "/football-ranking/matches.csv"

ADMIN_PASSWORD_HASH <- Sys.getenv("ADMIN_PASSWORD_HASH")
DROPBOX_TOKEN <- Sys.getenv("DROPBOX_TOKEN")
DROPBOX_MATCHES_PATH <- Sys.getenv(
  "DROPBOX_MATCHES_PATH",
  unset = "/football-ranking/matches.csv"
)

# Local fallback is useful while testing in RStudio.
LOCAL_MATCHES_FILE <- "matches_local.csv"
PLAYERS_FILE <- "players.csv"

participation_bonus <- 11

stage_rules <- tibble::tribble(
  ~stage,          ~win_base, ~loss_base,
  "Group Stage",          10,         10,
  "Round of 16",          12,          9,
  "Quarterfinal",         14,          8,
  "Semifinal",            16,          7,
  "Final",                20,          6
)

# ============================================================
# HELPERS
# ============================================================

hash_password <- function(password) {
  digest(password, algo = "sha256", serialize = FALSE)
}

is_admin_password_valid <- function(password) {
  if (ADMIN_PASSWORD_HASH == "") return(FALSE)
  identical(hash_password(password), ADMIN_PASSWORD_HASH)
}

gap_adjustment <- function(gap) {
  if (gap <= 20) return(0)
  if (gap <= 40) return(0.30)
  if (gap <= 60) return(0.50)
  if (gap <= 80) return(0.70)
  if (gap <= 100) return(1.00)
  1.50
}

draw_bonus <- function(gap) {
  if (gap <= 20) return(0)
  if (gap <= 40) return(1)
  if (gap <= 60) return(2)
  if (gap <= 80) return(3)
  if (gap <= 100) return(4)
  5
}

empty_matches <- tibble(
  match_id = integer(),
  date = as.Date(character()),
  cup = character(),
  stage = character(),
  player_a = character(),
  goals_a = integer(),
  player_b = character(),
  goals_b = integer(),
  rating_a_before = double(),
  rating_b_before = double(),
  participation_a = double(),
  participation_b = double(),
  rating_change_a = double(),
  rating_change_b = double(),
  rating_a_after = double(),
  rating_b_after = double()
)

normalize_matches <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(empty_matches)

  required <- names(empty_matches)
  for (nm in required) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x <- x[, required]

  x %>%
    mutate(
      match_id = as.integer(match_id),
      date = as.Date(date),
      cup = as.character(cup),
      stage = as.character(stage),
      player_a = as.character(player_a),
      goals_a = as.integer(goals_a),
      player_b = as.character(player_b),
      goals_b = as.integer(goals_b),
      rating_a_before = as.numeric(rating_a_before),
      rating_b_before = as.numeric(rating_b_before),
      participation_a = as.numeric(participation_a),
      participation_b = as.numeric(participation_b),
      rating_change_a = as.numeric(rating_change_a),
      rating_change_b = as.numeric(rating_change_b),
      rating_a_after = as.numeric(rating_a_after),
      rating_b_after = as.numeric(rating_b_after)
    )
}

# ============================================================
# DROPBOX STORAGE
# ============================================================

dropbox_enabled <- function() {
  nzchar(DROPBOX_TOKEN)
}

dropbox_download_matches <- function() {
  if (!dropbox_enabled()) {
    if (!file.exists(LOCAL_MATCHES_FILE)) return(empty_matches)
    return(
      normalize_matches(
        read_csv(LOCAL_MATCHES_FILE, show_col_types = FALSE)
      )
    )
  }

  req <- request("https://content.dropboxapi.com/2/files/download") %>%
    req_headers(
      Authorization = paste("Bearer", DROPBOX_TOKEN),
      `Dropbox-API-Arg` = toJSON(
        list(path = DROPBOX_MATCHES_PATH),
        auto_unbox = TRUE
      )
    )

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  # File does not exist yet -> start empty
  if (is.null(resp) || resp_status(resp) >= 400) {
    return(empty_matches)
  }

  tmp <- tempfile(fileext = ".csv")
  writeBin(resp_body_raw(resp), tmp)

  normalize_matches(
    read_csv(tmp, show_col_types = FALSE)
  )
}

dropbox_upload_matches <- function(x) {
  x <- normalize_matches(x)

  # Always keep a local cache in the app folder while running locally.
  write_csv(x, LOCAL_MATCHES_FILE)

  if (!dropbox_enabled()) return(invisible(TRUE))

  tmp <- tempfile(fileext = ".csv")
  write_csv(x, tmp)

  req <- request("https://content.dropboxapi.com/2/files/upload") %>%
    req_headers(
      Authorization = paste("Bearer", DROPBOX_TOKEN),
      `Dropbox-API-Arg` = toJSON(
        list(
          path = DROPBOX_MATCHES_PATH,
          mode = "overwrite",
          autorename = FALSE,
          mute = TRUE,
          strict_conflict = FALSE
        ),
        auto_unbox = TRUE
      ),
      `Content-Type` = "application/octet-stream"
    ) %>%
    req_body_file(tmp)

  resp <- req_perform(req)

  if (resp_status(resp) >= 300) {
    stop("Dropbox upload failed.")
  }

  invisible(TRUE)
}

# ============================================================
# DATA + RATING ENGINE
# ============================================================

load_players <- function() {
  read_csv(PLAYERS_FILE, show_col_types = FALSE)
}

player_rating_before_new_match <- function(player_name, players, matches) {
  base <- players %>%
    filter(player == player_name) %>%
    pull(base_rating)

  if (length(base) == 0) base <- 300

  pm <- matches %>%
    filter(player_a == player_name | player_b == player_name)

  if (nrow(pm) == 0) return(base)

  delta <- sum(
    ifelse(pm$player_a == player_name, pm$rating_change_a, pm$rating_change_b),
    na.rm = TRUE
  )

  bonus <- sum(
    ifelse(pm$player_a == player_name, pm$participation_a, pm$participation_b),
    na.rm = TRUE
  )

  base + delta + bonus
}

already_joined_cup <- function(player_name, cup_name, matches) {
  if (nrow(matches) == 0) return(FALSE)

  any(
    matches$cup == cup_name &
      (matches$player_a == player_name | matches$player_b == player_name),
    na.rm = TRUE
  )
}

calculate_match <- function(player_a, goals_a, player_b, goals_b, stage, cup, players, matches) {

  rating_a_base <- player_rating_before_new_match(player_a, players, matches)
  rating_b_base <- player_rating_before_new_match(player_b, players, matches)

  participation_a <- ifelse(
    already_joined_cup(player_a, cup, matches),
    0,
    participation_bonus
  )

  participation_b <- ifelse(
    already_joined_cup(player_b, cup, matches),
    0,
    participation_bonus
  )

  rating_a_before <- rating_a_base + participation_a
  rating_b_before <- rating_b_base + participation_b

  gap <- abs(rating_a_before - rating_b_before)
  adj <- gap_adjustment(gap)

  rule <- stage_rules %>% filter(stage == !!stage)
  win_base <- rule$win_base[1]
  loss_base <- rule$loss_base[1]

  if (goals_a > goals_b) {

    change_a <- if (rating_a_before <= rating_b_before) {
      round(win_base * (1 + adj))
    } else {
      round(win_base / (1 + adj))
    }

    change_b <- if (rating_b_before >= rating_a_before) {
      -round(loss_base * (1 + adj))
    } else {
      -round(loss_base / (1 + adj))
    }

  } else if (goals_b > goals_a) {

    change_b <- if (rating_b_before <= rating_a_before) {
      round(win_base * (1 + adj))
    } else {
      round(win_base / (1 + adj))
    }

    change_a <- if (rating_a_before >= rating_b_before) {
      -round(loss_base * (1 + adj))
    } else {
      -round(loss_base / (1 + adj))
    }

  } else {

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

    base <- players %>% filter(player == p)
    pm <- matches %>% filter(player_a == p | player_b == p)

    if (nrow(pm) == 0) {
      return(tibble(
        player = p,
        rating = base$base_rating,
        matches = base$base_matches,
        wins = base$base_wins,
        losses = base$base_losses,
        goals_for = base$base_gf,
        goals_against = base$base_ga
      ))
    }

    rating_change <- sum(
      ifelse(pm$player_a == p, pm$rating_change_a, pm$rating_change_b),
      na.rm = TRUE
    )

    participation <- sum(
      ifelse(pm$player_a == p, pm$participation_a, pm$participation_b),
      na.rm = TRUE
    )

    wins <- sum(
      (pm$player_a == p & pm$goals_a > pm$goals_b) |
        (pm$player_b == p & pm$goals_b > pm$goals_a)
    )

    losses <- sum(
      (pm$player_a == p & pm$goals_a < pm$goals_b) |
        (pm$player_b == p & pm$goals_b < pm$goals_a)
    )

    gf <- sum(ifelse(pm$player_a == p, pm$goals_a, pm$goals_b), na.rm = TRUE)
    ga <- sum(ifelse(pm$player_a == p, pm$goals_b, pm$goals_a), na.rm = TRUE)

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

  h <- tibble(
    game = 0,
    label = "Baseline",
    rating = base_rating
  )

  if (nrow(pm) == 0) return(h)

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

    opponent <- ifelse(
      row$player_a == player_name,
      row$player_b,
      row$player_a
    )

    current <- current + participation + delta

    h <- bind_rows(
      h,
      tibble(
        game = i,
        label = paste0(row$cup, " vs ", opponent),
        rating = current
      )
    )
  }

  h
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background:#f4f6f8; }
      .cardx {
        background:white;
        border-radius:14px;
        padding:18px;
        box-shadow:0 2px 12px rgba(0,0,0,.08);
        margin-bottom:18px;
      }
      .metric-label {
        color:#6c757d;
        font-size:12px;
        text-transform:uppercase;
        letter-spacing:.04em;
      }
      .metric {
        font-size:28px;
        font-weight:800;
      }
      .profile-title {
        font-size:30px;
        font-weight:800;
      }
      .admin-ok {
        color:#138a36;
        font-weight:700;
      }
      .admin-no {
        color:#b42318;
        font-weight:700;
      }
    "))
  ),

  navbarPage(
    title = "Football Rating",
    id = "main_tabs",

    tabPanel(
      "Ranking",
      div(
        class = "cardx",
        h3("Live Ranking"),
        p("Click a player to open the full profile."),
        DTOutput("ranking_table")
      )
    ),

    tabPanel(
      "Players",
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
        column(2, div(class="cardx", div(class="metric-label","Rating"), div(class="metric", textOutput("m_rating")))),
        column(2, div(class="cardx", div(class="metric-label","Rank"), div(class="metric", textOutput("m_rank")))),
        column(2, div(class="cardx", div(class="metric-label","Matches"), div(class="metric", textOutput("m_matches")))),
        column(2, div(class="cardx", div(class="metric-label","Wins"), div(class="metric", textOutput("m_wins")))),
        column(2, div(class="cardx", div(class="metric-label","Win rate"), div(class="metric", textOutput("m_winrate")))),
        column(2, div(class="cardx", div(class="metric-label","Goal diff"), div(class="metric", textOutput("m_gd"))))
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

      div(
        class = "cardx",
        h3("Player Match History"),
        DTOutput("player_matches")
      )
    ),

    tabPanel(
      "Matches",
      div(
        class = "cardx",
        h3("Official Matches"),
        DTOutput("public_matches")
      )
    ),

    tabPanel(
      "Admin",
      uiOutput("admin_panel")
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  players <- reactiveVal(load_players())
  matches <- reactiveVal(dropbox_download_matches())
  admin_logged_in <- reactiveVal(FALSE)

  # Refresh public data from Dropbox every 15 seconds.
  observe({
    invalidateLater(15000, session)

    latest <- tryCatch(
      dropbox_download_matches(),
      error = function(e) NULL
    )

    if (!is.null(latest)) {
      matches(latest)
    }
  })

  ranking <- reactive({
    build_ranking(players(), matches())
  })

  observe({
    p <- players()$player
    updateSelectInput(session, "profile_player", choices = p)
  })

  output$ranking_table <- renderDT({
    datatable(
      ranking() %>%
        mutate(win_rate = paste0(round(win_rate, 1), "%")),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 25, dom = "tip")
    )
  })

  observeEvent(input$ranking_table_rows_selected, {
    idx <- input$ranking_table_rows_selected
    if (length(idx) == 1) {
      selected <- ranking()$player[idx]
      updateSelectInput(session, "profile_player", selected = selected)
      updateTabsetPanel(session, "main_tabs", selected = "Players")
    }
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
        style = "font-size:16px;color:#6c757d;margin-left:12px;",
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
    ifelse(x > 0, paste0("+", x), as.character(x))
  })

  output$rating_chart <- renderPlot({
    req(input$profile_player)

    h <- player_history(input$profile_player, players(), matches())

    p <- ggplot(h, aes(game, rating)) +
      geom_point(size = 3) +
      labs(
        x = "Official match",
        y = "Rating",
        title = paste(input$profile_player, "Rating History")
      ) +
      theme_minimal(base_size = 13)

    if (nrow(h) >= 2) {
      p <- p + geom_line(linewidth = 1.1)
    }

    p
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

    ggplot(d, aes(result, count)) +
      geom_col(width = .65) +
      geom_text(aes(label = count), vjust = -0.4, size = 5) +
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
      return(
        datatable(
          data.frame(Message = "No new match history after baseline."),
          rownames = FALSE
        )
      )
    }

    datatable(
      m %>%
        mutate(
          Opponent = ifelse(player_a == p, player_b, player_a),
          GF = ifelse(player_a == p, goals_a, goals_b),
          GA = ifelse(player_a == p, goals_b, goals_a),
          Result = case_when(
            GF > GA ~ "Win",
            GF < GA ~ "Loss",
            TRUE ~ "Draw"
          ),
          Change = ifelse(
            player_a == p,
            rating_change_a,
            rating_change_b
          ),
          RatingAfter = ifelse(
            player_a == p,
            rating_a_after,
            rating_b_after
          )
        ) %>%
        transmute(
          Date = date,
          Cup = cup,
          Stage = stage,
          Opponent,
          Score = paste(GF, "-", GA),
          Result,
          `Rating Change` = ifelse(Change > 0, paste0("+", Change), as.character(Change)),
          `Rating After` = RatingAfter
        ),
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tip")
    )
  })

  output$public_matches <- renderDT({
    m <- matches()

    if (nrow(m) == 0) {
      return(
        datatable(
          data.frame(Message = "No new official matches yet."),
          rownames = FALSE
        )
      )
    }

    datatable(
      m %>%
        arrange(desc(match_id)) %>%
        transmute(
          Date = date,
          Cup = cup,
          Stage = stage,
          `Player A` = player_a,
          Score = paste(goals_a, "-", goals_b),
          `Player B` = player_b
        ),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "tip")
    )
  })

  # ----------------------------
  # ADMIN UI
  # ----------------------------

  output$admin_panel <- renderUI({

    if (!admin_logged_in()) {

      div(
        class = "cardx",
        h3("Admin Login"),
        passwordInput("admin_password", "Password"),
        actionButton("admin_login", "Login", class = "btn-primary"),
        br(), br(),
        div(class = "admin-no", "Match entry is locked.")
      )

    } else {

      tagList(
        div(
          class = "cardx",
          fluidRow(
            column(
              8,
              h3("Admin"),
              div(class = "admin-ok", "Admin access enabled.")
            ),
            column(
              4,
              actionButton("admin_logout", "Logout")
            )
          )
        ),

        div(
          class = "cardx",
          h3("Add Official Match"),
          textInput("cup", "Cup ID", placeholder = "Example: CUP-02"),
          selectInput("stage", "Stage", choices = stage_rules$stage),
          selectInput("player_a", "Player A", choices = players()$player),
          numericInput("goals_a", "Goals A", 0, min = 0),
          selectInput("player_b", "Player B", choices = players()$player),
          numericInput("goals_b", "Goals B", 0, min = 0),
          actionButton("add_match", "Save Match", class = "btn-success")
        ),

        div(
          class = "cardx",
          h3("Storage"),
          p(
            if (dropbox_enabled()) {
              paste("Dropbox:", DROPBOX_MATCHES_PATH)
            } else {
              paste("Local testing mode:", LOCAL_MATCHES_FILE)
            }
          ),
          downloadButton("download_matches", "Download Backup CSV")
        )
      )
    }
  })

  observeEvent(input$admin_login, {
    req(input$admin_password)

    if (is_admin_password_valid(input$admin_password)) {
      admin_logged_in(TRUE)
      showNotification("Admin login successful.", type = "message")
    } else {
      admin_logged_in(FALSE)
      showNotification("Incorrect password.", type = "error")
    }
  })

  observeEvent(input$admin_logout, {
    admin_logged_in(FALSE)
  })

  observeEvent(input$add_match, {

    # SERVER-SIDE SECURITY CHECK:
    # hidden UI alone is not considered authorization.
    req(admin_logged_in())

    req(
      input$cup,
      input$stage,
      input$player_a,
      input$player_b
    )

    if (trimws(input$cup) == "") {
      showNotification("Cup ID is required.", type = "error")
      return()
    }

    if (input$player_a == input$player_b) {
      showNotification("Players must be different.", type = "error")
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

    new_id <- if (nrow(m) == 0) {
      1L
    } else {
      as.integer(max(m$match_id, na.rm = TRUE) + 1L)
    }

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

    tryCatch({
      dropbox_upload_matches(updated)
      matches(updated)
      showNotification(
        "Match saved. Ranking and Dropbox backup updated.",
        type = "message"
      )
    }, error = function(e) {
      showNotification(
        paste("Save failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  output$download_matches <- downloadHandler(
    filename = function() {
      paste0("football_matches_backup_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(matches(), file)
    }
  )
}

shinyApp(ui, server)
