
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(httr2)
library(jsonlite)
library(digest)

# ============================================================
# WINNING ELEVEN 3 (98) - ALL IN ONE
# Public: Ranking / Players / Matches / Rules / Tournament
# Admin: Add/Edit/Delete Match + Add/Edit/Delete Player
# Storage: Dropbox
# ============================================================

PLAYERS_FILE <- "players.csv"
LOCAL_MATCHES_FILE <- "matches_local.csv"

ADMIN_PASSWORD_HASH <- Sys.getenv("ADMIN_PASSWORD_HASH")
DROPBOX_TOKEN <- Sys.getenv("DROPBOX_TOKEN")

DROPBOX_MATCHES_PATH <- Sys.getenv("DROPBOX_MATCHES_PATH", unset = "/matches.csv")
DROPBOX_PLAYERS_PATH <- "/players.csv"
DROPBOX_RANKING_PATH <- "/ranking.csv"
DROPBOX_PLAYERS_STATS_PATH <- "/players_stats.csv"
DROPBOX_RATING_HISTORY_PATH <- "/rating_history.csv"
DROPBOX_TOURNAMENT_PATH <- "/tournament_draw.csv"

participation_bonus <- 11

# ============================================================
# PLAYER DISPLAY NAMES = ACTUAL PHOTO FILENAMES
# Old internal names are converted to the exact photo filename stem.
# ============================================================

photo_name_map <- c(
  "A Aa" = "abbas",
  "A.A" = "abbas",
  "Abolfazl" = "abolfazl askari",
  "Ahmad" = "ahmad",
  "Ali Af" = "ali farahani",
  "Ghajar" = "ami ghajar",
  "Firoozi" = "amir firuzi",
  "M Fadaei" = "mehdi fadaei",
  "Mehdi Mameshli" = "mehdi mamashali",
  "Mhdi" = "mehdi",
  "Mortazavi" = "mohammad mortazavi",
  "Sepehri" = "mohammad sepehri",
  "Mojtaba Elyasi" = "mojtaba",
  "Reza Shokri" = "reza shokri",
  "Seyed Behdad Arshahi" = "seed behdad",
  "Sia" = "siavash",
  "Mori" = "morteza shafaye",
  "Naser" = "naser",
  "Tohid" = "tohid",
  "Vahid" = "vahid",
  "Rezgar" = "Rezgar"
)

# NOTE: Tanha is left unchanged until its exact photo filename is confirmed.

rename_to_photo_name <- function(x) {
  x <- trimws(as.character(x))
  key <- match(x, names(photo_name_map))
  hit <- !is.na(key)
  x[hit] <- unname(photo_name_map[key[hit]])
  x
}

rename_players_to_photo_names <- function(p) {
  p$player <- rename_to_photo_name(p$player)
  p
}

rename_matches_to_photo_names <- function(m) {
  m$player_a <- rename_to_photo_name(m$player_a)
  m$player_b <- rename_to_photo_name(m$player_b)
  m
}

# ============================================================
# PLAYER PHOTOS - EXACT MATCH TO DISPLAY NAME
# ============================================================

normalize_photo_name <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- trimws(tolower(x))
  x <- gsub("[_-]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x
}

player_photo <- function(player_name) {
  if (!dir.exists("www")) return(NULL)

  files <- list.files(
    "www",
    pattern = "\\.(jpg|jpeg|png|webp)$",
    ignore.case = TRUE,
    full.names = FALSE
  )

  if (length(files) == 0) return(NULL)

  target <- normalize_photo_name(player_name)
  stems <- vapply(files, normalize_photo_name, character(1))
  hit <- which(stems == target)

  if (length(hit) == 0) return(NULL)

  # Files in www/ are served from the Shiny app root.
  utils::URLencode(files[hit[1]], reserved = FALSE)
}

avatar_ui <- function(player_name, size = 90) {
  photo <- player_photo(player_name)
  if (!is.null(photo)) {
    tags$img(
      src = photo,
      alt = player_name,
      style = sprintf(
        "width:%spx;height:%spx;object-fit:cover;border-radius:50%%;border:3px solid #d0d5dd;box-shadow:0 2px 8px rgba(0,0,0,.15);",
        size, size
      )
    )
  } else {
    div(
      style = sprintf(
        "width:%spx;height:%spx;border-radius:50%%;background:#e5e7eb;display:flex;align-items:center;justify-content:center;font-size:%spx;font-weight:800;color:#667085;border:3px solid #d0d5dd;",
        size, size, round(size * .36)
      ),
      toupper(substr(player_name, 1, 1))
    )
  }
}

# ============================================================
# RULES
# ============================================================

stage_rules <- tibble::tribble(
  ~stage,          ~win_base, ~loss_base,
  "Group Stage",          10,         10,
  "Round of 16",          12,          9,
  "Quarterfinal",         14,          8,
  "Semifinal",            16,          7,
  "Final",                20,          6
)

gap_rules <- tibble::tribble(
  ~rating_gap, ~adjustment,
  "0–20", "0%",
  "21–40", "30%",
  "41–60", "50%",
  "61–80", "70%",
  "81–100", "100%",
  "100+", "150%"
)

gap_adjustment <- function(g) {
  if (g <= 20) 0
  else if (g <= 40) .30
  else if (g <= 60) .50
  else if (g <= 80) .70
  else if (g <= 100) 1.00
  else 1.50
}

draw_bonus <- function(g) {
  if (g <= 20) 0
  else if (g <= 40) 1
  else if (g <= 60) 2
  else if (g <= 80) 3
  else if (g <= 100) 4
  else 5
}

# ============================================================
# DATA SHAPES
# ============================================================

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
      across(
        c(
          rating_a_before, rating_b_before,
          participation_a, participation_b,
          rating_change_a, rating_change_b,
          rating_a_after, rating_b_after
        ),
        as.numeric
      )
    )
}

normalize_players <- function(x) {
  req <- c(
    "player", "base_rating", "base_matches", "base_wins",
    "base_losses", "base_gf", "base_ga"
  )

  if (is.null(x) || nrow(x) == 0) {
    return(tibble(
      player = character(),
      base_rating = double(),
      base_matches = integer(),
      base_wins = integer(),
      base_losses = integer(),
      base_gf = integer(),
      base_ga = integer()
    ))
  }

  for (nm in req) {
    if (!nm %in% names(x)) {
      x[[nm]] <- if (nm == "player") "" else 0
    }
  }

  x[, req] %>%
    mutate(
      player = as.character(player),
      base_rating = as.numeric(base_rating),
      base_matches = as.integer(base_matches),
      base_wins = as.integer(base_wins),
      base_losses = as.integer(base_losses),
      base_gf = as.integer(base_gf),
      base_ga = as.integer(base_ga)
    )
}

# ============================================================
# SECURITY
# ============================================================

hash_password <- function(x) {
  digest(x, algo = "sha256", serialize = FALSE)
}

is_admin_password_valid <- function(x) {
  nzchar(ADMIN_PASSWORD_HASH) &&
    identical(hash_password(x), ADMIN_PASSWORD_HASH)
}

# ============================================================
# DROPBOX
# ============================================================

dropbox_download_csv <- function(path) {
  if (!nzchar(DROPBOX_TOKEN)) return(NULL)

  req <- request("https://content.dropboxapi.com/2/files/download") %>%
    req_headers(
      Authorization = paste("Bearer", DROPBOX_TOKEN),
      `Dropbox-API-Arg` = toJSON(list(path = path), auto_unbox = TRUE)
    )

  resp <- tryCatch(req_perform(req), error = function(e) NULL)

  if (is.null(resp) || resp_status(resp) >= 400) return(NULL)

  tmp <- tempfile(fileext = ".csv")
  writeBin(resp_body_raw(resp), tmp)
  read_csv(tmp, show_col_types = FALSE)
}

dropbox_upload_csv <- function(x, path) {
  if (!nzchar(DROPBOX_TOKEN)) return(invisible(TRUE))

  tmp <- tempfile(fileext = ".csv")
  write_csv(x, tmp)

  req <- request("https://content.dropboxapi.com/2/files/upload") %>%
    req_headers(
      Authorization = paste("Bearer", DROPBOX_TOKEN),
      `Dropbox-API-Arg` = toJSON(
        list(
          path = path,
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
    stop(paste("Dropbox upload failed:", path))
  }

  invisible(TRUE)
}

load_players <- function() {
  cloud <- dropbox_download_csv(DROPBOX_PLAYERS_PATH)
  if (!is.null(cloud)) return(rename_players_to_photo_names(normalize_players(cloud)))

  if (file.exists(PLAYERS_FILE)) {
    return(rename_players_to_photo_names(normalize_players(read_csv(PLAYERS_FILE, show_col_types = FALSE))))
  }

  rename_players_to_photo_names(normalize_players(NULL))
}

load_matches <- function() {
  cloud <- dropbox_download_csv(DROPBOX_MATCHES_PATH)
  if (!is.null(cloud)) return(rename_matches_to_photo_names(normalize_matches(cloud)))

  if (file.exists(LOCAL_MATCHES_FILE)) {
    return(rename_matches_to_photo_names(normalize_matches(read_csv(LOCAL_MATCHES_FILE, show_col_types = FALSE))))
  }

  empty_matches
}

# ============================================================
# RATING ENGINE
# ============================================================

player_rating_before <- function(p, players, matches) {
  base <- players %>%
    filter(player == p) %>%
    pull(base_rating)

  if (length(base) == 0) base <- 300

  pm <- matches %>%
    filter(player_a == p | player_b == p)

  if (nrow(pm) == 0) return(base)

  base +
    sum(ifelse(pm$player_a == p, pm$rating_change_a, pm$rating_change_b), na.rm = TRUE) +
    sum(ifelse(pm$player_a == p, pm$participation_a, pm$participation_b), na.rm = TRUE)
}

joined_cup <- function(p, cup, matches) {
  nrow(matches) > 0 &&
    any(
      matches$cup == cup &
        (matches$player_a == p | matches$player_b == p),
      na.rm = TRUE
    )
}

calculate_match <- function(pa, ga, pb, gb, stage, cup, players, prior) {
  ra0 <- player_rating_before(pa, players, prior)
  rb0 <- player_rating_before(pb, players, prior)

  ba <- ifelse(joined_cup(pa, cup, prior), 0, participation_bonus)
  bb <- ifelse(joined_cup(pb, cup, prior), 0, participation_bonus)

  ra <- ra0 + ba
  rb <- rb0 + bb

  gap <- abs(ra - rb)
  adj <- gap_adjustment(gap)

  rule <- stage_rules %>% filter(.data$stage == stage)
  if (nrow(rule) == 0) stop("Invalid stage.")

  wb <- rule$win_base[1]
  lb <- rule$loss_base[1]

  if (ga > gb) {
    ca <- if (ra <= rb) round(wb * (1 + adj)) else round(wb / (1 + adj))
    cb <- if (rb >= ra) -round(lb * (1 + adj)) else -round(lb / (1 + adj))
  } else if (gb > ga) {
    cb <- if (rb <= ra) round(wb * (1 + adj)) else round(wb / (1 + adj))
    ca <- if (ra >= rb) -round(lb * (1 + adj)) else -round(lb / (1 + adj))
  } else {
    pts <- draw_bonus(gap)
    if (ra == rb) {
      ca <- 0
      cb <- 0
    } else if (ra < rb) {
      ca <- pts
      cb <- -pts
    } else {
      ca <- -pts
      cb <- pts
    }
  }

  tibble(
    rating_a_before = ra,
    rating_b_before = rb,
    participation_a = ba,
    participation_b = bb,
    rating_change_a = ca,
    rating_change_b = cb,
    rating_a_after = ra + ca,
    rating_b_after = rb + cb
  )
}

recalculate_all <- function(matches, players) {
  players <- rename_players_to_photo_names(players)
  matches <- rename_matches_to_photo_names(matches)
  matches <- normalize_matches(matches) %>% arrange(match_id)

  if (nrow(matches) == 0) return(empty_matches)

  out <- empty_matches

  for (i in seq_len(nrow(matches))) {
    r <- matches[i, ]

    calc <- calculate_match(
      r$player_a, r$goals_a,
      r$player_b, r$goals_b,
      r$stage, r$cup,
      players, out
    )

    out <- bind_rows(
      out,
      tibble(
        match_id = r$match_id,
        date = r$date,
        cup = r$cup,
        stage = r$stage,
        player_a = r$player_a,
        goals_a = r$goals_a,
        player_b = r$player_b,
        goals_b = r$goals_b,
        rating_a_before = calc$rating_a_before,
        rating_b_before = calc$rating_b_before,
        participation_a = calc$participation_a,
        participation_b = calc$participation_b,
        rating_change_a = calc$rating_change_a,
        rating_change_b = calc$rating_change_b,
        rating_a_after = calc$rating_a_after,
        rating_b_after = calc$rating_b_after
      )
    )
  }

  out
}

build_ranking <- function(players, matches) {
  players <- rename_players_to_photo_names(players)
  matches <- rename_matches_to_photo_names(matches)
  if (nrow(players) == 0) return(tibble())

  out <- lapply(players$player, function(p) {
    base <- players %>% filter(player == p)
    pm <- matches %>% filter(player_a == p | player_b == p)

    rc <- if (nrow(pm)) {
      sum(ifelse(pm$player_a == p, pm$rating_change_a, pm$rating_change_b), na.rm = TRUE)
    } else 0

    bonus <- if (nrow(pm)) {
      sum(ifelse(pm$player_a == p, pm$participation_a, pm$participation_b), na.rm = TRUE)
    } else 0

    wins <- if (nrow(pm)) {
      sum(
        (pm$player_a == p & pm$goals_a > pm$goals_b) |
          (pm$player_b == p & pm$goals_b > pm$goals_a)
      )
    } else 0

    losses <- if (nrow(pm)) {
      sum(
        (pm$player_a == p & pm$goals_a < pm$goals_b) |
          (pm$player_b == p & pm$goals_b < pm$goals_a)
      )
    } else 0

    gf <- if (nrow(pm)) {
      sum(ifelse(pm$player_a == p, pm$goals_a, pm$goals_b), na.rm = TRUE)
    } else 0

    ga <- if (nrow(pm)) {
      sum(ifelse(pm$player_a == p, pm$goals_b, pm$goals_a), na.rm = TRUE)
    } else 0

    tibble(
      player = p,
      rating = base$base_rating + rc + bonus,
      matches = base$base_matches + nrow(pm),
      wins = base$base_wins + wins,
      losses = base$base_losses + losses,
      goals_for = base$base_gf + gf,
      goals_against = base$base_ga + ga
    )
  })

  bind_rows(out) %>%
    mutate(
      goal_difference = goals_for - goals_against,
      win_rate = ifelse(matches == 0, 0, 100 * wins / matches)
    ) %>%
    arrange(desc(rating), desc(wins), desc(goal_difference), player) %>%
    mutate(rank = row_number()) %>%
    select(rank, everything())
}

build_rating_history_all <- function(players, matches) {
  out <- list()
  k <- 1L

  for (p in players$player) {
    base <- players %>% filter(player == p)
    current <- base$base_rating[1]

    out[[k]] <- tibble(
      player = p,
      game = 0L,
      match_id = NA_integer_,
      date = as.Date(NA),
      cup = "Baseline",
      stage = NA_character_,
      opponent = NA_character_,
      result = "Baseline",
      goals_for = NA_integer_,
      goals_against = NA_integer_,
      participation_bonus = 0,
      rating_change = 0,
      rating_after = current
    )
    k <- k + 1L

    pm <- matches %>%
      filter(player_a == p | player_b == p) %>%
      arrange(match_id)

    if (nrow(pm)) {
      for (i in seq_len(nrow(pm))) {
        r <- pm[i, ]
        is_a <- r$player_a == p
        gf <- if (is_a) r$goals_a else r$goals_b
        ga <- if (is_a) r$goals_b else r$goals_a
        opp <- if (is_a) r$player_b else r$player_a
        bonus <- if (is_a) r$participation_a else r$participation_b
        delta <- if (is_a) r$rating_change_a else r$rating_change_b
        result <- if (gf > ga) "Win" else if (gf < ga) "Loss" else "Draw"

        current <- current + bonus + delta

        out[[k]] <- tibble(
          player = p,
          game = i,
          match_id = r$match_id,
          date = r$date,
          cup = r$cup,
          stage = r$stage,
          opponent = opp,
          result = result,
          goals_for = gf,
          goals_against = ga,
          participation_bonus = bonus,
          rating_change = delta,
          rating_after = current
        )
        k <- k + 1L
      }
    }
  }

  bind_rows(out)
}

save_everything <- function(players, matches) {
  players <- rename_players_to_photo_names(normalize_players(players))
  matches <- rename_matches_to_photo_names(normalize_matches(matches))

  ranking_now <- build_ranking(players, matches)
  stats_now <- ranking_now %>%
    select(
      player, rating, matches, wins, losses,
      goals_for, goals_against, goal_difference, win_rate
    )
  history_now <- build_rating_history_all(players, matches)

  write_csv(players, "players_local.csv")
  write_csv(matches, "matches_local.csv")
  write_csv(ranking_now, "ranking_local.csv")
  write_csv(stats_now, "players_stats_local.csv")
  write_csv(history_now, "rating_history_local.csv")

  dropbox_upload_csv(players, DROPBOX_PLAYERS_PATH)
  dropbox_upload_csv(matches, DROPBOX_MATCHES_PATH)
  dropbox_upload_csv(ranking_now, DROPBOX_RANKING_PATH)
  dropbox_upload_csv(stats_now, DROPBOX_PLAYERS_STATS_PATH)
  dropbox_upload_csv(history_now, DROPBOX_RATING_HISTORY_PATH)

  invisible(TRUE)
}

# ============================================================
# TOURNAMENT DRAW
# 16 players: top 8 seeded vs bottom 8.
# Seed placement:
# Upper half: 1,4,6,7
# Lower half: 2,3,5,8
# ============================================================

make_draw_16 <- function(ranking) {
  if (nrow(ranking) < 16) {
    stop("At least 16 ranked players are required.")
  }

  top8 <- ranking %>% slice(1:8)
  bottom8 <- ranking %>% slice(9:16)

  upper_seeds <- c(1, 4, 6, 7)
  lower_seeds <- c(2, 3, 5, 8)

  upper_bottom <- sample(bottom8$player, 4)
  lower_bottom <- sample(setdiff(bottom8$player, upper_bottom), 4)

  upper <- tibble(
    bracket_half = "Upper",
    slot = 1:4,
    seed_rank = upper_seeds,
    seeded_player = top8$player[match(upper_seeds, top8$rank)],
    opponent = upper_bottom
  )

  lower <- tibble(
    bracket_half = "Lower",
    slot = 5:8,
    seed_rank = lower_seeds,
    seeded_player = top8$player[match(lower_seeds, top8$rank)],
    opponent = lower_bottom
  )

  bind_rows(upper, lower) %>%
    mutate(
      match = paste0("R16-", slot),
      pairing = paste0("#", seed_rank, " ", seeded_player, " vs ", opponent)
    ) %>%
    select(match, bracket_half, seed_rank, seeded_player, opponent, pairing)
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background:#f3f5f7; }
      .cardx {
        background:white;
        border-radius:14px;
        padding:18px;
        box-shadow:0 2px 12px rgba(0,0,0,.08);
        margin-bottom:18px;
      }
      .metric-label {
        color:#667085;
        font-size:12px;
        text-transform:uppercase;
        letter-spacing:.04em;
      }
      .metric {
        font-size:28px;
        font-weight:800;
      }
      .admin-ok { color:#138a36; font-weight:700; }
      .admin-no { color:#b42318; font-weight:700; }
      .danger-box { border:1px solid #d92d20; }
      .form-chip {
        display:inline-flex;
        align-items:center;
        justify-content:center;
        width:34px;
        height:34px;
        border-radius:50%;
        margin-right:6px;
        font-weight:800;
        color:white;
      }
      .form-win { background:#16a34a; }
      .form-loss { background:#dc2626; }
      .form-draw { background:#6b7280; }
      .bracket-box {
        background:#f8fafc;
        border:1px solid #d0d5dd;
        border-radius:12px;
        padding:14px;
        margin-bottom:10px;
        font-weight:700;
      }
      @media (max-width: 768px) {
        .container-fluid { padding-left:10px; padding-right:10px; }
        .cardx { padding:12px; border-radius:10px; }
        .metric { font-size:22px; }
        .navbar-nav > li > a { padding-left:9px; padding-right:9px; }
        table.dataTable { font-size:12px; }
        .mobile-stack { width:100% !important; }
      }
    "))
  ),

  navbarPage(
    title = "Football Rating",
    id = "tabs",

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
      div(
        class = "cardx",
        selectInput("profile_player", "Player", choices = NULL),
        uiOutput("profile_header")
      ),

      fluidRow(
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Rating"), div(class="metric",textOutput("m_rating")))),
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Rank"), div(class="metric",textOutput("m_rank")))),
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Matches"), div(class="metric",textOutput("m_matches")))),
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Wins"), div(class="metric",textOutput("m_wins")))),
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Win rate"), div(class="metric",textOutput("m_winrate")))),
        column(2, class="mobile-stack", div(class="cardx", div(class="metric-label","Goal diff"), div(class="metric",textOutput("m_gd"))))
      ),

      div(
        class = "cardx",
        h4("Recent Form"),
        uiOutput("recent_form")
      ),

      fluidRow(
        column(
          8,
          class="mobile-stack",
          div(
            class = "cardx",
            h3("Rating Progress"),
            plotOutput("rating_chart", height = "360px")
          )
        ),
        column(
          4,
          class="mobile-stack",
          div(
            class = "cardx",
            h3("Performance"),
            plotOutput("performance_chart", height = "360px")
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
      "Tournament",
      div(
        class = "cardx",
        h3("16-Player Knockout Draw"),
        p("Top 8 are seeded. First-round opponents are randomly drawn from ranks 9–16."),
        uiOutput("tournament_public")
      )
    ),

    tabPanel(
      "Rules",
      fluidRow(
        column(
          6,
          class="mobile-stack",
          div(
            class = "cardx",
            h3("Stage Points"),
            tableOutput("stage_rules_table")
          )
        ),
        column(
          6,
          class="mobile-stack",
          div(
            class = "cardx",
            h3("Rating Gap Adjustment"),
            tableOutput("gap_rules_table")
          )
        )
      ),

      div(
        class = "cardx",
        h3("Ranking Rules"),
        tags$ul(
          tags$li(tags$b("+11 participation points"), " once per player for each Cup ID."),
          tags$li("A win against a higher-rated player earns more rating."),
          tags$li("A win against a lower-rated player earns less rating."),
          tags$li("Losing to a stronger player costs less rating."),
          tags$li("Losing to a weaker player costs more rating."),
          tags$li("Knockout rounds have progressively higher win rewards and lower loss penalties."),
          tags$li("Editing or deleting an old match automatically recalculates every later rating."),
          tags$li("Ranking order: Rating, then Wins, then Goal Difference.")
        )
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
  matches <- reactiveVal(recalculate_all(load_matches(), load_players()))
  admin_logged_in <- reactiveVal(FALSE)

  selected_match_id <- reactiveVal(NULL)
  selected_player_name <- reactiveVal(NULL)
  tournament_draw <- reactiveVal(NULL)

  rankdat <- reactive({
    build_ranking(players(), matches())
  })

  observe({
    invalidateLater(20000, session)

    cloud_players <- tryCatch(load_players(), error = function(e) NULL)
    cloud_matches <- tryCatch(load_matches(), error = function(e) NULL)

    if (!is.null(cloud_players) && nrow(cloud_players) > 0) {
      players(cloud_players)
    }

    if (!is.null(cloud_matches)) {
      matches(recalculate_all(cloud_matches, players()))
    }
  })

  observe({
    p <- players()$player
    updateSelectInput(session, "profile_player", choices = p)
  })

  # ----------------------------
  # PUBLIC RANKING
  # ----------------------------

  output$ranking_table <- renderDT({
    dat <- rankdat()

    if (nrow(dat) == 0) {
      return(datatable(data.frame(Message = "No players."), rownames = FALSE))
    }

    datatable(
      dat %>%
        mutate(win_rate = paste0(round(win_rate, 1), "%")),
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = "tip",
        scrollX = TRUE
      )
    )
  })

  observeEvent(input$ranking_table_rows_selected, {
    i <- input$ranking_table_rows_selected
    if (length(i) == 1) {
      updateSelectInput(
        session,
        "profile_player",
        selected = rankdat()$player[i]
      )
      updateTabsetPanel(session, "tabs", selected = "Players")
    }
  })

  stats <- reactive({
    req(input$profile_player)
    rankdat() %>% filter(player == input$profile_player)
  })

  output$profile_header <- renderUI({
    s <- stats()

    fluidRow(
      column(
        2,
        class="mobile-stack",
        avatar_ui(s$player, 110)
      ),
      column(
        10,
        class="mobile-stack",
        div(
          style = "font-size:30px;font-weight:800;margin-top:18px;",
          s$player,
          tags$br(),
          tags$span(
            style = "font-size:16px;color:#667085;",
            paste0("#", s$rank, " • Rating ", s$rating)
          )
        )
      )
    )
  })

  output$m_rating <- renderText(stats()$rating)
  output$m_rank <- renderText(paste0("#", stats()$rank))
  output$m_matches <- renderText(stats()$matches)
  output$m_wins <- renderText(stats()$wins)
  output$m_winrate <- renderText(paste0(round(stats()$win_rate, 1), "%"))
  output$m_gd <- renderText({
    x <- stats()$goal_difference
    ifelse(x > 0, paste0("+", x), as.character(x))
  })

  player_match_data <- reactive({
    req(input$profile_player)
    p <- input$profile_player

    matches() %>%
      filter(player_a == p | player_b == p) %>%
      arrange(match_id) %>%
      mutate(
        is_a = player_a == p,
        opponent = ifelse(is_a, player_b, player_a),
        gf = ifelse(is_a, goals_a, goals_b),
        ga = ifelse(is_a, goals_b, goals_a),
        result = case_when(
          gf > ga ~ "Win",
          gf < ga ~ "Loss",
          TRUE ~ "Draw"
        ),
        change = ifelse(is_a, rating_change_a, rating_change_b),
        rating_after = ifelse(is_a, rating_a_after, rating_b_after)
      )
  })

  output$recent_form <- renderUI({
    d <- player_match_data()

    if (nrow(d) == 0) {
      return(tags$span(style="color:#667085;", "No recent matches."))
    }

    recent <- tail(d$result, 5)

    tagList(lapply(recent, function(x) {
      cls <- if (x == "Win") "form-chip form-win"
      else if (x == "Loss") "form-chip form-loss"
      else "form-chip form-draw"

      div(class = cls, substr(x, 1, 1))
    }))
  })

  output$rating_chart <- renderPlot({
    req(input$profile_player)

    hist <- build_rating_history_all(players(), matches()) %>%
      filter(player == input$profile_player)

    ggplot(hist, aes(game, rating_after)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 3) +
      theme_minimal(base_size = 13) +
      labs(
        x = "Official match",
        y = "Rating",
        title = paste(input$profile_player, "Rating History")
      )
  })

  output$performance_chart <- renderPlot({
    s <- stats()

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
      geom_text(aes(label = count), vjust = -.4, size = 5) +
      theme_minimal(base_size = 13) +
      scale_y_continuous(expand = expansion(mult = c(0, .15))) +
      labs(x = NULL, y = "Matches")
  })

  output$player_matches <- renderDT({
    d <- player_match_data() %>% arrange(desc(match_id))

    if (nrow(d) == 0) {
      return(datatable(data.frame(Message = "No match history."), rownames = FALSE))
    }

    datatable(
      d %>%
        transmute(
          Date = date,
          Cup = cup,
          Stage = stage,
          Opponent = opponent,
          Score = paste(gf, "-", ga),
          Result = result,
          `Rating Change` = ifelse(change > 0, paste0("+", change), as.character(change)),
          `Rating After` = rating_after
        ),
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
    )
  })

  output$public_matches <- renderDT({
    m <- matches()

    if (nrow(m) == 0) {
      return(datatable(data.frame(Message = "No official matches yet."), rownames = FALSE))
    }

    datatable(
      m %>%
        arrange(desc(match_id)) %>%
        transmute(
          ID = match_id,
          Date = date,
          Cup = cup,
          Stage = stage,
          `Player A` = player_a,
          Score = paste(goals_a, "-", goals_b),
          `Player B` = player_b
        ),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
    )
  })

  # ----------------------------
  # RULES
  # ----------------------------

  output$stage_rules_table <- renderTable({
    stage_rules %>%
      rename(
        Stage = stage,
        `Base Win` = win_base,
        `Base Loss` = loss_base
      )
  }, striped = TRUE, bordered = TRUE)

  output$gap_rules_table <- renderTable({
    gap_rules %>%
      rename(
        `Rating Gap` = rating_gap,
        Adjustment = adjustment
      )
  }, striped = TRUE, bordered = TRUE)

  # ----------------------------
  # TOURNAMENT
  # ----------------------------

  output$tournament_public <- renderUI({
    d <- tournament_draw()

    if (is.null(d) || nrow(d) == 0) {
      return(tags$p(
        style = "color:#667085;",
        "No draw has been generated yet."
      ))
    }

    tagList(
      h4("Upper Half"),
      lapply(seq_len(nrow(d %>% filter(bracket_half == "Upper"))), function(i) {
        r <- d %>% filter(bracket_half == "Upper") %>% slice(i)
        div(class = "bracket-box", r$pairing)
      }),
      h4("Lower Half"),
      lapply(seq_len(nrow(d %>% filter(bracket_half == "Lower"))), function(i) {
        r <- d %>% filter(bracket_half == "Lower") %>% slice(i)
        div(class = "bracket-box", r$pairing)
      }),
      tags$hr(),
      p("Bracket path is fixed after the first-round draw.")
    )
  })

  # ----------------------------
  # ADMIN LOGIN
  # ----------------------------

  observeEvent(input$admin_login, {
    req(input$admin_password)

    if (is_admin_password_valid(input$admin_password)) {
      admin_logged_in(TRUE)
      showNotification("Admin login successful.", type = "message")
    } else {
      showNotification("Incorrect password.", type = "error")
    }
  })

  observeEvent(input$admin_logout, {
    admin_logged_in(FALSE)
    selected_match_id(NULL)
    selected_player_name(NULL)
  })

  # ----------------------------
  # ADMIN UI
  # ----------------------------

  output$admin_panel <- renderUI({
    if (!admin_logged_in()) {
      return(
        div(
          class = "cardx",
          h3("Admin Login"),
          passwordInput("admin_password", "Password"),
          actionButton("admin_login", "Login", class = "btn-primary"),
          br(), br(),
          div(class = "admin-no", "Match and player management is locked.")
        )
      )
    }

    tagList(
      div(
        class = "cardx",
        fluidRow(
          column(8, h3("Admin"), div(class="admin-ok","Admin access enabled.")),
          column(4, actionButton("admin_logout", "Logout"))
        )
      ),

      tabsetPanel(
        id = "admin_tabs",

        tabPanel(
          "Add Match",
          div(
            class = "cardx",
            textInput("cup", "Cup ID", placeholder = "Example: CUP-02"),
            selectInput("stage", "Stage", choices = stage_rules$stage),
            selectInput("player_a", "Player A", choices = players()$player),
            numericInput("goals_a", "Goals A", 0, min = 0),
            selectInput(
              "player_b",
              "Player B",
              choices = players()$player,
              selected = if (length(players()$player) > 1) players()$player[2] else players()$player[1]
            ),
            numericInput("goals_b", "Goals B", 0, min = 0),
            actionButton("add_match", "Save Match", class = "btn-success")
          )
        ),

        tabPanel(
          "Manage Matches",
          div(
            class = "cardx",
            p("Select a match to edit or delete."),
            DTOutput("admin_matches")
          ),
          uiOutput("edit_match_panel")
        ),

        tabPanel(
          "Manage Players",
          fluidRow(
            column(
              5,
              class="mobile-stack",
              div(
                class = "cardx",
                h3("Add Player"),
                textInput("new_player_name", "Player Name"),
                numericInput("new_player_rating", "Starting Rating", 300, min = 0),
                actionButton("add_player", "Add Player", class = "btn-success")
              )
            ),
            column(
              7,
              class="mobile-stack",
              div(
                class = "cardx",
                h3("Existing Players"),
                p(style="color:#667085;font-size:12px;", "Player names are standardized to match photo filenames. New players should use the same name as their photo filename."),
                DTOutput("admin_players")
              )
            )
          ),
          uiOutput("edit_player_panel")
        ),

        tabPanel(
          "Tournament Draw",
          div(
            class = "cardx",
            h3("Generate 16-Player Knockout Draw"),
            p("Uses the current ranking. Top 8 are seeded against ranks 9–16."),
            actionButton("generate_draw", "Generate New Draw", class = "btn-primary"),
            tags$span(" "),
            actionButton("save_draw", "Save Draw to Dropbox", class = "btn-success"),
            br(), br(),
            DTOutput("admin_draw_table")
          )
        ),

        tabPanel(
          "Backup",
          div(
            class = "cardx",
            h3("Dropbox Backups"),
            tags$ul(
              tags$li("players.csv"),
              tags$li("matches.csv"),
              tags$li("ranking.csv"),
              tags$li("players_stats.csv"),
              tags$li("rating_history.csv"),
              tags$li("tournament_draw.csv")
            ),
            downloadButton("download_matches", "Download Matches CSV"),
            tags$span(" "),
            downloadButton("download_ranking", "Download Ranking CSV")
          )
        )
      )
    )
  })

  # ----------------------------
  # ADD MATCH
  # ----------------------------

  observeEvent(input$add_match, {
    req(admin_logged_in())

    if (trimws(input$cup) == "") {
      showNotification("Cup ID is required.", type = "error")
      return()
    }

    if (input$player_a == input$player_b) {
      showNotification("Players must be different.", type = "error")
      return()
    }

    m <- matches()

    new_id <- if (nrow(m) == 0) 1L else max(m$match_id, na.rm = TRUE) + 1L

    raw <- tibble(
      match_id = new_id,
      date = Sys.Date(),
      cup = trimws(input$cup),
      stage = input$stage,
      player_a = input$player_a,
      goals_a = as.integer(input$goals_a),
      player_b = input$player_b,
      goals_b = as.integer(input$goals_b),
      rating_a_before = NA_real_,
      rating_b_before = NA_real_,
      participation_a = NA_real_,
      participation_b = NA_real_,
      rating_change_a = NA_real_,
      rating_change_b = NA_real_,
      rating_a_after = NA_real_,
      rating_b_after = NA_real_
    )

    rebuilt <- recalculate_all(bind_rows(m, raw), players())

    tryCatch({
      save_everything(players(), rebuilt)
      matches(rebuilt)
      showNotification("Match saved and all backups updated.", type = "message")
    }, error = function(e) {
      showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })

  # ----------------------------
  # MANAGE MATCHES
  # ----------------------------

  output$admin_matches <- renderDT({
    req(admin_logged_in())

    m <- matches()

    if (nrow(m) == 0) {
      return(datatable(data.frame(Message = "No matches."), rownames = FALSE))
    }

    datatable(
      m %>%
        arrange(desc(match_id)) %>%
        transmute(
          ID = match_id,
          Date = date,
          Cup = cup,
          Stage = stage,
          `Player A` = player_a,
          `Goals A` = goals_a,
          `Player B` = player_b,
          `Goals B` = goals_b,
          `A Change` = rating_change_a,
          `B Change` = rating_change_b
        ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
    )
  })

  observeEvent(input$admin_matches_rows_selected, {
    i <- input$admin_matches_rows_selected
    if (length(i) == 1) {
      ordered <- matches() %>% arrange(desc(match_id))
      selected_match_id(ordered$match_id[i])
    }
  })

  output$edit_match_panel <- renderUI({
    req(admin_logged_in(), selected_match_id())

    r <- matches() %>% filter(match_id == selected_match_id())
    req(nrow(r) == 1)

    div(
      class = "cardx danger-box",
      h3(paste("Edit Match #", r$match_id)),
      textInput("edit_cup", "Cup ID", value = r$cup),
      selectInput("edit_stage", "Stage", choices = stage_rules$stage, selected = r$stage),
      selectInput("edit_pa", "Player A", choices = players()$player, selected = r$player_a),
      numericInput("edit_ga", "Goals A", value = r$goals_a, min = 0),
      selectInput("edit_pb", "Player B", choices = players()$player, selected = r$player_b),
      numericInput("edit_gb", "Goals B", value = r$goals_b, min = 0),
      actionButton("update_match", "Update Match", class = "btn-warning"),
      tags$span(" "),
      actionButton("delete_match", "Delete Match", class = "btn-danger")
    )
  })

  observeEvent(input$update_match, {
    req(admin_logged_in(), selected_match_id())

    if (trimws(input$edit_cup) == "") {
      showNotification("Cup ID is required.", type = "error")
      return()
    }

    if (input$edit_pa == input$edit_pb) {
      showNotification("Players must be different.", type = "error")
      return()
    }

    m <- matches()
    pos <- which(m$match_id == selected_match_id())

    m$cup[pos] <- trimws(input$edit_cup)
    m$stage[pos] <- input$edit_stage
    m$player_a[pos] <- input$edit_pa
    m$goals_a[pos] <- as.integer(input$edit_ga)
    m$player_b[pos] <- input$edit_pb
    m$goals_b[pos] <- as.integer(input$edit_gb)

    rebuilt <- recalculate_all(m, players())

    tryCatch({
      save_everything(players(), rebuilt)
      matches(rebuilt)
      selected_match_id(NULL)
      showNotification("Match updated. Ratings recalculated.", type = "message")
    }, error = function(e) {
      showNotification(paste("Update failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })

  observeEvent(input$delete_match, {
    req(admin_logged_in(), selected_match_id())

    showModal(
      modalDialog(
        title = "Delete Match",
        paste0("Delete match #", selected_match_id(), "? All ratings will be recalculated."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_match", "Delete", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_delete_match, {
    req(admin_logged_in(), selected_match_id())

    rebuilt <- recalculate_all(
      matches() %>% filter(match_id != selected_match_id()),
      players()
    )

    tryCatch({
      save_everything(players(), rebuilt)
      matches(rebuilt)
      selected_match_id(NULL)
      removeModal()
      showNotification("Match deleted. Ratings recalculated.", type = "message")
    }, error = function(e) {
      removeModal()
      showNotification(paste("Delete failed:", conditionMessage(e)), type = "error", duration = NULL)
    })
  })

  # ----------------------------
  # PLAYER MANAGEMENT
  # ----------------------------

  output$admin_players <- renderDT({
    req(admin_logged_in())

    datatable(
      rankdat() %>%
        select(rank, player, rating, matches, wins, losses),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 20, dom = "tip")
    )
  })

  observeEvent(input$admin_players_rows_selected, {
    i <- input$admin_players_rows_selected
    if (length(i) == 1) {
      selected_player_name(rankdat()$player[i])
    }
  })

  observeEvent(input$add_player, {
    req(admin_logged_in())

    nm <- trimws(input$new_player_name)

    if (!nzchar(nm)) {
      showNotification("Player name is required.", type = "error")
      return()
    }

    if (nm %in% players()$player) {
      showNotification("This player already exists.", type = "error")
      return()
    }

    p <- bind_rows(
      players(),
      tibble(
        player = nm,
        base_rating = as.numeric(input$new_player_rating),
        base_matches = 0L,
        base_wins = 0L,
        base_losses = 0L,
        base_gf = 0L,
        base_ga = 0L
      )
    )

    tryCatch({
      save_everything(p, matches())
      players(p)
      showNotification("Player added.", type = "message")
    }, error = function(e) {
      showNotification(paste("Player save failed:", conditionMessage(e)), type = "error")
    })
  })

  output$edit_player_panel <- renderUI({
    req(admin_logged_in(), selected_player_name())

    p <- players() %>% filter(player == selected_player_name())
    req(nrow(p) == 1)

    div(
      class = "cardx danger-box",
      h3(paste("Edit Player:", p$player)),
      textInput("edit_player_name", "Player Name", value = p$player),
      numericInput("edit_player_base_rating", "Base Rating", value = p$base_rating, min = 0),
      actionButton("update_player", "Update Player", class = "btn-warning"),
      tags$span(" "),
      actionButton("delete_player", "Delete Player", class = "btn-danger")
    )
  })

  observeEvent(input$update_player, {
    req(admin_logged_in(), selected_player_name())

    old <- selected_player_name()
    new <- trimws(input$edit_player_name)

    if (!nzchar(new)) {
      showNotification("Player name is required.", type = "error")
      return()
    }

    if (new != old && new %in% players()$player) {
      showNotification("Another player already has this name.", type = "error")
      return()
    }

    p <- players()
    m <- matches()

    pos <- which(p$player == old)
    p$player[pos] <- new
    p$base_rating[pos] <- as.numeric(input$edit_player_base_rating)

    # Rename player in historical matches too.
    m$player_a[m$player_a == old] <- new
    m$player_b[m$player_b == old] <- new

    rebuilt <- recalculate_all(m, p)

    tryCatch({
      save_everything(p, rebuilt)
      players(p)
      matches(rebuilt)
      selected_player_name(new)
      showNotification("Player updated everywhere.", type = "message")
    }, error = function(e) {
      showNotification(paste("Player update failed:", conditionMessage(e)), type = "error")
    })
  })

  observeEvent(input$delete_player, {
    req(admin_logged_in(), selected_player_name())

    p <- selected_player_name()
    used <- any(matches()$player_a == p | matches()$player_b == p)

    if (used) {
      showNotification(
        "This player has match history. Delete or edit those matches first.",
        type = "error",
        duration = NULL
      )
      return()
    }

    showModal(
      modalDialog(
        title = "Delete Player",
        paste0("Delete ", p, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_player", "Delete Player", class = "btn-danger")
        )
      )
    )
  })

  observeEvent(input$confirm_delete_player, {
    req(admin_logged_in(), selected_player_name())

    p <- players() %>% filter(player != selected_player_name())

    tryCatch({
      save_everything(p, matches())
      players(p)
      selected_player_name(NULL)
      removeModal()
      showNotification("Player deleted.", type = "message")
    }, error = function(e) {
      removeModal()
      showNotification(paste("Delete failed:", conditionMessage(e)), type = "error")
    })
  })

  # ----------------------------
  # TOURNAMENT DRAW ADMIN
  # ----------------------------

  observeEvent(input$generate_draw, {
    req(admin_logged_in())

    tryCatch({
      tournament_draw(make_draw_16(rankdat()))
      showNotification("New draw generated.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
    })
  })

  output$admin_draw_table <- renderDT({
    req(admin_logged_in())

    d <- tournament_draw()

    if (is.null(d)) {
      return(datatable(data.frame(Message = "No draw yet."), rownames = FALSE))
    }

    datatable(
      d,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })

  observeEvent(input$save_draw, {
    req(admin_logged_in())

    d <- tournament_draw()

    if (is.null(d)) {
      showNotification("Generate a draw first.", type = "error")
      return()
    }

    tryCatch({
      dropbox_upload_csv(d, DROPBOX_TOURNAMENT_PATH)
      showNotification("Tournament draw saved to Dropbox.", type = "message")
    }, error = function(e) {
      showNotification(paste("Draw save failed:", conditionMessage(e)), type = "error")
    })
  })

  # ----------------------------
  # DOWNLOADS
  # ----------------------------

  output$download_matches <- downloadHandler(
    filename = function() paste0("matches_", Sys.Date(), ".csv"),
    content = function(file) write_csv(matches(), file)
  )

  output$download_ranking <- downloadHandler(
    filename = function() paste0("ranking_", Sys.Date(), ".csv"),
    content = function(file) write_csv(rankdat(), file)
  )
}

shinyApp(ui, server)
