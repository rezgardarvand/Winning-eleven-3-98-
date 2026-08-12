
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
DROPBOX_APP_KEY <- Sys.getenv("DROPBOX_APP_KEY")
DROPBOX_APP_SECRET <- Sys.getenv("DROPBOX_APP_SECRET")
DROPBOX_REFRESH_TOKEN <- Sys.getenv("DROPBOX_REFRESH_TOKEN")

DROPBOX_MATCHES_PATH <- Sys.getenv("DROPBOX_MATCHES_PATH", unset = "/matches.csv")
DROPBOX_PLAYERS_PATH <- "/players.csv"
DROPBOX_RANKING_PATH <- "/ranking.csv"
DROPBOX_PLAYERS_STATS_PATH <- "/players_stats.csv"
DROPBOX_RATING_HISTORY_PATH <- "/rating_history.csv"
DROPBOX_TOURNAMENT_PATH <- "/tournament_draw.csv"
DROPBOX_TOURNAMENT_ARCHIVE_PATH <- "/tournament_draws.csv"
DROPBOX_GROUP_DRAWS_PATH <- "/tournament_group_draws.csv"

participation_bonus <- 11
RATING_BASELINE <- 300

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
  "Tanha" = "Ali Tanha",
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
      base_rating = RATING_BASELINE,
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
  if (!nzchar(ADMIN_PASSWORD_HASH)) return(FALSE)

  stored <- trimws(ADMIN_PASSWORD_HASH)

  # Preferred secure mode:
  # ADMIN_PASSWORD_HASH contains a 64-character SHA-256 hash.
  if (grepl("^[A-Fa-f0-9]{64}$", stored)) {
    return(identical(tolower(hash_password(x)), tolower(stored)))
  }

  # Backward-compatible mode:
  # If the user stored the actual password in ADMIN_PASSWORD_HASH,
  # accept it directly rather than locking the admin out.
  identical(as.character(x), stored)
}

# ============================================================
# DROPBOX
# ============================================================

# Cached short-lived Dropbox access token.
.dropbox_auth_cache <- new.env(parent = emptyenv())
.dropbox_auth_cache$access_token <- ""
.dropbox_auth_cache$expires_at <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")

dropbox_check_refresh_secrets <- function() {
  missing <- character(0)

  if (!nzchar(DROPBOX_APP_KEY)) missing <- c(missing, "DROPBOX_APP_KEY")
  if (!nzchar(DROPBOX_APP_SECRET)) missing <- c(missing, "DROPBOX_APP_SECRET")
  if (!nzchar(DROPBOX_REFRESH_TOKEN)) missing <- c(missing, "DROPBOX_REFRESH_TOKEN")

  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing Dropbox secret(s): ",
        paste(missing, collapse = ", "),
        ". Add them in Posit Connect Cloud -> Settings -> Variables and Republish."
      )
    )
  }

  invisible(TRUE)
}

dropbox_get_access_token <- function(force_refresh = FALSE) {
  dropbox_check_refresh_secrets()

  now <- Sys.time()

  if (
    !force_refresh &&
    nzchar(.dropbox_auth_cache$access_token) &&
    now < (.dropbox_auth_cache$expires_at - 90)
  ) {
    return(.dropbox_auth_cache$access_token)
  }

  resp <- request("https://api.dropbox.com/oauth2/token") %>%
    req_body_form(
      refresh_token = DROPBOX_REFRESH_TOKEN,
      grant_type = "refresh_token",
      client_id = DROPBOX_APP_KEY,
      client_secret = DROPBOX_APP_SECRET
    ) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  if (resp_status(resp) >= 300) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "")
    stop(
      paste0(
        "Dropbox token refresh failed (HTTP ",
        resp_status(resp),
        "). ",
        body
      )
    )
  }

  token_data <- resp_body_json(resp, simplifyVector = TRUE)

  if (
    is.null(token_data$access_token) ||
    !nzchar(as.character(token_data$access_token))
  ) {
    stop("Dropbox did not return a new access token.")
  }

  expires_in <- suppressWarnings(as.numeric(token_data$expires_in))
  if (is.na(expires_in) || expires_in <= 0) expires_in <- 14400

  .dropbox_auth_cache$access_token <- as.character(token_data$access_token)
  .dropbox_auth_cache$expires_at <- Sys.time() + expires_in

  .dropbox_auth_cache$access_token
}

dropbox_download_csv <- function(path, quiet = TRUE) {
  perform_download <- function(token) {
    request("https://content.dropboxapi.com/2/files/download") %>%
      req_headers(
        Authorization = paste("Bearer", token),
        `Dropbox-API-Arg` = toJSON(
          list(path = path),
          auto_unbox = TRUE
        )
      ) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }

  result <- tryCatch({
    token <- dropbox_get_access_token()
    resp <- perform_download(token)

    if (resp_status(resp) == 401) {
      token <- dropbox_get_access_token(force_refresh = TRUE)
      resp <- perform_download(token)
    }

    status <- resp_status(resp)

    if (status == 409) {
      if (quiet) return(NULL)
      stop(
        paste0(
          "Dropbox file not found or path conflict: ",
          path,
          ". ",
          resp_body_string(resp)
        )
      )
    }

    if (status >= 300) {
      stop(
        paste0(
          "Dropbox read failed for ",
          path,
          " (HTTP ",
          status,
          "). ",
          resp_body_string(resp)
        )
      )
    }

    tmp <- tempfile(fileext = ".csv")
    writeBin(resp_body_raw(resp), tmp)
    read_csv(tmp, show_col_types = FALSE)

  }, error = function(e) {
    if (quiet) return(NULL)
    stop(conditionMessage(e))
  })

  result
}

dropbox_upload_csv <- function(x, path) {
  perform_upload <- function(token, tmp) {
    request("https://content.dropboxapi.com/2/files/upload") %>%
      req_headers(
        Authorization = paste("Bearer", token),
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
      req_body_file(tmp) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }

  dropbox_check_refresh_secrets()

  tmp <- tempfile(fileext = ".csv")
  write_csv(x, tmp)

  token <- dropbox_get_access_token()
  resp <- perform_upload(token, tmp)

  if (resp_status(resp) == 401) {
    token <- dropbox_get_access_token(force_refresh = TRUE)
    resp <- perform_upload(token, tmp)
  }

  if (resp_status(resp) >= 300) {
    stop(
      paste0(
        "Dropbox upload failed for ",
        path,
        " (HTTP ",
        resp_status(resp),
        "). ",
        resp_body_string(resp)
      )
    )
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
  base <- RATING_BASELINE

  pm <- matches %>% filter(player_a == p | player_b == p)

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
  if (nrow(players) == 0) return(tibble())

  out <- lapply(players$player, function(p) {
    pm <- matches %>% filter(player_a == p | player_b == p)

    rating_change_total <- if (nrow(pm)) {
      sum(ifelse(pm$player_a == p, pm$rating_change_a, pm$rating_change_b), na.rm = TRUE)
    } else 0

    participation_total <- if (nrow(pm)) {
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

    gf <- if (nrow(pm)) sum(ifelse(pm$player_a == p, pm$goals_a, pm$goals_b), na.rm = TRUE) else 0
    ga <- if (nrow(pm)) sum(ifelse(pm$player_a == p, pm$goals_b, pm$goals_a), na.rm = TRUE) else 0

    total_points <- rating_change_total + participation_total
    played <- nrow(pm)

    tibble(
      player = p,
      rating = RATING_BASELINE + total_points,
      total_points = total_points,
      points_per_game = ifelse(played == 0, 0, total_points / played),
      matches = played,
      wins = wins,
      losses = losses,
      goals_for = gf,
      goals_against = ga
    )
  })

  bind_rows(out) %>%
    mutate(
      goal_difference = goals_for - goals_against,
      win_rate = ifelse(matches == 0, 0, 100 * wins / matches)
    ) %>%
    arrange(desc(rating), desc(wins), desc(goal_difference), player) %>%
    mutate(rank = row_number()) %>%
    select(
      rank, player, rating,
      total_points, points_per_game,
      matches, wins, losses,
      goals_for, goals_against,
      goal_difference, win_rate
    )
}

build_rating_history_all <- function(players, matches) {
  out <- list()
  k <- 1L

  for (p in players$player) {
    base <- players %>% filter(player == p)
    current <- RATING_BASELINE

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
# FLEXIBLE TOURNAMENT DRAW
# - Any practical participant count (minimum 4)
# - Bracket automatically expands to next power of 2
# - Global ranking positions 1-8 are seeded if participating
# - Seeded players never face each other in the first round
# - All other players are placed randomly
# - BYEs are assigned automatically
# ============================================================

next_power_of_two <- function(n) {
  if (n <= 1) return(1L)
  as.integer(2 ^ ceiling(log2(n)))
}

round_name_from_bracket <- function(bracket_size) {
  if (bracket_size == 4) return("Semifinal")
  if (bracket_size == 8) return("Quarterfinal")
  if (bracket_size == 16) return("Round of 16")
  if (bracket_size == 32) return("Round of 32")
  paste0("Round of ", bracket_size)
}

make_flexible_draw <- function(ranking, participants, cup_id) {
  participants <- unique(trimws(as.character(participants)))
  participants <- participants[nzchar(participants)]

  if (length(participants) < 4) {
    stop("Select at least 4 participants.")
  }

  missing_players <- setdiff(participants, ranking$player)
  if (length(missing_players) > 0) {
    stop(
      paste(
        "These participants are not in the ranking:",
        paste(missing_players, collapse = ", ")
      )
    )
  }

  if (!nzchar(trimws(cup_id))) {
    stop("Cup ID is required.")
  }

  # Re-rank only the players participating in this cup.
  selected_rank <- ranking %>%
    filter(player %in% participants) %>%
    arrange(rank) %>%
    mutate(cup_seed = row_number())

  n_players <- length(participants)
  bracket_size <- next_power_of_two(n_players)
  n_matches <- as.integer(bracket_size / 2L)
  first_round <- round_name_from_bracket(bracket_size)

  # Protected seed count must fit the bracket.
  # 8-slot bracket = max 4 seeds
  # 16-slot bracket = max 8 seeds
  seed_count <- min(8L, n_matches, nrow(selected_rank))

  seeded <- selected_rank %>%
    filter(cup_seed <= seed_count) %>%
    arrange(cup_seed)

  unseeded <- selected_rank %>%
    filter(cup_seed > seed_count) %>%
    pull(player)

  if (length(unseeded) > 0) {
    unseeded <- sample(unseeded, length(unseeded))
  }

  slots <- tibble(
    match_no = seq_len(n_matches),
    player_a = NA_character_,
    player_b = NA_character_,
    seed_a = NA_integer_,
    seed_b = NA_integer_
  )

  # --------------------------------------------------------
  # STANDARD SEEDED KNOCKOUT PLACEMENT
  #
  # In a 16-slot bracket (8 first-round matches):
  # match 1 -> Seed 1
  # match 2 -> Seed 8
  # match 3 -> Seed 4
  # match 4 -> Seed 5
  # match 5 -> Seed 2
  # match 6 -> Seed 7
  # match 7 -> Seed 3
  # match 8 -> Seed 6
  #
  # Therefore:
  # Seed 1 & 4 can meet only in Semifinal
  # Seed 2 & 3 can meet only in Semifinal
  # Seed 1 & 2 can meet only in Final
  # --------------------------------------------------------

  if (n_matches >= 8) {
    standard_seed_to_match <- c(
      `1` = 1L,
      `2` = 5L,
      `3` = 7L,
      `4` = 3L,
      `5` = 4L,
      `6` = 8L,
      `7` = 6L,
      `8` = 2L
    )

    if (n_matches > 8) {
      # Scale the standard 8 seed positions across larger brackets.
      base_positions <- c(1, 5, 7, 3, 4, 8, 6, 2)
      scaled <- pmax(
        1L,
        pmin(
          n_matches,
          round((base_positions - 1) * (n_matches - 1) / 7) + 1L
        )
      )
      standard_seed_to_match <- setNames(as.integer(scaled), as.character(1:8))
    }
  } else {
    # Smaller brackets: spread seeds across separate matches.
    # Example 8-slot bracket: seed 1 vs seed 4 only possible in semifinal.
    if (n_matches == 4) {
      standard_seed_to_match <- c(
        `1` = 1L,
        `2` = 3L,
        `3` = 4L,
        `4` = 2L
      )
    } else if (n_matches == 2) {
      standard_seed_to_match <- c(`1` = 1L, `2` = 2L)
    } else {
      standard_seed_to_match <- setNames(seq_len(n_matches), as.character(seq_len(n_matches)))
    }
  }

  if (nrow(seeded) > 0) {
    for (i in seq_len(nrow(seeded))) {
      s <- seeded$cup_seed[i]
      if (!as.character(s) %in% names(standard_seed_to_match)) next

      mi <- as.integer(standard_seed_to_match[as.character(s)])

      # Protected seed always occupies player_a side of its match.
      slots$player_a[mi] <- seeded$player[i]
      slots$seed_a[mi] <- s
    }
  }

  # Fill completely empty matches first with one unseeded player,
  # avoiding BYE vs BYE.
  empty_matches <- which(is.na(slots$player_a) & is.na(slots$player_b))

  while (length(unseeded) > 0 && length(empty_matches) > 0) {
    mi <- sample(empty_matches, 1)
    slots$player_a[mi] <- unseeded[1]
    unseeded <- unseeded[-1]

    empty_matches <- which(is.na(slots$player_a) & is.na(slots$player_b))
  }

  # Randomly place remaining unseeded players into all remaining open slots.
  if (length(unseeded) > 0) {
    open_positions <- bind_rows(
      tibble(match_no = which(is.na(slots$player_a)), side = "A"),
      tibble(match_no = which(is.na(slots$player_b)), side = "B")
    )

    chosen <- sample(seq_len(nrow(open_positions)), length(unseeded))

    for (i in seq_along(unseeded)) {
      pos <- open_positions[chosen[i], ]

      if (pos$side == "A") {
        slots$player_a[pos$match_no] <- unseeded[i]
      } else {
        slots$player_b[pos$match_no] <- unseeded[i]
      }
    }
  }

  # Remaining open positions become BYEs.
  slots <- slots %>%
    mutate(
      player_a = ifelse(is.na(player_a), "BYE", player_a),
      player_b = ifelse(is.na(player_b), "BYE", player_b),
      bracket_half = ifelse(
        match_no <= ceiling(n_matches / 2),
        "Upper",
        "Lower"
      ),
      bye = player_a == "BYE" | player_b == "BYE",
      pairing = paste0(
        ifelse(is.na(seed_a), "", paste0("#", seed_a, " ")),
        player_a,
        "  vs  ",
        ifelse(is.na(seed_b), "", paste0("#", seed_b, " ")),
        player_b
      ),
      cup_id = trimws(cup_id),
      participant_count = n_players,
      bracket_size = bracket_size,
      round = first_round,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(
      cup_id,
      participant_count,
      bracket_size,
      round,
      match_no,
      bracket_half,
      seed_a,
      player_a,
      seed_b,
      player_b,
      bye,
      pairing,
      generated_at
    )

  # Safety: no protected seeds face one another in first round.
  bad <- slots %>%
    filter(!is.na(seed_a) & !is.na(seed_b))

  if (nrow(bad) > 0) {
    stop("Draw safety check failed: two seeded players were paired together.")
  }

  # Final validation: every selected participant must appear exactly once.
  drawn_players <- c(slots$player_a, slots$player_b)
  drawn_players <- drawn_players[drawn_players != "BYE"]

  missing_from_draw <- setdiff(participants, drawn_players)
  duplicated_in_draw <- unique(drawn_players[duplicated(drawn_players)])

  if (length(missing_from_draw) > 0) {
    stop(
      paste(
        "Draw validation failed. Missing participant(s):",
        paste(missing_from_draw, collapse = ", ")
      )
    )
  }

  if (length(duplicated_in_draw) > 0) {
    stop(
      paste(
        "Draw validation failed. Duplicate participant(s):",
        paste(duplicated_in_draw, collapse = ", ")
      )
    )
  }

  slots
}


# ============================================================
# MODERN TOURNAMENT BRACKET RENDERER
# ============================================================

render_bracket_tree <- function(draw) {
  if (is.null(draw) || nrow(draw) == 0) {
    return(tags$p(
      style = "color:#667085;",
      "No tournament draw has been generated yet."
    ))
  }

  d <- draw %>% arrange(match_no)
  n_matches <- nrow(d)

  if (n_matches < 2) {
    return(tags$p("Not enough matches to build a bracket."))
  }

  half_n <- ceiling(n_matches / 2)
  left_first <- d[seq_len(half_n), , drop = FALSE]
  right_idx <- seq.int(half_n + 1, n_matches)
  right_first <- if (length(right_idx) > 0) d[right_idx, , drop = FALSE] else d[0, , drop = FALSE]

  player_line <- function(player, seed = NA_integer_) {
    cls <- if (identical(as.character(player), "BYE")) "mb-player mb-bye" else "mb-player"
    div(
      class = cls,
      if (!is.na(seed)) span(class = "mb-seed", paste0("#", seed)),
      span(player)
    )
  }

  match_box <- function(r, side = "left") {
    div(
      class = paste("mb-match", if (side == "left") "mb-left-match" else "mb-right-match"),
      div(class = "mb-matchno", paste0("Match ", r$match_no)),
      player_line(r$player_a, r$seed_a),
      player_line(r$player_b, r$seed_b)
    )
  }

  make_next_pairs <- function(ids, prefix) {
    if (length(ids) == 0) return(list())
    groups <- split(ids, ceiling(seq_along(ids) / 2))
    lapply(seq_along(groups), function(i) {
      x <- groups[[i]]
      a <- paste0("Winner Match ", x[1])
      b <- if (length(x) > 1) paste0("Winner Match ", x[2]) else "BYE"
      list(label = paste0(prefix, " ", i), a = a, b = b)
    })
  }

  left_qf <- make_next_pairs(left_first$match_no, "QF")
  right_qf <- make_next_pairs(right_first$match_no, "QF")

  make_placeholder_box <- function(x, side = "left") {
    div(
      class = paste("mb-match", if (side == "left") "mb-left-match" else "mb-right-match"),
      div(class = "mb-matchno", x$label),
      div(class = "mb-player", x$a),
      div(class = "mb-player", x$b)
    )
  }

  left_sf <- list(list(label = "SF", a = "Winner QF 1", b = "Winner QF 2"))
  right_sf <- list(list(label = "SF", a = "Winner QF 1", b = "Winner QF 2"))

  left_col1 <- div(
    class = "mb-col",
    div(class = "mb-title", d$round[1]),
    lapply(seq_len(nrow(left_first)), function(i) match_box(left_first[i, ], "left"))
  )

  left_col2 <- div(
    class = "mb-col mb-round2",
    div(class = "mb-title", "Quarterfinal"),
    lapply(left_qf, function(x) make_placeholder_box(x, "left"))
  )

  left_col3 <- div(
    class = "mb-col mb-round3",
    div(class = "mb-title", "Semifinal"),
    lapply(left_sf, function(x) make_placeholder_box(x, "left"))
  )

  right_col3 <- div(
    class = "mb-col mb-round3",
    div(class = "mb-title", "Semifinal"),
    lapply(right_sf, function(x) make_placeholder_box(x, "right"))
  )

  right_col2 <- div(
    class = "mb-col mb-round2",
    div(class = "mb-title", "Quarterfinal"),
    lapply(right_qf, function(x) make_placeholder_box(x, "right"))
  )

  right_col1 <- div(
    class = "mb-col",
    div(class = "mb-title", d$round[1]),
    lapply(seq_len(nrow(right_first)), function(i) match_box(right_first[i, ], "right"))
  )

  center <- div(
    class = "mb-center",
    div(class = "mb-cup", "🏆"),
    div(
      class = "mb-final",
      div(class = "mb-final-label", "FINAL"),
      div("Winner Left SF"),
      div(class = "mb-vs", "vs"),
      div("Winner Right SF")
    ),
    div(class = "mb-champion", "CHAMPION")
  )

  div(
    class = "modern-bracket-wrap",
    div(
      class = "modern-bracket",
      left_col1,
      left_col2,
      left_col3,
      center,
      right_col3,
      right_col2,
      right_col1
    )
  )
}


# ============================================================
# GROUP STAGE + KNOCKOUT DRAW HELPERS
# ============================================================

empty_group_draw_archive <- function() {
  tibble(
    cup_id = character(),
    group_name = character(),
    group_slot = integer(),
    player = character(),
    global_rank = integer(),
    pot = integer(),
    n_groups = integer(),
    teams_per_group = integer(),
    qualifiers_per_group = integer(),
    generated_at = character()
  )
}

normalize_group_draw_archive <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(empty_group_draw_archive())

  required <- c(
    "cup_id","group_name","group_slot","player","global_rank","pot",
    "n_groups","teams_per_group","qualifiers_per_group","generated_at"
  )

  for (nm in required) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x %>%
    mutate(
      cup_id = as.character(cup_id),
      group_name = as.character(group_name),
      group_slot = as.integer(group_slot),
      player = as.character(player),
      global_rank = as.integer(global_rank),
      pot = as.integer(pot),
      n_groups = as.integer(n_groups),
      teams_per_group = as.integer(teams_per_group),
      qualifiers_per_group = as.integer(qualifiers_per_group),
      generated_at = as.character(generated_at)
    ) %>%
    arrange(cup_id, group_name, group_slot)
}

load_group_draws_from_dropbox <- function() {
  x <- dropbox_download_csv(DROPBOX_GROUP_DRAWS_PATH, quiet = TRUE)
  normalize_group_draw_archive(x)
}

save_group_draws_to_dropbox <- function(x) {
  payload <- normalize_group_draw_archive(x)
  dropbox_upload_csv(payload, DROPBOX_GROUP_DRAWS_PATH)

  verify <- dropbox_download_csv(DROPBOX_GROUP_DRAWS_PATH, quiet = FALSE)
  if (is.null(verify)) {
    stop("Group draw save verification failed.")
  }

  normalize_group_draw_archive(verify)
}

make_group_stage_draw <- function(
  ranking,
  participants,
  cup_id,
  n_groups,
  teams_per_group,
  qualifiers_per_group
) {
  participants <- unique(trimws(as.character(participants)))
  participants <- participants[nzchar(participants)]

  cup_id <- trimws(as.character(cup_id))
  n_groups <- as.integer(n_groups)
  teams_per_group <- as.integer(teams_per_group)
  qualifiers_per_group <- as.integer(qualifiers_per_group)

  if (!nzchar(cup_id)) stop("Cup ID is required.")
  if (n_groups < 2) stop("At least 2 groups are required.")
  if (teams_per_group < 2) stop("Each group must have at least 2 players.")
  if (qualifiers_per_group < 1) stop("At least 1 player must qualify from each group.")
  if (qualifiers_per_group >= teams_per_group) {
    stop("Qualifiers per group must be smaller than players per group.")
  }

  needed <- n_groups * teams_per_group

  if (length(participants) != needed) {
    stop(
      paste0(
        "This setup needs exactly ", needed, " participants (",
        n_groups, " groups × ", teams_per_group, " players). You selected ",
        length(participants), "."
      )
    )
  }

  missing_players <- setdiff(participants, ranking$player)
  if (length(missing_players) > 0) {
    stop(
      paste(
        "These participants are not in Ranking:",
        paste(missing_players, collapse = ", ")
      )
    )
  }

  ranked <- ranking %>%
    filter(player %in% participants) %>%
    arrange(rank) %>%
    mutate(
      draw_order = row_number(),
      pot = ceiling(draw_order / n_groups)
    )

  group_names <- LETTERS[seq_len(n_groups)]
  assignments <- list()

  # Pot system: one player from each pot goes into each group.
  # This separates the strongest ranked players in the early stage.
  for (p in sort(unique(ranked$pot))) {
    pot_players <- ranked %>%
      filter(pot == p) %>%
      arrange(rank)

    target_groups <- sample(group_names, length(group_names), replace = FALSE)

    for (i in seq_len(nrow(pot_players))) {
      assignments[[length(assignments) + 1]] <- tibble(
        cup_id = cup_id,
        group_name = target_groups[i],
        group_slot = p,
        player = pot_players$player[i],
        global_rank = pot_players$rank[i],
        pot = p,
        n_groups = n_groups,
        teams_per_group = teams_per_group,
        qualifiers_per_group = qualifiers_per_group,
        generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    }
  }

  d <- bind_rows(assignments) %>%
    arrange(group_name, group_slot)

  if (nrow(d) != needed) stop("Group draw validation failed: wrong participant count.")
  if (anyDuplicated(d$player)) stop("Group draw validation failed: duplicate player.")
  if (any(table(d$group_name) != teams_per_group)) {
    stop("Group draw validation failed: unequal group sizes.")
  }

  d
}

build_group_knockout_template <- function(n_groups, qualifiers_per_group) {
  n_groups <- as.integer(n_groups)
  qualifiers_per_group <- as.integer(qualifiers_per_group)

  q <- expand.grid(
    place = seq_len(qualifiers_per_group),
    group_name = LETTERS[seq_len(n_groups)],
    stringsAsFactors = FALSE
  ) %>%
    arrange(place, group_name) %>%
    mutate(label = paste0(group_name, place))

  remaining <- q$label
  pairings <- list()

  while (length(remaining) >= 2) {
    a <- remaining[1]
    a_group <- substr(a, 1, 1)

    # Prefer another group and the lowest available qualification position.
    candidates <- remaining[
      remaining != a &
      substr(remaining, 1, 1) != a_group
    ]

    if (length(candidates) == 0) {
      candidates <- remaining[remaining != a]
    }

    b <- tail(candidates, 1)

    pairings[[length(pairings) + 1]] <- tibble(
      match_no = length(pairings) + 1L,
      player_a = a,
      player_b = b
    )

    remaining <- setdiff(remaining, c(a, b))
  }

  if (length(remaining) == 1) {
    pairings[[length(pairings) + 1]] <- tibble(
      match_no = length(pairings) + 1L,
      player_a = remaining[1],
      player_b = "BYE"
    )
  }

  bind_rows(pairings)
}

render_group_cards <- function(d) {
  if (is.null(d) || nrow(d) == 0) {
    return(tags$p("No group draw available."))
  }

  groups <- unique(d$group_name)

  div(
    class = "group-grid",
    lapply(groups, function(g) {
      gd <- d %>%
        filter(group_name == g) %>%
        arrange(group_slot)

      div(
        class = "group-card",
        div(class = "group-card-title", paste("Group", g)),
        lapply(seq_len(nrow(gd)), function(i) {
          r <- gd[i,]
          div(
            class = "group-player-row",
            span(class = "group-rank-badge", paste0("#", r$global_rank)),
            span(class = "group-player-name", r$player),
            span(class = "group-pot", paste0("Pot ", r$pot))
          )
        })
      )
    })
  )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
      tags$style(HTML('
/* ============================================================
   MODERN SYMMETRIC TOURNAMENT BRACKET
   ============================================================ */
.modern-bracket-wrap{
  width:100%;
  overflow-x:auto;
  padding:20px 6px 30px;
}
.modern-bracket{
  display:grid;
  grid-template-columns: 1fr 1fr 1fr 150px 1fr 1fr 1fr;
  gap:18px;
  min-width:1250px;
  align-items:stretch;
}
.mb-col{
  display:flex;
  flex-direction:column;
  justify-content:space-around;
  min-height:620px;
}
.mb-title{
  text-align:center;
  font-weight:800;
  font-size:14px;
  margin-bottom:12px;
  opacity:.85;
}
.mb-match{
  position:relative;
  background:#171a1d;
  border:1px solid #3f4650;
  border-radius:10px;
  overflow:visible;
  box-shadow:0 4px 14px rgba(0,0,0,.18);
  min-height:72px;
}
.mb-player{
  padding:9px 11px;
  min-height:35px;
  display:flex;
  align-items:center;
  gap:7px;
  font-weight:600;
  white-space:nowrap;
}
.mb-player + .mb-player{
  border-top:1px solid #343a40;
}
.mb-seed{
  font-size:11px;
  opacity:.6;
}
.mb-bye{
  opacity:.45;
  font-style:italic;
}
.mb-matchno{
  position:absolute;
  top:-10px;
  left:8px;
  font-size:10px;
  background:#171a1d;
  padding:1px 5px;
  opacity:.7;
  border-radius:4px;
}
.mb-center{
  display:flex;
  flex-direction:column;
  align-items:center;
  justify-content:center;
  min-height:620px;
}
.mb-cup{
  width:84px;
  height:84px;
  border-radius:50%;
  display:flex;
  align-items:center;
  justify-content:center;
  font-size:44px;
  margin-bottom:16px;
  background:#23272b;
  border:1px solid #4a5158;
  box-shadow:0 6px 22px rgba(0,0,0,.28);
}
.mb-final{
  width:145px;
  text-align:center;
  padding:14px 10px;
  border-radius:12px;
  background:#171a1d;
  border:1px solid #5a626b;
  font-weight:800;
}
.mb-champion{
  margin-top:12px;
  font-size:12px;
  opacity:.7;
}

/* connecting lines - left side */
.mb-left .mb-match::after{
  content:"";
  position:absolute;
  top:50%;
  right:-18px;
  width:18px;
  border-top:2px solid #59616a;
}
.mb-left .mb-match::before{
  content:"";
  position:absolute;
  top:50%;
  right:-18px;
  height:52%;
  border-right:2px solid #59616a;
}

/* connecting lines - right side */
.mb-right .mb-match::after{
  content:"";
  position:absolute;
  top:50%;
  left:-18px;
  width:18px;
  border-top:2px solid #59616a;
}
.mb-right .mb-match::before{
  content:"";
  position:absolute;
  top:50%;
  left:-18px;
  height:52%;
  border-left:2px solid #59616a;
}

.mb-round2 .mb-match,
.mb-round3 .mb-match{
  margin-top:28px;
  margin-bottom:28px;
}

.mb-side-label{
  text-align:center;
  font-size:12px;
  opacity:.55;
  margin-bottom:6px;
}

@media(max-width:900px){
  .modern-bracket{min-width:1120px; gap:14px;}
  .mb-player{font-size:13px;}
}
')),
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
    ")),
    tags$style(HTML("
      /* Readable tournament cards on desktop + mobile */
      .modern-bracket-wrap {
        background: linear-gradient(180deg,#f8fafc 0%,#eef2f6 100%);
        border-radius: 16px;
        padding: 18px;
      }
      .mb-match, .mb-final {
        background: #ffffff !important;
        border: 1px solid #cbd5e1 !important;
        box-shadow: 0 5px 16px rgba(15,23,42,.10) !important;
        color: #111827 !important;
      }
      .mb-player {
        color: #111827 !important;
        font-weight: 700;
      }
      .mb-player + .mb-player {
        border-top: 1px solid #e5e7eb !important;
      }
      .mb-title, .mb-matchno, .mb-champion {
        color: #334155 !important;
      }
      .mb-matchno {
        background: #f8fafc !important;
        border: 1px solid #dbe3ec;
      }
      .mb-bye {
        color: #94a3b8 !important;
      }
      .mb-left .mb-match::after,
      .mb-right .mb-match::after {
        border-top-color: #64748b !important;
      }
      .mb-left .mb-match::before {
        border-right-color: #64748b !important;
      }
      .mb-right .mb-match::before {
        border-left-color: #64748b !important;
      }
      .mb-cup {
        background: #ffffff !important;
        border: 1px solid #cbd5e1 !important;
      }

      .group-grid {
        display:grid;
        grid-template-columns:repeat(auto-fit,minmax(240px,1fr));
        gap:16px;
      }
      .group-card {
        background:#ffffff;
        border:1px solid #d0d5dd;
        border-radius:14px;
        box-shadow:0 4px 12px rgba(15,23,42,.06);
        overflow:hidden;
      }
      .group-card-title {
        background:#0f172a;
        color:#ffffff;
        font-weight:800;
        font-size:18px;
        padding:12px 14px;
      }
      .group-player-row {
        display:flex;
        align-items:center;
        gap:10px;
        padding:11px 14px;
        border-top:1px solid #eef2f6;
        color:#111827;
      }
      .group-rank-badge {
        min-width:36px;
        font-weight:800;
        color:#475569;
      }
      .group-player-name {
        flex:1;
        font-weight:700;
      }
      .group-pot {
        color:#64748b;
        font-size:12px;
      }

      @media (max-width:768px) {
        .modern-bracket-wrap {
          padding: 10px;
          border-radius: 12px;
        }
        .modern-bracket {
          min-width: 980px !important;
          gap: 12px !important;
        }
        .mb-col {
          min-width: 150px !important;
        }
        .mb-match {
          min-height: 62px !important;
        }
        .mb-player {
          padding: 8px 9px !important;
          font-size: 12px !important;
        }
        .mb-title {
          font-size: 12px !important;
        }
        .group-grid {
          grid-template-columns:1fr;
        }
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
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Rating"), div(class="metric",textOutput("m_rating")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Total Points"), div(class="metric",textOutput("m_total_points")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Points / Game"), div(class="metric",textOutput("m_points_per_game")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Rank"), div(class="metric",textOutput("m_rank"))))
      ),
      fluidRow(
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Matches"), div(class="metric",textOutput("m_matches")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Wins"), div(class="metric",textOutput("m_wins")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Win rate"), div(class="metric",textOutput("m_winrate")))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Goal diff"), div(class="metric",textOutput("m_gd"))))
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
      "Head-to-Head",
      fluidRow(
        column(
          6,
          class = "mobile-stack",
          div(
            class = "cardx",
            h3("Head-to-Head"),
            selectInput("h2h_player_a", "Player A", choices = NULL),
            selectInput("h2h_player_b", "Player B", choices = NULL)
          )
        ),
        column(
          6,
          class = "mobile-stack",
          div(
            class = "cardx",
            h3("Summary"),
            uiOutput("h2h_summary")
          )
        )
      ),
      div(
        class = "cardx",
        h3("Direct Match History"),
        DTOutput("h2h_matches")
      )
    ),

    tabPanel(
      "Records",
      fluidRow(
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Highest Rating"), div(class="metric", textOutput("rec_high_rating")), textOutput("rec_high_rating_player"))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Most Wins"), div(class="metric", textOutput("rec_most_wins")), textOutput("rec_most_wins_player"))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Top Scorer"), div(class="metric", textOutput("rec_top_goals")), textOutput("rec_top_goals_player"))),
        column(3, class="mobile-stack", div(class="cardx", div(class="metric-label","Best Win Rate"), div(class="metric", textOutput("rec_best_wr")), textOutput("rec_best_wr_player")))
      ),
      fluidRow(
        column(
          6,
          class="mobile-stack",
          div(
            class="cardx",
            h3("All-Time Records"),
            tableOutput("records_table")
          )
        ),
        column(
          6,
          class="mobile-stack",
          div(
            class="cardx",
            h3("Biggest Win"),
            uiOutput("biggest_win_ui")
          )
        )
      )
    ),

    tabPanel(
      "Cup History",
      div(
        class = "cardx",
        selectInput("cup_history_id", "Cup", choices = NULL),
        uiOutput("cup_summary")
      ),
      div(
        class = "cardx",
        h3("Cup Matches"),
        DTOutput("cup_matches_table")
      ),
      div(
        class = "cardx",
        h3("Cup Player Statistics"),
        DTOutput("cup_stats_table")
      )
    ),

    tabPanel(
      "Tournament",
      tabsetPanel(
        id = "public_tournament_mode",

        tabPanel(
          "Knockout",
          div(
            class = "cardx",
            h3("Knockout Tournament Draws"),
            p("Choose a cup to view its complete saved knockout draw."),
            selectInput(
              "public_tournament_cup",
              "Cup",
              choices = NULL
            )
          ),
          uiOutput("tournament_public")
        ),

        tabPanel(
          "Group Stage",
          div(
            class = "cardx",
            h3("Group Stage Draws"),
            p("Choose a cup to view its groups and the planned knockout qualification path."),
            selectInput(
              "public_group_cup",
              "Cup",
              choices = NULL
            )
          ),
          uiOutput("public_group_draw")
        )
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
          tags$li(tags$b("Every player starts at Rating 300.")),
          tags$li("There is no Reset system and no automatic Restore."),
          tags$li("Dropbox Recovery runs only when the Admin manually presses the recovery button."),
          tags$li("All matches in matches.csv are processed permanently in match_id order."),
          tags$li("Each match uses the ratings produced by all earlier matches."),
          tags$li(tags$b("Total Points"), " = Rating − 300."),
          tags$li(tags$b("Points/Game"), " = Total Points divided by Matches."),
          tags$li(tags$b("+11 participation points"), " once per player for each Cup ID."),
          tags$li(
            tags$b("Tournament draw: "),
            "the top 8 participating players are re-ranked as cup Seeds 1–8; all other participants are randomly placed and BYEs are assigned automatically when the field is not a power of two."
          ),
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
# TOURNAMENT DRAW ARCHIVE HELPERS
# ============================================================

normalize_tournament_archive <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)

  required <- c(
    "cup_id","participant_count","bracket_size","round",
    "match_no","bracket_half","seed_a","player_a",
    "seed_b","player_b","bye","pairing","generated_at"
  )

  for (nm in required) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x %>%
    mutate(
      cup_id = as.character(cup_id),
      participant_count = as.integer(participant_count),
      bracket_size = as.integer(bracket_size),
      round = as.character(round),
      match_no = as.integer(match_no),
      bracket_half = as.character(bracket_half),
      seed_a = suppressWarnings(as.integer(seed_a)),
      player_a = as.character(player_a),
      seed_b = suppressWarnings(as.integer(seed_b)),
      player_b = as.character(player_b),
      bye = as.logical(bye),
      pairing = as.character(pairing),
      generated_at = as.character(generated_at)
    ) %>%
    arrange(cup_id, match_no)
}

save_tournament_archive <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    # Write an empty structured file so deletion of the last cup persists.
    empty <- tibble(
      cup_id = character(),
      participant_count = integer(),
      bracket_size = integer(),
      round = character(),
      match_no = integer(),
      bracket_half = character(),
      seed_a = integer(),
      player_a = character(),
      seed_b = integer(),
      player_b = character(),
      bye = logical(),
      pairing = character(),
      generated_at = character()
    )
    dropbox_upload_csv(empty, DROPBOX_TOURNAMENT_ARCHIVE_PATH)
    return(invisible(TRUE))
  }

  dropbox_upload_csv(
    normalize_tournament_archive(x),
    DROPBOX_TOURNAMENT_ARCHIVE_PATH
  )
  invisible(TRUE)
}

rebuild_draw_fields <- function(d, ranking) {
  if (is.null(d) || nrow(d) == 0) return(d)

  participants <- unique(c(d$player_a, d$player_b))
  participants <- participants[participants != "BYE"]

  bracket_size <- as.integer(d$bracket_size[1])
  n_matches <- as.integer(bracket_size / 2L)

  # Protected seed count must match the bracket:
  # 4-slot bracket  -> max 2 seeds
  # 8-slot bracket  -> max 4 seeds
  # 16-slot+        -> max 8 seeds
  seed_count <- min(8L, n_matches, length(participants))

  cup_rank <- ranking %>%
    filter(player %in% participants) %>%
    arrange(rank) %>%
    mutate(cup_seed = row_number())

  seed_map <- setNames(
    ifelse(
      cup_rank$cup_seed <= seed_count,
      cup_rank$cup_seed,
      NA_integer_
    ),
    cup_rank$player
  )

  d %>%
    mutate(
      seed_a = ifelse(
        player_a != "BYE" & player_a %in% names(seed_map),
        as.integer(seed_map[player_a]),
        NA_integer_
      ),
      seed_b = ifelse(
        player_b != "BYE" & player_b %in% names(seed_map),
        as.integer(seed_map[player_b]),
        NA_integer_
      ),
      bye = player_a == "BYE" | player_b == "BYE",
      pairing = paste0(
        ifelse(is.na(seed_a), "", paste0("#", seed_a, " ")),
        player_a,
        "  vs  ",
        ifelse(is.na(seed_b), "", paste0("#", seed_b, " ")),
        player_b
      )
    )
}

validate_manual_draw <- function(d) {
  if (is.null(d) || nrow(d) == 0) stop("No draw loaded.")

  real_players <- c(d$player_a, d$player_b)
  real_players <- real_players[real_players != "BYE"]

  dup <- unique(real_players[duplicated(real_players)])
  if (length(dup) > 0) {
    stop(
      paste(
        "A player appears more than once:",
        paste(dup, collapse = ", ")
      )
    )
  }

  # Only TRUE protected seeds are stored as non-NA seed_a/seed_b.
  bad_seed <- d %>%
    filter(!is.na(seed_a) & !is.na(seed_b))

  if (nrow(bad_seed) > 0) {
    stop(
      "Two protected seeds cannot face each other in the first round."
    )
  }

  invisible(TRUE)
}
# ============================================================
# AUTHORITATIVE TOURNAMENT ARCHIVE I/O
# ============================================================

empty_tournament_archive <- function() {
  tibble(
    cup_id = character(),
    participant_count = integer(),
    bracket_size = integer(),
    round = character(),
    match_no = integer(),
    bracket_half = character(),
    seed_a = integer(),
    player_a = character(),
    seed_b = integer(),
    player_b = character(),
    bye = logical(),
    pairing = character(),
    generated_at = character()
  )
}

load_tournament_archive_from_dropbox <- function() {
  arch <- dropbox_download_csv(
    DROPBOX_TOURNAMENT_ARCHIVE_PATH,
    quiet = FALSE
  )

  # Existing archive file is authoritative even when empty.
  if (!is.null(arch)) {
    if (nrow(arch) == 0) return(empty_tournament_archive())
    return(normalize_tournament_archive(arch))
  }

  empty_tournament_archive()
}

save_and_verify_tournament_archive <- function(x, expected_cup = NULL) {
  payload <- if (is.null(x) || nrow(x) == 0) {
    empty_tournament_archive()
  } else {
    normalize_tournament_archive(x)
  }

  # 1) Persist to Dropbox.
  dropbox_upload_csv(payload, DROPBOX_TOURNAMENT_ARCHIVE_PATH)

  # 2) Read the exact same file back from Dropbox.
  verify <- dropbox_download_csv(
    DROPBOX_TOURNAMENT_ARCHIVE_PATH,
    quiet = FALSE
  )

  if (is.null(verify)) {
    stop("Dropbox verification failed: tournament_draws.csv could not be read back after saving.")
  }

  verify <- if (nrow(verify) == 0) {
    empty_tournament_archive()
  } else {
    normalize_tournament_archive(verify)
  }

  # 3) If saving a cup, verify that cup actually exists in Dropbox.
  if (!is.null(expected_cup) && nzchar(expected_cup)) {
    if (nrow(verify) == 0 || !expected_cup %in% verify$cup_id) {
      stop(
        paste0(
          "Dropbox verification failed: ",
          expected_cup,
          " was not found in tournament_draws.csv after upload."
        )
      )
    }
  }

  verify
}

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  group_draw_archive <- reactiveVal(
    tryCatch(
      load_group_draws_from_dropbox(),
      error = function(e) empty_group_draw_archive()
    )
  )

  current_group_draw <- reactiveVal(NULL)


  players <- reactiveVal(load_players())
  matches <- reactiveVal(recalculate_all(load_matches(), load_players()))
  admin_logged_in <- reactiveVal(FALSE)

  selected_match_id <- reactiveVal(NULL)
  selected_player_name <- reactiveVal(NULL)
  saved_archive <- tryCatch(
    load_tournament_archive_from_dropbox(),
    error = function(e) {
      message("Tournament archive startup load failed: ", conditionMessage(e))
      empty_tournament_archive()
    }
  )

  tournament_archive <- reactiveVal(saved_archive)
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

    current_a <- isolate(input$h2h_player_a)
    current_b <- isolate(input$h2h_player_b)

    selected_a <- if (!is.null(current_a) && current_a %in% p) current_a else if (length(p) >= 1) p[1] else character(0)
    selected_b <- if (!is.null(current_b) && current_b %in% p && current_b != selected_a) current_b else if (length(p) >= 2) p[2] else selected_a

    updateSelectInput(session, "h2h_player_a", choices = p, selected = selected_a)
    updateSelectInput(session, "h2h_player_b", choices = p, selected = selected_b)

    cups <- sort(unique(matches()$cup))
    cups <- cups[!is.na(cups) & nzchar(cups)]

    old_cup <- isolate(input$cup_history_id)
    selected_cup <- if (!is.null(old_cup) && old_cup %in% cups) old_cup else if (length(cups)) cups[1] else character(0)

    updateSelectInput(session, "cup_history_id", choices = cups, selected = selected_cup)

    # Tournament participant selector follows the current ranking.
    ranked_players <- rankdat()$player
    current_participants <- isolate(input$draw_participants)

    keep_selected <- if (!is.null(current_participants)) {
      intersect(current_participants, ranked_players)
    } else {
      character(0)
    }

    updateSelectizeInput(
      session,
      "draw_participants",
      choices = ranked_players,
      selected = keep_selected,
      server = TRUE
    )

    current_group_selected <- isolate(input$group_draw_participants)
    keep_group_selected <- if (!is.null(current_group_selected)) {
      intersect(current_group_selected, ranked_players)
    } else {
      character(0)
    }

    updateSelectizeInput(
      session,
      "group_draw_participants",
      choices = ranked_players,
      selected = keep_group_selected,
      server = TRUE
    )
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
        mutate(
          total_points = ifelse(total_points > 0, paste0("+", total_points), as.character(total_points)),
          points_per_game = round(points_per_game, 2),
          win_rate = paste0(round(win_rate, 1), "%")
        ),
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
  output$m_total_points <- renderText({
    x <- stats()$total_points
    ifelse(x > 0, paste0("+", x), as.character(x))
  })
  output$m_points_per_game <- renderText(round(stats()$points_per_game, 2))
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
  # HEAD TO HEAD
  # ----------------------------

  h2h_data <- reactive({
    req(input$h2h_player_a, input$h2h_player_b)

    a <- input$h2h_player_a
    b <- input$h2h_player_b

    if (a == b) return(empty_matches)

    matches() %>%
      filter(
        (player_a == a & player_b == b) |
          (player_a == b & player_b == a)
      ) %>%
      arrange(desc(match_id))
  })

  output$h2h_summary <- renderUI({
    req(input$h2h_player_a, input$h2h_player_b)

    a <- input$h2h_player_a
    b <- input$h2h_player_b

    if (a == b) {
      return(tags$p(style="color:#b42318;", "Choose two different players."))
    }

    d <- h2h_data()

    if (nrow(d) == 0) {
      return(tags$p(style="color:#667085;", "These players have not played each other yet."))
    }

    a_wins <- sum(
      (d$player_a == a & d$goals_a > d$goals_b) |
        (d$player_b == a & d$goals_b > d$goals_a)
    )
    b_wins <- sum(
      (d$player_a == b & d$goals_a > d$goals_b) |
        (d$player_b == b & d$goals_b > d$goals_a)
    )
    draws <- nrow(d) - a_wins - b_wins

    a_goals <- sum(ifelse(d$player_a == a, d$goals_a, d$goals_b), na.rm = TRUE)
    b_goals <- sum(ifelse(d$player_a == b, d$goals_a, d$goals_b), na.rm = TRUE)

    tagList(
      fluidRow(
        column(4, div(class="metric-label", a), div(class="metric", a_wins), "wins"),
        column(4, div(class="metric-label", "Draws"), div(class="metric", draws)),
        column(4, div(class="metric-label", b), div(class="metric", b_wins), "wins")
      ),
      tags$hr(),
      tags$b(paste0("Goals: ", a, " ", a_goals, " – ", b_goals, " ", b)),
      tags$br(),
      tags$span(style="color:#667085;", paste0("Direct matches: ", nrow(d)))
    )
  })

  output$h2h_matches <- renderDT({
    req(input$h2h_player_a, input$h2h_player_b)

    a <- input$h2h_player_a
    b <- input$h2h_player_b
    d <- h2h_data()

    if (a == b) {
      return(datatable(data.frame(Message = "Choose two different players."), rownames = FALSE))
    }

    if (nrow(d) == 0) {
      return(datatable(data.frame(Message = "No direct matches yet."), rownames = FALSE))
    }

    datatable(
      d %>%
        transmute(
          Date = date,
          Cup = cup,
          Stage = stage,
          `Player A` = player_a,
          Score = paste(goals_a, "-", goals_b),
          `Player B` = player_b,
          `A Change` = ifelse(rating_change_a > 0, paste0("+", rating_change_a), as.character(rating_change_a)),
          `B Change` = ifelse(rating_change_b > 0, paste0("+", rating_change_b), as.character(rating_change_b))
        ),
      rownames = FALSE,
      options = list(pageLength = 15, dom = "tip", scrollX = TRUE)
    )
  })

  # ----------------------------
  # RECORDS
  # ----------------------------

  records_data <- reactive({
    r <- rankdat()
    if (nrow(r) == 0) return(NULL)

    eligible_wr <- r %>% filter(matches > 0)

    high_rating <- r %>% arrange(desc(rating), rank) %>% slice(1)
    most_wins <- r %>% arrange(desc(wins), desc(rating)) %>% slice(1)
    top_goals <- r %>% arrange(desc(goals_for), desc(rating)) %>% slice(1)

    best_wr <- if (nrow(eligible_wr)) {
      eligible_wr %>%
        arrange(desc(win_rate), desc(matches), desc(rating)) %>%
        slice(1)
    } else {
      r %>% slice(1) %>% mutate(win_rate = 0)
    }

    list(
      high_rating = high_rating,
      most_wins = most_wins,
      top_goals = top_goals,
      best_wr = best_wr
    )
  })

  output$rec_high_rating <- renderText({
    req(records_data())
    records_data()$high_rating$rating
  })
  output$rec_high_rating_player <- renderText({
    req(records_data())
    records_data()$high_rating$player
  })

  output$rec_most_wins <- renderText({
    req(records_data())
    records_data()$most_wins$wins
  })
  output$rec_most_wins_player <- renderText({
    req(records_data())
    records_data()$most_wins$player
  })

  output$rec_top_goals <- renderText({
    req(records_data())
    records_data()$top_goals$goals_for
  })
  output$rec_top_goals_player <- renderText({
    req(records_data())
    records_data()$top_goals$player
  })

  output$rec_best_wr <- renderText({
    req(records_data())
    paste0(round(records_data()$best_wr$win_rate, 1), "%")
  })
  output$rec_best_wr_player <- renderText({
    req(records_data())
    records_data()$best_wr$player
  })

  output$records_table <- renderTable({
    r <- rankdat()
    if (nrow(r) == 0) return(data.frame())

    best_gd <- r %>% arrange(desc(goal_difference), desc(rating)) %>% slice(1)
    most_matches <- r %>% arrange(desc(matches), desc(rating)) %>% slice(1)
    most_goals_against <- r %>% arrange(desc(goals_against), desc(matches)) %>% slice(1)

    data.frame(
      Record = c(
        "Best Goal Difference",
        "Most Matches",
        "Most Goals Against"
      ),
      Player = c(
        best_gd$player,
        most_matches$player,
        most_goals_against$player
      ),
      Value = c(
        best_gd$goal_difference,
        most_matches$matches,
        most_goals_against$goals_against
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$biggest_win_ui <- renderUI({
    m <- matches()

    if (nrow(m) == 0) {
      return(tags$p(style="color:#667085;", "No matches yet."))
    }

    d <- m %>%
      mutate(margin = abs(goals_a - goals_b)) %>%
      arrange(desc(margin), desc(match_id)) %>%
      slice(1)

    winner <- if (d$goals_a > d$goals_b) d$player_a
      else if (d$goals_b > d$goals_a) d$player_b
      else "Draw"

    tagList(
      div(class="metric", paste0(d$goals_a, " – ", d$goals_b)),
      tags$b(paste(d$player_a, "vs", d$player_b)),
      tags$br(),
      tags$span(style="color:#667085;", paste0("Winner: ", winner, " • Margin: ", d$margin, " goals")),
      tags$br(),
      tags$span(style="color:#667085;", paste0(d$cup, " • ", d$stage, " • ", d$date))
    )
  })

  # ----------------------------
  # CUP HISTORY
  # ----------------------------

  cup_data <- reactive({
    req(input$cup_history_id)
    matches() %>%
      filter(cup == input$cup_history_id) %>%
      arrange(match_id)
  })

  cup_stats <- reactive({
    d <- cup_data()
    if (nrow(d) == 0) return(tibble())

    plist <- sort(unique(c(d$player_a, d$player_b)))

    bind_rows(lapply(plist, function(p) {
      pm <- d %>% filter(player_a == p | player_b == p)

      gf <- sum(ifelse(pm$player_a == p, pm$goals_a, pm$goals_b), na.rm = TRUE)
      ga <- sum(ifelse(pm$player_a == p, pm$goals_b, pm$goals_a), na.rm = TRUE)

      wins <- sum(
        (pm$player_a == p & pm$goals_a > pm$goals_b) |
          (pm$player_b == p & pm$goals_b > pm$goals_a)
      )
      losses <- sum(
        (pm$player_a == p & pm$goals_a < pm$goals_b) |
          (pm$player_b == p & pm$goals_b < pm$goals_a)
      )
      draws <- nrow(pm) - wins - losses

      tibble(
        Player = p,
        Matches = nrow(pm),
        Wins = wins,
        Draws = draws,
        Losses = losses,
        GF = gf,
        GA = ga,
        GD = gf - ga
      )
    })) %>%
      arrange(desc(Wins), desc(GD), desc(GF), Player)
  })

  output$cup_summary <- renderUI({
    d <- cup_data()

    if (nrow(d) == 0) {
      return(tags$p(style="color:#667085;", "No matches in this cup."))
    }

    participants <- length(unique(c(d$player_a, d$player_b)))

    final_match <- d %>%
      filter(stage == "Final") %>%
      arrange(desc(match_id)) %>%
      slice_head(n = 1)

    champion <- "Not decided / not recorded"
    if (nrow(final_match) == 1 && final_match$goals_a != final_match$goals_b) {
      champion <- if (final_match$goals_a > final_match$goals_b) {
        final_match$player_a
      } else {
        final_match$player_b
      }
    }

    fluidRow(
      column(4, div(class="metric-label","Matches"), div(class="metric", nrow(d))),
      column(4, div(class="metric-label","Players"), div(class="metric", participants)),
      column(4, div(class="metric-label","Champion"), div(style="font-size:20px;font-weight:800;", champion))
    )
  })

  output$cup_matches_table <- renderDT({
    d <- cup_data()

    if (nrow(d) == 0) {
      return(datatable(data.frame(Message = "No matches in this cup."), rownames = FALSE))
    }

    datatable(
      d %>%
        arrange(desc(match_id)) %>%
        transmute(
          Date = date,
          Stage = stage,
          `Player A` = player_a,
          Score = paste(goals_a, "-", goals_b),
          `Player B` = player_b
        ),
      rownames = FALSE,
      options = list(pageLength = 20, dom = "tip", scrollX = TRUE)
    )
  })

  output$cup_stats_table <- renderDT({
    d <- cup_stats()

    if (nrow(d) == 0) {
      return(datatable(data.frame(Message = "No cup statistics."), rownames = FALSE))
    }

    datatable(
      d,
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

  observe({
    arch <- normalize_tournament_archive(tournament_archive())

    cups <- if (is.null(arch) || nrow(arch) == 0) {
      character(0)
    } else {
      unique(arch$cup_id)
    }

    current <- isolate(input$public_tournament_cup)
    selected <- if (!is.null(current) && current %in% cups) {
      current
    } else if (length(cups) > 0) {
      cups[1]
    } else {
      character(0)
    }

    updateSelectInput(
      session,
      "public_tournament_cup",
      choices = cups,
      selected = selected
    )
  })

  output$tournament_public <- renderUI({
    arch <- normalize_tournament_archive(tournament_archive())

    if (is.null(arch) || nrow(arch) == 0) {
      return(
        div(
          class="cardx",
          tags$p(
            style = "color:#667085;",
            "No saved tournament draws yet."
          )
        )
      )
    }

    req(input$public_tournament_cup)

    d <- arch %>%
      filter(cup_id == input$public_tournament_cup) %>%
      arrange(match_no)

    if (nrow(d) == 0) {
      return(tags$p("No draw found for this cup."))
    }

    cup_name <- d$cup_id[1]
    participant_count <- d$participant_count[1]
    bracket_size <- d$bracket_size[1]
    round_name <- d$round[1]

    draw_box <- function(r) {
      div(
        class = "bracket-box",
        style = if (isTRUE(r$bye)) "opacity:.78;" else "",
        tags$b(paste0("Match ", r$match_no)),
        tags$br(),
        r$pairing,
        if (isTRUE(r$bye)) {
          tagList(
            tags$br(),
            tags$small(
              style="color:#667085;",
              "Automatic BYE"
            )
          )
        }
      )
    }

    upper <- d %>% filter(bracket_half == "Upper")
    lower <- d %>% filter(bracket_half == "Lower")

    tagList(
      div(
        class = "cardx",
        h2(paste0(cup_name, " — Tournament Draw")),
        p(
          paste0(
            participant_count, " participants • ",
            bracket_size, "-slot bracket • ",
            round_name
          )
        )
      ),

      div(
        class = "cardx",
        h3("Tournament Bracket"),
        tryCatch(
          render_bracket_tree(d),
          error = function(e) {
            tags$p(
              style = "color:#d92d20;",
              paste("Bracket display error:", conditionMessage(e))
            )
          }
        )
      ),

      div(
        class = "cardx",
        h3("Complete First-Round Draw"),
        fluidRow(
          column(
            6,
            h4("Upper Half"),
            lapply(seq_len(nrow(upper)), function(i) {
              draw_box(upper[i, ])
            })
          ),
          column(
            6,
            h4("Lower Half"),
            lapply(seq_len(nrow(lower)), function(i) {
              draw_box(lower[i, ])
            })
          )
        )
      ),

      div(
        class="cardx",
        h3("Full Draw Table"),
        tags$table(
          class="table table-striped",
          tags$thead(
            tags$tr(
              tags$th("Match"),
              tags$th("Half"),
              tags$th("Player A"),
              tags$th("Player B"),
              tags$th("BYE")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(d)), function(i) {
              r <- d[i,]
              tags$tr(
                tags$td(r$match_no),
                tags$td(r$bracket_half),
                tags$td(
                  ifelse(
                    is.na(r$seed_a),
                    r$player_a,
                    paste0("#", r$seed_a, " ", r$player_a)
                  )
                ),
                tags$td(
                  ifelse(
                    is.na(r$seed_b),
                    r$player_b,
                    paste0("#", r$seed_b, " ", r$player_b)
                  )
                ),
                tags$td(ifelse(r$bye, "Yes", "No"))
              )
            })
          )
        )
      )
    )
  })

  # ----------------------------
  # ADMIN LOGIN
  # ----------------------------

  observeEvent(input$admin_login, {
    req(input$admin_password)

    if (is_admin_password_valid(input$admin_password)) {
      admin_logged_in(TRUE)

      latest_arch <- tryCatch(
        load_tournament_archive_from_dropbox(),
        error = function(e) NULL
      )
      if (!is.null(latest_arch) && nrow(latest_arch) > 0) {
        tournament_archive(latest_arch)
      }

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
          "Backup / Recovery",
          div(
            class = "cardx",
            h3("Recover App from Dropbox"),
            p(
              "Use this only if the app ever opens with missing or incorrect data. ",
              "Nothing is restored automatically."
            ),
            tags$p(
              tags$b("Recovery source: "),
              "the normal files already saved by the app in Dropbox: ",
              tags$code("/matches.csv"),
              ", ",
              tags$code("/players.csv"),
              ", ",
              tags$code("/ranking.csv"),
              "."
            ),
            uiOutput("recovery_status"),
            br(),
            actionButton(
              "recover_from_dropbox",
              "Recover Everything from Dropbox",
              class = "btn-primary"
            ),
            br(), br(),
            tags$small(
              style = "color:#667085;",
              "This button does not reset ratings and does not delete matches. "
            )
          )
        ),

        tabPanel(
          "Tournament Draw",

          div(
            class = "cardx",
            h3("Saved Tournament Draws"),
            fluidRow(
              column(
                6,
                selectInput(
                  "admin_saved_cup",
                  "Saved Cup",
                  choices = NULL
                )
              ),
              column(
                6,
                br(),
                actionButton(
                  "refresh_saved_draws",
                  "Refresh Saved Draws",
                  class = "btn-default"
                ),
                tags$span(" "),
                actionButton(
                  "load_saved_draw",
                  "Load / Edit Draw",
                  class = "btn-info"
                ),
                tags$span(" "),
                actionButton(
                  "delete_saved_draw",
                  "Delete Draw",
                  class = "btn-danger"
                )
              )
            ),
            uiOutput("saved_draws_status")
          ),

          div(
            class = "cardx",
            h3("Tournament Draw Generator / Editor"),
            p(
              "Create a new draw or load an existing cup. ",
              "After loading, Player A and Player B cells can be edited manually."
            ),

            fluidRow(
              column(
                4,
                textInput(
                  "draw_cup_id",
                  "Cup ID",
                  value = "CUP-03",
                  placeholder = "Example: CUP-10"
                )
              ),
              column(
                8,
                selectizeInput(
                  "draw_participants",
                  "Participants",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    placeholder = "Select tournament participants..."
                  )
                )
              )
            ),

            fluidRow(
              column(
                3,
                actionButton(
                  "draw_select_all",
                  "Select All Players",
                  class = "btn-default"
                )
              ),
              column(
                3,
                actionButton(
                  "draw_clear_all",
                  "Clear Selection",
                  class = "btn-default"
                )
              ),
              column(
                3,
                actionButton(
                  "generate_draw",
                  "Generate / Re-draw",
                  class = "btn-primary"
                )
              ),
              column(
                3,
                actionButton(
                  "save_draw",
                  "Save Changes",
                  class = "btn-success"
                )
              )
            ),

            br(),
            uiOutput("draw_admin_summary"),
            br(),

            tags$p(
              tags$b("Edit mode: "),
              "double-click Player A or Player B in the table and type a different player name or BYE."
            ),

            DTOutput("admin_draw_table"),

            br(),
            h3("Bracket Tree Preview"),
            uiOutput("admin_bracket_tree")
          )
        ),

        tabPanel(
          "Group Draw",
          div(
            class = "cardx",
            h3("Group Stage + Knockout Draw"),
            p(
              "Choose the group format. Ranked players are separated into pots so the strongest players ",
              "do not meet in the same group at the start."
            ),

            fluidRow(
              column(
                3,
                textInput(
                  "group_draw_cup_id",
                  "Cup ID",
                  value = "CUP-04"
                )
              ),
              column(
                3,
                numericInput(
                  "group_draw_n_groups",
                  "Number of Groups",
                  value = 2,
                  min = 2,
                  max = 8,
                  step = 1
                )
              ),
              column(
                3,
                numericInput(
                  "group_draw_teams_per_group",
                  "Players per Group",
                  value = 4,
                  min = 2,
                  max = 8,
                  step = 1
                )
              ),
              column(
                3,
                numericInput(
                  "group_draw_qualifiers",
                  "Qualifiers per Group",
                  value = 2,
                  min = 1,
                  max = 4,
                  step = 1
                )
              )
            ),

            selectizeInput(
              "group_draw_participants",
              "Participants",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select group-stage participants..."
              )
            ),

            fluidRow(
              column(
                3,
                actionButton(
                  "group_select_all",
                  "Select All Players",
                  class = "btn-default"
                )
              ),
              column(
                3,
                actionButton(
                  "group_clear_all",
                  "Clear Selection",
                  class = "btn-default"
                )
              ),
              column(
                3,
                actionButton(
                  "generate_group_draw",
                  "Generate Group Draw",
                  class = "btn-primary"
                )
              ),
              column(
                3,
                actionButton(
                  "save_group_draw",
                  "Save Group Draw",
                  class = "btn-success"
                )
              )
            ),

            br(),
            uiOutput("group_draw_summary"),
            br(),
            uiOutput("admin_group_tables"),
            br(),

            h3("Knockout Qualification Path"),
            p(
              "A1 means Group A winner, A2 means Group A runner-up, etc. ",
              "The template separates qualifiers from the same group in the first knockout round whenever possible."
            ),
            DTOutput("group_knockout_template"),

            br(),
            actionButton(
              "delete_group_draw",
              "Delete Saved Group Draw",
              class = "btn-danger"
            )
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
              tags$li("tournament_draw.csv"),
              tags$li("tournament_draws.csv — all saved cup draws")
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
        base_rating = RATING_BASELINE,
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
  # MANUAL DROPBOX RECOVERY
  # ----------------------------

  recovery_message <- reactiveVal(
    "Recovery has not been run. Normal app operation uses the saved Dropbox files automatically."
  )

  output$recovery_status <- renderUI({
    tagList(
      tags$b("Status: "),
      tags$span(style = "color:#667085;", recovery_message())
    )
  })

  observeEvent(input$recover_from_dropbox, {
    req(admin_logged_in())

    showNotification(
      "Reading the latest saved data from Dropbox...",
      type = "message",
      duration = 3
    )

    tryCatch({
      # matches.csv is the permanent source of match/rating history.
      cloud_matches <- dropbox_download_csv(
        DROPBOX_MATCHES_PATH,
        quiet = FALSE
      )

      if (is.null(cloud_matches)) {
        stop("matches.csv could not be read from Dropbox.")
      }

      # Prefer players.csv. If it is unavailable but matches.csv is readable,
      # keep the current player list instead of failing the entire recovery.
      cloud_players <- tryCatch(
        dropbox_download_csv(DROPBOX_PLAYERS_PATH, quiet = FALSE),
        error = function(e) NULL
      )

      if (!is.null(cloud_players) && nrow(cloud_players) > 0) {
        p <- rename_players_to_photo_names(
          normalize_players(cloud_players)
        )
      } else {
        p <- rename_players_to_photo_names(
          normalize_players(players())
        )

        if (nrow(p) == 0) {
          stop(
            paste(
              "players.csv could not be read and the current app has no player list.",
              "Enable Dropbox files.content.read and create a new access token."
            )
          )
        }
      }

      m_raw <- rename_matches_to_photo_names(
        normalize_matches(cloud_matches)
      ) %>%
        arrange(match_id)

      # Reconstruct every match sequentially from the original 300 baseline.
      rebuilt <- recalculate_all(m_raw, p)

      # Update live app state.
      players(p)
      matches(rebuilt)

      # Regenerate derived files in the SAME Dropbox app folder.
      # This updates ranking.csv, players_stats.csv and rating_history.csv.
      save_everything(p, rebuilt)

      recovery_message(
        paste0(
          "Successful. Restored ",
          nrow(rebuilt),
          " matches and ",
          nrow(p),
          " players from Dropbox and rebuilt all ratings/statistics."
        )
      )

      showNotification(
        "Recovery completed successfully.",
        type = "message",
        duration = 7
      )

    }, error = function(e) {
      msg <- conditionMessage(e)
      recovery_message(paste("Failed:", msg))

      showNotification(
        paste("Recovery failed:", msg),
        type = "error",
        duration = NULL
      )
    })
  })

  # ----------------------------
  # TOURNAMENT DRAW ARCHIVE / EDIT / DELETE
  # ----------------------------

  output$saved_draws_status <- renderUI({
    req(admin_logged_in())
    arch <- normalize_tournament_archive(tournament_archive())
    if (is.null(arch) || nrow(arch) == 0) {
      return(tags$small(style="color:#667085;", "No saved tournament draws loaded."))
    }
    cups <- sort(unique(arch$cup_id))
    tags$small(
      style="color:#667085;",
      paste0(length(cups), " saved cup draw(s): ", paste(cups, collapse=", "))
    )
  })

  observeEvent(input$refresh_saved_draws, {
    req(admin_logged_in())

    tryCatch({
      arch <- load_tournament_archive_from_dropbox()

      # Dropbox is the source of truth on refresh.
      tournament_archive(arch)

      if (is.null(arch) || nrow(arch) == 0) {
        showNotification(
          "No saved draws were found in Dropbox.",
          type = "warning",
          duration = 6
        )
      } else {
        showNotification(
          paste0(
            length(unique(arch$cup_id)),
            " saved cup draw(s) loaded from Dropbox."
          ),
          type = "message",
          duration = 6
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Could not refresh saved draws:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  observe({
    # Re-run both when the archive changes AND when Admin logs in.
    logged <- admin_logged_in()
    arch <- normalize_tournament_archive(tournament_archive())

    if (!isTRUE(logged)) return()

    cups <- if (is.null(arch) || nrow(arch) == 0) {
      character(0)
    } else {
      sort(unique(arch$cup_id))
    }

    current <- isolate(input$admin_saved_cup)
    selected <- if (!is.null(current) && current %in% cups) {
      current
    } else if (length(cups) > 0) {
      cups[1]
    } else {
      character(0)
    }

    updateSelectInput(
      session,
      "admin_saved_cup",
      choices = cups,
      selected = selected
    )
  })

  observeEvent(input$load_saved_draw, {
    req(admin_logged_in())
    req(input$admin_saved_cup)

    arch <- normalize_tournament_archive(tournament_archive())

    d <- arch %>%
      filter(cup_id == input$admin_saved_cup) %>%
      arrange(match_no)

    if (nrow(d) == 0) {
      showNotification(
        "Saved draw not found.",
        type="error"
      )
      return()
    }

    tournament_draw(d)

    participants <- unique(c(d$player_a, d$player_b))
    participants <- participants[participants != "BYE"]

    updateTextInput(
      session,
      "draw_cup_id",
      value = d$cup_id[1]
    )

    updateSelectizeInput(
      session,
      "draw_participants",
      choices = rankdat()$player,
      selected = participants,
      server = TRUE
    )

    showNotification(
      paste0(d$cup_id[1], " loaded for editing."),
      type="message"
    )
  })

  observeEvent(input$delete_saved_draw, {
    req(admin_logged_in())
    req(input$admin_saved_cup)

    arch <- normalize_tournament_archive(tournament_archive())
    cup_to_delete <- input$admin_saved_cup

    new_arch <- arch %>%
      filter(cup_id != cup_to_delete)

    tryCatch({
      verified_arch <- save_and_verify_tournament_archive(
        new_arch,
        expected_cup = NULL
      )

      # Keep the old compatibility file consistent too.
      # It is no longer used as the normal source once the archive exists,
      # but clearing/updating it avoids stale deleted draws.
      if (nrow(new_arch) == 0) {
        empty_legacy <- tibble(
          cup_id = character(),
          participant_count = integer(),
          bracket_size = integer(),
          round = character(),
          match_no = integer(),
          bracket_half = character(),
          seed_a = integer(),
          player_a = character(),
          seed_b = integer(),
          player_b = character(),
          bye = logical(),
          pairing = character(),
          generated_at = character()
        )
        dropbox_upload_csv(empty_legacy, DROPBOX_TOURNAMENT_PATH)
      } else {
        first_remaining_cup <- unique(new_arch$cup_id)[1]
        legacy_draw <- new_arch %>%
          filter(cup_id == first_remaining_cup) %>%
          arrange(match_no)
        dropbox_upload_csv(legacy_draw, DROPBOX_TOURNAMENT_PATH)
      }

      tournament_archive(verified_arch)

      current_draw <- tournament_draw()
      if (!is.null(current_draw) &&
          nrow(current_draw) > 0 &&
          current_draw$cup_id[1] == cup_to_delete) {
        tournament_draw(NULL)
      }

      remaining_cups <- if (nrow(new_arch) > 0) unique(new_arch$cup_id) else character(0)

      updateSelectInput(
        session,
        "admin_saved_cup",
        choices = remaining_cups,
        selected = if (length(remaining_cups) > 0) remaining_cups[1] else character(0)
      )

      updateSelectInput(
        session,
        "public_tournament_cup",
        choices = remaining_cups,
        selected = if (length(remaining_cups) > 0) remaining_cups[1] else character(0)
      )

      showNotification(
        paste0(cup_to_delete, " draw permanently deleted from Dropbox."),
        type="message",
        duration=6
      )
    }, error=function(e) {
      showNotification(
        paste("Delete failed:", conditionMessage(e)),
        type="error",
        duration=NULL
      )
    })
  })

  observeEvent(input$draw_select_all, {
    req(admin_logged_in())

    p <- rankdat()$player

    updateSelectizeInput(
      session,
      "draw_participants",
      choices = p,
      selected = p,
      server = TRUE
    )
  })

  observeEvent(input$draw_clear_all, {
    req(admin_logged_in())

    updateSelectizeInput(
      session,
      "draw_participants",
      selected = character(0),
      server = TRUE
    )
  })

  output$draw_admin_summary <- renderUI({
    req(admin_logged_in())

    d <- tournament_draw()

    if (!is.null(d) && nrow(d) > 0) {
      return(
        tagList(
          tags$b(paste0("Editing: ", d$cup_id[1])),
          " • ",
          paste0(d$participant_count[1], " participants"),
          " • ",
          paste0(d$bracket_size[1], "-slot bracket")
        )
      )
    }

    n <- length(input$draw_participants)

    if (n == 0) {
      return(tags$span(
        style="color:#667085;",
        "No participants selected."
      ))
    }

    bracket_size <- next_power_of_two(n)
    byes <- bracket_size - n

    tagList(
      tags$b(paste0(n, " participants")),
      " • ",
      paste0(bracket_size, "-slot bracket"),
      " • ",
      paste0(byes, " BYE", ifelse(byes == 1, "", "s"))
    )
  })

  observeEvent(input$generate_draw, {
    req(admin_logged_in())

    tryCatch({
      d <- make_flexible_draw(
        ranking = rankdat(),
        participants = input$draw_participants,
        cup_id = input$draw_cup_id
      )

      tournament_draw(d)

      showNotification(
        paste0(
          "Draw generated for ",
          d$cup_id[1],
          "."
        ),
        type="message"
      )
    }, error=function(e) {
      showNotification(
        conditionMessage(e),
        type="error",
        duration=NULL
      )
    })
  })

  output$admin_bracket_tree <- renderUI({
    req(admin_logged_in())

    d <- tournament_draw()

    if (is.null(d) || nrow(d) == 0) {
      return(tags$p(
        style="color:#667085;",
        "Generate or load a draw to preview the bracket."
      ))
    }

    tryCatch(
      render_bracket_tree(d),
      error = function(e) {
        tags$p(
          style = "color:#d92d20;",
          paste("Bracket preview error:", conditionMessage(e))
        )
      }
    )
  })

  output$admin_draw_table <- renderDT({
    req(admin_logged_in())

    d <- tournament_draw()

    if (is.null(d) || nrow(d) == 0) {
      return(
        datatable(
          data.frame(Message="No draw loaded."),
          rownames=FALSE,
          options=list(dom="t")
        )
      )
    }

    display <- d %>%
      transmute(
        Match = match_no,
        Half = bracket_half,
        `Player A` = player_a,
        `Player B` = player_b,
        BYE = ifelse(bye, "Yes", "No")
      )

    datatable(
      display,
      rownames=FALSE,
      editable=list(
        target="cell",
        disable=list(columns=c(0,1,4))
      ),
      options=list(
        dom="t",
        paging=FALSE,
        ordering=FALSE,
        scrollX=TRUE
      )
    )
  })

  observeEvent(input$admin_draw_table_cell_edit, {
    req(admin_logged_in())

    d <- tournament_draw()
    req(!is.null(d), nrow(d) > 0)

    info <- input$admin_draw_table_cell_edit
    r <- info$row
    cidx <- info$col

    # DT column indexes are 0-based:
    # Match=0, Half=1, Player A=2, Player B=3, BYE=4
    if (!cidx %in% c(2,3)) return()

    value <- trimws(as.character(info$value))

    if (!nzchar(value)) {
      showNotification(
        "Player value cannot be empty. Use BYE if needed.",
        type="error"
      )
      return()
    }

    valid <- c(rankdat()$player, "BYE")

    if (!value %in% valid) {
      showNotification(
        paste0(
          "Unknown player: ", value,
          ". Use an exact player name from Ranking or BYE."
        ),
        type="error",
        duration=NULL
      )
      return()
    }

    if (cidx == 2) {
      d$player_a[r] <- value
    } else {
      d$player_b[r] <- value
    }

    d <- rebuild_draw_fields(d, rankdat())

    tryCatch({
      validate_manual_draw(d)
      tournament_draw(d)
    }, error=function(e) {
      showNotification(
        conditionMessage(e),
        type="error",
        duration=NULL
      )
    })
  })

  observeEvent(input$save_draw, {
    req(admin_logged_in())

    d <- tournament_draw()

    if (is.null(d) || nrow(d) == 0) {
      showNotification(
        "Generate or load a draw first.",
        type="error"
      )
      return()
    }

    d <- rebuild_draw_fields(d, rankdat())

    tryCatch({
      validate_manual_draw(d)

      arch <- normalize_tournament_archive(tournament_archive())

      if (is.null(arch) || nrow(arch) == 0) {
        new_arch <- d
      } else {
        new_arch <- bind_rows(
          arch %>% filter(cup_id != d$cup_id[1]),
          d
        ) %>%
          arrange(cup_id, match_no)
      }

      # Save to Dropbox, read the same file back, and verify the cup exists.
      verified_arch <- save_and_verify_tournament_archive(
        new_arch,
        expected_cup = d$cup_id[1]
      )

      # Legacy file is only a mirror, never the source of truth.
      dropbox_upload_csv(d, DROPBOX_TOURNAMENT_PATH)

      # IMPORTANT: use what was actually read back from Dropbox,
      # not the in-memory object we attempted to save.
      tournament_archive(verified_arch)

      verified_draw <- verified_arch %>%
        filter(cup_id == d$cup_id[1]) %>%
        arrange(match_no)

      tournament_draw(verified_draw)

      saved_cups <- sort(unique(verified_arch$cup_id))

      updateSelectInput(
        session,
        "admin_saved_cup",
        choices = saved_cups,
        selected = d$cup_id[1]
      )

      updateSelectInput(
        session,
        "public_tournament_cup",
        choices = saved_cups,
        selected = d$cup_id[1]
      )

      showNotification(
        paste0(
          d$cup_id[1],
          " saved and verified in Dropbox."
        ),
        type="message",
        duration=6
      )
    }, error=function(e) {
      showNotification(
        paste("Save failed:", conditionMessage(e)),
        type="error",
        duration=NULL
      )
    })
  })


  # ----------------------------
  # GROUP STAGE + KNOCKOUT
  # ----------------------------

  observe({
    arch <- group_draw_archive()

    cups <- if (is.null(arch) || nrow(arch) == 0) {
      character(0)
    } else {
      sort(unique(arch$cup_id))
    }

    current <- isolate(input$public_group_cup)

    selected <- if (!is.null(current) && current %in% cups) {
      current
    } else if (length(cups) > 0) {
      cups[1]
    } else {
      character(0)
    }

    updateSelectInput(
      session,
      "public_group_cup",
      choices = cups,
      selected = selected
    )
  })

  observeEvent(input$group_select_all, {
    req(admin_logged_in())

    p <- rankdat()$player

    updateSelectizeInput(
      session,
      "group_draw_participants",
      choices = p,
      selected = p,
      server = TRUE
    )
  })

  observeEvent(input$group_clear_all, {
    req(admin_logged_in())

    updateSelectizeInput(
      session,
      "group_draw_participants",
      selected = character(0),
      server = TRUE
    )
  })

  output$group_draw_summary <- renderUI({
    req(admin_logged_in())

    ng <- as.integer(input$group_draw_n_groups)
    pg <- as.integer(input$group_draw_teams_per_group)
    qg <- as.integer(input$group_draw_qualifiers)

    needed <- ng * pg
    selected <- length(input$group_draw_participants)
    knockout_total <- ng * qg

    tagList(
      tags$b(paste0("Selected: ", selected, " / ", needed)),
      " • ",
      paste0(ng, " groups"),
      " • ",
      paste0(pg, " players per group"),
      " • ",
      paste0(qg, " qualify from each group"),
      " • ",
      paste0(knockout_total, " total qualifiers")
    )
  })

  observeEvent(input$generate_group_draw, {
    req(admin_logged_in())

    tryCatch({
      d <- make_group_stage_draw(
        ranking = rankdat(),
        participants = input$group_draw_participants,
        cup_id = input$group_draw_cup_id,
        n_groups = input$group_draw_n_groups,
        teams_per_group = input$group_draw_teams_per_group,
        qualifiers_per_group = input$group_draw_qualifiers
      )

      current_group_draw(d)

      showNotification(
        paste0(d$cup_id[1], " group draw generated."),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
    })
  })

  output$admin_group_tables <- renderUI({
    req(admin_logged_in())

    d <- current_group_draw()

    if (is.null(d) || nrow(d) == 0) {
      return(tags$p(
        style = "color:#667085;",
        "Generate or load a group draw."
      ))
    }

    render_group_cards(d)
  })

  output$group_knockout_template <- renderDT({
    req(admin_logged_in())

    d <- current_group_draw()

    if (is.null(d) || nrow(d) == 0) {
      return(
        datatable(
          data.frame(Message = "No group draw generated."),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }

    ko <- build_group_knockout_template(
      n_groups = d$n_groups[1],
      qualifiers_per_group = d$qualifiers_per_group[1]
    )

    datatable(
      ko,
      rownames = FALSE,
      colnames = c("Match", "Qualifier A", "Qualifier B"),
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      )
    )
  })

  observeEvent(input$save_group_draw, {
    req(admin_logged_in())

    d <- current_group_draw()

    if (is.null(d) || nrow(d) == 0) {
      showNotification(
        "Generate a group draw first.",
        type = "error"
      )
      return()
    }

    tryCatch({
      arch <- group_draw_archive()

      new_arch <- bind_rows(
        arch %>% filter(cup_id != d$cup_id[1]),
        d
      ) %>%
        arrange(cup_id, group_name, group_slot)

      verified <- save_group_draws_to_dropbox(new_arch)

      if (!d$cup_id[1] %in% verified$cup_id) {
        stop("Dropbox verification failed: saved group cup was not found after upload.")
      }

      group_draw_archive(verified)

      showNotification(
        paste0(d$cup_id[1], " group draw saved and verified in Dropbox."),
        type = "message",
        duration = 6
      )
    }, error = function(e) {
      showNotification(
        paste("Group draw save failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  observeEvent(input$delete_group_draw, {
    req(admin_logged_in())

    cup <- trimws(as.character(input$group_draw_cup_id))
    if (!nzchar(cup)) return()

    tryCatch({
      arch <- group_draw_archive()
      new_arch <- arch %>% filter(cup_id != cup)

      verified <- save_group_draws_to_dropbox(new_arch)
      group_draw_archive(verified)

      current <- current_group_draw()
      if (!is.null(current) && nrow(current) > 0 && current$cup_id[1] == cup) {
        current_group_draw(NULL)
      }

      showNotification(
        paste0(cup, " group draw permanently deleted."),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("Delete failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
    })
  })

  output$public_group_draw <- renderUI({
    arch <- group_draw_archive()

    if (is.null(arch) || nrow(arch) == 0) {
      return(
        div(
          class = "cardx",
          tags$p("No saved group-stage draws yet.")
        )
      )
    }

    req(input$public_group_cup)

    d <- arch %>%
      filter(cup_id == input$public_group_cup) %>%
      arrange(group_name, group_slot)

    if (nrow(d) == 0) {
      return(tags$p("No saved draw found for this cup."))
    }

    ko <- build_group_knockout_template(
      n_groups = d$n_groups[1],
      qualifiers_per_group = d$qualifiers_per_group[1]
    )

    tagList(
      div(
        class = "cardx",
        h2(paste0(d$cup_id[1], " — Group Stage")),
        p(
          paste0(
            d$n_groups[1], " groups • ",
            d$teams_per_group[1], " players per group • ",
            d$qualifiers_per_group[1], " qualify from each group"
          )
        ),
        render_group_cards(d)
      ),

      div(
        class = "cardx",
        h3("Knockout Qualification Path"),
        tags$table(
          class = "table table-striped",
          tags$thead(
            tags$tr(
              tags$th("Match"),
              tags$th("Qualifier A"),
              tags$th("Qualifier B")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(ko)), function(i) {
              tags$tr(
                tags$td(ko$match_no[i]),
                tags$td(ko$player_a[i]),
                tags$td(ko$player_b[i])
              )
            })
          )
        )
      )
    )
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
