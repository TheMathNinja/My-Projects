## build_transactions_history.R
##
## Builds a complete historical NFL transactions dataset by scraping NFL.com
## for every month in every year (default 1965–2025), for type = "all".
##
## RUN ORDER (newest first):
##   Dec 2025 → Jan 2025, then Dec 2024 → Jan 2024, ... down to Dec 1965 → Jan 1965
##
## Outputs (under root/data/):
## - checkpoints/txns_YYYY_MM.rds          (monthly checkpoints, resumable)
## - logs/build_log.csv                   (one row per year-month run)

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(glue)
  library(digest)
  library(tibble)
})

# ---------------------------
# CONFIG
# ---------------------------
root <- "C:/Users/filim/Documents/R/nflreadRprojects/nfl_transactions_scrape"
dir.create(root, recursive = TRUE, showWarnings = FALSE)

dir_data  <- file.path(root, "data")
dir_cache <- file.path(dir_data, "cache")
dir_ckpt  <- file.path(dir_data, "checkpoints")
dir_logs  <- file.path(dir_data, "logs")
dir_out   <- file.path(dir_data, "output") # reserved for finalize step

dir.create(dir_cache, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_ckpt,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_logs,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out,   recursive = TRUE, showWarnings = FALSE)

# NEWEST → OLDEST
years  <- 2025:1965
months <- 12:1

# Polite pacing
pause_s <- 1.0

# Handle NFL.com page load / retry timing
stabilize_waits <- rep(4, 5)

# Log file
log_path <- file.path(dir_logs, "build_log.csv")

# ---------------------------
# HELPERS
# ---------------------------
mm2 <- function(mm) stringr::str_pad(mm, width = 2, side = "left", pad = "0")

ckpt_path <- function(year, month) {
  file.path(dir_ckpt, glue::glue("txns_{year}_{mm2(month)}.rds"))
}

append_log <- function(row_df) {
  stopifnot(is.data.frame(row_df), nrow(row_df) == 1)
  if (!file.exists(log_path)) {
    readr::write_csv(row_df, log_path)
  } else {
    readr::write_csv(row_df, log_path, append = TRUE)
  }
}

add_row_id <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  key <- paste(
    df$transaction_type_group,
    df$season,
    df$month,
    df$date_raw,
    df$from_team,
    df$to_team,
    df$player_name,
    df$position,
    df$transaction_desc,
    sep = "||"
  )
  
  df %>%
    mutate(
      scraped_at = as.POSIXct(Sys.time(), tz = "UTC"),
      row_id = vapply(key, digest::digest, character(1), algo = "xxhash64")
    )
}

fmt_type_counts <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("none")
  df %>%
    count(transaction_type_group, sort = TRUE) %>%
    mutate(piece = paste0(transaction_type_group, "=", n)) %>%
    pull(piece) %>%
    paste(collapse = ", ")
}

# ---------------------------
# SAFETY CHECK
# ---------------------------
if (!exists("scrape_nfl_transactions_month", mode = "function")) {
  stop(
    "scrape_nfl_transactions_month() not found.\n",
    "First run:\n",
    'source("C:/Users/filim/Documents/R/nflreadRprojects/nfl_transactions_scrape/R/scrape_nfl_transactions.R")\n'
  )
}

# ---------------------------
# MAIN LOOP (RESUMABLE)
# ---------------------------
for (yy in years) {
  for (mm in months) {
    
    out_file <- ckpt_path(yy, mm)
    
    if (file.exists(out_file)) {
      message(glue::glue("[SKIP] {yy}-{mm2(mm)} checkpoint exists"))
      next
    }
    
    message(glue::glue("[RUN ] {yy}-{mm2(mm)}"))
    
    started <- Sys.time()
    status <- "ok"
    err_msg <- NA_character_
    
    df <- tryCatch({
      cache_dir <- file.path(dir_cache, glue::glue("html_{yy}_{mm2(mm)}"))
      
      res <- scrape_nfl_transactions_month(
        type = "all",
        year = yy,
        month = mm,
        pause_s = pause_s,
        stabilize_waits = stabilize_waits,
        cache_dir = cache_dir,
        verbose = FALSE,
        debug_pagination = FALSE
      )
      
      add_row_id(res)
    }, error = function(e) {
      status <<- "error"
      err_msg <<- conditionMessage(e)
      tibble::tibble()
    })
    
    # Save checkpoint (even if empty or error -> lets us resume deterministically)
    saveRDS(df, out_file)
    
    ended <- Sys.time()
    
    append_log(tibble::tibble(
      year = yy,
      month = mm,
      ym = glue::glue("{yy}-{mm2(mm)}"),
      status = status,
      n_rows = nrow(df),
      started_at = as.character(started),
      ended_at = as.character(ended),
      seconds = as.numeric(difftime(ended, started, units = "secs")),
      error = err_msg
    ))
    
    # Mini report (total + breakdown by type)
    type_breakdown <- fmt_type_counts(df)
    
    message(glue::glue(
      "      -> {status}, rows={nrow(df)}, types: {type_breakdown}, saved={basename(out_file)}"
    ))
  }
}

message("✅ Done building checkpoints. Next: run finalize_transactions.R")
