## scrape_nfl_transactions.R
#
# Purpose:
#   Scrape NFL.com transactions (https://www.nfl.com/transactions/) into a clean tibble.

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(glue)
  library(janitor)
  library(tidyr)
  library(digest)
})

# ---------------------------
# URL helpers
# ---------------------------
nfl_txn_build_url <- function(
    type = c("signings","trades","waivers","reserve-list","terminations","other"),
    year, month, after = NULL
) {
  type <- match.arg(type)
  stopifnot(is.numeric(year), length(year) == 1)
  stopifnot(is.numeric(month), length(month) == 1, month >= 1, month <= 12)
  
  base <- glue("https://www.nfl.com/transactions/league/{type}/{year}/{month}")
  if (!is.null(after) && nzchar(after)) paste0(base, "?after=", after) else base
}

# Remove/append cache-buster without poisoning caching/loop detection
nfl_txn_strip_cb <- function(url) {
  url <- sub("([?&])_cb=[^&]*&?", "\\1", url)
  sub("[?&]$", "", url)
}

nfl_txn_add_cb <- function(url) {
  sep <- if (grepl("\\?", url)) "&" else "?"
  paste0(
    url,
    sep,
    "_cb=",
    sprintf("%d%03d", as.integer(Sys.time()), sample.int(999, 1) - 1)
  )
}

# ---------------------------
# Request HTML (sessioned, cookie-preserving, cache-busted)
# ---------------------------
nfl_txn_request_html <- function(
    url,
    cookie_jar,
    user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36",
    referer = NULL,
    timeout_s = 30
) {
  url_req <- nfl_txn_add_cb(url)
  
  req <- request(url_req) |>
    req_user_agent(user_agent) |>
    req_cookie_preserve(cookie_jar) |>
    req_timeout(timeout_s) |>
    req_retry(max_tries = 3, backoff = ~ min(8, 0.5 * (2 ^ .x))) |>
    req_headers(
      "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      "accept-language" = "en-US,en;q=0.9",
      "cache-control" = "no-cache, no-store, must-revalidate",
      "pragma" = "no-cache",
      "upgrade-insecure-requests" = "1"
    ) |>
    req_options(followlocation = TRUE)
  
  if (!is.null(referer) && nzchar(referer)) {
    req <- req |> req_headers(referer = referer)
  }
  
  resp <- req |> req_perform()
  
  if (resp_status(resp) >= 400) {
    stop(glue("HTTP {resp_status(resp)} for {url}"))
  }
  
  resp_body_string(resp)
}

# ---------------------------
# Parse table (memory-safe: first <table> fragment only)
# ---------------------------
nfl_txn_parse_table <- function(html) {
  tbl_html <- str_match(
    html,
    regex("(<table\\b[\\s\\S]*?</table>)", ignore_case = TRUE)
  )[,2]
  
  if (is.na(tbl_html) || !nzchar(tbl_html)) return(NULL)
  
  doc <- xml2::read_html(tbl_html)
  
  raw <- doc |>
    rvest::html_element("table") |>
    rvest::html_table(fill = TRUE)
  
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  
  janitor::clean_names(raw)
}

# ---------------------------
# Pagination helpers
# ---------------------------
nfl_txn_extract_next_url <- function(html, current_url) {
  base_no_query <- sub("\\?.*$", "", current_url)
  
  get_after <- function(u) {
    m <- stringr::str_match(u, stringr::regex("[\\?&]after=([^&]+)", ignore_case = TRUE))
    m[,2]
  }
  
  current_after <- get_after(current_url)
  
  tokens <- stringr::str_match_all(
    html,
    stringr::regex("\\bafter=([A-Za-z0-9%_=\\-]+)", ignore_case = TRUE)
  )[[1]]
  
  if (is.null(tokens) || nrow(tokens) == 0) return(NULL)
  
  # keep order as seen, but unique
  afters <- tokens[,2]
  afters <- afters[!is.na(afters) & nzchar(afters)]
  afters <- afters[!duplicated(afters)]
  
  if (!is.na(current_after) && nzchar(current_after)) {
    afters <- afters[afters != current_after]
  }
  
  if (length(afters) == 0) return(NULL)
  
  # heuristic: the last cursor observed tends to be "next"
  next_after <- tail(afters, 1)
  paste0(base_no_query, "?after=", next_after)
}

nfl_txn_debug_next_snippet <- function(html) {
  str_extract(
    html,
    regex(".{0,250}nfl-o-table-pagination__next.{0,250}", ignore_case = TRUE)
  )
}

# ---------------------------
# Date parsing + standardization
# ---------------------------
parse_date_time_flex <- function(x, default_year) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "--")] <- NA_character_
  
  if (length(default_year) == 1) {
    default_year <- rep.int(as.integer(default_year), length(x))
  } else {
    default_year <- as.integer(default_year)
    if (length(default_year) != length(x)) stop("default_year must be length 1 or same length as x.")
  }
  
  try_formats_one <- function(val, fmts) {
    for (fmt in fmts) {
      out <- tryCatch(as.POSIXct(val, tz = "UTC", format = fmt), error = function(e) as.POSIXct(NA))
      if (!is.na(out)[1]) return(out)
    }
    as.POSIXct(NA)
  }
  
  fmts_full <- c(
    "%Y-%m-%d",
    "%Y-%m-%d %H:%M:%S",
    "%m/%d/%Y",
    "%m/%d/%y",
    "%b %d, %Y",
    "%B %d, %Y",
    "%b %d %Y",
    "%B %d %Y"
  )
  
  res <- vector("list", length(x))
  
  for (i in seq_along(x)) {
    val <- x[i]
    yr  <- default_year[i]
    
    if (is.na(val) || is.na(yr)) {
      res[[i]] <- as.POSIXct(NA)
      next
    }
    
    # mm/dd (no year)
    if (str_detect(val, "^\\d{1,2}/\\d{1,2}$")) {
      res[[i]] <- try_formats_one(paste0(val, "/", yr), c("%m/%d/%Y"))
      next
    }
    
    # "Aug 31" (no year)
    if (str_detect(val, "^[A-Za-z]{3,9}\\s+\\d{1,2}$")) {
      res[[i]] <- try_formats_one(paste0(val, " ", yr), c("%b %d %Y", "%B %d %Y"))
      next
    }
    
    res[[i]] <- try_formats_one(val, fmts_full)
  }
  
  as.POSIXct(do.call(c, res), origin = "1970-01-01", tz = "UTC")
}

nfl_txn_standardize_cols <- function(df, type, year, month, source_url) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  
  col_from <- intersect(names(df), c("from", "from_team"))[1]
  col_to   <- intersect(names(df), c("to", "to_team"))[1]
  col_date <- intersect(names(df), c("date"))[1]
  col_name <- intersect(names(df), c("name", "player", "player_name"))[1]
  col_pos  <- intersect(names(df), c("pos", "position"))[1]
  col_txn  <- intersect(names(df), c("transaction", "transactions", "transaction_description"))[1]
  
  out <- tibble(
    transaction_type_group = type,
    season = as.integer(year),
    month  = as.integer(month),
    date_raw = if (!is.na(col_date)) as.character(df[[col_date]]) else NA_character_,
    from_team = if (!is.na(col_from)) as.character(df[[col_from]]) else NA_character_,
    to_team   = if (!is.na(col_to))   as.character(df[[col_to]])   else NA_character_,
    player_name = if (!is.na(col_name)) as.character(df[[col_name]]) else NA_character_,
    position = if (!is.na(col_pos)) as.character(df[[col_pos]]) else NA_character_,
    transaction_desc = if (!is.na(col_txn)) as.character(df[[col_txn]]) else NA_character_,
    source_url = as.character(source_url)
  ) |>
    mutate(
      across(c(from_team, to_team, player_name, position, transaction_desc, date_raw), ~ str_squish(.x)),
      across(c(from_team, to_team), ~ na_if(.x, "--"))
    ) |>
    mutate(
      # Fix duplicated team names like "Steelers Steelers"
      from_team = str_replace(from_team, "^(.+?)\\s+\\1$", "\\1"),
      to_team   = str_replace(to_team,   "^(.+?)\\s+\\1$", "\\1")
    ) |>
    mutate(
      date = suppressWarnings(parse_date_time_flex(date_raw, default_year = year)),
      date = as.Date(date)
    ) |>
    select(
      transaction_type_group, season, month, date,
      from_team, to_team, player_name, position,
      transaction_desc, source_url, date_raw
    )
  
  out
}

nfl_txn_empty_tbl <- function() {
  tibble(
    transaction_type_group = character(),
    season                 = integer(),
    month                  = integer(),
    date                   = as.Date(character()),
    from_team              = character(),
    to_team                = character(),
    player_name            = character(),
    position               = character(),
    transaction_desc       = character(),
    source_url             = character(),
    date_raw               = character()
  )
}

nfl_txn_table_sig <- function(std_df) {
  # std_df is the standardized tibble for a page
  # Build a stable row signature using raw columns (not parsed date)
  sig <- paste(
    std_df$date_raw,
    std_df$from_team,
    std_df$to_team,
    std_df$player_name,
    std_df$position,
    std_df$transaction_desc,
    sep = "|"
  )
  digest::digest(paste(sig, collapse = "\n"), algo = "md5")
}


# ---------------------------
# Core scraper
# ---------------------------
scrape_nfl_transactions_month <- function(
    type = c("all","signings","trades","waivers","reserve-list","terminations","other"),
    year,
    month,
    pause_s = 1.0,
    stabilize_waits = rep(4, 5),   # 5 tries of 4s by default
    max_pages = 5000,
    user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36",
    verbose = TRUE,
    debug_pagination = FALSE,
    cache_dir = NULL,
    progress_every = 10,
    page_size = 25
) {
  type <- match.arg(type)
  
  # ---- small local helpers ----
  get_after <- function(u) {
    m <- stringr::str_match(u, stringr::regex("[\\?&]after=([^&]+)", ignore_case = TRUE))
    m[,2]
  }
  
  # If we already have these globally, fine; these local versions are safe.
  nfl_txn_strip_cb <- function(url) {
    url <- sub("([?&])_cb=[^&]*&?", "\\1", url)
    sub("[?&]$", "", url)
  }
  
  nfl_txn_table_sig <- function(std_df) {
    # Stable signature of TABLE CONTENT (not whole HTML)
    sig <- paste(
      std_df$date_raw,
      std_df$from_team,
      std_df$to_team,
      std_df$player_name,
      std_df$position,
      std_df$transaction_desc,
      sep = "|"
    )
    digest::digest(paste(sig, collapse = "\n"), algo = "md5")
  }
  
  # ---- caching (optional) ----
  cache_get <- function(canon_url) {
    if (is.null(cache_dir)) return(NULL)
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    key <- digest::digest(canon_url, algo = "xxhash64")
    path <- file.path(cache_dir, paste0(key, ".html.gz"))
    if (!file.exists(path)) return(NULL)
    raw <- readBin(path, what = "raw", n = file.info(path)$size)
    mem <- memDecompress(raw, type = "gzip")
    rawToChar(mem)
  }
  
  cache_set <- function(canon_url, html) {
    if (is.null(cache_dir)) return(invisible(NULL))
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    key <- digest::digest(canon_url, algo = "xxhash64")
    path <- file.path(cache_dir, paste0(key, ".html.gz"))
    raw <- memCompress(charToRaw(html), type = "gzip")
    writeBin(raw, path)
    invisible(NULL)
  }
  
  # ---- session cookie jar for this run ----
  cookie_jar <- tempfile("nfl_txn_cookiejar_", fileext = ".txt")
  
  # NOTE: fetch_html uses nfl_txn_request_html(), which should already
  # add a cache-buster (_cb) internally (or we can add it there).
  fetch_html <- function(url, referer = NULL, use_cache = TRUE) {
    canon_url <- nfl_txn_strip_cb(url)
    
    if (use_cache) {
      cached <- cache_get(canon_url)
      if (!is.null(cached)) return(cached)
    }
    
    Sys.sleep(pause_s)
    
    html <- nfl_txn_request_html(
      url = canon_url,
      cookie_jar = cookie_jar,
      user_agent = user_agent,
      referer = referer,
      timeout_s = 30
    )
    
    # IMPORTANT: do NOT cache here blindly; we only cache after we know the page is useful.
    html
  }
  
  # ---- handle type == all ----
  if (type == "all") {
    all_types <- c("signings","trades","waivers","reserve-list","terminations","other")
    res <- purrr::map_dfr(all_types, function(tt) {
      scrape_nfl_transactions_month(
        type = tt,
        year = year,
        month = month,
        pause_s = pause_s,
        stabilize_waits = stabilize_waits,
        max_pages = max_pages,
        user_agent = user_agent,
        verbose = verbose,
        debug_pagination = debug_pagination,
        cache_dir = cache_dir,
        progress_every = progress_every,
        page_size = page_size
      )
    })
    
    if (nrow(res) == 0) return(nfl_txn_empty_tbl())
    
    return(
      res |>
        dplyr::distinct(
          transaction_type_group, season, month, date,
          from_team, to_team, player_name, position, transaction_desc, date_raw,
          .keep_all = TRUE
        ) |>
        dplyr::arrange(dplyr::desc(date), transaction_type_group, player_name)
    )
  }
  
  # ---- pagination loop ----
  url <- nfl_txn_build_url(type = type, year = year, month = month)
  
  all <- list()
  prev_url <- NULL
  
  seen_urls <- character()       # canonical URL (no _cb)
  seen_after <- character()
  seen_table_sigs <- character()
  
  for (i in seq_len(max_pages)) {
    
    if (verbose && (i %% progress_every == 1 || i <= 3)) {
      message(glue::glue("[{type}] {year}-{stringr::str_pad(month, 2, pad='0')} page {i}: {url}"))
    }
    
    canon_url <- nfl_txn_strip_cb(url)
    if (canon_url %in% seen_urls) {
      if (verbose) message("  - URL already seen (loop). Stopping.")
      break
    }
    seen_urls <- c(seen_urls, canon_url)
    
    curr_after <- get_after(url)
    if (!is.na(curr_after) && nzchar(curr_after)) {
      if (curr_after %in% seen_after) {
        if (verbose) message("  - after cursor already seen (loop). Stopping.")
        break
      }
      seen_after <- c(seen_after, curr_after)
    }
    
    # ---- fetch + parse ----
    html <- fetch_html(url, referer = prev_url, use_cache = TRUE)
    
    if (!stringr::str_detect(html, stringr::regex("<table\\b", ignore_case = TRUE))) {
      if (verbose) message("  - No table found. Done.")
      break
    }
    
    tbl <- nfl_txn_parse_table(html)
    if (is.null(tbl) || nrow(tbl) == 0) {
      if (verbose) message("  - Parsed empty table. Done.")
      break
    }
    
    std <- nfl_txn_standardize_cols(tbl, type = type, year = year, month = month, source_url = url) |>
      dplyr::distinct(
        transaction_type_group, season, month, date,
        from_team, to_team, player_name, position, transaction_desc, date_raw,
        .keep_all = TRUE
      )
    
    page_rows <- nrow(std)
    sig <- nfl_txn_table_sig(std)
    
    if (verbose) message(glue::glue("  - page_rows: {page_rows} | table_sig_md5: {sig}"))
    
    # ---- If table content repeats, retry live fetches ----
    if (sig %in% seen_table_sigs) {
      if (verbose) message("  - Table content repeated (sig seen). Retrying live fetches...")
      
      got_new <- FALSE
      for (k in seq_along(stabilize_waits)) {
        w <- stabilize_waits[[k]]
        if (verbose) message(glue::glue("    * retry {k}/{length(stabilize_waits)} after {w}s"))
        Sys.sleep(w)
        
        html_try <- fetch_html(url, referer = prev_url, use_cache = FALSE)
        
        if (!stringr::str_detect(html_try, stringr::regex("<table\\b", ignore_case = TRUE))) next
        tbl_try <- nfl_txn_parse_table(html_try)
        if (is.null(tbl_try) || nrow(tbl_try) == 0) next
        
        std_try <- nfl_txn_standardize_cols(tbl_try, type = type, year = year, month = month, source_url = url) |>
          dplyr::distinct(
            transaction_type_group, season, month, date,
            from_team, to_team, player_name, position, transaction_desc, date_raw,
            .keep_all = TRUE
          )
        
        sig_try <- nfl_txn_table_sig(std_try)
        
        if (!(sig_try %in% seen_table_sigs)) {
          std <- std_try
          sig <- sig_try
          page_rows <- nrow(std_try)
          got_new <- TRUE
          if (verbose) message(glue::glue("    * got new table variant. page_rows now {page_rows}. Continuing."))
          break
        }
      }
      
      if (!got_new) {
        if (verbose) message("  - Still repeated table after retries. Stopping.")
        break
      }
    }
    
    # Now that we have a non-repeated table, remember it and cache HTML
    seen_table_sigs <- c(seen_table_sigs, sig)
    if (!is.null(cache_dir)) cache_set(canon_url, html)
    
    all[[length(all) + 1]] <- std
    
    if (debug_pagination && verbose) {
      cat("\n--- DEBUG next snippet ---\n")
      cat(nfl_txn_debug_next_snippet(html), "\n")
      cat("--- END DEBUG ---\n\n")
    }
    
    # SAFE STOP: final page often has < page_size rows and a bogus/self link
    if (page_rows < page_size) {
      if (verbose) {
        message(glue::glue(
          "  - Last page detected ({page_rows} rows < {page_size}). Stopping pagination."
        ))
      }
      break
    }
    
    # ---- advance ----
    next_url <- nfl_txn_extract_next_url(html, current_url = url)
    
    # If next_url points to itself or doesn't advance, we're done (no need to retry forever)
    if (is.null(next_url) || identical(next_url, url) || identical(get_after(next_url), curr_after)) {
      # Try a couple live refetches in case pagination cursor is in a different variant
      advanced <- FALSE
      for (k in seq_along(stabilize_waits)) {
        w <- stabilize_waits[[k]]
        if (verbose) message(glue::glue("  - Pagination stuck. Waiting {w}s and retrying ({k}/{length(stabilize_waits)})"))
        Sys.sleep(w)
        
        html_live <- fetch_html(url, referer = prev_url, use_cache = FALSE)
        
        if (debug_pagination && verbose) {
          cat("\n--- DEBUG next snippet (refetch) ---\n")
          cat(nfl_txn_debug_next_snippet(html_live), "\n")
          cat("--- END DEBUG ---\n\n")
        }
        
        next_url <- nfl_txn_extract_next_url(html_live, current_url = url)
        if (!is.null(next_url) && !identical(next_url, url) && !identical(get_after(next_url), curr_after)) {
          html <- html_live
          advanced <- TRUE
          break
        }
      }
      
      if (!advanced) {
        if (verbose) message("  - No advancing next page. Done.")
        break
      }
    }
    
    prev_url <- url
    url <- next_url
  }
  
  if (length(all) == 0) return(nfl_txn_empty_tbl())
  
  dplyr::bind_rows(all) |>
    dplyr::distinct(
      transaction_type_group, season, month, date,
      from_team, to_team, player_name, position, transaction_desc, date_raw,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(dplyr::desc(date), player_name)
}
