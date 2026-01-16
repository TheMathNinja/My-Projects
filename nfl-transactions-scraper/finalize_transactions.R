suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(glue)
})

root <- "C:/Users/filim/Documents/R/nflreadRprojects/nfl_transactions_scrape"
dir_ckpt <- file.path(root, "data", "checkpoints")
dir_out  <- file.path(root, "data", "output")

ckpt_files <- list.files(dir_ckpt, pattern = "\\.rds$", full.names = TRUE)
stopifnot(length(ckpt_files) > 0)

message(glue("Loading {length(ckpt_files)} checkpoint files..."))

all_df <- purrr::map_dfr(ckpt_files, readRDS)

message(glue("Rows before dedupe: {nrow(all_df)}"))

required_cols <- c(
  "transaction_type_group","season","month","date",
  "from_team","to_team","player_name","position",
  "transaction_desc","source_url","date_raw",
  "scraped_at","row_id"
)

missing <- setdiff(required_cols, names(all_df))
if (length(missing) > 0) {
  stop(glue("Missing expected columns: {paste(missing, collapse=', ')}"))
}

final_df <- all_df %>%
  distinct(row_id, .keep_all = TRUE) %>%
  arrange(desc(date), transaction_type_group, player_name)

message(glue("Rows after dedupe: {nrow(final_df)}"))

raw_path   <- file.path(dir_out, "transactions_1965_2025_raw.rds")
final_path <- file.path(dir_out, "transactions_1965_2025_final.rds")

saveRDS(all_df, raw_path)
saveRDS(final_df, final_path)

message(glue("Wrote:\n- {raw_path}\n- {final_path}"))

type_counts <- final_df %>% count(transaction_type_group, sort = TRUE)
print(type_counts)
