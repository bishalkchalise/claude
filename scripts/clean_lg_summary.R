# =============================================================================
# Clean: बजेट र खर्चको सारांश / LG Summary
#
# Per-LG operational fund summary (single-level flat header).
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - row 5    : header
#   - rows 6..n-1 : per-LG data
#   - row n    : जम्मा (drop)
#   - cols     : sn | संकेत | स्थानीय तह | शुरुको मौज्दात | प्राप्त |
#                जम्मा प्राप्ती | खर्च | मौज्दात
# Note: negative values appear in parentheses (e.g. "(२०,००,०००.००)").
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "बजेट र खर्चको सारांश/LG Summary"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "lg_summary.xlsx"

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}
parse_np_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- np_digits_to_ascii(x)
  neg <- grepl("^\\s*\\(.*\\)\\s*$", x)
  x <- gsub("[(),\\s]", "", x, perl = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[neg] <- -v[neg]; v
}
extract_fy <- function(x) {
  s <- np_digits_to_ascii(as.character(x))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m)) m else NA_character_
}
load_lookup <- function(path) {
  if (!file.exists(path)) { warning("Lookup not found: ", path); return(NULL) }
  lk <- read.csv(path, fileEncoding = "UTF-8",
                 stringsAsFactors = FALSE, check.names = FALSE)
  lk %>% transmute(district_np = str_squish(district_np),
                   mun_np      = str_squish(mun_np),
                   lgcode      = as.character(lgcode)) %>%
        distinct(district_np, mun_np, .keep_all = TRUE)
}

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  fy <- extract_fy(raw[[4, 1]])

  # Validate row 5 header
  expected_r5 <- c("क्र.सं.","संकेत","स्थानीय तह","शुरुको मौज्दात","प्राप्त",
                   "जम्मा प्राप्ती","खर्च","मौज्दात")
  actual_r5 <- as.character(unlist(raw[5, 1:8]))
  if (!identical(actual_r5, expected_r5)) {
    stop("Header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_r5, collapse=" | "),
         "\n  got     : ", paste(actual_r5, collapse=" | "))
  }

  df <- tibble(
    lg_code_raw     = np_digits_to_ascii(raw[[2]]),
    lg_name_np      = as.character(raw[[3]]),
    opening_balance = parse_np_num(raw[[4]]),
    received        = parse_np_num(raw[[5]]),
    total_receipts  = parse_np_num(raw[[6]]),
    expenditure     = parse_np_num(raw[[7]]),
    closing_balance = parse_np_num(raw[[8]])
  )
  df <- df[6:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "opening_balance","received","total_receipts","expenditure","closing_balance",
         "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  miss <- sum(is.na(combined$lgcode))
  per <- combined %>% count(source_file, fy)
  message(sprintf("Rows: %d | unmatched lgcode: %d", nrow(combined), miss))
  for (i in seq_len(nrow(per))) message(sprintf("  %s (fy=%s) -> %d rows",
                                                per$source_file[i], per$fy[i], per$n[i]))
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", nrow(combined), " x ", ncol(combined), ")")
  invisible(combined)
}

main()
