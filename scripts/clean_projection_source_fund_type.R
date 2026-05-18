# =============================================================================
# Clean: आय र व्यय प्रक्षेपण को सारांश / Source Fund Type
#
# Per-LG projection: income broken down by source, expense broken down by
# fund type, plus date of municipal-assembly approval.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-6 : 2-level header
#   - rows 7..n-1 : 753 LG rows
#   - row n    : जम्मा (total) -- DROP
#
# Columns:
#   sn | संकेत | नाम |
#   आय अनुमान : संघीय सरकार | प्रदेश सरकार | राजस्व बाँडफाँट |
#                आन्तरिक राजस्व | जन सहभागिता | कुल आय अनुमान |
#   व्यय अनुमान : चालु | पूँजीगत | वित्तीय | जम्मा |
#   सभाबाट स्वीकृत मिति  (BS date YYYY/MM/DD, kept as string)
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "आय र व्यय प्रक्षेपण को सारांश/Source Fund Type"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "projection_source_fund_type.xlsx"

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
  lk <- read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                 check.names = FALSE)
  lk %>% transmute(district_np = str_squish(district_np),
                   mun_np      = str_squish(mun_np),
                   lgcode      = as.character(lgcode)) %>%
        distinct(district_np, mun_np, .keep_all = TRUE)
}

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  stopifnot(nrow(raw) >= 7, ncol(raw) >= 14)
  fy <- extract_fy(raw[[4, 1]])

  # Validate row 6 income / expense / date sub-headers
  expected_r6 <- c("संकेत","नाम","संघीय सरकार","प्रदेश सरकार","राजस्व बाँडफाँट",
                   "आन्तरिक राजस्व","जन सहभागिता","कुल आय अनुमान",
                   "चालु","पूँजीगत","वित्तीय","जम्मा")
  actual_r6 <- as.character(unlist(raw[6, 2:13]))
  if (!identical(actual_r6, expected_r6)) {
    stop("Header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_r6, collapse=" | "),
         "\n  got     : ", paste(actual_r6, collapse=" | "))
  }
  if (raw[[5, 14]] != "सभाबाट स्वीकृत मिति") {
    stop("Expected 'सभाबाट स्वीकृत मिति' at R5C14, got: ", raw[[5, 14]])
  }

  df <- tibble(
    lg_code_raw   = np_digits_to_ascii(raw[[2]]),
    lg_name_np    = as.character(raw[[3]]),
    inc_fed       = parse_np_num(raw[[4]]),
    inc_prov      = parse_np_num(raw[[5]]),
    inc_revshare  = parse_np_num(raw[[6]]),
    inc_internal  = parse_np_num(raw[[7]]),
    inc_public    = parse_np_num(raw[[8]]),
    inc_total     = parse_np_num(raw[[9]]),
    exp_current   = parse_np_num(raw[[10]]),
    exp_capital   = parse_np_num(raw[[11]]),
    exp_financing = parse_np_num(raw[[12]]),
    exp_total     = parse_np_num(raw[[13]]),
    approval_date = np_digits_to_ascii(raw[[14]])
  )
  df <- df[7:nrow(df), , drop = FALSE]

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
         "inc_fed","inc_prov","inc_revshare","inc_internal","inc_public","inc_total",
         "exp_current","exp_capital","exp_financing","exp_total",
         "approval_date","source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  miss_code <- sum(is.na(combined$lgcode))
  per <- combined %>% count(source_file, fy)
  message(sprintf("Rows: %d | unmatched lgcode: %d", nrow(combined), miss_code))
  for (i in seq_len(nrow(per))) message(sprintf("  %s (fy=%s) -> %d rows",
                                                per$source_file[i], per$fy[i], per$n[i]))

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", nrow(combined), " x ", ncol(combined), ")")
  invisible(combined)
}

main()
