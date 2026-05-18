# =============================================================================
# Clean: विभाज्य कोष / (folder root - no subfolders)
#
# Per-(LG, revenue heading) divisible fund accounting: receipts then
# distribution among federal / provincial / local government, with closing
# balance.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-6 : 2-level header
#       R5: sn | स्थानीय तह | राजस्व शीर्षक | प्राप्त | बाँडफाँड | Distribution Percentage | मौज्दात
#       R6: संकेत | नाम | संकेत | शीर्षक | शुरुको मौज्दात | प्राप्त | जम्मा प्राप्ती |
#           नेपाल सरकार | प्रदेश सरकार | स्थानीय तह | Total Distibution
#   - rows 7..n-1 : data (long format - one row per LG × heading)
#   - row n    : जम्मा (drop)
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "विभाज्य कोष"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "divisible_fund.xlsx"

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

  # Validate row 5 / 6 headers
  if (raw[[5, 4]] != "राजस्व शीर्षक" || raw[[5, 14]] != "मौज्दात")
    stop("R5 header mismatch in ", basename(file))
  expected_r6 <- c("संकेत","नाम","संकेत","शीर्षक","शुरुको मौज्दात","प्राप्त",
                   "जम्मा प्राप्ती","नेपाल सरकार","प्रदेश सरकार","स्थानीय तह",
                   "Total Distibution")  # note: source typo "Distibution"
  actual_r6 <- as.character(unlist(raw[6, 2:12]))
  if (!identical(actual_r6, expected_r6)) {
    stop("R6 header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_r6, collapse=" | "),
         "\n  got     : ", paste(actual_r6, collapse=" | "))
  }

  df <- tibble(
    lg_code_raw          = np_digits_to_ascii(raw[[2]]),
    lg_name_np           = as.character(raw[[3]]),
    revenue_heading_code = np_digits_to_ascii(raw[[4]]),
    revenue_heading_np   = as.character(raw[[5]]),
    opening_balance      = parse_np_num(raw[[6]]),
    received             = parse_np_num(raw[[7]]),
    total_receipts       = parse_np_num(raw[[8]]),
    dist_fed             = parse_np_num(raw[[9]]),
    dist_prov            = parse_np_num(raw[[10]]),
    dist_lg              = parse_np_num(raw[[11]]),
    dist_total           = parse_np_num(raw[[12]]),
    dist_pct             = parse_np_num(raw[[13]]),
    closing_balance      = parse_np_num(raw[[14]])
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np) &
          !is.na(df$revenue_heading_code) &
          grepl("^[0-9]+$", df$revenue_heading_code) &
          !is.na(df$revenue_heading_np) &
          nzchar(str_squish(df$revenue_heading_np))
  df <- df[keep, , drop = FALSE]

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "revenue_heading_code","revenue_heading_np",
         "opening_balance","received","total_receipts",
         "dist_fed","dist_prov","dist_lg","dist_total",
         "dist_pct","closing_balance",
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
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols (%d unmatched lgcode)",
                  out_path, nrow(combined), ncol(combined), miss))
}

main()
