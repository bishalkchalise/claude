# =============================================================================
# Clean: राजस्व अनुमान / (folder root - no subfolders)
#
# Per-LG revenue projection (Revenue Estimate) with breakdown by source type
# and grant subtype.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-6 : 2-level header
#   - rows 7..n-1 : 753 LG rows
#   - row n    : जम्मा (drop)
#   - cols 24 total
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "राजस्व अनुमान"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "revenue_estimate.xlsx"

# Column 4..24 -> output column name
# (excludes col 22 "कुल आय अनुमान" which is a total and dropped)
COL_NAMES <- c(
  fed_equalization        = 4,
  fed_conditional         = 5,
  fed_special             = 6,
  fed_complementary       = 7,
  fed_other_grant         = 8,
  prov_equalization       = 9,
  prov_conditional        = 10,
  prov_special            = 11,
  prov_complementary      = 12,
  prov_other_grant        = 13,
  inter_lg                = 14,
  foreign_src             = 15,
  revshare_fed            = 16,
  revshare_prov           = 17,
  internal_delegated_tax  = 18,
  internal_existing_rights= 19,
  internal_other          = 20,
  internal_public         = 21,
  # 22 = कुल आय अनुमान (total income, DROPPED per "no totals" rule)
  loan                    = 23,
  cash_balance            = 24
)

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

  # Validate R5 top-level headers
  expected_r5 <- c(
    "क्र.सं." = 1, "स्थानीय तह" = 2,
    "संघीय सरकार" = 4, "प्रदेश सरकार" = 9,
    "अन्तर स्थानीय तह" = 14, "वैदेशिक स्रोत" = 15,
    "राजस्व बाँडफाँट" = 16, "आन्तरिक श्रोत" = 18,
    "कुल आय अनुमान" = 22, "न्यूनपूर्ति गर्ने श्रोतहरु" = 23
  )
  for (i in seq_along(expected_r5)) {
    nm  <- names(expected_r5)[i]
    pos <- expected_r5[[i]]
    if (raw[[5, pos]] != nm)
      stop("R5 header mismatch at C", pos, " in ", basename(file),
           ": expected '", nm, "' got '", raw[[5, pos]], "'")
  }

  vals <- as.data.frame(lapply(COL_NAMES, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(COL_NAMES)

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[2]]),
           lg_name_np  = as.character(raw[[3]])),
    vals
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
         names(COL_NAMES), "source_file"), drop = FALSE]
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
