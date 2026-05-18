# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / Summary  (Gross Receipt + Net Receipt)
#
# Per-LG revenue receipts summary. The "Gross Receipt" and "Net Receipt"
# subfolders share the same column structure but contain different values:
#   - Gross = receipts before refunds / adjustments
#   - Net   = receipts after refunds / adjustments
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - row 5    : header
#   - rows 6..n-1 : 753 LG rows
#   - row n    : जम्मा (drop)
#   - cols     : sn | संकेत | स्थानीय तह | अनुमान | प्राप्ती |
#                Receipts Percentage | मौज्दात
#
# Writes:
#   cleaned_output/summary_gross_receipt.xlsx
#   cleaned_output/summary_net_receipt.xlsx
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/Summary"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

# Receipt type -> input subfolder + output filename
RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "summary_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "summary_net_receipt.xlsx")
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

clean_one <- function(file, lookup, receipt_type) {
  message("Reading: ", basename(file), " (", receipt_type, ")")
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  fy <- extract_fy(raw[[4, 1]])

  expected_r5 <- c("क्र.सं.","संकेत","स्थानीय तह","अनुमान","प्राप्ती",
                   "Receipts Percentage","मौज्दात")
  actual_r5 <- as.character(unlist(raw[5, 1:7]))
  if (!identical(actual_r5, expected_r5)) {
    stop("Header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_r5, collapse=" | "),
         "\n  got     : ", paste(actual_r5, collapse=" | "))
  }

  df <- tibble(
    lg_code_raw  = np_digits_to_ascii(raw[[2]]),
    lg_name_np   = as.character(raw[[3]]),
    estimate     = parse_np_num(raw[[4]]),
    received     = parse_np_num(raw[[5]]),
    receipt_pct  = parse_np_num(raw[[6]]),
    balance      = parse_np_num(raw[[7]])
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
  df$receipt_type <- receipt_type
  df$source_file <- basename(file)
  df[, c("fy","receipt_type","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "estimate","received","receipt_pct","balance","source_file"), drop = FALSE]
}

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)

  for (rt in names(RECEIPT_VARIANTS)) {
    spec  <- RECEIPT_VARIANTS[[rt]]
    indir <- file.path(BASE_INPUT, spec$dir)
    if (!dir.exists(indir)) { message("Missing dir: ", indir); next }

    files <- list.files(indir, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { message("No files in ", indir); next }

    combined <- map_dfr(files, ~ clean_one(.x, lookup, rt))

    miss <- sum(is.na(combined$lgcode))
    message(sprintf("  %s -> %d rows, %d unmatched lgcode", spec$out, nrow(combined), miss))
    out_path <- file.path(OUTPUT_DIR, spec$out)
    writexl::write_xlsx(combined, out_path)
    message(sprintf("  wrote %s (%d x %d)", out_path, nrow(combined), ncol(combined)))
  }
}

main()
