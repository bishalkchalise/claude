# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / LG Revenue Heading  (Gross + Net)
#
# Already-long-format: one row per (LG, revenue heading) with estimate /
# received / receipt% / balance.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY ± optional province/district filter)
#   - rows 5-6 : 2-level header
#   - rows 7..n : data
#   - cols     : sn | संकेत(LG) | नाम(LG) | संकेत(heading) | नाम(heading) |
#                अनुमान | प्राप्ती | Receipts Percentage | मौज्दात
#
# Writes:
#   cleaned_output/lg_revenue_heading_gross_receipt.xlsx
#   cleaned_output/lg_revenue_heading_net_receipt.xlsx
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/LG Revenue Heading"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "lg_revenue_heading_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "lg_revenue_heading_net_receipt.xlsx")
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
parse_title <- function(s) {
  s <- np_digits_to_ascii(as.character(s))
  fy <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  prov <- regmatches(s, regexpr("प्रदेश\\s*:\\s*[^[:space:]जिल्ला]+", s))
  prov <- if (length(prov)) str_squish(sub("^प्रदेश\\s*:\\s*", "", prov)) else NA_character_
  dist <- regmatches(s, regexpr("जिल्ला\\s*:\\s*.+", s))
  dist <- if (length(dist)) str_squish(sub("^जिल्ला\\s*:\\s*", "", dist)) else NA_character_
  list(fy = if (length(fy)) fy else NA_character_,
       province = prov, district_filter = dist)
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
  meta <- parse_title(raw[[4, 1]])

  # Validate headers
  if (raw[[5, 1]] != "क्र.सं." || raw[[5, 4]] != "राजस्व शीर्षक" ||
      raw[[5, 6]] != "अनुमान"  || raw[[5, 9]] != "मौज्दात")
    stop("R5 header mismatch in ", basename(file))
  if (raw[[6, 2]] != "संकेत" || raw[[6, 3]] != "नाम" ||
      raw[[6, 4]] != "संकेत" || raw[[6, 5]] != "नाम")
    stop("R6 header mismatch in ", basename(file))

  df <- tibble(
    lg_code_raw         = np_digits_to_ascii(raw[[2]]),
    lg_name_np          = as.character(raw[[3]]),
    revenue_heading_code= np_digits_to_ascii(raw[[4]]),
    revenue_heading_np  = as.character(raw[[5]]),
    estimate            = parse_np_num(raw[[6]]),
    received            = parse_np_num(raw[[7]]),
    receipt_pct         = parse_np_num(raw[[8]]),
    balance             = parse_np_num(raw[[9]])
  )
  df <- df[7:nrow(df), , drop = FALSE]

  # Keep only valid data rows: LG code numeric, LG name has comma,
  # heading code looks numeric, heading name not empty.
  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np) &
          !is.na(df$revenue_heading_code) &
          grepl("^[0-9]+$", df$revenue_heading_code) &
          !is.na(df$revenue_heading_np) & nzchar(str_squish(df$revenue_heading_np))
  df <- df[keep, , drop = FALSE]

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- meta$fy
  df$province <- meta$province
  df$district_filter <- meta$district_filter
  df$receipt_type <- receipt_type
  df$source_file <- basename(file)

  df[, c("fy","receipt_type","province","district_filter",
         "lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "revenue_heading_code","revenue_heading_np",
         "estimate","received","receipt_pct","balance",
         "source_file"), drop = FALSE]
}

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)

  for (rt in names(RECEIPT_VARIANTS)) {
    spec  <- RECEIPT_VARIANTS[[rt]]
    indir <- file.path(BASE_INPUT, spec$dir)
    if (!dir.exists(indir)) { message("Missing: ", indir); next }
    files <- list.files(indir, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { message("No files: ", indir); next }

    combined <- map_dfr(files, ~ clean_one(.x, lookup, rt))
    miss <- sum(is.na(combined$lgcode))
    distinct_lgs <- length(unique(combined$lg_code_raw))
    distinct_heads <- length(unique(combined$revenue_heading_code))
    out_path <- file.path(OUTPUT_DIR, spec$out)
    writexl::write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows | %d LGs | %d headings | unmatched: %d",
                    basename(out_path), nrow(combined), distinct_lgs, distinct_heads, miss))
  }
}

main()
