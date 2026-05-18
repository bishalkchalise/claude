# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / LG Revenue Monthly  (Gross + Net)
#
# Long format: one row per (LG, source, revenue heading) with 12 monthly
# received values. source_np kept in Nepali because the values mix federal,
# specific provinces, internal source etc. (no clean short English mapping).
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY ± optional province/district filter)
#   - rows 5-6 : 2-level header
#   - rows 7..n-1 : data
#   - row n    : जम्मा (drop)
#   - cols     : sn | संकेत(LG) | नाम(LG) | स्रोत | संकेत(head) | नाम(head) |
#                <12 months: साउन..असार> | जम्मा (drop)
#
# Writes:
#   cleaned_output/lg_revenue_monthly_gross_receipt.xlsx
#   cleaned_output/lg_revenue_monthly_net_receipt.xlsx
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/LG Revenue Monthly"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "lg_revenue_monthly_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "lg_revenue_monthly_net_receipt.xlsx")
)

MONTH_LABEL <- c(
  "साउन"    = "received_saun",
  "भदौ"     = "received_bhadau",
  "आस्बिन"  = "received_asoj",
  "कार्तिक"  = "received_kartik",
  "मार्ग"    = "received_mangsir",
  "पौष"    = "received_poush",
  "माघ"    = "received_magh",
  "फागुन"   = "received_falgun",
  "चैत्र"    = "received_chaitra",
  "बैशाख"   = "received_baisakh",
  "जेठ"    = "received_jestha",
  "असार"   = "received_asar"
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

  # Validate header structure
  if (raw[[5, 4]] != "स्रोत" || raw[[5, 5]] != "राजस्व शीर्षक" ||
      raw[[5, 7]] != "महिना")
    stop("R5 header mismatch in ", basename(file))

  r6_months <- as.character(unlist(raw[6, 7:18]))
  if (!identical(r6_months, names(MONTH_LABEL))) {
    stop("Month-header mismatch in ", basename(file),
         "\n  expected: ", paste(names(MONTH_LABEL), collapse=" | "),
         "\n  got     : ", paste(r6_months, collapse=" | "))
  }
  if (raw[[6, 19]] != "जम्मा") stop("Expected जम्मा at R6C19")

  monthly_vals <- as.data.frame(lapply(raw[, 7:18], parse_np_num))
  names(monthly_vals) <- unname(MONTH_LABEL)

  df <- bind_cols(
    tibble(
      lg_code_raw          = np_digits_to_ascii(raw[[2]]),
      lg_name_np           = as.character(raw[[3]]),
      source_np            = as.character(raw[[4]]),
      revenue_heading_code = np_digits_to_ascii(raw[[5]]),
      revenue_heading_np   = as.character(raw[[6]])
    ),
    monthly_vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

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
         "source_np","revenue_heading_code","revenue_heading_np",
         unname(MONTH_LABEL),
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
    out_path <- file.path(OUTPUT_DIR, spec$out)
    writexl::write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows | %d LGs | unmatched lgcode: %d",
                    basename(out_path), nrow(combined),
                    length(unique(combined$lg_code_raw)), miss))
  }
}

main()
