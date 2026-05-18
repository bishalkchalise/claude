# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / Revenue Sharing by GoN  (Gross + Net)
#
# Revenue sharing from Government of Nepal to LGs, by specific revenue
# heading codes (e.g. 11411, 11421, 14153-14158 etc.). The codes that
# appear as columns vary by file; the script detects them dynamically.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY ± optional province/district filter)
#   - rows 5-6 : 2-level header
#       R5: क्र.सं. | स्थानीय तह | राजस्व शीर्षक
#       R6: संकेत | नाम | <revenue heading codes...> | जम्मा
#   - rows 7..n-1 : data (one row per LG, with shared amount per heading)
#   - row n    : जम्मा (drop) if present
#
# In the sample files in this repo, NO data rows are present (only headers).
# Locally the user will have data. Script handles both cases.
#
# Output is in LONG format (one row per LG x revenue heading):
#   fy, receipt_type, lgcode, district_np, mun_np, lg_code_raw, lg_name_np,
#   revenue_heading_code, shared_amount, source_file
#
# Writes:
#   cleaned_output/revenue_sharing_gross_receipt.xlsx
#   cleaned_output/revenue_sharing_net_receipt.xlsx
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr); library(tidyr)
  library(stringr); library(purrr)
})

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/Revenue Sharing by GoN"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "revenue_sharing_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "revenue_sharing_net_receipt.xlsx")
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

# Detect revenue-heading code columns by scanning R6 from C4 onwards.
# Stop at "जम्मा" (or end of row). Returns named integer vector: code -> col index.
detect_heading_cols <- function(raw) {
  r6 <- as.character(unlist(raw[6, ]))
  out <- list()
  for (c in 4:length(r6)) {
    v <- r6[c]
    if (is.na(v) || v == "") next
    if (v == "जम्मा") break
    # heading codes are numeric (may be in Latin or Devanagari digits)
    code <- np_digits_to_ascii(v)
    if (grepl("^[0-9]+$", code)) out[[code]] <- c
  }
  if (!length(out)) return(integer())
  setNames(unlist(out), names(out))
}

clean_one <- function(file, lookup, receipt_type) {
  message("Reading: ", basename(file), " (", receipt_type, ")")
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  meta <- parse_title(raw[[4, 1]])

  if (raw[[5, 4]] != "राजस्व शीर्षक")
    stop("Expected 'राजस्व शीर्षक' at R5C4 in ", basename(file))

  heading_cols <- detect_heading_cols(raw)
  if (!length(heading_cols)) {
    message("  No heading-code columns detected (and likely no data)")
    return(tibble(
      fy = character(), receipt_type = character(),
      province = character(), district_filter = character(),
      lgcode = character(), district_np = character(), mun_np = character(),
      lg_code_raw = character(), lg_name_np = character(),
      revenue_heading_code = character(), shared_amount = numeric(),
      source_file = character()
    ))
  }

  # Data rows start at row 7
  if (nrow(raw) < 7) {
    return(tibble())  # no data at all
  }

  # Slice value columns out as a numeric matrix
  vals <- as.data.frame(lapply(heading_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(heading_cols)

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[2]]),
           lg_name_np  = as.character(raw[[3]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  # Keep only valid LG rows
  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]

  if (!nrow(df)) {
    message("  0 data rows after filtering -- file probably contains only headers")
    return(tibble(
      fy = character(), receipt_type = character(),
      province = character(), district_filter = character(),
      lgcode = character(), district_np = character(), mun_np = character(),
      lg_code_raw = character(), lg_name_np = character(),
      revenue_heading_code = character(), shared_amount = numeric(),
      source_file = character()
    ))
  }

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  # Pivot wide heading cols to LONG
  long <- df %>%
    pivot_longer(cols = all_of(names(heading_cols)),
                 names_to = "revenue_heading_code",
                 values_to = "shared_amount") %>%
    filter(!is.na(shared_amount))

  long$fy <- meta$fy
  long$province <- meta$province
  long$district_filter <- meta$district_filter
  long$receipt_type <- receipt_type
  long$source_file <- basename(file)

  long[, c("fy","receipt_type","province","district_filter",
           "lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
           "revenue_heading_code","shared_amount","source_file"),
       drop = FALSE]
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
    miss <- if (nrow(combined)) sum(is.na(combined$lgcode)) else 0
    out_path <- file.path(OUTPUT_DIR, spec$out)
    writexl::write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows | unmatched lgcode: %d",
                    basename(out_path), nrow(combined), miss))
  }
}

main()
