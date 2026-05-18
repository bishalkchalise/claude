# =============================================================================
# Clean & append LG fiscal xlsx files -> one tidy Excel
#
# What it does:
#   1. Reads every .xlsx in INPUT_DIR
#   2. Auto-detects the header block (multi-row merged headers)
#   3. Flattens headers, converts Devanagari digits, parses "mun, district"
#   4. Joins on (district + mun_name) with LOOKUP_FILE to attach lgcode
#   5. Renames Nepali columns to short English names (edit COL_MAP below)
#   6. Appends all files into ONE tibble and writes a single xlsx
#
# Usage: edit the three paths under "CONFIG", then source the file.
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  # Optional: for fuzzy fallback. install.packages("stringdist") if missing.
  has_stringdist <- requireNamespace("stringdist", quietly = TRUE)
})

# ----- CONFIG ----------------------------------------------------------------
INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector LG"   # folder with the raw .xlsx
LOOKUP_FILE <- "lookup/lg_lookup_master.xlsx"         # district+mun -> lgcode
OUTPUT_DIR  <- "cleaned_output"                       # where the combined xlsx is written
OUTPUT_FILE <- "sector_lg_clean.xlsx"

# Lookup column names (adjust to whatever your lookup uses)
LOOKUP_DISTRICT_NP <- "जिल्ला"
LOOKUP_MUN_NP      <- "स्थानीय तह"
LOOKUP_DISTRICT_EN <- "district"
LOOKUP_MUN_EN      <- "mun_name"
LOOKUP_LGCODE      <- "lgcode"   # 5-digit; vmun_code / newcode are the same thing

# Header detection: row that contains this marker is the LAST header row.
# (Sample files have "बजेट" / "खर्च" pair in the final header row.)
HEADER_LAST_ROW_MARKER <- "बजेट"
HEADER_SEARCH_MAX      <- 15      # scan first N rows to find it

# Column renames: Nepali (or flattened header) -> short English.
# Add entries as you discover new columns. Anything unmapped keeps its
# auto-generated snake_case name.
COL_MAP <- c(
  "क्र.सं."                       = "sn",
  "स्थानीय तह_संकेत"              = "lg_code_raw",
  "स्थानीय तह_नाम"                = "lg_name_np",
  "संकेत"                          = "lg_code_raw",
  "नाम"                            = "lg_name_np"
  # extend as needed, e.g.:
  # "आर्थिक विकास_उद्योग_बजेट"   = "econ_industry_budget",
  # "आर्थिक विकास_उद्योग_खर्च"   = "econ_industry_exp"
)

# ----- HELPERS ---------------------------------------------------------------

# Devanagari digit -> ASCII; also strips thousands separators
np_to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x <- gsub("[,\\s]", "", x, perl = TRUE)
  suppressWarnings(as.numeric(x))
}

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}

# Normalize a Nepali LG / district name for joining:
#  - trim, collapse whitespace
#  - drop punctuation (.,()-)
#  - strip common LG-type suffixes and their abbreviations
#  - drop trailing "जिल्ला" on district names
normalize_np <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\\.,()\\-]", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  suffixes <- c(
    "महानगरपालिका", "उपमहानगरपालिका", "नगरपालिका", "गाउँपालिका", "गाउंपालिका",
    "म\\.न\\.पा\\.?", "उ\\.म\\.न\\.पा\\.?", "न\\.पा\\.?", "गा\\.पा\\.?",
    "जिल्ला"
  )
  for (sfx in suffixes) x <- str_replace(x, paste0("\\s*", sfx, "\\s*$"), "")
  str_trim(x)
}

# Fuzzy match a name within a district. Returns NA if best distance > max_dist.
fuzzy_match <- function(name, candidates, max_dist = 2) {
  if (!has_stringdist || length(candidates) == 0 || is.na(name)) return(NA_character_)
  d <- stringdist::stringdist(name, candidates, method = "osa")
  if (min(d) <= max_dist) candidates[which.min(d)] else NA_character_
}

# Make a flattened header from a block of header rows (handles merged cells via
# forward-fill across columns within each row, then concatenates downward).
build_headers <- function(header_block) {
  # header_block is a data.frame with one row per header line
  hb <- as.data.frame(lapply(header_block, as.character), stringsAsFactors = FALSE)
  # Within each ROW, forward-fill to the right (merged cells become NA except first)
  hb_filled <- t(apply(hb, 1, function(r) {
    last <- NA_character_
    out <- character(length(r))
    for (j in seq_along(r)) {
      v <- r[j]
      if (is.na(v) || v == "") v <- last else last <- v
      out[j] <- v
    }
    out
  }))
  # Concatenate down each column, skip NAs/dupes
  apply(hb_filled, 2, function(col) {
    parts <- col[!is.na(col) & col != ""]
    parts <- parts[!duplicated(parts)]
    paste(parts, collapse = "_")
  })
}

# Safe snake-case for fallback names
snake <- function(x) {
  x <- str_replace_all(x, "[\\s\\.,/()\\-]+", "_")
  x <- str_replace_all(x, "_+", "_")
  x <- str_replace_all(x, "^_|_$", "")
  x
}

# Find the last header row by scanning column A for the marker
detect_header_end <- function(file) {
  probe <- suppressMessages(read_excel(file, col_names = FALSE,
                                       n_max = HEADER_SEARCH_MAX,
                                       .name_repair = "minimal"))
  for (r in seq_len(nrow(probe))) {
    row_txt <- paste(unlist(probe[r, ]), collapse = " ")
    if (grepl(HEADER_LAST_ROW_MARKER, row_txt, fixed = TRUE)) return(r)
  }
  # Fallback to known default
  7L
}

# ----- LOAD LOOKUP -----------------------------------------------------------

load_lookup <- function(path) {
  if (!file.exists(path)) {
    warning("Lookup file not found: ", path, " - lgcode will be NA.")
    return(NULL)
  }
  lk <- read_excel(path)
  needed <- c(LOOKUP_DISTRICT_NP, LOOKUP_MUN_NP, LOOKUP_LGCODE)
  miss   <- setdiff(needed, names(lk))
  if (length(miss)) stop("Lookup missing columns: ", paste(miss, collapse = ", "))
  lk %>%
    transmute(
      district_np = str_squish(.data[[LOOKUP_DISTRICT_NP]]),
      mun_np      = str_squish(.data[[LOOKUP_MUN_NP]]),
      district_key = normalize_np(.data[[LOOKUP_DISTRICT_NP]]),
      mun_key      = normalize_np(.data[[LOOKUP_MUN_NP]]),
      district     = if (LOOKUP_DISTRICT_EN %in% names(.)) .data[[LOOKUP_DISTRICT_EN]] else NA_character_,
      mun_name     = if (LOOKUP_MUN_EN      %in% names(.)) .data[[LOOKUP_MUN_EN]]      else NA_character_,
      lgcode       = as.character(.data[[LOOKUP_LGCODE]])
    ) %>%
    distinct(district_key, mun_key, .keep_all = TRUE)
}

# ----- CLEAN ONE FILE --------------------------------------------------------

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  hdr_end <- detect_header_end(file)

  # Read raw with no header so we control everything
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))

  # Find the first row that looks like a title block vs the header block.
  # Heuristic: headers occupy the few rows immediately above hdr_end where any
  # row beyond column 2 has non-NA. Walk up from hdr_end while that holds.
  hdr_start <- hdr_end
  while (hdr_start > 1) {
    prev <- raw[hdr_start - 1, ]
    nonempty_beyond_2 <- sum(!is.na(prev[1, -(1:2)]) & prev[1, -(1:2)] != "")
    if (nonempty_beyond_2 == 0) break
    hdr_start <- hdr_start - 1
  }

  header_block <- raw[hdr_start:hdr_end, , drop = FALSE]
  data_block   <- raw[(hdr_end + 1):nrow(raw), , drop = FALSE]

  flat <- build_headers(header_block)
  flat <- ifelse(flat == "" | is.na(flat), paste0("col_", seq_along(flat)), flat)

  # Drop trailing fully-empty columns
  keep <- colSums(!is.na(data_block) & data_block != "") > 0
  data_block <- data_block[, keep, drop = FALSE]
  flat       <- flat[keep]

  names(data_block) <- flat

  # Apply translation map; unmapped -> snake-case
  new_names <- ifelse(flat %in% names(COL_MAP), unname(COL_MAP[flat]), snake(flat))
  # Disambiguate dupes
  new_names <- make.unique(new_names, sep = "_")
  names(data_block) <- new_names

  # Drop totals / footer rows (where the first col isn't a number and 2nd col is NA)
  data_block <- data_block %>%
    filter(!(is.na(.[[2]]) | .[[2]] == ""))

  # Identify name column for parsing district
  name_col <- intersect(c("lg_name_np", "नाम"), names(data_block))[1]
  if (!is.na(name_col)) {
    parts <- str_split_fixed(data_block[[name_col]], ",", 2)
    data_block$mun_np      <- str_squish(parts[, 1])
    data_block$district_np <- str_squish(parts[, 2])
  } else {
    data_block$mun_np      <- NA_character_
    data_block$district_np <- NA_character_
  }

  # Convert Devanagari digits everywhere; coerce numeric cols
  data_block <- data_block %>%
    mutate(across(everything(), ~ if (is.character(.)) np_digits_to_ascii(.) else .))

  num_candidates <- setdiff(names(data_block),
                            c("sn", "lg_code_raw", "lg_name_np",
                              "mun_np", "district_np"))
  for (nc in num_candidates) {
    coerced <- suppressWarnings(np_to_num(data_block[[nc]]))
    # Only replace if most values parse as numeric
    if (mean(!is.na(coerced)) > 0.6) data_block[[nc]] <- coerced
  }

  # Build normalized join keys
  data_block$district_key <- normalize_np(data_block$district_np)
  data_block$mun_key      <- normalize_np(data_block$mun_np)

  # Join lookup -> lgcode (normalized exact match first, then fuzzy within district)
  if (!is.null(lookup)) {
    data_block <- data_block %>%
      left_join(lookup %>% select(district_key, mun_key,
                                  district, mun_name, lgcode),
                by = c("district_key", "mun_key"))

    miss_idx <- which(is.na(data_block$lgcode))
    if (length(miss_idx) && has_stringdist) {
      for (i in miss_idx) {
        dk <- data_block$district_key[i]
        cand <- lookup %>% filter(district_key == dk)
        if (nrow(cand)) {
          m <- fuzzy_match(data_block$mun_key[i], cand$mun_key, max_dist = 2)
          if (!is.na(m)) {
            hit <- cand %>% filter(mun_key == m) %>% slice(1)
            data_block$lgcode[i]   <- hit$lgcode
            data_block$district[i] <- hit$district
            data_block$mun_name[i] <- hit$mun_name
          }
        }
      }
    }
  } else {
    data_block$lgcode   <- NA_character_
    data_block$district <- NA_character_
    data_block$mun_name <- NA_character_
  }

  # Reorder useful cols up front
  front <- intersect(c("lgcode", "district", "mun_name",
                       "district_np", "mun_np",
                       "lg_code_raw", "lg_name_np", "sn"),
                     names(data_block))
  data_block <- data_block %>%
    mutate(source_file = basename(file)) %>%
    select(all_of(front), everything(), source_file)

  data_block
}

# ----- RUN -------------------------------------------------------------------

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  lookup <- load_lookup(LOOKUP_FILE)

  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$",
                      full.names = TRUE, recursive = FALSE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)

  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  # Report join quality
  if ("lgcode" %in% names(combined)) {
    miss <- sum(is.na(combined$lgcode))
    message(sprintf("Rows: %d | unmatched lgcode: %d (%.1f%%)",
                    nrow(combined), miss, 100 * miss / nrow(combined)))
    if (miss > 0) {
      unmatched <- combined %>%
        filter(is.na(lgcode)) %>%
        distinct(district_np, mun_np)
      writexl::write_xlsx(unmatched,
                          file.path(OUTPUT_DIR, "unmatched_lookup.xlsx"))
      message("Wrote ", file.path(OUTPUT_DIR, "unmatched_lookup.xlsx"))
    }
  }

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", nrow(combined), " rows, ",
          ncol(combined), " cols)")
  invisible(combined)
}

main()
