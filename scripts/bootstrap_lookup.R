# =============================================================================
# Reconcile your existing LG lookup with the names actually used in the data.
#
# Problem: your lookup has district + mun_name + lgcode (5-digit), but the
# mun_name spelling/spacing/suffix in the data files may not match exactly.
# This script:
#   1. Scans every xlsx under SCAN_DIRS and extracts unique (district, mun)
#      pairs from column C (format: "<mun>, <district>")
#   2. Loads your existing EXISTING_LOOKUP
#   3. Joins the two on NORMALIZED names (strips suffixes, whitespace,
#      punctuation), then fuzzy-matches anything still missing
#   4. Writes ONE reconciled lookup with district_np / mun_np EXACTLY as they
#      appear in the data, plus the lgcode from your lookup
#   5. Flags rows you need to fix by hand
#
# Run once. Fix the flagged rows. Then point LOOKUP_FILE in
# clean_lg_fiscal.R at the resulting file.
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(purrr)
  has_stringdist <- requireNamespace("stringdist", quietly = TRUE)
})

# ----- CONFIG ----------------------------------------------------------------

# Scan EVERY xlsx under these folders (recursive). Add/remove as needed.
SCAN_DIRS <- c(
  "क्षेत्रगत बजेट तथा खर्च",
  "बजेट र खर्चको सारांश",
  "खर्च शीर्षक अनुसार बजेट तथा खर्च",
  "व्यय अनुमान",
  "लक्षित समूह अनुसार बजेट तथा खर्च",
  "स्थानीय तह अनुसार बजेट र खर्चको सारांश",
  "स्थानीय तह अनुसार श्रोतगत बजेट तथा खर्च",
  "स्थानीय तह अनुसार Budget estimates and expense reports",
  "स्थानीय तह अनुसार Revenue projection and receipts",
  "स्थानीय तह अनुसार Sectoral budget and expenditure"
)

# Your existing lookup. Set the path and the column names below.
EXISTING_LOOKUP <- "lookup/lg_lookup_master.xlsx"
LK_DISTRICT_NP  <- "जिल्ला"        # Nepali district column in your lookup
LK_MUN_NP       <- "स्थानीय तह"    # Nepali mun column in your lookup
LK_DISTRICT_EN  <- "district"      # set to NA if not present
LK_MUN_EN       <- "mun_name"      # set to NA if not present
LK_LGCODE       <- "lgcode"        # 5-digit code (vmun_code / newcode are the same)

OUTPUT_DIR  <- "lookup"
OUTPUT_FILE <- "lg_lookup_reconciled.xlsx"

# Column in the data files holding "<mun>, <district>". Sample uses column 3.
NAME_COL_DEFAULT <- 3
SKIP_CANDIDATES  <- 5:9   # header rows to try; picks the one that yields most data

# ----- HELPERS ---------------------------------------------------------------

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

fuzzy_match <- function(name, candidates, max_dist = 2) {
  if (!has_stringdist || length(candidates) == 0 || is.na(name)) return(NA_integer_)
  d <- stringdist::stringdist(name, candidates, method = "osa")
  if (min(d) <= max_dist) which.min(d) else NA_integer_
}

# Try a few skip-row counts and pick the one that yields the most rows whose
# value in NAME_COL_DEFAULT contains a comma (the "mun, district" pattern).
extract_names_from_file <- function(file) {
  best <- list(n = 0L, df = NULL)
  for (sk in SKIP_CANDIDATES) {
    df <- tryCatch(
      suppressMessages(read_excel(file, skip = sk, col_names = FALSE,
                                  .name_repair = "minimal")),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) < NAME_COL_DEFAULT) next
    v <- as.character(df[[NAME_COL_DEFAULT]])
    ok <- !is.na(v) & grepl(",", v)
    if (sum(ok) > best$n) best <- list(n = sum(ok), df = df[ok, , drop = FALSE])
  }
  if (best$n == 0) {
    message("  skip (no parseable rows): ", basename(file))
    return(NULL)
  }
  parts <- str_split_fixed(as.character(best$df[[NAME_COL_DEFAULT]]), ",", 2)
  tibble(
    mun_np      = str_squish(parts[, 1]),
    district_np = str_squish(parts[, 2]),
    source_file = basename(file)
  )
}

# ----- RUN -------------------------------------------------------------------

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  # 1. Collect every (district, mun) seen in the data
  dirs <- SCAN_DIRS[dir.exists(SCAN_DIRS)]
  if (!length(dirs)) stop("None of SCAN_DIRS exist from working directory: ", getwd())
  files <- unlist(lapply(dirs, list.files,
                         pattern = "\\.xlsx$",
                         recursive = TRUE, full.names = TRUE))
  message("Scanning ", length(files), " files ...")

  data_names <- map_dfr(files, extract_names_from_file) %>%
    mutate(district_key = normalize_np(district_np),
           mun_key      = normalize_np(mun_np)) %>%
    group_by(district_key, mun_key) %>%
    summarise(district_np = first(district_np),
              mun_np      = first(mun_np),
              n_files     = n_distinct(source_file),
              .groups = "drop")

  message("Unique (district, mun) pairs in data: ", nrow(data_names))

  # 2. Load existing lookup
  if (!file.exists(EXISTING_LOOKUP)) {
    message("No existing lookup at ", EXISTING_LOOKUP,
            " - writing data-only stub with empty lgcode for manual fill.")
    out <- data_names %>%
      mutate(lgcode = NA_character_, district = NA_character_,
             mun_name = NA_character_, match_type = "none",
             needs_review = TRUE) %>%
      select(district_np, mun_np, district, mun_name, lgcode,
             match_type, needs_review, n_files)
    writexl::write_xlsx(out, file.path(OUTPUT_DIR, OUTPUT_FILE))
    return(invisible(out))
  }

  lk_raw <- read_excel(EXISTING_LOOKUP)
  needed <- c(LK_DISTRICT_NP, LK_MUN_NP, LK_LGCODE)
  miss <- setdiff(needed, names(lk_raw))
  if (length(miss)) stop("Existing lookup missing columns: ", paste(miss, collapse = ", "))

  lk <- lk_raw %>%
    transmute(
      lk_district_np = .data[[LK_DISTRICT_NP]],
      lk_mun_np      = .data[[LK_MUN_NP]],
      district_key   = normalize_np(.data[[LK_DISTRICT_NP]]),
      mun_key        = normalize_np(.data[[LK_MUN_NP]]),
      district = if (!is.na(LK_DISTRICT_EN) && LK_DISTRICT_EN %in% names(lk_raw))
                   .data[[LK_DISTRICT_EN]] else NA_character_,
      mun_name = if (!is.na(LK_MUN_EN) && LK_MUN_EN %in% names(lk_raw))
                   .data[[LK_MUN_EN]] else NA_character_,
      lgcode   = as.character(.data[[LK_LGCODE]])
    ) %>%
    distinct(district_key, mun_key, .keep_all = TRUE)

  # 3. Normalized exact join, then fuzzy fallback for misses
  joined <- data_names %>%
    left_join(lk %>% select(district_key, mun_key,
                            district, mun_name, lgcode),
              by = c("district_key", "mun_key")) %>%
    mutate(match_type = if_else(!is.na(lgcode), "exact_normalized", NA_character_))

  miss_idx <- which(is.na(joined$lgcode))
  if (length(miss_idx) && has_stringdist) {
    message("Fuzzy-matching ", length(miss_idx), " unmatched rows ...")
    for (i in miss_idx) {
      dk <- joined$district_key[i]
      cand <- lk %>% filter(district_key == dk)
      if (!nrow(cand)) next
      hit <- fuzzy_match(joined$mun_key[i], cand$mun_key, max_dist = 2)
      if (!is.na(hit)) {
        joined$lgcode[i]     <- cand$lgcode[hit]
        joined$district[i]   <- cand$district[hit]
        joined$mun_name[i]   <- cand$mun_name[hit]
        joined$match_type[i] <- "fuzzy"
      }
    }
  }
  joined$match_type[is.na(joined$match_type)] <- "unmatched"
  joined$needs_review <- joined$match_type %in% c("fuzzy", "unmatched")

  out <- joined %>%
    select(district_np, mun_np, district, mun_name, lgcode,
           match_type, needs_review, n_files) %>%
    arrange(needs_review, district_np, mun_np)

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(out, out_path)

  msg <- table(out$match_type)
  message(sprintf("Wrote %s : %d rows | %s",
                  out_path, nrow(out),
                  paste(names(msg), msg, sep = "=", collapse = ", ")))
  message("Open the file, fix rows where needs_review=TRUE, then point ",
          "LOOKUP_FILE in clean_lg_fiscal.R at it.")
  invisible(out)
}

main()
