# =============================================================================
# Bootstrap an LG lookup from the raw data files themselves.
#
# The xlsx files already carry the LG code in column B (e.g. ८०१०१४०१) and the
# combined "mun name, district" string in column C. We scan every file, extract
# unique (district, mun_name, lg_code) tuples, and write them to one xlsx that
# YOU then review once and use as the canonical lookup for clean_lg_fiscal.R.
#
# Run once. After review, point LOOKUP_FILE in clean_lg_fiscal.R at the result.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# ----- CONFIG ----------------------------------------------------------------
# Scan EVERY xlsx under these folders (recursive). Add/remove as needed.
SCAN_DIRS    <- c(
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

OUTPUT_DIR  <- "lookup"
OUTPUT_FILE <- "lg_lookup_bootstrap.xlsx"

# Where the LG code and combined "mun, district" string typically live.
# These match the sample but the script also tries to auto-detect if it doesn't
# find numbers / commas at the expected columns.
CODE_COL_DEFAULT <- 2
NAME_COL_DEFAULT <- 3

# Header rows to skip when scanning for data. We try a range and pick whichever
# yields the most parseable rows.
SKIP_CANDIDATES <- 5:9

# ----- HELPERS ---------------------------------------------------------------

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}

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

# Score a (skip, code_col, name_col) combo by # of rows that look like real data
score_combo <- function(file, skip, code_col, name_col) {
  df <- tryCatch(
    suppressMessages(read_excel(file, skip = skip, col_names = FALSE,
                                .name_repair = "minimal")),
    error = function(e) NULL
  )
  if (is.null(df) || ncol(df) < max(code_col, name_col)) return(0L)
  codes <- np_digits_to_ascii(df[[code_col]])
  names_v <- as.character(df[[name_col]])
  ok <- !is.na(codes) & grepl("^[0-9]{4,10}$", codes) &
        !is.na(names_v) & grepl(",", names_v)
  sum(ok)
}

extract_one <- function(file) {
  best <- list(n = 0L, skip = NA, code = CODE_COL_DEFAULT, name = NAME_COL_DEFAULT)
  for (sk in SKIP_CANDIDATES) {
    for (cc in c(CODE_COL_DEFAULT, 1, 3)) {
      for (nc in c(NAME_COL_DEFAULT, 2, 4)) {
        if (cc == nc) next
        n <- score_combo(file, sk, cc, nc)
        if (n > best$n) best <- list(n = n, skip = sk, code = cc, name = nc)
      }
    }
  }
  if (best$n == 0) {
    message("  skip (no parseable rows): ", basename(file))
    return(NULL)
  }
  df <- suppressMessages(read_excel(file, skip = best$skip, col_names = FALSE,
                                    .name_repair = "minimal"))
  codes  <- np_digits_to_ascii(df[[best$code]])
  names_v <- as.character(df[[best$name]])
  ok <- !is.na(codes) & grepl("^[0-9]{4,10}$", codes) &
        !is.na(names_v) & grepl(",", names_v)
  parts <- str_split_fixed(names_v[ok], ",", 2)
  tibble(
    lg_code_raw = codes[ok],
    mun_np      = str_squish(parts[, 1]),
    district_np = str_squish(parts[, 2]),
    source_file = basename(file)
  )
}

# ----- RUN -------------------------------------------------------------------

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  dirs <- SCAN_DIRS[dir.exists(SCAN_DIRS)]
  if (!length(dirs)) stop("None of SCAN_DIRS exist from working directory: ", getwd())

  files <- unlist(lapply(dirs, list.files,
                         pattern = "\\.xlsx$",
                         recursive = TRUE, full.names = TRUE))
  message("Scanning ", length(files), " files ...")
  raw <- map_dfr(files, extract_one)
  if (!nrow(raw)) stop("No (code, name, district) rows extracted. Check column / skip defaults.")

  # The same (district, mun) should map to a single lgcode. lgcode in lookups
  # is typically 5 digits (vmun_code / newcode); take the LAST 5 of the raw code
  # but also keep the full raw code for traceability.
  agg <- raw %>%
    mutate(
      district_key = normalize_np(district_np),
      mun_key      = normalize_np(mun_np),
      lgcode_5     = str_sub(lg_code_raw, -5)
    ) %>%
    group_by(district_key, mun_key) %>%
    summarise(
      district_np   = first(district_np),
      mun_np        = first(mun_np),
      lgcode        = paste(sort(unique(lgcode_5)), collapse = "|"),
      lg_code_raw   = paste(sort(unique(lg_code_raw)), collapse = "|"),
      n_files       = n_distinct(source_file),
      .groups = "drop"
    ) %>%
    arrange(district_np, mun_np)

  # Flag rows where the same (district, mun) ended up with >1 distinct 5-digit
  # code — those need your manual review.
  agg <- agg %>%
    mutate(needs_review = grepl("\\|", lgcode))

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(agg, out_path)
  message(sprintf("Wrote %s : %d unique LGs (%d need review)",
                  out_path, nrow(agg), sum(agg$needs_review)))
  message("Add english 'district' and 'mun_name' columns by hand, save, ",
          "then point LOOKUP_FILE at this file.")
  invisible(agg)
}

main()
