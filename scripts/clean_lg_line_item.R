# =============================================================================
# Clean: खर्च शीर्षक अनुसार बजेट तथा खर्च / LG Line Item
#
# Per-LG, per-line-item budget vs expenditure. Files in this folder are
# partitioned by district within each FY. One FY subfolder = one output xlsx.
#
# Structure (varies in column count file-to-file):
#   - rows 1-4 : title; R4C1 holds "आ.व. : <fy> ... प्रदेश : <prov> जिल्ला : <dist>"
#   - row 5   : top-level group: चालु (current) | पूंजीगत (capital) | जम्मा (drop)
#   - row 6   : line-item codes (every 2 cols, like २११११ २१११२ ...)
#   - row 7   : बजेट/खर्च alternating
#   - rows 8..n-1 : 1 row per LG (~10 per file)
#   - row n   : जम्मा (grand-total row) -- DROP
#   - cols 1-3 : sn | संकेत (8-digit LG code) | नाम (mun, district)
#
# Output is in LONG format (much tidier given 100+ line items):
#   fy, province, district_filter, lgcode, district_np, mun_np,
#   lg_code_raw, lg_name_np, line_item (code), fund_type (current|capital),
#   bud, exp, source_file
#
# Loops every FY subfolder under INPUT_DIR and writes ONE xlsx per FY:
#   cleaned_output/lg_line_item_<fy>.xlsx
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr); library(tidyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "खर्च शीर्षक अनुसार बजेट तथा खर्च/LG Line Item"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

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

# Extract fy "2076/77", province, district filter from R4C1
parse_title <- function(s) {
  s <- np_digits_to_ascii(as.character(s))
  fy   <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  prov <- regmatches(s, regexpr("प्रदेश\\s*:\\s*[^[:space:]जिल्ला]+", s))
  prov <- if (length(prov)) str_squish(sub("^प्रदेश\\s*:\\s*", "", prov))
          else NA_character_
  dist <- regmatches(s, regexpr("जिल्ला\\s*:\\s*.+", s))
  dist <- if (length(dist)) str_squish(sub("^जिल्ला\\s*:\\s*", "", dist))
          else NA_character_
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

# Walk row 5 and pin the column ranges for current / capital / total
detect_ranges <- function(raw) {
  ncols <- ncol(raw)
  r5 <- as.character(unlist(raw[5, ]))
  chalu_pos <- which(r5 == "चालु")
  punji_pos <- which(r5 == "पूंजीगत")
  jamma_pos <- which(r5 == "जम्मा")
  if (length(chalu_pos) != 1 || length(punji_pos) != 1) {
    stop("Could not find चालु / पूंजीगत headers in row 5")
  }
  chalu_end <- punji_pos - 1
  punji_end <- if (length(jamma_pos)) jamma_pos - 1 else ncols
  list(chalu = c(chalu_pos, chalu_end), punji = c(punji_pos, punji_end))
}

# Given a column-range pair (start, end) and the raw sheet, return a tibble
# with one row per (lg_row, line_item_code) holding bud + exp.
extract_block <- function(raw, range, fund_type, lg_rows) {
  cols <- range[1]:range[2]
  if (length(cols) %% 2 != 0) {
    # Should be paired bud/exp; if odd, drop trailing col
    cols <- cols[-length(cols)]
  }
  out <- list()
  for (k in seq(1, length(cols), by = 2)) {
    cbud <- cols[k]; cexp <- cols[k + 1]
    code <- np_digits_to_ascii(raw[[6, cbud]])
    if (is.na(code) || code == "") next
    bud <- parse_np_num(raw[[cbud]])[lg_rows]
    exp <- parse_np_num(raw[[cexp]])[lg_rows]
    out[[length(out) + 1]] <- tibble(
      row_id    = seq_along(lg_rows),
      line_item = code,
      fund_type = fund_type,
      bud       = bud,
      exp       = exp
    )
  }
  bind_rows(out)
}

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  meta <- parse_title(raw[[4, 1]])

  ranges <- detect_ranges(raw)

  # Identify LG data rows: from row 8 to second-to-last where the row has
  # a numeric-looking lg code in col 2 and a name with a comma in col 3.
  codes <- np_digits_to_ascii(raw[[2]])
  names_v <- as.character(raw[[3]])
  is_lg <- !is.na(codes) & grepl("^[0-9]{6,10}$", codes) &
           !is.na(names_v) & grepl(",", names_v)
  lg_rows <- which(is_lg)

  if (!length(lg_rows)) stop("No LG rows detected in ", basename(file))

  # Build per-LG id frame
  id_df <- tibble(
    row_id      = seq_along(lg_rows),
    lg_code_raw = codes[lg_rows],
    lg_name_np  = names_v[lg_rows]
  )
  parts <- str_split_fixed(id_df$lg_name_np, ",", 2)
  id_df$mun_np      <- str_squish(parts[, 1])
  id_df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) {
    id_df <- id_df %>% left_join(lookup, by = c("district_np", "mun_np"))
  } else {
    id_df$lgcode <- NA_character_
  }

  cur_block <- extract_block(raw, ranges$chalu, "current", lg_rows)
  cap_block <- extract_block(raw, ranges$punji, "capital", lg_rows)
  vals <- bind_rows(cur_block, cap_block)

  df <- id_df %>%
    left_join(vals, by = "row_id", relationship = "one-to-many") %>%
    mutate(fy = meta$fy,
           province        = meta$province,
           district_filter = meta$district_filter,
           source_file     = basename(file)) %>%
    select(fy, province, district_filter,
           lgcode, district_np, mun_np,
           lg_code_raw, lg_name_np,
           line_item, fund_type, bud, exp,
           source_file)
  df
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)

  fy_dirs <- list.dirs(INPUT_DIR, recursive = FALSE)
  if (!length(fy_dirs)) stop("No FY subfolders in ", INPUT_DIR)

  for (fyd in fy_dirs) {
    files <- list.files(fyd, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { message("Skipping empty: ", fyd); next }

    combined <- map_dfr(files, ~ clean_one(.x, lookup))
    fy_short <- tolower(basename(fyd))  # e.g. "fy207677"
    out_path <- file.path(OUTPUT_DIR, paste0("lg_line_item_", fy_short, ".xlsx"))

    write_xlsx(combined, out_path)
    miss <- sum(is.na(combined$lgcode))
    distinct_li <- length(unique(combined$line_item))
    distinct_lgs <- length(unique(combined$lg_code_raw))
    message(sprintf("  %s -> %d rows | %d distinct LGs | %d distinct line items | unmatched lgcode: %d",
                    basename(out_path), nrow(combined), distinct_lgs, distinct_li, miss))
  }
}

main()
