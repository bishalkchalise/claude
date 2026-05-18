# =============================================================================
# Clean: खर्च शीर्षक अनुसार बजेट तथा खर्च / LG Line Item
# One output per FY subfolder. Long format: row per (LG, line_item).
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "खर्च शीर्षक अनुसार बजेट तथा खर्च/LG Line Item"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

# Walk row 5 and pin the column ranges for current / capital / total.
detect_ranges <- function(raw) {
  ncols <- ncol(raw)
  r5 <- as.character(unlist(raw[5, ]))
  chalu_pos <- which(r5 == "चालु")
  punji_pos <- which(r5 == "पूंजीगत")
  jamma_pos <- which(r5 == "जम्मा")
  if (!length(chalu_pos))
    stop("Could not find चालु header in row 5")
  chalu_end <- if (length(punji_pos)) punji_pos - 1
               else if (length(jamma_pos)) jamma_pos - 1
               else ncols
  punji_end <- if (length(jamma_pos)) jamma_pos - 1 else ncols
  list(
    chalu = c(chalu_pos[1], chalu_end[1]),
    punji = if (length(punji_pos)) c(punji_pos[1], punji_end[1]) else NULL
  )
}

extract_block <- function(raw, range, fund_type, lg_rows) {
  if (is.null(range)) return(NULL)
  cols <- range[1]:range[2]
  if (length(cols) %% 2 != 0) cols <- cols[-length(cols)]
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
  if (nrow(raw) < 8 || ncol(raw) < 8) stop("file too small")
  meta <- parse_title(raw[[4, 1]])

  ranges <- detect_ranges(raw)

  codes <- np_digits_to_ascii(raw[[2]])
  names_v <- as.character(raw[[3]])
  is_lg <- !is.na(codes) & grepl("^[0-9]{6,10}$", codes) &
           !is.na(names_v) & grepl(",", names_v)
  lg_rows <- which(is_lg)
  if (!length(lg_rows)) stop("no LG rows detected")

  id_df <- tibble(
    row_id      = seq_along(lg_rows),
    lg_code_raw = codes[lg_rows],
    lg_name_np  = names_v[lg_rows]
  )
  .splt <- split_lg_name(id_df$lg_name_np)
  id_df$mun_np      <- .splt$mun_np
  id_df$district_np <- .splt$district_np
  id_df <- attach_lgcode(id_df, lookup)

  cur_block <- extract_block(raw, ranges$chalu, "current", lg_rows)
  cap_block <- extract_block(raw, ranges$punji, "capital", lg_rows)
  vals <- bind_rows(cur_block, cap_block)
  if (!nrow(vals)) stop("no line item values extracted")

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
    combined <- safe_map_files(files, clean_one, lookup = lookup)
    if (!nrow(combined)) { message("  ", basename(fyd), " -- no rows"); next }
    fy_short <- tolower(basename(fyd))
    out_path <- file.path(OUTPUT_DIR, paste0("lg_line_item_", fy_short, ".xlsx"))
    write_xlsx(combined, out_path)
    miss <- sum(is.na(combined$lgcode))
    message(sprintf("  %s -> %d rows | %d LGs | %d line items | unmatched: %d",
                    basename(out_path), nrow(combined),
                    length(unique(combined$lg_code_raw)),
                    length(unique(combined$line_item)), miss))
  }
}

main()
