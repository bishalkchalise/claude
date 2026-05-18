# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector LG
#
# Per-LG sectoral budget vs expenditure. Subsector positions are detected
# from row 6 so files with fewer/more subsectors still work.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector LG"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_lg.xlsx"

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 8 || ncol(raw) < 6)
    stop("file too small (", nrow(raw), "x", ncol(raw), ")")

  fy <- extract_fy(raw[[4, 1]])

  # Detect each subsector pair from row 6.
  r5_full <- ffill(as.character(unlist(raw[5, ])))
  r6_full <- as.character(unlist(raw[6, ]))
  r7_full <- as.character(unlist(raw[7, ]))

  subsec_positions <- which(!is.na(r6_full) & r6_full != "")
  if (!length(subsec_positions))
    stop("no subsector cells in row 6")

  bud_cols <- integer(); exp_cols <- integer(); names_vec <- character()

  for (c in subsec_positions) {
    sec_full <- r5_full[c]
    sub_full <- r6_full[c]
    if (is.na(sec_full) || sec_full == "जम्मा") next   # drop grand-total pair
    if (!(sec_full %in% names(SECTOR_PREFIX))) {
      warning("Unknown sector '", sec_full, "' at C", c,
              " in ", basename(file), " — skipped this pair", call. = FALSE)
      next
    }
    if (!(sub_full %in% names(SUBSECTOR_LABEL))) {
      warning("Unknown subsector '", sub_full, "' at C", c,
              " in ", basename(file), " — skipped this pair", call. = FALSE)
      next
    }
    if (c + 1 > length(r7_full) ||
        !(r7_full[c]   %in% names(BUDEXP_LABEL)) ||
        !(r7_full[c+1] %in% names(BUDEXP_LABEL))) next

    sec_pref  <- unname(SECTOR_PREFIX[sec_full])
    sub_lbl   <- unname(SUBSECTOR_LABEL[sub_full])
    be_first  <- unname(BUDEXP_LABEL[r7_full[c]])
    be_second <- unname(BUDEXP_LABEL[r7_full[c+1]])

    bud_cols  <- c(bud_cols, c)
    exp_cols  <- c(exp_cols, c + 1)
    names_vec <- c(names_vec,
                   paste(sec_pref, sub_lbl, be_first,  sep = "_"),
                   paste(sec_pref, sub_lbl, be_second, sep = "_"))
  }
  if (!length(bud_cols)) stop("no usable bud/exp pairs detected")

  data_cols  <- as.integer(rbind(bud_cols, exp_cols))
  data_names <- make.unique(names_vec, sep = "_")

  id <- tibble(
    lg_code_raw = np_digits_to_ascii(raw[[2]]),
    lg_name_np  = as.character(raw[[3]])
  )
  vals <- as.data.frame(lapply(raw[, data_cols], parse_np_num),
                        col.names = data_names)
  df <- bind_cols(id, vals)
  df <- df[8:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid LG rows")

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) {
    df <- df %>% left_join(lookup, by = c("district_np", "mun_np"))
  } else {
    df$lgcode <- NA_character_
  }

  df$fy <- fy
  df$source_file <- basename(file)

  id_cols <- c("fy", "lgcode", "district_np", "mun_np",
               "lg_code_raw", "lg_name_np", "source_file")
  val_cols <- setdiff(names(df), id_cols)
  df[, c(id_cols, val_cols), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)

  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$",
                      full.names = TRUE, recursive = FALSE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)

  combined <- safe_map_files(files, clean_one, lookup = lookup)
  if (!nrow(combined)) { message("No data parsed."); return(invisible(NULL)) }

  miss <- sum(is.na(combined$lgcode))
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols (%d unmatched lgcode)",
                  out_path, nrow(combined), ncol(combined), miss))
}

main()
