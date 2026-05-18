# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector Fund Type
#
# National rollup. Drops sector rollup rows + grand-total row.
# Output: one row per (sector, subsector) × fund type measure.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector Fund Type"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_fund_type.xlsx"

# Expected fund types and trailing measures. The script also tolerates
# extra columns and missing ones by detecting positions from row 5.
FUND_TYPE_LABEL <- c("चालु" = "current", "पूँजीगत" = "capital",
                     "वित्तीय" = "financing", "जम्मा" = "total")
TRAIL_LABEL <- c("खर्च(%)" = "exp_pct", "मौज्दात" = "balance")

clean_one <- function(file) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 4) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  # Detect fund-type pairs in row 5 and trailing single columns
  ft_cols  <- list()
  for (nm in names(FUND_TYPE_LABEL)) {
    pos <- which(r5 == nm)
    if (length(pos)) ft_cols[[FUND_TYPE_LABEL[[nm]]]] <- pos[1]
  }
  trail_cols <- list()
  for (nm in names(TRAIL_LABEL)) {
    pos <- which(r5 == nm)
    if (length(pos)) trail_cols[[TRAIL_LABEL[[nm]]]] <- pos[1]
  }
  if (!length(ft_cols)) stop("no fund-type headers found in row 5")

  val_pairs <- list()
  for (lbl in names(ft_cols)) {
    c <- ft_cols[[lbl]]
    if (r6[c] %in% names(BUDEXP_LABEL) && c + 1 <= ncol(raw) &&
        r6[c + 1] %in% names(BUDEXP_LABEL)) {
      val_pairs[[paste0(lbl, "_", BUDEXP_LABEL[[r6[c]]])]]   <- c
      val_pairs[[paste0(lbl, "_", BUDEXP_LABEL[[r6[c+1]]])]] <- c + 1
    }
  }
  for (lbl in names(trail_cols)) val_pairs[[lbl]] <- trail_cols[[lbl]]

  vals <- as.data.frame(lapply(val_pairs, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_pairs)

  df <- bind_cols(
    tibble(sn = as.character(raw[[1]]), raw_np = as.character(raw[[2]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]
  df <- df %>% filter(!is.na(raw_np), !grepl("कुल जम्मा|जम्मा$",
                                              str_squish(as.character(sn))))

  sector_names <- names(SECTOR_PREFIX)
  current_sector <- NA_character_; sectors_seen <- character()
  keep_flag <- logical(nrow(df)); sector_col <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    nm <- str_squish(df$raw_np[i])
    if (nm %in% sector_names && !(nm %in% sectors_seen)) {
      sectors_seen <- c(sectors_seen, nm)
      current_sector <- unname(SECTOR_PREFIX[nm])
      keep_flag[i] <- FALSE
    } else {
      keep_flag[i] <- TRUE; sector_col[i] <- current_sector
    }
  }
  df <- df[keep_flag, , drop = FALSE]
  df$sector <- sector_col[keep_flag]
  df$subsector <- ifelse(df$raw_np %in% names(SUBSECTOR_LABEL),
                         unname(SUBSECTOR_LABEL[df$raw_np]), NA_character_)
  df <- df %>% filter(!is.na(subsector))

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","sector","subsector", names(val_pairs), "source_file"),
     drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- safe_map_files(files, clean_one)
  if (!nrow(combined)) { message("No data parsed."); return(invisible(NULL)) }
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols",
                  out_path, nrow(combined), ncol(combined)))
}

main()
