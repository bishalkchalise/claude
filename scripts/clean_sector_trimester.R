# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector Trimester
#
# National rollup. 3 trimesters + total. Trimester columns detected from row 5.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector Trimester"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_trimester.xlsx"

TRIMESTER_PREFIX <- c(
  "प्रथम चौमासिक" = "t1",
  "दोश्रो चौमासिक" = "t2",
  "तेस्रो चौमासिक" = "t3",
  "जम्मा"          = "total"
)

clean_one <- function(file) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 4) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  val_cols <- list()
  for (nm in names(TRIMESTER_PREFIX)) {
    pos <- which(r5 == nm)
    if (length(pos)) {
      c <- pos[1]
      if (c + 1 <= ncol(raw) &&
          r6[c]   %in% names(BUDEXP_LABEL) &&
          r6[c+1] %in% names(BUDEXP_LABEL)) {
        pref <- TRIMESTER_PREFIX[[nm]]
        val_cols[[paste0(pref, "_", BUDEXP_LABEL[[r6[c]]])]]   <- c
        val_cols[[paste0(pref, "_", BUDEXP_LABEL[[r6[c+1]]])]] <- c + 1
      }
    }
  }
  pct_pos <- which(r6 == "खर्च(%)")
  if (length(pct_pos)) val_cols[["exp_pct"]] <- pct_pos[1]
  bal_pos <- which(r6 == "मौज्दात")
  if (length(bal_pos)) val_cols[["balance"]] <- bal_pos[1]

  if (!length(val_cols)) stop("no trimester columns detected")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

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
  df[, c("fy","sector","subsector", names(val_cols), "source_file"),
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
