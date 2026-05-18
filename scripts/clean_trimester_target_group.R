# =============================================================================
# Clean: लक्षित समूह अनुसार बजेट तथा खर्च / Trimester Target Group
# National rollup: target group × 3 trimesters.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "लक्षित समूह अनुसार बजेट तथा खर्च/Trimester Target Group"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "trimester_target_group.xlsx"

TG_LABEL <- c(
  "100" = "other", "101" = "women", "102" = "children",
  "103" = "indigenous", "104" = "madhesi", "105" = "dalit",
  "106" = "marginalized", "107" = "disabled", "108" = "senior_citizen"
)

TRIMESTER_PREFIX <- c(
  "प्रथम चौमासिक" = "t1",
  "दोश्रो चौमासिक" = "t2",
  "तेस्रो चौमासिक" = "t3"
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
    tibble(target_group_code = np_digits_to_ascii(raw[[2]]),
           target_group_np   = as.character(raw[[3]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]
  df <- df %>% filter(!is.na(target_group_code),
                      grepl("^[0-9]+$", target_group_code),
                      !is.na(target_group_np),
                      nzchar(str_squish(target_group_np)))
  df$target_group <- ifelse(df$target_group_code %in% names(TG_LABEL),
                            unname(TG_LABEL[df$target_group_code]),
                            NA_character_)

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","target_group_code","target_group","target_group_np",
         names(val_cols), "source_file"), drop = FALSE]
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
