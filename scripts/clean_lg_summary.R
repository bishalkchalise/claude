# =============================================================================
# Clean: बजेट र खर्चको सारांश / LG Summary
# Per-LG operational fund summary. Headers detected from row 5 by name.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "बजेट र खर्चको सारांश/LG Summary"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "lg_summary.xlsx"

HEAD_MAP <- c(
  "शुरुको मौज्दात"  = "opening_balance",
  "प्राप्त"        = "received",
  "जम्मा प्राप्ती"   = "total_receipts",
  "खर्च"         = "expenditure",
  "मौज्दात"       = "closing_balance"
)

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 6 || ncol(raw) < 5) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  code_col <- which(r5 == "संकेत")[1]
  name_col <- which(r5 == "स्थानीय तह")[1]
  if (is.na(code_col) || is.na(name_col))
    stop("missing संकेत / स्थानीय तह in row 5")

  val_cols <- list()
  for (nm in names(HEAD_MAP)) {
    pos <- which(r5 == nm)
    if (length(pos)) val_cols[[HEAD_MAP[[nm]]]] <- pos[1]
  }
  if (!length(val_cols)) stop("no value headers found in row 5")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[code_col]]),
           lg_name_np  = as.character(raw[[name_col]])),
    vals
  )
  df <- df[6:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid LG rows")

  .splt <- split_lg_name(df$lg_name_np)
  df$mun_np      <- .splt$mun_np
  df$district_np <- .splt$district_np

  df <- attach_lgcode(df, lookup)

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         names(val_cols), "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
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
