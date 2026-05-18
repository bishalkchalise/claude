# =============================================================================
# Clean: विभाज्य कोष / (folder root)
# Per-(LG, revenue heading) divisible fund accounting.
# Tolerates source typos like "Distibution" vs "Distribution".
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "विभाज्य कोष"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "divisible_fund.xlsx"

R6_MAP <- c(
  "शुरुको मौज्दात" = "opening_balance",
  "प्राप्त"        = "received",
  "जम्मा प्राप्ती"  = "total_receipts",
  "नेपाल सरकार"  = "dist_fed",
  "प्रदेश सरकार"   = "dist_prov",
  "स्थानीय तह"    = "dist_lg",
  "Total Distibution"     = "dist_total",   # source typo
  "Total Distribution"    = "dist_total",
  "Distibution Percentage"= "dist_pct",
  "Distribution Percentage"= "dist_pct"
)

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 8) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  # LG block: संकेत/नाम at cols 2/3 (or detect)
  lg_code_col <- which(r6 == "संकेत")[1]
  lg_name_col <- which(r6 == "नाम")[1]
  if (is.na(lg_code_col)) lg_code_col <- 2
  if (is.na(lg_name_col)) lg_name_col <- 3

  # Revenue heading block under "राजस्व शीर्षक" in r5
  rh_top <- which(r5 == "राजस्व शीर्षक")
  rh_code_col <- if (length(rh_top))
    which(r6 == "संकेत" & seq_along(r6) >= rh_top[1])[1] else NA_integer_
  rh_name_col <- if (length(rh_top))
    which(r6 == "शीर्षक" & seq_along(r6) >= rh_top[1])[1] else NA_integer_
  if (is.na(rh_code_col)) rh_code_col <- 4
  if (is.na(rh_name_col)) rh_name_col <- 5

  val_cols <- list()
  for (nm in names(R6_MAP)) {
    pos <- which(r6 == nm)
    if (length(pos)) val_cols[[R6_MAP[[nm]]]] <- pos[1]
  }
  # Percentage column lives in row 5 (sometimes "Distibution" sometimes
  # "Distribution"). मौज्दात (closing balance) also in row 5.
  for (nm in c("Distibution Percentage", "Distribution Percentage")) {
    pos <- which(r5 == nm)
    if (length(pos)) { val_cols[["dist_pct"]] <- pos[1]; break }
  }
  bal_pos <- which(r5 == "मौज्दात")
  if (length(bal_pos)) val_cols[["closing_balance"]] <- bal_pos[1]
  if (!length(val_cols)) stop("no value columns detected")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

  df <- bind_cols(
    tibble(
      lg_code_raw          = np_digits_to_ascii(raw[[lg_code_col]]),
      lg_name_np           = as.character(raw[[lg_name_col]]),
      revenue_heading_code = np_digits_to_ascii(raw[[rh_code_col]]),
      revenue_heading_np   = as.character(raw[[rh_name_col]])
    ),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np) &
          !is.na(df$revenue_heading_code) &
          grepl("^[0-9]+$", df$revenue_heading_code) &
          !is.na(df$revenue_heading_np) &
          nzchar(str_squish(df$revenue_heading_np))
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid rows")

  .splt <- split_lg_name(df$lg_name_np)
  df$mun_np      <- .splt$mun_np
  df$district_np <- .splt$district_np
  df <- attach_lgcode(df, lookup)

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "revenue_heading_code","revenue_heading_np",
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
