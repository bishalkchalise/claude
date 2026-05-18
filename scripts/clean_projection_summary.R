# =============================================================================
# Clean: आय र व्यय प्रक्षेपण को सारांश / Summary
# Per-LG revenue + expense projection (current/capital/financing/total).
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "आय र व्यय प्रक्षेपण को सारांश/Summary"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "projection_summary.xlsx"

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 8) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  rev_col <- which(r5 == "राजस्व अनुमान")
  exp_col <- which(r5 == "व्यय अनुमान")
  if (!length(rev_col) || !length(exp_col))
    stop("missing राजस्व अनुमान / व्यय अनुमान in row 5")
  rev_col <- rev_col[1]; exp_col <- exp_col[1]

  cur_col <- which(r6 == "चालु"); pun_col <- which(r6 == "पूँजीगत")
  fin_col <- which(r6 == "वित्तीय"); tot_col <- which(r6 == "जम्मा")
  if (!length(cur_col) || !length(pun_col) ||
      !length(fin_col) || !length(tot_col))
    stop("missing fund-type sub-headers in row 6")

  code_col <- which(r6 == "संकेत")[1]
  name_col <- which(r6 == "नाम")[1]
  if (is.na(code_col) || is.na(name_col))
    stop("missing संकेत / नाम in row 6")

  df <- tibble(
    lg_code_raw   = np_digits_to_ascii(raw[[code_col]]),
    lg_name_np    = as.character(raw[[name_col]]),
    revenue_proj  = parse_np_num(raw[[rev_col]]),
    exp_current   = parse_np_num(raw[[cur_col[1]]]),
    exp_capital   = parse_np_num(raw[[pun_col[1]]]),
    exp_financing = parse_np_num(raw[[fin_col[1]]]),
    exp_total     = parse_np_num(raw[[tot_col[1]]])
  )
  df <- df[7:nrow(df), , drop = FALSE]

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
         "revenue_proj","exp_current","exp_capital","exp_financing","exp_total",
         "source_file"), drop = FALSE]
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
