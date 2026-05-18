# =============================================================================
# Clean: आय र व्यय प्रक्षेपण को सारांश / Source Fund Type
# Per-LG income (by source) + expense (by fund type) + approval date.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "आय र व्यय प्रक्षेपण को सारांश/Source Fund Type"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "projection_source_fund_type.xlsx"

INC_PREFIX <- c(
  "संघीय सरकार"   = "inc_fed",
  "प्रदेश सरकार"    = "inc_prov",
  "राजस्व बाँडफाँट" = "inc_revshare",
  "राजस्व बाडफाड" = "inc_revshare",
  "आन्तरिक राजस्व" = "inc_internal",
  "अन्तरिक श्रोत"   = "inc_internal",
  "जन सहभागिता" = "inc_public",
  "जनसहभागिता"  = "inc_public",
  "कुल आय अनुमान" = "inc_total"
)
EXP_PREFIX <- c(
  "चालु"  = "exp_current",
  "पूँजीगत" = "exp_capital",
  "वित्तीय" = "exp_financing",
  "जम्मा"  = "exp_total"
)

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 8) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  code_col <- which(r6 == "संकेत")[1]
  name_col <- which(r6 == "नाम")[1]
  if (is.na(code_col) || is.na(name_col)) stop("missing संकेत / नाम in row 6")

  val_cols <- list()
  for (nm in names(INC_PREFIX)) {
    p <- which(r6 == nm)
    if (length(p)) val_cols[[INC_PREFIX[[nm]]]] <- p[1]
  }
  for (nm in names(EXP_PREFIX)) {
    p <- which(r6 == nm)
    if (length(p)) val_cols[[EXP_PREFIX[[nm]]]] <- p[1]
  }
  appr_col <- which(r5 == "सभाबाट स्वीकृत मिति")
  if (length(appr_col)) val_cols[["approval_date_col"]] <- appr_col[1]

  if (!length(val_cols)) stop("no value columns detected")

  vals <- lapply(val_cols, function(c) {
    v <- raw[[c]]
    # date column kept as string; rest numeric
    if (identical(c, val_cols$approval_date_col)) np_digits_to_ascii(v)
    else parse_np_num(v)
  })
  vals <- as.data.frame(vals, stringsAsFactors = FALSE)
  names(vals) <- names(val_cols)
  if ("approval_date_col" %in% names(vals))
    names(vals)[names(vals) == "approval_date_col"] <- "approval_date"

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[code_col]]),
           lg_name_np  = as.character(raw[[name_col]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid LG rows")

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)
  id_cols <- c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np")
  data_cols <- setdiff(names(df), c(id_cols, "source_file"))
  df[, c(id_cols, data_cols, "source_file"), drop = FALSE]
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
