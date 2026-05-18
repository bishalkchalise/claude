# =============================================================================
# Clean: बजेट र खर्चको सारांश / LG Kosh
# Per-LG budget vs expenditure across special funds (कोष).
# Fund columns detected from row 5 so additional / missing funds are tolerated.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "बजेट र खर्चको सारांश/LG Kosh"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "lg_kosh.xlsx"

KOSH_PREFIX <- c(
  "आकस्मिक कोष"                                    = "contingency",
  "कर्मचारी कल्याण कोष"                              = "emp_welfare",
  "कार्य संचालन कोष - बिबिध"                       = "ops_misc",
  "गरिवी निवारण तथा सामाजिक परिचालन कोष"      = "poverty_social",
  "प्रकोप व्यवस्थापन कोष"                            = "disaster",
  "बिबिध खर्च खाता/कोष- बैंक (ग २-७)"             = "misc_bank",
  "मर्मत सम्भार कोष"                                = "maintenance",
  "महिला तथा वाल विकास कोष"                     = "women_child",
  "मानव संशाधन विकास कोष"                       = "hrd_fund",
  "वातावरण ब्यवस्थापन कोष"                        = "env_mgmt",
  "स्थानीय विकास कोष कार्यक्रम संचालन कोष"      = "ldf_ops",
  "स्थानीय विकास घुम्ती कोष"                        = "ldf_revolving"
)

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 5) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  code_col <- which(r6 == "संकेत")[1]
  name_col <- which(r6 == "नाम")[1]
  if (is.na(code_col) || is.na(name_col)) stop("missing संकेत / नाम in row 6")

  val_cols <- list()
  for (nm in names(KOSH_PREFIX)) {
    pos <- which(r5 == nm)
    if (length(pos)) {
      c <- pos[1]
      if (c + 1 <= ncol(raw) &&
          r6[c]   %in% names(BUDEXP_LABEL) &&
          r6[c+1] %in% names(BUDEXP_LABEL)) {
        pref <- KOSH_PREFIX[[nm]]
        val_cols[[paste0(pref, "_", BUDEXP_LABEL[[r6[c]]])]]   <- c
        val_cols[[paste0(pref, "_", BUDEXP_LABEL[[r6[c+1]]])]] <- c + 1
      }
    }
  }
  if (!length(val_cols)) stop("no fund columns detected")
  # Drop the grand-total jamma block: if r5 == "जम्मा" was matched above
  # it would have prefix "total" via fallback; we don't include it here.

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

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
