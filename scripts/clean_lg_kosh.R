# =============================================================================
# Clean: बजेट र खर्चको सारांश / LG Kosh
#
# Per-LG budget & expenditure across 12 special funds (कोष).
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-6 : 2-level header (fund > बजेट/खर्च)
#   - rows 7..n-1 : per-LG data
#   - row n   : जम्मा (drop)
#   - cols    : sn | संकेत | नाम | 12 funds x (बजेट, खर्च) | जम्मा (drop)
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "बजेट र खर्चको सारांश/LG Kosh"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "lg_kosh.xlsx"

# Fund-name -> short English prefix, in the column order they appear in row 5.
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

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}
parse_np_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- np_digits_to_ascii(x)
  neg <- grepl("^\\s*\\(.*\\)\\s*$", x)
  x <- gsub("[(),\\s]", "", x, perl = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[neg] <- -v[neg]; v
}
extract_fy <- function(x) {
  s <- np_digits_to_ascii(as.character(x))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m)) m else NA_character_
}
load_lookup <- function(path) {
  if (!file.exists(path)) { warning("Lookup not found: ", path); return(NULL) }
  lk <- read.csv(path, fileEncoding = "UTF-8",
                 stringsAsFactors = FALSE, check.names = FALSE)
  lk %>% transmute(district_np = str_squish(district_np),
                   mun_np      = str_squish(mun_np),
                   lgcode      = as.character(lgcode)) %>%
        distinct(district_np, mun_np, .keep_all = TRUE)
}

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  fy <- extract_fy(raw[[4, 1]])

  # Validate row 5 fund-name headers at C4, C6, C8, ..., C26
  fund_cols <- seq(4, 26, by = 2)
  r5_funds  <- as.character(unlist(raw[5, fund_cols]))
  expected_funds <- names(KOSH_PREFIX)
  if (!identical(r5_funds, expected_funds)) {
    stop("Fund-header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_funds, collapse=" | "),
         "\n  got     : ", paste(r5_funds, collapse=" | "))
  }
  if (raw[[5, 28]] != "जम्मा") stop("Expected जम्मा at R5C28")

  # Row 6: संकेत | नाम | bud/exp alternation in C4..C29
  if (raw[[6, 2]] != "संकेत") stop("Expected संकेत at R6C2")
  if (raw[[6, 3]] != "नाम")   stop("Expected नाम at R6C3")
  be <- as.character(unlist(raw[6, 4:29]))
  if (!identical(be, rep(c("बजेट","खर्च"), 13))) {
    stop("बजेट/खर्च alternation broken in ", basename(file))
  }

  # Build value cols: 12 funds * (bud, exp). DROP C28/C29 (जम्मा total pair).
  val_names <- character()
  for (sp in unname(KOSH_PREFIX)) {
    val_names <- c(val_names, paste0(sp, "_bud"), paste0(sp, "_exp"))
  }
  vals <- as.data.frame(lapply(raw[, 4:27], parse_np_num))
  names(vals) <- val_names

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[2]]),
           lg_name_np  = as.character(raw[[3]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         val_names, "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  miss <- sum(is.na(combined$lgcode))
  per <- combined %>% count(source_file, fy)
  message(sprintf("Rows: %d | unmatched lgcode: %d", nrow(combined), miss))
  for (i in seq_len(nrow(per))) message(sprintf("  %s (fy=%s) -> %d rows",
                                                per$source_file[i], per$fy[i], per$n[i]))

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", nrow(combined), " x ", ncol(combined), ")")
  invisible(combined)
}

main()
