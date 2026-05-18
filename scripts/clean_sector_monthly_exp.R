# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector Monthly Exp
#
# Structure (locked spec):
#   - rows 1-4 : title metadata (R4C1 = fiscal year)
#   - rows 5-6 : 2-level header (group > month)
#   - rows 7..n-1 : data (sector rollups + subsectors interleaved)
#   - row n    : कुल जम्मा -- DROP
#   - cols     : sn | शीर्षक | बजेट | <12 month exp cols> | जम्मा | खर्च(%) | मौज्दात
#
# National rollup, NOT per-LG. No lgcode join.
# Sector rollup rows dropped (first occurrence of each sector name).
# Output: one row per (sector, subsector). Expected 43 rows per file.
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(purrr)
})

INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector Monthly Exp"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_monthly_exp.xlsx"

SECTOR_PREFIX <- c(
  "आर्थिक विकास"                          = "econ",
  "सामाजिक विकास"                         = "social",
  "पूर्वाधार विकास"                          = "infra",
  "सुशासन तथा अन्तरसम्बन्धित क्षेत्र"            = "gov",
  "कार्यालय सञ्चालन तथा प्रशासनिक"          = "admin"
)

SUBSECTOR_LABEL <- c(
  "नभएको" = "none", "आपूर्ति" = "supply", "उद्योग" = "industry",
  "कृषि" = "agri", "जलश्रोत तथा सिंचाई" = "water_irrig", "पर्यटन" = "tourism",
  "पशुपन्छी विकास" = "livestock", "बन" = "forest", "भूमि व्यवस्था" = "land",
  "वाणिज्य" = "commerce", "वित्तीय क्षेत्र" = "finance",
  "सहकारी" = "coop", "खानेपानी तथा सरसफाई" = "wash",
  "जनसंख्या तथा बसाईसराई" = "pop_migration", "भाषा तथा संस्कृति" = "lang_culture",
  "युवा तथा खेलकुद" = "youth_sports",
  "लैंगिक समानता तथा सामाजिक समावेशीकरण" = "gesi",
  "शिक्षा" = "edu", "स्वास्थ्य" = "health",
  "सामाजिक सुरक्षा तथा संरक्षण" = "social_protection", "उर्जा" = "energy",
  "पुननिर्माण" = "reconstruction", "बिज्ञान तथा प्रबिधि" = "sci_tech",
  "भवन, आवास तथा सहरी विकास" = "housing_urban", "यातयात पूर्वाधार" = "transport",
  "संचार तथा सूचना प्रबिधि" = "ict",
  "सम्पदा पूर्वाधार" = "heritage", "अनुगमन तथा मूल्यांकन" = "me_eval",
  "कानुन तथा न्याय" = "law_justice", "गरिबी निवारण" = "poverty",
  "तथ्यांक प्रणाली" = "stats", "परराष्ट्र" = "foreign",
  "प्रशासकीय सुशासन" = "pub_admin", "मानब संशाधन विकास" = "hrd",
  "योजना तर्जुमा र कार्यन्वयन" = "planning", "वातावरण तथा जलवायु" = "env_climate",
  "वित्तीय सुशासन" = "fin_gov", "विपद व्यवस्थापन" = "disaster",
  "श्रम तथा रोजगारी" = "labor", "शान्ति तथा सुव्यवस्था" = "peace",
  "शासन प्रणाली" = "governance",
  "कार्यालय सञ्चालन तथा प्रशासनिक" = "ops"
)

# Nepali fiscal-year month order: साउन (mid-July) -> असार (mid-July next year).
MONTH_LABEL <- c(
  "साउन"    = "exp_saun",
  "भदौ"     = "exp_bhadau",
  "आस्बिन"  = "exp_asoj",
  "कार्तिक"  = "exp_kartik",
  "मार्ग"    = "exp_mangsir",
  "पौष"    = "exp_poush",
  "माघ"    = "exp_magh",
  "फागुन"   = "exp_falgun",
  "चैत्र"    = "exp_chaitra",
  "बैशाख"   = "exp_baisakh",
  "जेठ"    = "exp_jestha",
  "असार"   = "exp_asar"
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
  v[neg] <- -v[neg]
  v
}

extract_fy <- function(title_cell) {
  s <- np_digits_to_ascii(as.character(title_cell))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m) == 0) NA_character_ else m
}

clean_one <- function(file) {
  message("Reading: ", basename(file))

  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  stopifnot(nrow(raw) >= 7, ncol(raw) >= 18)

  fy <- extract_fy(raw[[4, 1]])

  # Verify month headers are where we expect (row 6, cols 4-15)
  expected_months <- names(MONTH_LABEL)
  actual_months <- as.character(unlist(raw[6, 4:15]))
  if (!identical(actual_months, expected_months)) {
    stop("Month header mismatch in ", basename(file),
         ": expected ", paste(expected_months, collapse=","),
         " got ", paste(actual_months, collapse=","))
  }
  if (raw[[6, 16]] != "जम्मा") {
    stop("Expected जम्मा at R6C16, got: ", raw[[6, 16]])
  }

  # Numeric cols C3..C18
  vals <- as.data.frame(lapply(raw[, 3:18], parse_np_num))
  names(vals) <- c("annual_bud",
                   unname(MONTH_LABEL),
                   "exp_total",
                   "exp_pct",
                   "balance")

  df <- bind_cols(
    tibble(sn = as.character(raw[[1]]),
           raw_np = as.character(raw[[2]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  # Drop grand-total row
  df <- df %>% filter(sn != "कुल जम्मा" & !is.na(raw_np))

  # Sector rollup state machine (same logic as clean_sector_fund_type.R)
  sector_names <- names(SECTOR_PREFIX)
  current_sector <- NA_character_
  sectors_seen   <- character()
  keep_flag      <- logical(nrow(df))
  sector_col     <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    nm <- str_squish(df$raw_np[i])
    if (nm %in% sector_names && !(nm %in% sectors_seen)) {
      sectors_seen   <- c(sectors_seen, nm)
      current_sector <- unname(SECTOR_PREFIX[nm])
      keep_flag[i]   <- FALSE
    } else {
      keep_flag[i]   <- TRUE
      sector_col[i]  <- current_sector
    }
  }

  df <- df[keep_flag, , drop = FALSE]
  df$sector    <- sector_col[keep_flag]
  df$subsector <- ifelse(df$raw_np %in% names(SUBSECTOR_LABEL),
                         unname(SUBSECTOR_LABEL[df$raw_np]),
                         NA_character_)

  bad <- df %>% filter(is.na(subsector))
  if (nrow(bad)) {
    stop("Unmapped subsector names in ", basename(file), ":\n  ",
         paste(head(bad$raw_np, 5), collapse = "\n  "))
  }

  df$fy <- fy
  df$source_file <- basename(file)

  val_cols <- c("annual_bud",
                unname(MONTH_LABEL),
                "exp_total", "exp_pct", "balance")
  df[, c("fy", "sector", "subsector", val_cols, "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$",
                      full.names = TRUE, recursive = FALSE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)

  combined <- map_dfr(files, clean_one)

  n_per_file <- combined %>% count(source_file)
  message(sprintf("Rows: %d (%s)",
                  nrow(combined),
                  paste(sprintf("%s=%d", n_per_file$source_file, n_per_file$n),
                        collapse = ", ")))

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", nrow(combined), " rows x ",
          ncol(combined), " cols)")
  invisible(combined)
}

main()
