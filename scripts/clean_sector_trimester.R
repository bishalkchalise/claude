# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector Trimester
#
# National rollup. 3 Nepali fiscal-year trimesters (चौमासिक) x (budget, exp).
# Columns: sn | शीर्षक | त्रिमास1..3 (बजेट/खर्च) | जम्मा (bud/exp) | खर्च(%) | मौज्दात
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector Trimester"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_trimester.xlsx"

SECTOR_PREFIX <- c(
  "आर्थिक विकास" = "econ", "सामाजिक विकास" = "social",
  "पूर्वाधार विकास" = "infra",
  "सुशासन तथा अन्तरसम्बन्धित क्षेत्र" = "gov",
  "कार्यालय सञ्चालन तथा प्रशासनिक" = "admin"
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
  "योजना तर्जुमा र कार्यन्वयन" = "planning",
  "वातावरण तथा जलवायु" = "env_climate",
  "वित्तीय सुशासन" = "fin_gov", "विपद व्यवस्थापन" = "disaster",
  "श्रम तथा रोजगारी" = "labor", "शान्ति तथा सुव्यवस्था" = "peace",
  "शासन प्रणाली" = "governance",
  "कार्यालय सञ्चालन तथा प्रशासनिक" = "ops"
)

TRIMESTER_PREFIX <- c(
  "प्रथम चौमासिक"  = "t1",
  "दोश्रो चौमासिक" = "t2",
  "तेस्रो चौमासिक" = "t3",
  "जम्मा"          = "total"
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

clean_one <- function(file) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  stopifnot(nrow(raw) >= 7, ncol(raw) >= 12)
  fy <- extract_fy(raw[[4, 1]])

  # Validate trimester headers at R5 C3,C5,C7,C9
  tri_cells <- as.character(unlist(raw[5, c(3,5,7,9)]))
  if (!identical(tri_cells, names(TRIMESTER_PREFIX))) {
    stop("Trimester-header mismatch in ", basename(file),
         "\n  expected: ", paste(names(TRIMESTER_PREFIX), collapse=" | "),
         "\n  got     : ", paste(tri_cells, collapse=" | "))
  }
  # R6 cols 3..10 alternate बजेट/खर्च; C11 = खर्च(%); C12 = मौज्दात
  be <- as.character(unlist(raw[6, 3:10]))
  if (!identical(be, rep(c("बजेट","खर्च"), 4))) {
    stop("बजेट/खर्च alternation broken in ", basename(file))
  }
  if (raw[[6, 11]] != "खर्च(%)") stop("Expected खर्च(%) at R6C11")
  if (raw[[6, 12]] != "मौज्दात")  stop("Expected मौज्दात at R6C12")

  val_names <- character()
  for (tp in unname(TRIMESTER_PREFIX)) {
    val_names <- c(val_names, paste0(tp, "_bud"), paste0(tp, "_exp"))
  }
  val_names <- c(val_names, "exp_pct", "balance")

  vals <- as.data.frame(lapply(raw[, 3:12], parse_np_num))
  names(vals) <- val_names

  df <- bind_cols(
    tibble(sn = as.character(raw[[1]]), raw_np = as.character(raw[[2]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]
  df <- df %>% filter(sn != "कुल जम्मा" & !is.na(raw_np))

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
  bad <- df %>% filter(is.na(subsector))
  if (nrow(bad)) stop("Unmapped subsectors in ", basename(file), ":\n  ",
                      paste(head(bad$raw_np, 5), collapse = "\n  "))

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","sector","subsector", val_names, "source_file"), drop = FALSE]
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
