# =============================================================================
# Clean: क्षेत्रगत बजेट तथा खर्च / Sector LG
#
# Structure (locked spec):
#   - rows 1-4 : title metadata (R4C1 holds the fiscal year)
#   - rows 5-7 : 3-level merged header (sector > subsector > budget/exp)
#   - rows 8 .. n-1 : 753 LG rows
#   - row n    : जम्मा (grand total) -- DROP
#   - cols 1-3 : sn, lg code (8-digit Devanagari), name "<mun>, <district>"
#   - cols 4-87 : 42 subsectors x (budget, expense) = 84 cols
#   - cols 88-89 : grand total budget/expense pair -- DROP
#   - col 90   : empty (stray जम्मा header in row 5) -- DROP
#
# Output: cleaned_output/sector_lg.xlsx, one row per LG per file.
#   ID cols: fy, lgcode, district_np, mun_np, lg_code_raw, lg_name_np, source_file
#   Value : 42 subsectors x {_bud, _exp} = 84 numeric cols
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(purrr)
})

# ----- CONFIG ----------------------------------------------------------------
INPUT_DIR   <- "क्षेत्रगत बजेट तथा खर्च/Sector LG"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "sector_lg.xlsx"

# ----- COLUMN NAMING ---------------------------------------------------------
# Sector prefix lookup (row 5 -> short prefix)
SECTOR_PREFIX <- c(
  "आर्थिक विकास"                          = "econ",
  "सामाजिक विकास"                         = "social",
  "पूर्वाधार विकास"                          = "infra",
  "सुशासन तथा अन्तरसम्बन्धित क्षेत्र"            = "gov",
  "कार्यालय सञ्चालन तथा प्रशासनिक"          = "admin"
)

# Subsector name lookup (row 6 -> short label). Note: "कार्यालय सञ्चालन तथा
# प्रशासनिक" appears at both row 5 (sector) and row 6 (subsector) since this
# sector has only one subsector; we map it to a plain "ops" suffix so the col
# becomes admin_ops_{bud,exp}.
SUBSECTOR_LABEL <- c(
  # econ
  "नभएको"                  = "none",
  "आपूर्ति"                  = "supply",
  "उद्योग"                   = "industry",
  "कृषि"                    = "agri",
  "जलश्रोत तथा सिंचाई"        = "water_irrig",
  "पर्यटन"                   = "tourism",
  "पशुपन्छी विकास"           = "livestock",
  "बन"                     = "forest",
  "भूमि व्यवस्था"             = "land",
  "वाणिज्य"                  = "commerce",
  "वित्तीय क्षेत्र"             = "finance",
  # social
  "सहकारी"                  = "coop",
  "खानेपानी तथा सरसफाई"     = "wash",
  "जनसंख्या तथा बसाईसराई"   = "pop_migration",
  "भाषा तथा संस्कृति"          = "lang_culture",
  "युवा तथा खेलकुद"          = "youth_sports",
  "लैंगिक समानता तथा सामाजिक समावेशीकरण" = "gesi",
  "शिक्षा"                   = "edu",
  "स्वास्थ्य"                 = "health",
  # infra
  "सामाजिक सुरक्षा तथा संरक्षण" = "social_protection",
  "उर्जा"                    = "energy",
  "पुननिर्माण"               = "reconstruction",
  "बिज्ञान तथा प्रबिधि"        = "sci_tech",
  "भवन, आवास तथा सहरी विकास" = "housing_urban",
  "यातयात पूर्वाधार"          = "transport",
  "संचार तथा सूचना प्रबिधि"    = "ict",
  # gov
  "सम्पदा पूर्वाधार"           = "heritage",
  "अनुगमन तथा मूल्यांकन"      = "me_eval",
  "कानुन तथा न्याय"          = "law_justice",
  "गरिबी निवारण"            = "poverty",
  "तथ्यांक प्रणाली"            = "stats",
  "परराष्ट्र"                  = "foreign",
  "प्रशासकीय सुशासन"         = "pub_admin",
  "मानब संशाधन विकास"      = "hrd",
  "योजना तर्जुमा र कार्यन्वयन" = "planning",
  "वातावरण तथा जलवायु"     = "env_climate",
  "वित्तीय सुशासन"            = "fin_gov",
  "विपद व्यवस्थापन"          = "disaster",
  "श्रम तथा रोजगारी"         = "labor",
  "शान्ति तथा सुव्यवस्था"      = "peace",
  "शासन प्रणाली"             = "governance",
  # admin (the single subsector under "कार्यालय सञ्चालन तथा प्रशासनिक")
  "कार्यालय सञ्चालन तथा प्रशासनिक" = "ops"
)

BUDEXP_LABEL <- c("बजेट" = "bud", "खर्च" = "exp")

# ----- HELPERS ---------------------------------------------------------------

# Devanagari digits -> ASCII; preserves non-numeric strings unchanged.
np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}

# Number parser: Devanagari digits, thousands ',', parens for negative.
parse_np_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- np_digits_to_ascii(x)
  neg <- grepl("^\\s*\\(.*\\)\\s*$", x)
  x <- gsub("[(),\\s]", "", x, perl = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[neg] <- -v[neg]
  v
}

# Extract fiscal year from R4C1, e.g. "आ.व. : २०७७/७८  अवधी : ..." -> "2077/78"
extract_fy <- function(title_cell) {
  s <- np_digits_to_ascii(as.character(title_cell))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m) == 0) NA_character_ else m
}

# Forward-fill a vector (left-to-right) replacing NA / "" with the last value.
ffill <- function(v) {
  v <- as.character(v)
  last <- NA_character_
  for (i in seq_along(v)) {
    if (is.na(v[i]) || v[i] == "") v[i] <- last else last <- v[i]
  }
  v
}

# Build short English names for the 86 data cols.
build_data_colnames <- function(row5, row6, row7, ncol_data) {
  sector_full   <- ffill(row5)
  subsec_full   <- ffill(row6)
  budexp        <- row7

  sector_pref <- ifelse(sector_full %in% names(SECTOR_PREFIX),
                        unname(SECTOR_PREFIX[sector_full]),
                        paste0("unk_", seq_along(sector_full)))
  subsec_lbl  <- ifelse(subsec_full %in% names(SUBSECTOR_LABEL),
                        unname(SUBSECTOR_LABEL[subsec_full]),
                        paste0("sub", seq_along(subsec_full)))
  be_lbl      <- ifelse(budexp %in% names(BUDEXP_LABEL),
                        unname(BUDEXP_LABEL[budexp]),
                        ifelse(is.na(budexp) | budexp == "", "x", budexp))

  paste(sector_pref, subsec_lbl, be_lbl, sep = "_")
}

# ----- LOAD LOOKUP -----------------------------------------------------------

load_lookup <- function(path) {
  if (!file.exists(path)) {
    warning("Lookup file not found: ", path, " - lgcode will be NA.")
    return(NULL)
  }
  lk <- read.csv(path, fileEncoding = "UTF-8",
                 stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("district_np", "mun_np", "lgcode")
  miss <- setdiff(needed, names(lk))
  if (length(miss)) stop("Lookup missing columns: ", paste(miss, collapse = ", "))
  lk %>%
    transmute(district_np = str_squish(district_np),
              mun_np      = str_squish(mun_np),
              lgcode      = as.character(lgcode)) %>%
    distinct(district_np, mun_np, .keep_all = TRUE)
}

# ----- CLEAN ONE FILE --------------------------------------------------------

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))

  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  stopifnot(nrow(raw) >= 8, ncol(raw) >= 90)

  fy <- extract_fy(raw[[4, 1]])

  # Header rows 5-7 across cols 4..87. Cols 88-89 are the grand-total pair
  # and col 90 is an empty stray header -- all three dropped.
  data_cols <- 4:87
  row5 <- as.character(unlist(raw[5, data_cols]))
  row6 <- as.character(unlist(raw[6, data_cols]))
  row7 <- as.character(unlist(raw[7, data_cols]))
  data_names <- build_data_colnames(row5, row6, row7, length(data_cols))

  # Sanity: every name should resolve to known sector + subsector + (bud/exp).
  bad <- data_names[grepl("^unk_|_sub\\d+_|_x$", data_names)]
  if (length(bad)) {
    stop("Unmapped headers in ", basename(file), ":\n  ",
         paste(head(bad, 5), collapse = "\n  "))
  }

  # Identifier columns: C2 = code, C3 = name. C1 = sn (drop).
  id <- tibble(
    lg_code_raw = np_digits_to_ascii(raw[[2]]),
    lg_name_np  = as.character(raw[[3]])
  )

  # Numeric data block, cols 4..89 of all rows
  vals <- as.data.frame(lapply(raw[, data_cols], parse_np_num),
                        col.names = data_names)

  df <- bind_cols(id, vals)

  # Keep only rows 8..end (data starts at row 8)
  df <- df[8:nrow(df), , drop = FALSE]

  # DROP totals / non-LG rows: require an 8-digit code and a comma in the name
  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) &
          grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]

  # Split "mun, district"
  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  # Join lgcode
  if (!is.null(lookup)) {
    df <- df %>% left_join(lookup, by = c("district_np", "mun_np"))
  } else {
    df$lgcode <- NA_character_
  }

  # Final column order: ids first, then values
  df$fy <- fy
  df$source_file <- basename(file)

  id_cols <- c("fy", "lgcode", "district_np", "mun_np",
               "lg_code_raw", "lg_name_np", "source_file")
  val_cols <- setdiff(names(df), id_cols)
  df[, c(id_cols, val_cols), drop = FALSE]
}

# ----- RUN -------------------------------------------------------------------

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

  lookup <- load_lookup(LOOKUP_FILE)

  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$",
                      full.names = TRUE, recursive = FALSE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)

  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  # QC
  n_total <- nrow(combined)
  n_per_file <- combined %>% count(source_file)
  miss_code  <- sum(is.na(combined$lgcode))
  message(sprintf("Rows: %d (%s) | unmatched lgcode: %d",
                  n_total,
                  paste(sprintf("%s=%d", n_per_file$source_file, n_per_file$n),
                        collapse = ", "),
                  miss_code))

  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message("Wrote: ", out_path, " (", n_total, " rows x ",
          ncol(combined), " cols)")
  invisible(combined)
}

main()
