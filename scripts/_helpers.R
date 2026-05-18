# =============================================================================
# Shared helpers for all clean_*.R scripts.
# Source this at the top of each cleaner:
#   source("scripts/_helpers.R")
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# ----- Devanagari helpers ---------------------------------------------------

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}

# Parse a Devanagari (or ASCII) number with commas / parens.
# (parens) -> negative. Returns numeric vector (NA where unparseable).
parse_np_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- np_digits_to_ascii(x)
  neg <- grepl("^\\s*\\(.*\\)\\s*$", x)
  x <- gsub("[(),\\s]", "", x, perl = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[neg] <- -v[neg]
  v
}

# Find a fiscal year token like "2076/77" in the title cell.
extract_fy <- function(x) {
  s <- np_digits_to_ascii(as.character(x))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m)) m else NA_character_
}

# Parse fy + province + district filter from a title cell.
parse_title <- function(s) {
  s <- np_digits_to_ascii(as.character(s))
  fy <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  prov <- regmatches(s, regexpr("प्रदेश\\s*:\\s*[^[:space:]जिल्ला]+", s))
  prov <- if (length(prov)) str_squish(sub("^प्रदेश\\s*:\\s*", "", prov)) else NA_character_
  dist <- regmatches(s, regexpr("जिल्ला\\s*:\\s*.+", s))
  dist <- if (length(dist)) str_squish(sub("^जिल्ला\\s*:\\s*", "", dist)) else NA_character_
  list(fy = if (length(fy)) fy else NA_character_,
       province = prov, district_filter = dist)
}

# Forward-fill a character vector (NA / "" replaced by last non-empty).
ffill <- function(v) {
  v <- as.character(v)
  last <- NA_character_
  for (i in seq_along(v)) {
    if (is.na(v[i]) || v[i] == "") v[i] <- last else last <- v[i]
  }
  v
}

# Normalize a Nepali name for fuzzy matching (strip suffixes, whitespace).
normalize_np <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\\.,()\\-]", " ")
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  suffixes <- c(
    "महानगरपालिका", "उपमहानगरपालिका", "नगरपालिका", "गाउँपालिका", "गाउंपालिका",
    "म\\.न\\.पा\\.?", "उ\\.म\\.न\\.पा\\.?", "न\\.पा\\.?", "गा\\.पा\\.?",
    "जिल्ला"
  )
  for (sfx in suffixes) x <- str_replace(x, paste0("\\s*", sfx, "\\s*$"), "")
  str_trim(x)
}

# ----- Lookup loader (district + mun -> 5-digit lgcode) ---------------------

load_lookup <- function(path) {
  if (!file.exists(path)) {
    warning("Lookup not found: ", path, " - lgcode will be NA.")
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

# ----- File-level error wrapper ---------------------------------------------
#
# Run `fn(f, ...)` for every file `f` in `files`; on error, emit a warning
# with the filename and skip that file (returning an empty tibble). Combine
# the per-file results with bind_rows.

safe_map_files <- function(files, fn, ...) {
  results <- list()
  for (f in files) {
    res <- tryCatch(fn(f, ...),
                    error = function(e) {
                      warning("FAILED ", basename(f), ": ",
                              conditionMessage(e), immediate. = TRUE,
                              call. = FALSE)
                      NULL
                    })
    if (!is.null(res) && nrow(res) > 0) results[[length(results) + 1]] <- res
  }
  if (!length(results)) return(tibble())
  bind_rows(results)
}

# ----- Shared sector / subsector translations -------------------------------
# (Used by the sectoral cleaners. Defined here once so adding a new term
#  is a one-place edit.)

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
  "भवन, आवास तथा सहरी विकास" = "housing_urban",
  "यातयात पूर्वाधार" = "transport",
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

BUDEXP_LABEL <- c("बजेट" = "bud", "खर्च" = "exp")
