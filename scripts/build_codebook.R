# =============================================================================
# Build / update docs/codebook.xlsx — cumulative Nepali -> English mapping
# across all folder cleaning scripts.
#
# Run after adding a new cleaning script to register its translations.
# Each block below documents one source folder. Append new blocks as we go.
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(writexl)
  library(dplyr)
})

OUTPUT_DIR  <- "docs"
OUTPUT_FILE <- "codebook.xlsx"

entry <- function(folder, type, nepali, english, notes = "") {
  tibble(folder = folder, type = type,
         nepali = nepali, english = english, notes = notes)
}

# --- क्षेत्रगत बजेट तथा खर्च / Sector LG --------------------------------------
sector_lg <- bind_rows(
  # sectors (top-level row 5)
  entry("Sector LG", "sector", "आर्थिक विकास",                       "econ"),
  entry("Sector LG", "sector", "सामाजिक विकास",                      "social"),
  entry("Sector LG", "sector", "पूर्वाधार विकास",                       "infra"),
  entry("Sector LG", "sector", "सुशासन तथा अन्तरसम्बन्धित क्षेत्र",        "gov"),
  entry("Sector LG", "sector", "कार्यालय सञ्चालन तथा प्रशासनिक",      "admin"),
  # subsectors (row 6) — econ
  entry("Sector LG", "subsector", "नभएको",              "none"),
  entry("Sector LG", "subsector", "आपूर्ति",              "supply"),
  entry("Sector LG", "subsector", "उद्योग",               "industry"),
  entry("Sector LG", "subsector", "कृषि",                "agri"),
  entry("Sector LG", "subsector", "जलश्रोत तथा सिंचाई",    "water_irrig"),
  entry("Sector LG", "subsector", "पर्यटन",               "tourism"),
  entry("Sector LG", "subsector", "पशुपन्छी विकास",       "livestock"),
  entry("Sector LG", "subsector", "बन",                  "forest"),
  entry("Sector LG", "subsector", "भूमि व्यवस्था",         "land"),
  entry("Sector LG", "subsector", "वाणिज्य",              "commerce"),
  entry("Sector LG", "subsector", "वित्तीय क्षेत्र",         "finance"),
  # social
  entry("Sector LG", "subsector", "सहकारी",              "coop"),
  entry("Sector LG", "subsector", "खानेपानी तथा सरसफाई", "wash"),
  entry("Sector LG", "subsector", "जनसंख्या तथा बसाईसराई", "pop_migration"),
  entry("Sector LG", "subsector", "भाषा तथा संस्कृति",      "lang_culture"),
  entry("Sector LG", "subsector", "युवा तथा खेलकुद",      "youth_sports"),
  entry("Sector LG", "subsector", "लैंगिक समानता तथा सामाजिक समावेशीकरण", "gesi"),
  entry("Sector LG", "subsector", "शिक्षा",               "edu"),
  entry("Sector LG", "subsector", "स्वास्थ्य",             "health"),
  # infra
  entry("Sector LG", "subsector", "सामाजिक सुरक्षा तथा संरक्षण", "social_protection"),
  entry("Sector LG", "subsector", "उर्जा",                "energy"),
  entry("Sector LG", "subsector", "पुननिर्माण",           "reconstruction"),
  entry("Sector LG", "subsector", "बिज्ञान तथा प्रबिधि",    "sci_tech"),
  entry("Sector LG", "subsector", "भवन, आवास तथा सहरी विकास", "housing_urban"),
  entry("Sector LG", "subsector", "यातयात पूर्वाधार",      "transport"),
  entry("Sector LG", "subsector", "संचार तथा सूचना प्रबिधि", "ict"),
  # gov
  entry("Sector LG", "subsector", "सम्पदा पूर्वाधार",       "heritage"),
  entry("Sector LG", "subsector", "अनुगमन तथा मूल्यांकन",   "me_eval"),
  entry("Sector LG", "subsector", "कानुन तथा न्याय",      "law_justice"),
  entry("Sector LG", "subsector", "गरिबी निवारण",        "poverty"),
  entry("Sector LG", "subsector", "तथ्यांक प्रणाली",        "stats"),
  entry("Sector LG", "subsector", "परराष्ट्र",              "foreign"),
  entry("Sector LG", "subsector", "प्रशासकीय सुशासन",     "pub_admin"),
  entry("Sector LG", "subsector", "मानब संशाधन विकास",  "hrd"),
  entry("Sector LG", "subsector", "योजना तर्जुमा र कार्यन्वयन", "planning"),
  entry("Sector LG", "subsector", "वातावरण तथा जलवायु", "env_climate"),
  entry("Sector LG", "subsector", "वित्तीय सुशासन",        "fin_gov"),
  entry("Sector LG", "subsector", "विपद व्यवस्थापन",      "disaster"),
  entry("Sector LG", "subsector", "श्रम तथा रोजगारी",     "labor"),
  entry("Sector LG", "subsector", "शान्ति तथा सुव्यवस्था",  "peace"),
  entry("Sector LG", "subsector", "शासन प्रणाली",         "governance"),
  # admin subsector
  entry("Sector LG", "subsector", "कार्यालय सञ्चालन तथा प्रशासनिक", "ops",
        "single subsector under admin"),
  # bud/exp suffix
  entry("Sector LG", "measure",   "बजेट",                "bud", "budget"),
  entry("Sector LG", "measure",   "खर्च",                "exp", "expenditure"),
  # identifier cols
  entry("Sector LG", "id_col",    "क्र.सं.",              "(dropped)", "serial number"),
  entry("Sector LG", "id_col",    "संकेत",                "lg_code_raw", "8-digit LG code"),
  entry("Sector LG", "id_col",    "स्थानीय तह नाम",       "lg_name_np", "split into mun_np + district_np"),
  entry("Sector LG", "id_col",    "जम्मा",                "(dropped)",  "grand total col/row")
)

# --- combine all blocks ------------------------------------------------------
codebook <- bind_rows(
  sector_lg
  # future blocks: line_item, target_group, summary, ...
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
write_xlsx(codebook, out_path)
cat("Wrote", out_path, "with", nrow(codebook), "rows across",
    length(unique(codebook$folder)), "folder(s).\n")
