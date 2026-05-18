# =============================================================================
# Build / update docs/codebook.xlsx — cumulative Nepali -> English mapping
# across all folder cleaning scripts.
#
# Columns:
#   folder      source folder name
#   type        sector | subsector | fund_type | source | trimester |
#               month | measure | id_col
#   nepali      original Nepali label
#   english     short snake_case label used in cleaned df column names
#   description plain-English description so a non-Nepali reader can
#               understand what the variable means
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(writexl); library(dplyr)
})

OUTPUT_DIR  <- "docs"
OUTPUT_FILE <- "codebook.xlsx"

e <- function(folder, type, nepali, english, description = "") {
  tibble(folder = folder, type = type, nepali = nepali,
         english = english, description = description)
}

# ----- SHARED VOCAB (used by multiple folders) ------------------------------

sectors <- list(
  c("आर्थिक विकास",                       "econ",   "Economic Development (sector)"),
  c("सामाजिक विकास",                      "social", "Social Development (sector)"),
  c("पूर्वाधार विकास",                       "infra",  "Infrastructure Development (sector)"),
  c("सुशासन तथा अन्तरसम्बन्धित क्षेत्र",        "gov",
    "Good Governance & Inter-related Sectors (sector)"),
  c("कार्यालय सञ्चालन तथा प्रशासनिक",       "admin",
    "Office Operations & Administration (sector)")
)

subsectors <- list(
  c("नभएको",              "none",          "Unspecified / not categorized"),
  c("आपूर्ति",              "supply",        "Supply"),
  c("उद्योग",               "industry",      "Industry"),
  c("कृषि",                "agri",          "Agriculture"),
  c("जलश्रोत तथा सिंचाई",    "water_irrig",   "Water Resources & Irrigation"),
  c("पर्यटन",               "tourism",       "Tourism"),
  c("पशुपन्छी विकास",       "livestock",     "Livestock Development"),
  c("बन",                  "forest",        "Forestry"),
  c("भूमि व्यवस्था",         "land",          "Land Management"),
  c("वाणिज्य",              "commerce",      "Commerce"),
  c("वित्तीय क्षेत्र",         "finance",       "Financial sector"),
  c("सहकारी",              "coop",          "Cooperatives"),
  c("खानेपानी तथा सरसफाई", "wash",          "Drinking Water, Sanitation & Hygiene"),
  c("जनसंख्या तथा बसाईसराई", "pop_migration", "Population & Migration"),
  c("भाषा तथा संस्कृति",      "lang_culture",  "Language & Culture"),
  c("युवा तथा खेलकुद",      "youth_sports",  "Youth & Sports"),
  c("लैंगिक समानता तथा सामाजिक समावेशीकरण", "gesi",
    "Gender Equality & Social Inclusion (GESI)"),
  c("शिक्षा",               "edu",           "Education"),
  c("स्वास्थ्य",             "health",        "Health"),
  c("सामाजिक सुरक्षा तथा संरक्षण", "social_protection",
    "Social Security & Protection"),
  c("उर्जा",                "energy",        "Energy"),
  c("पुननिर्माण",           "reconstruction","Reconstruction"),
  c("बिज्ञान तथा प्रबिधि",    "sci_tech",      "Science & Technology"),
  c("भवन, आवास तथा सहरी विकास", "housing_urban",
    "Buildings, Housing & Urban Development"),
  c("यातयात पूर्वाधार",      "transport",     "Transportation Infrastructure"),
  c("संचार तथा सूचना प्रबिधि", "ict",
    "Communications & Information Technology"),
  c("सम्पदा पूर्वाधार",       "heritage",      "Heritage Infrastructure"),
  c("अनुगमन तथा मूल्यांकन", "me_eval",       "Monitoring & Evaluation"),
  c("कानुन तथा न्याय",      "law_justice",   "Law & Justice"),
  c("गरिबी निवारण",        "poverty",       "Poverty Alleviation"),
  c("तथ्यांक प्रणाली",        "stats",         "Statistical System"),
  c("परराष्ट्र",              "foreign",       "Foreign Affairs"),
  c("प्रशासकीय सुशासन",    "pub_admin",     "Administrative Governance"),
  c("मानब संशाधन विकास", "hrd",
    "Human Resource Development"),
  c("योजना तर्जुमा र कार्यन्वयन", "planning",
    "Planning Formulation & Implementation"),
  c("वातावरण तथा जलवायु", "env_climate",   "Environment & Climate"),
  c("वित्तीय सुशासन",       "fin_gov",       "Financial Governance"),
  c("विपद व्यवस्थापन",     "disaster",      "Disaster Management"),
  c("श्रम तथा रोजगारी",    "labor",         "Labor & Employment"),
  c("शान्ति तथा सुव्यवस्था", "peace",         "Peace & Public Order"),
  c("शासन प्रणाली",        "governance",    "Governance System"),
  c("कार्यालय सञ्चालन तथा प्रशासनिक", "ops",
    "Office Operations (subsector — only one under 'admin' sector)")
)

# Helper: emit rows for a given folder using the shared sectors/subsectors lists
emit_shared <- function(folder) {
  bind_rows(
    bind_rows(lapply(sectors,    function(x) e(folder, "sector",    x[1], x[2], x[3]))),
    bind_rows(lapply(subsectors, function(x) e(folder, "subsector", x[1], x[2], x[3])))
  )
}

# ----- क्षेत्रगत बजेट तथा खर्च / Sector LG ------------------------------------
sector_lg <- bind_rows(
  emit_shared("Sector LG"),
  e("Sector LG", "measure", "बजेट", "bud", "Budget (allocation)"),
  e("Sector LG", "measure", "खर्च", "exp", "Expenditure (actual spend)"),
  e("Sector LG", "id_col",  "क्र.सं.", "(dropped)", "Serial number"),
  e("Sector LG", "id_col",  "संकेत",  "lg_code_raw", "8-digit LG code from the source file (kept for traceability)"),
  e("Sector LG", "id_col",  "स्थानीय तह नाम", "lg_name_np",
    "Full LG name; split into mun_np + district_np in the cleaned output"),
  e("Sector LG", "id_col",  "जम्मा (col)", "(dropped)", "Grand-total column at end of source"),
  e("Sector LG", "id_col",  "जम्मा (row)", "(dropped)", "Grand-total row at bottom of source")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Fund Type -----------------------------
sector_fund_type <- bind_rows(
  emit_shared("Sector Fund Type"),
  e("Sector Fund Type", "fund_type", "चालु",     "current",
    "Current (recurrent) spending"),
  e("Sector Fund Type", "fund_type", "पूँजीगत",    "capital",
    "Capital spending"),
  e("Sector Fund Type", "fund_type", "वित्तीय",    "financing",
    "Financing (debt, equity etc.)"),
  e("Sector Fund Type", "fund_type", "जम्मा",     "total",
    "Total across all fund types"),
  e("Sector Fund Type", "measure",   "बजेट",     "bud", "Budget (allocation)"),
  e("Sector Fund Type", "measure",   "खर्च",     "exp", "Expenditure (actual spend)"),
  e("Sector Fund Type", "measure",   "खर्च(%)",  "exp_pct",
    "Expenditure as % of budget"),
  e("Sector Fund Type", "measure",   "मौज्दात",   "balance",
    "Closing balance (unspent)"),
  e("Sector Fund Type", "id_col",    "शीर्षक",   "(split)",
    "Heading — split into sector + subsector"),
  e("Sector Fund Type", "id_col",    "कुल जम्मा", "(dropped)", "Grand-total row")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Monthly Exp ---------------------------
sector_monthly_exp <- bind_rows(
  emit_shared("Sector Monthly Exp"),
  e("Sector Monthly Exp", "month", "साउन",   "exp_saun",
    "Expenditure in Saun (Nepali FY month 1, ~mid-Jul to mid-Aug)"),
  e("Sector Monthly Exp", "month", "भदौ",    "exp_bhadau",
    "Expenditure in Bhadau (month 2, ~mid-Aug to mid-Sep)"),
  e("Sector Monthly Exp", "month", "आस्बिन", "exp_asoj",
    "Expenditure in Asoj (month 3, ~mid-Sep to mid-Oct)"),
  e("Sector Monthly Exp", "month", "कार्तिक", "exp_kartik",
    "Expenditure in Kartik (month 4, ~mid-Oct to mid-Nov)"),
  e("Sector Monthly Exp", "month", "मार्ग",   "exp_mangsir",
    "Expenditure in Mangsir (month 5, ~mid-Nov to mid-Dec)"),
  e("Sector Monthly Exp", "month", "पौष",   "exp_poush",
    "Expenditure in Poush (month 6, ~mid-Dec to mid-Jan)"),
  e("Sector Monthly Exp", "month", "माघ",   "exp_magh",
    "Expenditure in Magh (month 7, ~mid-Jan to mid-Feb)"),
  e("Sector Monthly Exp", "month", "फागुन",  "exp_falgun",
    "Expenditure in Falgun (month 8, ~mid-Feb to mid-Mar)"),
  e("Sector Monthly Exp", "month", "चैत्र",   "exp_chaitra",
    "Expenditure in Chaitra (month 9, ~mid-Mar to mid-Apr)"),
  e("Sector Monthly Exp", "month", "बैशाख",  "exp_baisakh",
    "Expenditure in Baisakh (month 10, ~mid-Apr to mid-May)"),
  e("Sector Monthly Exp", "month", "जेठ",   "exp_jestha",
    "Expenditure in Jestha (month 11, ~mid-May to mid-Jun)"),
  e("Sector Monthly Exp", "month", "असार",  "exp_asar",
    "Expenditure in Asar (month 12, ~mid-Jun to mid-Jul)"),
  e("Sector Monthly Exp", "measure", "बजेट",   "annual_bud",
    "Annual budget (allocation for the year)"),
  e("Sector Monthly Exp", "measure", "जम्मा",  "exp_total",
    "Total annual expenditure (source-reported; may differ from sum of months)"),
  e("Sector Monthly Exp", "measure", "खर्च(%)", "exp_pct", "Expenditure as % of budget"),
  e("Sector Monthly Exp", "measure", "मौज्दात", "balance",  "Closing balance (unspent)")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Source --------------------------------
sector_source <- bind_rows(
  emit_shared("Sector Source"),
  e("Sector Source", "source", "संघीय सरकार",   "fed",
    "Federal Government — funds from the central (federal) budget"),
  e("Sector Source", "source", "प्रदेश सरकार",    "prov",
    "Provincial Government — funds from provincial budgets"),
  e("Sector Source", "source", "राजस्व बाडफाड", "revshare",
    "Revenue Sharing — funds shared between levels of government"),
  e("Sector Source", "source", "अन्तरिक श्रोत",   "internal",
    "Internal Source — local government's own internal revenue"),
  e("Sector Source", "source", "बैदेशिक श्रोत",    "foreign_src",
    "Foreign Source — foreign aid / external financing"),
  e("Sector Source", "source", "स्थानीय तह",     "lg",
    "Local Level — funds raised/held by the local government"),
  e("Sector Source", "source", "जनसहभागिता", "public",
    "Public Participation — community / public contributions"),
  e("Sector Source", "source", "जम्मा",          "total",
    "Total across all funding sources"),
  e("Sector Source", "measure", "बजेट",         "<src>_bud",
    "Budget portion attributable to each source"),
  e("Sector Source", "measure", "खर्च",         "<src>_exp",
    "Expenditure portion attributable to each source"),
  e("Sector Source", "measure", "खर्च(%)",      "exp_pct",
    "Expenditure as % of budget (overall)")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Trimester -----------------------------
sector_trimester <- bind_rows(
  emit_shared("Sector Trimester"),
  e("Sector Trimester", "trimester", "प्रथम चौमासिक",  "t1",
    "1st Trimester (Saun-Mangsir, months 1-4 of Nepali FY)"),
  e("Sector Trimester", "trimester", "दोश्रो चौमासिक", "t2",
    "2nd Trimester (Poush-Chaitra, months 5-8)"),
  e("Sector Trimester", "trimester", "तेस्रो चौमासिक",  "t3",
    "3rd Trimester (Baisakh-Asar, months 9-12)"),
  e("Sector Trimester", "trimester", "जम्मा",         "total",
    "Total across the three trimesters"),
  e("Sector Trimester", "measure", "बजेट",  "<t#>_bud", "Trimester-level budget allocation"),
  e("Sector Trimester", "measure", "खर्च",  "<t#>_exp", "Trimester-level expenditure"),
  e("Sector Trimester", "measure", "खर्च(%)", "exp_pct", "Expenditure as % of budget"),
  e("Sector Trimester", "measure", "मौज्दात", "balance", "Closing balance (unspent)")
)

# ----- combine all blocks ----------------------------------------------------
codebook <- bind_rows(
  sector_lg,
  sector_fund_type,
  sector_monthly_exp,
  sector_source,
  sector_trimester
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
write_xlsx(codebook, out_path)
cat("Wrote", out_path, "with", nrow(codebook), "rows across",
    length(unique(codebook$folder)), "folder(s).\n")
