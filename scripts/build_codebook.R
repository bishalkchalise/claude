# =============================================================================
# Build / update docs/codebook.xlsx — cumulative Nepali -> English mapping
# across all folder cleaning scripts.
#
# Columns:
#   folder        source subfolder name (Nepali)
#   output_file   cleaned xlsx filename (so you know which df a var lives in)
#   type          sector | subsector | fund_type | source | trimester |
#                 month | measure | id_col
#   nepali        original Nepali label
#   english       short snake_case label used in the cleaned df columns
#   description   plain-English description so a non-Nepali reader can
#                 understand what the variable means
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({ library(writexl); library(dplyr) })

OUTPUT_DIR  <- "docs"
OUTPUT_FILE <- "codebook.xlsx"

# Per-folder context for the current block (set with set_ctx, read by `e`)
.ctx <- list(folder = NA_character_, output_file = NA_character_)
set_ctx <- function(folder, output_file) {
  .ctx$folder      <<- folder
  .ctx$output_file <<- output_file
}
e <- function(type, nepali, english, description = "") {
  tibble(folder = .ctx$folder, output_file = .ctx$output_file,
         type = type, nepali = nepali, english = english,
         description = description)
}

# ----- SHARED VOCAB ---------------------------------------------------------

sectors <- list(
  c("आर्थिक विकास",                       "econ",
    "Economic Development (sector)"),
  c("सामाजिक विकास",                      "social",
    "Social Development (sector)"),
  c("पूर्वाधार विकास",                       "infra",
    "Infrastructure Development (sector)"),
  c("सुशासन तथा अन्तरसम्बन्धित क्षेत्र",        "gov",
    "Good Governance & Inter-related Sectors (sector)"),
  c("कार्यालय सञ्चालन तथा प्रशासनिक",       "admin",
    "Office Operations & Administration (sector)")
)
subsectors <- list(
  c("नभएको",              "none",              "Unspecified / not categorized"),
  c("आपूर्ति",              "supply",            "Supply"),
  c("उद्योग",               "industry",          "Industry"),
  c("कृषि",                "agri",              "Agriculture"),
  c("जलश्रोत तथा सिंचाई",    "water_irrig",       "Water Resources & Irrigation"),
  c("पर्यटन",               "tourism",           "Tourism"),
  c("पशुपन्छी विकास",       "livestock",         "Livestock Development"),
  c("बन",                  "forest",            "Forestry"),
  c("भूमि व्यवस्था",         "land",              "Land Management"),
  c("वाणिज्य",              "commerce",          "Commerce"),
  c("वित्तीय क्षेत्र",         "finance",           "Financial sector"),
  c("सहकारी",              "coop",              "Cooperatives"),
  c("खानेपानी तथा सरसफाई", "wash",              "Drinking Water, Sanitation & Hygiene"),
  c("जनसंख्या तथा बसाईसराई","pop_migration",     "Population & Migration"),
  c("भाषा तथा संस्कृति",      "lang_culture",      "Language & Culture"),
  c("युवा तथा खेलकुद",      "youth_sports",      "Youth & Sports"),
  c("लैंगिक समानता तथा सामाजिक समावेशीकरण","gesi",
    "Gender Equality & Social Inclusion (GESI)"),
  c("शिक्षा",               "edu",               "Education"),
  c("स्वास्थ्य",             "health",            "Health"),
  c("सामाजिक सुरक्षा तथा संरक्षण","social_protection",
    "Social Security & Protection"),
  c("उर्जा",                "energy",            "Energy"),
  c("पुननिर्माण",           "reconstruction",    "Reconstruction"),
  c("बिज्ञान तथा प्रबिधि",    "sci_tech",          "Science & Technology"),
  c("भवन, आवास तथा सहरी विकास","housing_urban",
    "Buildings, Housing & Urban Development"),
  c("यातयात पूर्वाधार",      "transport",         "Transportation Infrastructure"),
  c("संचार तथा सूचना प्रबिधि","ict",
    "Communications & Information Technology"),
  c("सम्पदा पूर्वाधार",       "heritage",          "Heritage Infrastructure"),
  c("अनुगमन तथा मूल्यांकन", "me_eval",           "Monitoring & Evaluation"),
  c("कानुन तथा न्याय",      "law_justice",       "Law & Justice"),
  c("गरिबी निवारण",        "poverty",           "Poverty Alleviation"),
  c("तथ्यांक प्रणाली",        "stats",             "Statistical System"),
  c("परराष्ट्र",              "foreign",           "Foreign Affairs"),
  c("प्रशासकीय सुशासन",    "pub_admin",         "Administrative Governance"),
  c("मानब संशाधन विकास", "hrd",
    "Human Resource Development"),
  c("योजना तर्जुमा र कार्यन्वयन","planning",
    "Planning Formulation & Implementation"),
  c("वातावरण तथा जलवायु","env_climate",       "Environment & Climate"),
  c("वित्तीय सुशासन",       "fin_gov",           "Financial Governance"),
  c("विपद व्यवस्थापन",     "disaster",          "Disaster Management"),
  c("श्रम तथा रोजगारी",    "labor",             "Labor & Employment"),
  c("शान्ति तथा सुव्यवस्था", "peace",             "Peace & Public Order"),
  c("शासन प्रणाली",        "governance",        "Governance System"),
  c("कार्यालय सञ्चालन तथा प्रशासनिक","ops",
    "Office Operations (subsector — only one under 'admin' sector)")
)

emit_shared <- function() {
  bind_rows(
    bind_rows(lapply(sectors,    function(x) e("sector",    x[1], x[2], x[3]))),
    bind_rows(lapply(subsectors, function(x) e("subsector", x[1], x[2], x[3])))
  )
}

# ----- क्षेत्रगत बजेट तथा खर्च / Sector LG ------------------------------------
set_ctx("Sector LG", "sector_lg.xlsx")
sector_lg <- bind_rows(
  emit_shared(),
  e("measure", "बजेट", "bud", "Budget (allocation)"),
  e("measure", "खर्च", "exp", "Expenditure (actual spend)"),
  e("id_col",  "क्र.सं.", "(dropped)", "Serial number"),
  e("id_col",  "संकेत",   "lg_code_raw",
    "8-digit LG code from the source file (kept for traceability)"),
  e("id_col",  "स्थानीय तह नाम", "lg_name_np",
    "Full LG name; split into mun_np + district_np"),
  e("id_col",  "जम्मा (col)", "(dropped)", "Grand-total column at end of source"),
  e("id_col",  "जम्मा (row)", "(dropped)", "Grand-total row at bottom of source")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Fund Type -----------------------------
set_ctx("Sector Fund Type", "sector_fund_type.xlsx")
sector_fund_type <- bind_rows(
  emit_shared(),
  e("fund_type", "चालु",    "current",   "Current (recurrent) spending"),
  e("fund_type", "पूँजीगत",   "capital",   "Capital spending"),
  e("fund_type", "वित्तीय",   "financing", "Financing (debt, equity etc.)"),
  e("fund_type", "जम्मा",    "total",     "Total across all fund types"),
  e("measure",   "बजेट",    "bud",       "Budget (allocation)"),
  e("measure",   "खर्च",    "exp",       "Expenditure (actual spend)"),
  e("measure",   "खर्च(%)", "exp_pct",   "Expenditure as % of budget"),
  e("measure",   "मौज्दात",  "balance",   "Closing balance (unspent)"),
  e("id_col",    "शीर्षक",  "(split)",   "Heading -- split into sector + subsector"),
  e("id_col",    "कुल जम्मा","(dropped)", "Grand-total row")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Monthly Exp ---------------------------
set_ctx("Sector Monthly Exp", "sector_monthly_exp.xlsx")
sector_monthly_exp <- bind_rows(
  emit_shared(),
  e("month", "साउन",   "exp_saun",
    "Expenditure in Saun (Nepali FY month 1, ~mid-Jul to mid-Aug)"),
  e("month", "भदौ",    "exp_bhadau",
    "Expenditure in Bhadau (month 2, ~mid-Aug to mid-Sep)"),
  e("month", "आस्बिन", "exp_asoj",
    "Expenditure in Asoj (month 3, ~mid-Sep to mid-Oct)"),
  e("month", "कार्तिक", "exp_kartik",
    "Expenditure in Kartik (month 4, ~mid-Oct to mid-Nov)"),
  e("month", "मार्ग",   "exp_mangsir",
    "Expenditure in Mangsir (month 5, ~mid-Nov to mid-Dec)"),
  e("month", "पौष",   "exp_poush",
    "Expenditure in Poush (month 6, ~mid-Dec to mid-Jan)"),
  e("month", "माघ",   "exp_magh",
    "Expenditure in Magh (month 7, ~mid-Jan to mid-Feb)"),
  e("month", "फागुन",  "exp_falgun",
    "Expenditure in Falgun (month 8, ~mid-Feb to mid-Mar)"),
  e("month", "चैत्र",   "exp_chaitra",
    "Expenditure in Chaitra (month 9, ~mid-Mar to mid-Apr)"),
  e("month", "बैशाख",  "exp_baisakh",
    "Expenditure in Baisakh (month 10, ~mid-Apr to mid-May)"),
  e("month", "जेठ",   "exp_jestha",
    "Expenditure in Jestha (month 11, ~mid-May to mid-Jun)"),
  e("month", "असार",  "exp_asar",
    "Expenditure in Asar (month 12, ~mid-Jun to mid-Jul)"),
  e("measure", "बजेट",   "annual_bud", "Annual budget (allocation for the year)"),
  e("measure", "जम्मा",  "exp_total",
    "Total annual expenditure (source-reported; may differ from sum of months)"),
  e("measure", "खर्च(%)","exp_pct",    "Expenditure as % of budget"),
  e("measure", "मौज्दात", "balance",    "Closing balance (unspent)")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Source --------------------------------
set_ctx("Sector Source", "sector_source.xlsx")
sector_source <- bind_rows(
  emit_shared(),
  e("source", "संघीय सरकार",   "fed",
    "Federal Government — funds from the central (federal) budget"),
  e("source", "प्रदेश सरकार",    "prov",
    "Provincial Government — funds from provincial budgets"),
  e("source", "राजस्व बाडफाड", "revshare",
    "Revenue Sharing — funds shared between levels of government"),
  e("source", "अन्तरिक श्रोत",   "internal",
    "Internal Source — local government's own internal revenue"),
  e("source", "बैदेशिक श्रोत",    "foreign_src",
    "Foreign Source — foreign aid / external financing"),
  e("source", "स्थानीय तह",     "lg",
    "Local Level — funds raised/held by the local government"),
  e("source", "जनसहभागिता", "public",
    "Public Participation — community / public contributions"),
  e("source", "जम्मा",          "total", "Total across all funding sources"),
  e("measure", "बजेट",         "<src>_bud", "Budget portion attributable to each source"),
  e("measure", "खर्च",         "<src>_exp", "Expenditure portion attributable to each source"),
  e("measure", "खर्च(%)",      "exp_pct",   "Expenditure as % of budget (overall)")
)

# ----- क्षेत्रगत बजेट तथा खर्च / Sector Trimester -----------------------------
set_ctx("Sector Trimester", "sector_trimester.xlsx")
sector_trimester <- bind_rows(
  emit_shared(),
  e("trimester", "प्रथम चौमासिक",  "t1",
    "1st Trimester (Saun-Mangsir, months 1-4 of Nepali FY)"),
  e("trimester", "दोश्रो चौमासिक", "t2",
    "2nd Trimester (Poush-Chaitra, months 5-8)"),
  e("trimester", "तेस्रो चौमासिक",  "t3",
    "3rd Trimester (Baisakh-Asar, months 9-12)"),
  e("trimester", "जम्मा",         "total", "Total across the three trimesters"),
  e("measure",   "बजेट",  "<t#>_bud", "Trimester-level budget allocation"),
  e("measure",   "खर्च",  "<t#>_exp", "Trimester-level expenditure"),
  e("measure",   "खर्च(%)", "exp_pct", "Expenditure as % of budget"),
  e("measure",   "मौज्दात", "balance", "Closing balance (unspent)")
)

# ----- आय र व्यय प्रक्षेपण को सारांश / Summary --------------------------------
set_ctx("Projection Summary", "projection_summary.xlsx")
projection_summary <- bind_rows(
  e("measure", "राजस्व अनुमान", "revenue_proj",
    "Projected revenue (income) for the LG for the fiscal year"),
  e("measure", "चालु",  "exp_current",
    "Projected current (recurrent) expenditure"),
  e("measure", "पूँजीगत", "exp_capital",
    "Projected capital expenditure"),
  e("measure", "वित्तीय", "exp_financing",
    "Projected financing expenditure (debt servicing / equity)"),
  e("measure", "जम्मा",  "exp_total",
    "Projected total expenditure"),
  e("id_col",  "क्र.सं.", "(dropped)",   "Serial number"),
  e("id_col",  "संकेत",  "lg_code_raw", "8-digit LG code from source"),
  e("id_col",  "स्थानीय तह नाम","lg_name_np",
    "Full LG name; split into mun_np + district_np"),
  e("id_col",  "जम्मा (row)","(dropped)", "Grand-total row at bottom of source")
)

# ----- आय र व्यय प्रक्षेपण को सारांश / Source Fund Type -----------------------
set_ctx("Projection Source Fund Type", "projection_source_fund_type.xlsx")
projection_source_fund_type <- bind_rows(
  # income source columns
  e("source", "संघीय सरकार",   "inc_fed",
    "Projected income from Federal Government transfers"),
  e("source", "प्रदेश सरकार",    "inc_prov",
    "Projected income from Provincial Government transfers"),
  e("source", "राजस्व बाँडफाँट", "inc_revshare",
    "Projected income from Revenue Sharing (shared revenue across govt levels)"),
  e("source", "आन्तरिक राजस्व", "inc_internal",
    "Projected income from the LG's own internal revenue"),
  e("source", "जन सहभागिता", "inc_public",
    "Projected income from public/community participation (contributions)"),
  e("source", "कुल आय अनुमान","inc_total",
    "Projected total income (sum across sources)"),
  # expense fund-type columns
  e("fund_type", "चालु",  "exp_current",   "Projected current (recurrent) expenditure"),
  e("fund_type", "पूँजीगत", "exp_capital",   "Projected capital expenditure"),
  e("fund_type", "वित्तीय", "exp_financing", "Projected financing expenditure"),
  e("fund_type", "जम्मा",  "exp_total",     "Projected total expenditure"),
  # other
  e("measure", "सभाबाट स्वीकृत मिति", "approval_date",
    "Date the budget was approved by the municipal assembly (Bikram Sambat, YYYY/MM/DD)"),
  e("id_col",  "क्र.सं.",  "(dropped)",   "Serial number"),
  e("id_col",  "संकेत",   "lg_code_raw", "8-digit LG code from source"),
  e("id_col",  "स्थानीय तह नाम","lg_name_np",
    "Full LG name; split into mun_np + district_np"),
  e("id_col",  "जम्मा (row)","(dropped)", "Grand-total row at bottom of source")
)

# ----- combine ---------------------------------------------------------------
codebook <- bind_rows(
  sector_lg, sector_fund_type, sector_monthly_exp, sector_source, sector_trimester,
  projection_summary, projection_source_fund_type
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
write_xlsx(codebook, out_path)
cat("Wrote", out_path, "with", nrow(codebook), "rows across",
    length(unique(codebook$output_file)), "output file(s).\n")
