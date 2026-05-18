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

# ----- खर्च शीर्षक अनुसार बजेट तथा खर्च / LG Line Item -----------------------
# One output file per FY: lg_line_item_fy207677.xlsx ... lg_line_item_fy208182.xlsx
# (all 6 share the same column schema)
set_ctx("LG Line Item", "lg_line_item_fy<YYYY>.xlsx")
lg_line_item <- bind_rows(
  e("id_col", "(parsed from R4C1)", "fy",
    "Nepali fiscal year (YYYY/YY) parsed from the title row"),
  e("id_col", "प्रदेश",     "province",
    "Province name parsed from the title row (files are partitioned by province + district)"),
  e("id_col", "जिल्ला",    "district_filter",
    "District name parsed from the title row (the district that this source file represents)"),
  e("id_col", "संकेत",     "lg_code_raw",   "8-digit LG code from source"),
  e("id_col", "स्थानीय तह नाम", "lg_name_np",
    "Full LG name; split into mun_np + district_np"),
  e("id_col", "क्र.सं.",   "(dropped)",     "Serial number"),
  e("id_col", "जम्मा (row)", "(dropped)",   "Grand-total row at bottom of source"),
  e("id_col", "जम्मा (col block)", "(dropped)",
    "Grand-total column block at right of source"),
  e("fund_type", "चालु",   "current",
    "Current (recurrent) expenditure line items -- codes typically start with 2xxxx"),
  e("fund_type", "पूंजीगत",  "capital",
    "Capital expenditure line items -- codes typically start with 3xxxx"),
  e("measure",   "बजेट",   "bud",   "Budget for the line item"),
  e("measure",   "खर्च",   "exp",   "Expenditure for the line item"),
  e("id_col", "(line item code)", "line_item",
    paste("Government chart-of-accounts code (e.g. 21111 = compensation",
          "of employees, 22xxx = goods & services, 31xxx = fixed capital,",
          "etc.). The data is LONG-format: one row per (LG, line item).",
          "First digit indicates fund type (see fund_type col)."))
)

# ----- बजेट र खर्चको सारांश / LG Kosh ---------------------------------------
set_ctx("LG Kosh", "lg_kosh.xlsx")
lg_kosh <- bind_rows(
  e("fund", "आकस्मिक कोष",                      "contingency",
    "Contingency Fund — money set aside for unforeseen expenses"),
  e("fund", "कर्मचारी कल्याण कोष",                "emp_welfare",
    "Employee Welfare Fund — benefits / welfare for LG staff"),
  e("fund", "कार्य संचालन कोष - बिबिध",         "ops_misc",
    "Operations Fund (Miscellaneous) — general operations spending"),
  e("fund", "गरिवी निवारण तथा सामाजिक परिचालन कोष", "poverty_social",
    "Poverty Alleviation & Social Mobilization Fund"),
  e("fund", "प्रकोप व्यवस्थापन कोष",              "disaster",
    "Disaster Management Fund — emergency response & relief"),
  e("fund", "बिबिध खर्च खाता/कोष- बैंक (ग २-७)", "misc_bank",
    "Miscellaneous Expense Account / Fund (Bank Group 2-7)"),
  e("fund", "मर्मत सम्भार कोष",                  "maintenance",
    "Maintenance Fund — for asset upkeep & repairs"),
  e("fund", "महिला तथा वाल विकास कोष",         "women_child",
    "Women & Child Development Fund"),
  e("fund", "मानव संशाधन विकास कोष",          "hrd_fund",
    "Human Resource Development Fund"),
  e("fund", "वातावरण ब्यवस्थापन कोष",           "env_mgmt",
    "Environmental Management Fund"),
  e("fund", "स्थानीय विकास कोष कार्यक्रम संचालन कोष", "ldf_ops",
    "Local Development Fund Program Operations Fund"),
  e("fund", "स्थानीय विकास घुम्ती कोष",          "ldf_revolving",
    "Local Development Revolving Fund"),
  e("fund", "जम्मा",                            "(dropped)",
    "Grand-total fund column at end of source"),
  e("measure", "बजेट", "<fund>_bud", "Budget allocation per fund"),
  e("measure", "खर्च", "<fund>_exp", "Expenditure per fund")
)

# ----- बजेट र खर्चको सारांश / LG Summary -------------------------------------
set_ctx("LG Summary", "lg_summary.xlsx")
lg_summary <- bind_rows(
  e("measure", "शुरुको मौज्दात",  "opening_balance",
    "Opening balance at start of fiscal year (Operational Fund)"),
  e("measure", "प्राप्त",        "received",
    "Funds received during the year (may be negative for adjustments)"),
  e("measure", "जम्मा प्राप्ती",   "total_receipts",
    "Total receipts = opening balance + received"),
  e("measure", "खर्च",         "expenditure",
    "Expenditure during the year"),
  e("measure", "मौज्दात",       "closing_balance",
    "Closing balance at end of fiscal year"),
  e("id_col",  "क्र.सं.",      "(dropped)",   "Serial number"),
  e("id_col",  "जम्मा (row)", "(dropped)",  "Grand-total row at bottom of source")
)

# ----- राजस्व अनुदान (प्राप्ति) / Summary -------------------------------------
set_ctx("Revenue Summary", "summary_{gross,net}_receipt.xlsx")
revenue_summary <- bind_rows(
  e("id_col",  "(file path)", "receipt_type",
    "gross OR net -- gross = before refunds/adjustments, net = after"),
  e("measure", "अनुमान",    "estimate",
    "Estimated revenue (annual budget for receipts)"),
  e("measure", "प्राप्ती",    "received",
    "Actual revenue received"),
  e("measure", "Receipts Percentage", "receipt_pct",
    "Receipts as % of estimate"),
  e("measure", "मौज्दात",    "balance",
    "Remaining balance (estimate - received); negative if over-collected")
)

# ----- राजस्व अनुदान (प्राप्ति) / LG Revenue Heading -------------------------
set_ctx("LG Revenue Heading", "lg_revenue_heading_{gross,net}_receipt.xlsx")
lg_revenue_heading <- bind_rows(
  e("id_col",  "(file path)", "receipt_type",
    "gross OR net"),
  e("id_col",  "प्रदेश",    "province", "Province filter from title row, if present"),
  e("id_col",  "जिल्ला",   "district_filter", "District filter from title row, if present"),
  e("id_col",  "राजस्व शीर्षक संकेत", "revenue_heading_code",
    "5-digit revenue heading code (chart of accounts on revenue side)"),
  e("id_col",  "राजस्व शीर्षक नाम", "revenue_heading_np",
    "Revenue heading name in Nepali (e.g. 'भुमिकर/मालपोत' = Land tax)"),
  e("measure", "अनुमान",   "estimate",    "Estimated revenue for this heading"),
  e("measure", "प्राप्ती",   "received",    "Actual revenue received for this heading"),
  e("measure", "Receipts Percentage", "receipt_pct", "Receipts as % of estimate"),
  e("measure", "मौज्दात",   "balance",     "Remaining balance")
)

# ----- राजस्व अनुदान (प्राप्ति) / LG Revenue Monthly -------------------------
set_ctx("LG Revenue Monthly", "lg_revenue_monthly_{gross,net}_receipt.xlsx")
lg_revenue_monthly <- bind_rows(
  e("id_col",  "(file path)", "receipt_type", "gross OR net"),
  e("id_col",  "प्रदेश",    "province", "Province filter from title row"),
  e("id_col",  "जिल्ला",   "district_filter", "District filter from title row"),
  e("id_col",  "स्रोत",   "source_np",
    "Funding source (kept as Nepali because values mix federal, specific provinces, internal source)"),
  e("id_col",  "राजस्व शीर्षक संकेत", "revenue_heading_code", "5-digit revenue heading code"),
  e("id_col",  "राजस्व शीर्षक नाम", "revenue_heading_np", "Revenue heading name in Nepali"),
  e("month",   "साउन",   "received_saun",    "Received in Saun (month 1, ~mid-Jul to mid-Aug)"),
  e("month",   "भदौ",    "received_bhadau",  "Received in Bhadau (month 2)"),
  e("month",   "आस्बिन", "received_asoj",    "Received in Asoj (month 3)"),
  e("month",   "कार्तिक", "received_kartik",  "Received in Kartik (month 4)"),
  e("month",   "मार्ग",   "received_mangsir", "Received in Mangsir (month 5)"),
  e("month",   "पौष",   "received_poush",   "Received in Poush (month 6)"),
  e("month",   "माघ",   "received_magh",    "Received in Magh (month 7)"),
  e("month",   "फागुन",  "received_falgun",  "Received in Falgun (month 8)"),
  e("month",   "चैत्र",   "received_chaitra", "Received in Chaitra (month 9)"),
  e("month",   "बैशाख",  "received_baisakh", "Received in Baisakh (month 10)"),
  e("month",   "जेठ",   "received_jestha",  "Received in Jestha (month 11)"),
  e("month",   "असार",  "received_asar",    "Received in Asar (month 12)"),
  e("id_col",  "जम्मा (col)", "(dropped)", "Per-row monthly total column at end")
)

# ----- राजस्व अनुदान (प्राप्ति) / Revenue Sharing by GoN ---------------------
set_ctx("Revenue Sharing by GoN", "revenue_sharing_{gross,net}_receipt.xlsx")
revenue_sharing <- bind_rows(
  e("id_col",  "(file path)", "receipt_type", "gross OR net"),
  e("id_col",  "प्रदेश",    "province", "Province filter from title row"),
  e("id_col",  "जिल्ला",   "district_filter", "District filter from title row"),
  e("id_col",  "(revenue heading code)", "revenue_heading_code",
    paste("5-digit revenue heading code, detected from row 6 columns.",
          "Typical codes: 11411 (VAT), 11421 (Excise), 14153-14158 (various",
          "sharable revenue lines). Pivoted to long format.")),
  e("measure", "(amount per code)", "shared_amount",
    "Amount shared from this revenue heading by Govt of Nepal to the LG"),
  e("id_col",  "जम्मा (col)", "(dropped)", "Per-row total column at end")
)

# ----- राजस्व अनुमान / revenue_estimate ------------------------------------
set_ctx("Revenue Estimate", "revenue_estimate.xlsx")
revenue_estimate <- bind_rows(
  e("source",   "संघीय सरकार",    "fed_*",
    "Federal Government grants: equalization / conditional / special / complementary / other_grant"),
  e("source",   "प्रदेश सरकार",     "prov_*",
    "Provincial Government grants: same 5 grant subtypes as federal"),
  e("source",   "अन्तर स्थानीय तह","inter_lg",
    "Inter-LG transfers (between local governments)"),
  e("source",   "वैदेशिक स्रोत",     "foreign_src",
    "Foreign source income (external aid / financing)"),
  e("source",   "राजस्व बाँडफाँट",  "revshare_{fed,prov}",
    "Revenue sharing income: fed = from federal, prov = from province"),
  e("source",   "आन्तरिक श्रोत",    "internal_*",
    "Internal sources: delegated_tax / existing_rights / other / public (community contrib.)"),
  e("source",   "ऋण",            "loan",
    "Loan used to cover budget shortfall"),
  e("source",   "नगद मौज्दात",    "cash_balance",
    "Cash balance used to cover budget shortfall"),
  e("measure",  "समानिकरण अनुदान","equalization",
    "Equalization Grant — unconditional fiscal transfer to LGs based on a formula"),
  e("measure",  "सशर्त अनुदान",   "conditional",
    "Conditional Grant — must be spent on a specified purpose"),
  e("measure",  "विषेश अनुदान",   "special",
    "Special Grant — for specific projects / disasters / priorities"),
  e("measure",  "समपुरक अनुदान", "complementary",
    "Complementary / Matching Grant — requires LG counterpart funding"),
  e("measure",  "अन्य अनुदान",   "other_grant",     "Other grants"),
  e("measure",  "प्रत्यायोजित कराधिकार","delegated_tax",
    "Delegated taxation authority — taxes LG is empowered to collect"),
  e("measure",  "विद्यमान अधिकार","existing_rights",
    "Existing rights — established LG revenue authorities"),
  e("measure",  "जन सहभागिता","public",
    "Public participation — community contributions"),
  e("id_col",   "कुल आय अनुमान","(dropped)",
    "Total income projection (sum of all sources -- dropped per no-totals rule)"),
  e("id_col",   "जम्मा (row)",   "(dropped)", "Grand-total row at bottom of source")
)

# ----- लक्षित समूह / LG Target Group ----------------------------------------
set_ctx("LG Target Group", "lg_target_group.xlsx")
lg_target_group <- bind_rows(
  e("target_group", "अपाङ्ग",            "disabled",       "Persons with disabilities"),
  e("target_group", "अन्य",              "other",          "Other / unspecified target group"),
  e("target_group", "जेष्ठ नागरिक",      "senior_citizen", "Senior citizens / elderly"),
  e("target_group", "आदिवासी /जनजाती","indigenous",
    "Indigenous peoples / janajati (ethnic minority groups)"),
  e("target_group", "मधेसी",            "madhesi",
    "Madhesi (Terai-region community)"),
  e("target_group", "दलित",             "dalit",
    "Dalit (historically marginalized caste community)"),
  e("target_group", "सीमान्तकृत",        "marginalized", "Marginalized communities"),
  e("target_group", "महिला",            "women",        "Women"),
  e("target_group", "बालबालिका",       "children",     "Children"),
  e("measure",      "बजेट", "<tg>_bud", "Budget allocation for the target group"),
  e("measure",      "खर्च", "<tg>_exp", "Expenditure for the target group"),
  e("id_col",       "संकेत (11-digit)", "lg_code_raw",
    "11-digit code = 8-digit LG code + 3-digit target-group suffix"),
  e("id_col",       "(first 8 digits)", "lg_code_8",
    "First 8 digits of the raw code — used to join with the lookup"),
  e("id_col",       "जम्मा (col)", "(dropped)", "Grand-total budget+expense pair at end of source"),
  e("id_col",       "जम्मा (row)", "(dropped)", "Grand-total row")
)

# ----- लक्षित समूह / Trimester Target Group ----------------------------------
set_ctx("Trimester Target Group", "trimester_target_group.xlsx")
trimester_tg <- bind_rows(
  e("id_col",       "संकेत", "target_group_code", "3-digit target group code (100-108)"),
  e("id_col",       "नाम", "target_group_np",
    "Target group name in Nepali"),
  e("id_col",       "(mapped)", "target_group",
    "Short English label: women/children/dalit/madhesi/disabled/etc."),
  e("trimester",    "प्रथम चौमासिक",  "t1", "1st Trimester (months 1-4)"),
  e("trimester",    "दोश्रो चौमासिक", "t2", "2nd Trimester (months 5-8)"),
  e("trimester",    "तेस्रो चौमासिक",  "t3", "3rd Trimester (months 9-12)"),
  e("measure",      "बजेट", "<t#>_bud", "Trimester budget"),
  e("measure",      "खर्च", "<t#>_exp", "Trimester expenditure"),
  e("measure",      "खर्च(%)", "exp_pct", "Expenditure as % of budget"),
  e("measure",      "मौज्दात", "balance", "Closing balance"),
  e("id_col",       "(NA row)", "(dropped)",
    "Leading aggregate row with NA target group code (sum across all groups)"),
  e("id_col",       "कुल जम्मा", "(dropped)", "Grand-total row")
)

# ----- विभाज्य कोष / Divisible Fund ----------------------------------------
set_ctx("Divisible Fund", "divisible_fund.xlsx")
divisible_fund <- bind_rows(
  e("id_col",  "राजस्व शीर्षक संकेत", "revenue_heading_code",
    "5-digit revenue heading code (chart of accounts)"),
  e("id_col",  "राजस्व शीर्षक शीर्षक", "revenue_heading_np",
    "Revenue heading name in Nepali"),
  e("measure", "शुरुको मौज्दात", "opening_balance", "Opening balance of the divisible fund"),
  e("measure", "प्राप्त",       "received",        "Revenue received during the year"),
  e("measure", "जम्मा प्राप्ती",  "total_receipts",  "Total receipts (opening + received)"),
  e("measure", "नेपाल सरकार",  "dist_fed",        "Amount distributed to Federal Government"),
  e("measure", "प्रदेश सरकार",   "dist_prov",       "Amount distributed to Provincial Government"),
  e("measure", "स्थानीय तह",    "dist_lg",
    "Amount distributed to / retained by the Local Government"),
  e("measure", "Total Distibution", "dist_total",
    "Sum of distributions (sic: source spells 'Distibution')"),
  e("measure", "Distibution Percentage", "dist_pct", "Distributed as % of receipts"),
  e("measure", "मौज्दात",       "closing_balance", "Closing balance"),
  e("id_col",  "जम्मा (row)",   "(dropped)",       "Grand-total row")
)

# ----- combine ---------------------------------------------------------------
codebook <- bind_rows(
  sector_lg, sector_fund_type, sector_monthly_exp, sector_source, sector_trimester,
  projection_summary, projection_source_fund_type,
  lg_line_item,
  lg_kosh, lg_summary,
  revenue_summary, lg_revenue_heading, lg_revenue_monthly, revenue_sharing,
  revenue_estimate, lg_target_group, trimester_tg, divisible_fund
)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
write_xlsx(codebook, out_path)
cat("Wrote", out_path, "with", nrow(codebook), "rows across",
    length(unique(codebook$output_file)), "output file(s).\n")
