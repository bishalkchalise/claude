# =============================================================================
# Runs every cleaner in scripts/ in a sensible order.
#
# Prereqs:
#   1. R packages installed (see scripts/README.md)
#   2. lookup/lgcode.csv exists with lgcode filled in (run
#      scripts/bootstrap_lookup.R once first)
#   3. Working directory is the repo root
#
# Outputs land in cleaned_output/. The codebook is rebuilt at the end.
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))

CLEANERS <- c(
  # क्षेत्रगत बजेट तथा खर्च
  "scripts/clean_sector_lg.R",
  "scripts/clean_sector_fund_type.R",
  "scripts/clean_sector_monthly_exp.R",
  "scripts/clean_sector_source.R",
  "scripts/clean_sector_trimester.R",

  # आय र व्यय प्रक्षेपण को सारांश
  "scripts/clean_projection_summary.R",
  "scripts/clean_projection_source_fund_type.R",

  # खर्च शीर्षक अनुसार बजेट तथा खर्च
  "scripts/clean_lg_line_item.R",

  # बजेट र खर्चको सारांश
  "scripts/clean_lg_kosh.R",
  "scripts/clean_lg_summary.R",

  # राजस्व अनुदान (प्राप्ति)
  "scripts/clean_revenue_summary.R",
  "scripts/clean_revenue_heading.R",
  "scripts/clean_revenue_monthly.R",
  "scripts/clean_revenue_sharing.R",

  # राजस्व अनुमान
  "scripts/clean_revenue_estimate.R",

  # लक्षित समूह अनुसार बजेट तथा खर्च
  "scripts/clean_lg_target_group.R",
  "scripts/clean_trimester_target_group.R",

  # विभाज्य कोष
  "scripts/clean_divisible_fund.R"

  # (more cleaners get appended here as we add them)
)

results <- list()
for (s in CLEANERS) {
  if (!file.exists(s)) { message("Skipping missing script: ", s); next }
  message("================ ", s, " ================")
  res <- tryCatch({
    source(s, local = new.env())
    "OK"
  }, error = function(e) {
    message("  ERROR: ", conditionMessage(e))
    paste("ERROR:", conditionMessage(e))
  })
  results[[s]] <- res
}

# Rebuild codebook
message("================ scripts/build_codebook.R ================")
tryCatch(source("scripts/build_codebook.R", local = new.env()),
         error = function(e) message("  codebook ERROR: ", conditionMessage(e)))

message("\n=== Summary ===")
for (s in names(results)) {
  message(sprintf("  %-50s %s", basename(s), results[[s]]))
}
