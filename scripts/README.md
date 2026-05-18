# LG Fiscal Data — cleaning scripts

R scripts that read the raw Devanagari xlsx files in the repo, clean
them, and write tidy English-named xlsx outputs to `cleaned_output/`.

## Local setup

### R packages
All scripts depend on the same set:

```r
install.packages(c(
  "readxl", "writexl", "dplyr", "tidyr",
  "stringr", "purrr", "stringdist"
))
```

`stringdist` is only needed by `bootstrap_lookup.R`; the cleaners
require the other six.

### Working directory
All paths are relative to the repository root. Open R / RStudio with
the working directory set to the repo root (the folder containing
`scripts/`, `lookup/`, `cleaned_output/`, the Nepali data folders…).

### Locale
Every script starts with:

```r
suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
```

On Linux / macOS this just works. On **Windows R ≥ 4.2** this is a
no-op (R 4.2+ is UTF-8 native). If you're on older Windows R and see
file-path errors, try `"Nepali_Nepal.65001"` or `"English_United States.utf8"`.

## One-time setup

### 1. Build the lookup (`lookup/lgcode.csv`)
The cleaners join LG names to a 5-digit `lgcode`. Run **once** to
create the lookup, then reuse it forever:

```r
source("scripts/bootstrap_lookup.R")
```

This scans every xlsx, extracts unique `(district, mun)` pairs, and
writes `lookup/lgcode.csv` with a blank `lgcode` column you fill in
once (or matched against an existing master lookup at
`lookup/lg_lookup_master.xlsx` if you have one).

## Running cleaners

Each subfolder of source data has its own cleaning script in `scripts/`.

Run a single one:
```r
source("scripts/clean_sector_lg.R")
```

Or all of them in one go:
```r
source("scripts/run_all.R")
```

Outputs land in `cleaned_output/` as `*.xlsx`.

## What each cleaner produces

| Script                                | Output xlsx                                    | Granularity |
|---------------------------------------|------------------------------------------------|-------------|
| clean_sector_lg.R                     | sector_lg.xlsx                                 | per LG |
| clean_sector_fund_type.R              | sector_fund_type.xlsx                          | national  |
| clean_sector_monthly_exp.R            | sector_monthly_exp.xlsx                        | national |
| clean_sector_source.R                 | sector_source.xlsx                             | national |
| clean_sector_trimester.R              | sector_trimester.xlsx                          | national |
| clean_projection_summary.R            | projection_summary.xlsx                        | per LG |
| clean_projection_source_fund_type.R   | projection_source_fund_type.xlsx               | per LG |
| clean_lg_line_item.R                  | lg_line_item_fy<YYYYY>.xlsx (one per FY)       | per LG (long) |
| clean_lg_kosh.R                       | lg_kosh.xlsx                                   | per LG |
| clean_lg_summary.R                    | lg_summary.xlsx                                | per LG |
| clean_revenue_summary.R               | summary_{gross,net}_receipt.xlsx               | per LG |
| clean_revenue_heading.R               | lg_revenue_heading_{gross,net}_receipt.xlsx   | per LG (long) |
| clean_revenue_monthly.R               | lg_revenue_monthly_{gross,net}_receipt.xlsx   | per LG (long) |
| clean_revenue_sharing.R               | revenue_sharing_{gross,net}_receipt.xlsx       | per LG (long) |
| clean_revenue_estimate.R              | revenue_estimate.xlsx                          | per LG |
| clean_lg_target_group.R               | lg_target_group.xlsx                           | per LG |
| clean_trimester_target_group.R        | trimester_target_group.xlsx                    | national |
| clean_divisible_fund.R                | divisible_fund.xlsx                            | per LG (long) |

(More cleaners get added as we work through the remaining subfolders.)

## Codebook

`docs/codebook.xlsx` documents every variable in every cleaned xlsx —
the original Nepali label, the short English column name we use, the
type (sector / subsector / measure / month / id_col …), and a plain
English description so a non-Nepali reader can interpret each
variable. Regenerate it after editing labels:

```r
source("scripts/build_codebook.R")
```

## Behavioral notes

- **Devanagari digits** are converted to ASCII; values in parentheses
  like `(20,00,000.00)` are parsed as negatives.
- **Title row R4C1** is parsed for fiscal year and (when present)
  province / district filters.
- **Totals are removed**: grand-total rows and grand-total columns are
  dropped from every output.
- Per-LG datasets carry `lgcode`, `district_np`, `mun_np`,
  `lg_code_raw`, `lg_name_np` for joining with the lookup. If your
  `lgcode.csv` is incomplete, those rows will show `NA` in `lgcode`;
  the script reports the count.
- Each cleaner globs **all** `*.xlsx` under its target subfolder, so
  adding more files locally is transparent.
