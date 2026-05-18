# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / LG Revenue Heading  (Gross + Net)
# Long format: one row per (LG, revenue heading).
# =============================================================================

source("scripts/_helpers.R")

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/LG Revenue Heading"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "lg_revenue_heading_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "lg_revenue_heading_net_receipt.xlsx")
)

VAL_MAP <- c(
  "अनुमान"             = "estimate",
  "प्राप्ती"             = "received",
  "Receipts Percentage" = "receipt_pct",
  "मौज्दात"            = "balance"
)

clean_one <- function(file, lookup, receipt_type) {
  message("Reading: ", basename(file), " (", receipt_type, ")")
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 6) stop("file too small")
  meta <- parse_title(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))

  # LG block: स्थानीय तह in r5 with संकेत / नाम in r6 directly below.
  lg_top <- which(r5 == "स्थानीय तह")
  rh_top <- which(r5 == "राजस्व शीर्षक")
  if (!length(lg_top) || !length(rh_top))
    stop("missing स्थानीय तह / राजस्व शीर्षक in row 5")

  # Find संकेत/नाम under each block (first two non-empty positions)
  lg_code_col <- which(r6 == "संकेत" & seq_along(r6) >= lg_top[1] & seq_along(r6) < rh_top[1])
  lg_name_col <- which(r6 == "नाम"   & seq_along(r6) >= lg_top[1] & seq_along(r6) < rh_top[1])
  rh_code_col <- which(r6 == "संकेत" & seq_along(r6) >= rh_top[1])
  rh_name_col <- which(r6 == "नाम"   & seq_along(r6) >= rh_top[1])
  if (!length(lg_code_col) || !length(lg_name_col) ||
      !length(rh_code_col) || !length(rh_name_col))
    stop("missing संकेत / नाम sub-headers in row 6")
  lg_code_col <- lg_code_col[1]; lg_name_col <- lg_name_col[1]
  rh_code_col <- rh_code_col[1]; rh_name_col <- rh_name_col[1]

  val_cols <- list()
  for (nm in names(VAL_MAP)) {
    p <- which(r5 == nm)
    if (length(p)) val_cols[[VAL_MAP[[nm]]]] <- p[1]
  }
  if (!length(val_cols)) stop("no value headers in row 5")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

  df <- bind_cols(
    tibble(
      lg_code_raw          = np_digits_to_ascii(raw[[lg_code_col]]),
      lg_name_np           = as.character(raw[[lg_name_col]]),
      revenue_heading_code = np_digits_to_ascii(raw[[rh_code_col]]),
      revenue_heading_np   = as.character(raw[[rh_name_col]])
    ),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np) &
          !is.na(df$revenue_heading_code) &
          grepl("^[0-9]+$", df$revenue_heading_code) &
          !is.na(df$revenue_heading_np) &
          nzchar(str_squish(df$revenue_heading_np))
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid rows")

  .splt <- split_lg_name(df$lg_name_np)
  df$mun_np      <- .splt$mun_np
  df$district_np <- .splt$district_np

  df <- attach_lgcode(df, lookup)

  df$fy <- meta$fy
  df$province <- meta$province
  df$district_filter <- meta$district_filter
  df$receipt_type <- receipt_type
  df$source_file <- basename(file)
  df[, c("fy","receipt_type","province","district_filter",
         "lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         "revenue_heading_code","revenue_heading_np",
         names(val_cols), "source_file"), drop = FALSE]
}

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  for (rt in names(RECEIPT_VARIANTS)) {
    spec  <- RECEIPT_VARIANTS[[rt]]
    indir <- file.path(BASE_INPUT, spec$dir)
    if (!dir.exists(indir)) { message("Missing: ", indir); next }
    files <- list.files(indir, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { message("No files: ", indir); next }
    combined <- safe_map_files(files, clean_one, lookup = lookup, receipt_type = rt)
    if (!nrow(combined)) { message("  ", spec$out, " -- no rows"); next }
    miss <- sum(is.na(combined$lgcode))
    out_path <- file.path(OUTPUT_DIR, spec$out)
    write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows | %d LGs | %d headings | unmatched: %d",
                    basename(out_path), nrow(combined),
                    length(unique(combined$lg_code_raw)),
                    length(unique(combined$revenue_heading_code)), miss))
  }
}

main()
