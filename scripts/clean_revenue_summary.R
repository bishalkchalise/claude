# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / Summary  (Gross + Net)
# Per-LG receipts summary.
# =============================================================================

source("scripts/_helpers.R")

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/Summary"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "summary_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "summary_net_receipt.xlsx")
)

HEAD_MAP <- c(
  "अनुमान"             = "estimate",
  "प्राप्ती"             = "received",
  "Receipts Percentage" = "receipt_pct",
  "मौज्दात"            = "balance"
)

clean_one <- function(file, lookup, receipt_type) {
  message("Reading: ", basename(file), " (", receipt_type, ")")
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 6 || ncol(raw) < 4) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  code_col <- which(r5 == "संकेत")[1]
  name_col <- which(r5 == "स्थानीय तह")[1]
  if (is.na(code_col) || is.na(name_col))
    stop("missing संकेत / स्थानीय तह in row 5")

  val_cols <- list()
  for (nm in names(HEAD_MAP)) {
    pos <- which(r5 == nm)
    if (length(pos)) val_cols[[HEAD_MAP[[nm]]]] <- pos[1]
  }
  if (!length(val_cols)) stop("no value headers in row 5")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[code_col]]),
           lg_name_np  = as.character(raw[[name_col]])),
    vals
  )
  df <- df[6:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid LG rows")

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$receipt_type <- receipt_type
  df$source_file <- basename(file)
  df[, c("fy","receipt_type","lgcode","district_np","mun_np",
         "lg_code_raw","lg_name_np",
         names(val_cols), "source_file"), drop = FALSE]
}

main <- function() {
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  for (rt in names(RECEIPT_VARIANTS)) {
    spec  <- RECEIPT_VARIANTS[[rt]]
    indir <- file.path(BASE_INPUT, spec$dir)
    if (!dir.exists(indir)) { message("Missing dir: ", indir); next }
    files <- list.files(indir, pattern = "\\.xlsx$", full.names = TRUE)
    if (!length(files)) { message("No files in ", indir); next }
    combined <- safe_map_files(files, clean_one, lookup = lookup, receipt_type = rt)
    if (!nrow(combined)) { message("  ", spec$out, " -- no rows parsed"); next }
    miss <- sum(is.na(combined$lgcode))
    out_path <- file.path(OUTPUT_DIR, spec$out)
    write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows × %d cols (%d unmatched)",
                    basename(out_path), nrow(combined), ncol(combined), miss))
  }
}

main()
