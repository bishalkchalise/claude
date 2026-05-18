# =============================================================================
# Clean: राजस्व अनुदान (प्राप्ति) / Revenue Sharing by GoN  (Gross + Net)
# Detects revenue-heading codes dynamically from row 6, pivots to long.
# =============================================================================

source("scripts/_helpers.R")

BASE_INPUT  <- "राजस्व  अनुदान (प्राप्ति)/Revenue Sharing by GoN"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"

RECEIPT_VARIANTS <- list(
  gross = list(dir = "Gross Receipt", out = "revenue_sharing_gross_receipt.xlsx"),
  net   = list(dir = "Net Receipt",   out = "revenue_sharing_net_receipt.xlsx")
)

detect_heading_cols <- function(raw) {
  r6 <- as.character(unlist(raw[6, ]))
  out <- list()
  for (c in 4:length(r6)) {
    v <- r6[c]
    if (is.na(v) || v == "") next
    if (v == "जम्मा") break
    code <- np_digits_to_ascii(v)
    if (grepl("^[0-9]+$", code)) out[[code]] <- c
  }
  if (!length(out)) return(integer())
  setNames(unlist(out), names(out))
}

empty_result <- function() {
  tibble(fy=character(), receipt_type=character(),
         province=character(), district_filter=character(),
         lgcode=character(), district_np=character(), mun_np=character(),
         lg_code_raw=character(), lg_name_np=character(),
         revenue_heading_code=character(), shared_amount=numeric(),
         source_file=character())
}

clean_one <- function(file, lookup, receipt_type) {
  message("Reading: ", basename(file), " (", receipt_type, ")")
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 5) return(empty_result())
  meta <- parse_title(raw[[4, 1]])

  heading_cols <- detect_heading_cols(raw)
  if (!length(heading_cols)) return(empty_result())
  if (nrow(raw) < 7) return(empty_result())

  vals <- as.data.frame(lapply(heading_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(heading_cols)
  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[2]]),
           lg_name_np  = as.character(raw[[3]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{6,10}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(empty_result())

  .splt <- split_lg_name(df$lg_name_np)
  df$mun_np      <- .splt$mun_np
  df$district_np <- .splt$district_np
  df <- attach_lgcode(df, lookup)

  long <- df %>%
    pivot_longer(cols = all_of(names(heading_cols)),
                 names_to = "revenue_heading_code",
                 values_to = "shared_amount") %>%
    filter(!is.na(shared_amount))

  long$fy <- meta$fy
  long$province <- meta$province
  long$district_filter <- meta$district_filter
  long$receipt_type <- receipt_type
  long$source_file <- basename(file)
  long[, c("fy","receipt_type","province","district_filter",
           "lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
           "revenue_heading_code","shared_amount","source_file"),
       drop = FALSE]
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
    miss <- if (nrow(combined)) sum(is.na(combined$lgcode)) else 0
    out_path <- file.path(OUTPUT_DIR, spec$out)
    write_xlsx(combined, out_path)
    message(sprintf("  %s -> %d rows | unmatched: %d",
                    basename(out_path), nrow(combined), miss))
  }
}

main()
