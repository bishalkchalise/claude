# =============================================================================
# Clean: राजस्व अनुमान / (folder root)
# Per-LG revenue projection with grant-type breakdown.
# Headers detected from rows 5/6 so column count variation is tolerated.
# =============================================================================

source("scripts/_helpers.R")

INPUT_DIR   <- "राजस्व अनुमान"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "revenue_estimate.xlsx"

# Top-level groups (row 5) -> name prefix for sub-cells under them
TOP_GROUPS <- c(
  "संघीय सरकार" = "fed",
  "प्रदेश सरकार" = "prov",
  "अन्तर स्थानीय तह" = "inter_lg",
  "वैदेशिक स्रोत" = "foreign_src",
  "राजस्व बाँडफाँट" = "revshare",
  "राजस्व बाडफाड"  = "revshare",
  "आन्तरिक श्रोत" = "internal",
  "कुल आय अनुमान" = "total_income",     # dropped per no-totals rule
  "न्यूनपूर्ति गर्ने श्रोतहरु" = "shortfall"
)

# Sub-cell labels (row 6) -> short suffix
SUB_LABEL <- c(
  "समानिकरण अनुदान" = "equalization",
  "सशर्त अनुदान"    = "conditional",
  "विषेश अनुदान"    = "special",
  "समपुरक अनुदान"  = "complementary",
  "अन्य अनुदान"    = "other_grant",
  "संघीय सरकार"    = "fed",
  "प्रदेश सरकार"     = "prov",
  "प्रत्यायोजित कराधिकार" = "delegated_tax",
  "विद्यमान अधिकार" = "existing_rights",
  "अन्य"           = "other",
  "जन सहभागिता"  = "public",
  "जनसहभागिता"   = "public",
  "ऋण"           = "loan",
  "नगद मौज्दात"   = "cash_balance"
)

# Standalone shortfall sources (row 6 under न्यूनपूर्ति) get plain labels.
STANDALONE_KEEP <- c("loan" = "loan", "cash_balance" = "cash_balance")

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 7 || ncol(raw) < 6) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))
  r5_ff <- ffill(r5)

  val_cols <- list()
  for (c in seq_along(r6)) {
    sub <- r6[c]
    if (is.na(sub) || sub == "") next
    if (sub == "संकेत" || sub == "नाम") next
    if (!(sub %in% names(SUB_LABEL))) next

    top <- r5_ff[c]
    if (is.na(top) || !(top %in% names(TOP_GROUPS))) next
    if (top == "कुल आय अनुमान") next   # drop total

    top_pref <- TOP_GROUPS[[top]]
    sub_lbl  <- SUB_LABEL[[sub]]

    # Special case: under न्यूनपूर्ति, use plain "loan" / "cash_balance"
    if (top == "न्यूनपूर्ति गर्ने श्रोतहरु" && sub_lbl %in% names(STANDALONE_KEEP)) {
      col_name <- STANDALONE_KEEP[[sub_lbl]]
    } else if (top_pref == sub_lbl) {
      # row 5 group and row 6 sub same label (e.g. inter_lg) -> use top_pref alone
      col_name <- top_pref
    } else if (top_pref == "internal" && sub_lbl == "public") {
      col_name <- "internal_public"
    } else {
      col_name <- paste(top_pref, sub_lbl, sep = "_")
    }
    val_cols[[col_name]] <- c
  }

  # Top-level groups with NO row-6 sub-cell (e.g. अन्तर स्थानीय तह, वैदेशिक स्रोत
  # span a single column). Pick them up directly.
  for (nm in c("अन्तर स्थानीय तह", "वैदेशिक स्रोत")) {
    pos <- which(r5 == nm)
    if (length(pos)) {
      c <- pos[1]
      if (is.na(r6[c]) || r6[c] == "") val_cols[[TOP_GROUPS[[nm]]]] <- c
    }
  }

  if (!length(val_cols)) stop("no recognised value columns")

  vals <- as.data.frame(lapply(val_cols, function(c) parse_np_num(raw[[c]])))
  names(vals) <- names(val_cols)

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
  if (!nrow(df)) stop("no valid LG rows")

  .splt <- split_lg_name(df$lg_name_np)
  df$mun_np      <- .splt$mun_np
  df$district_np <- .splt$district_np
  df <- attach_lgcode(df, lookup)

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_raw","lg_name_np",
         names(val_cols), "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- safe_map_files(files, clean_one, lookup = lookup)
  if (!nrow(combined)) { message("No data parsed."); return(invisible(NULL)) }
  miss <- sum(is.na(combined$lgcode))
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols (%d unmatched lgcode)",
                  out_path, nrow(combined), ncol(combined), miss))
}

main()
