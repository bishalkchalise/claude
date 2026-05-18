# =============================================================================
# Clean: लक्षित समूह अनुसार बजेट तथा खर्च / LG Target Group
#
# Per-LG budget vs expenditure split by 9 target groups (vulnerable / equity
# categories such as women, dalit, indigenous, disabled etc.).
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-7 : 3-level header (target_group > बजेट/खर्च)
#   - rows 8..n-1 : 753 LG rows
#   - row n    : जम्मा (drop)
#   - cols 1-3 : sn | संकेत (11-digit) | नाम (mun, district)
#       The LG code is 11 digits = 8-digit LG code + 3-digit target-group
#       suffix. We extract the first 8 digits to look up lgcode.
#   - cols 4-21 : 9 target groups × (बजेट, खर्च) = 18 value cols
#   - cols 22-24 : जम्मा (drop)
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "लक्षित समूह अनुसार बजेट तथा खर्च/LG Target Group"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "lg_target_group.xlsx"

TARGET_GROUP_LABEL <- c(
  "अपाङ्ग"            = "disabled",
  "अन्य"              = "other",
  "जेष्ठ नागरिक"      = "senior_citizen",
  "आदिवासी /जनजाती" = "indigenous",
  "मधेसी"            = "madhesi",
  "दलित"             = "dalit",
  "सीमान्तकृत"        = "marginalized",
  "महिला"            = "women",
  "बालबालिका"       = "children"
)

np_digits_to_ascii <- function(x) {
  x <- as.character(x)
  np <- c("०","१","२","३","४","५","६","७","८","९")
  for (i in 0:9) x <- gsub(np[i+1], as.character(i), x, fixed = TRUE)
  x
}
parse_np_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- np_digits_to_ascii(x)
  neg <- grepl("^\\s*\\(.*\\)\\s*$", x)
  x <- gsub("[(),\\s]", "", x, perl = TRUE)
  v <- suppressWarnings(as.numeric(x))
  v[neg] <- -v[neg]; v
}
extract_fy <- function(x) {
  s <- np_digits_to_ascii(as.character(x))
  m <- regmatches(s, regexpr("\\d{4}/\\d{2}", s))
  if (length(m)) m else NA_character_
}
load_lookup <- function(path) {
  if (!file.exists(path)) { warning("Lookup not found: ", path); return(NULL) }
  lk <- read.csv(path, fileEncoding = "UTF-8",
                 stringsAsFactors = FALSE, check.names = FALSE)
  lk %>% transmute(district_np = str_squish(district_np),
                   mun_np      = str_squish(mun_np),
                   lgcode      = as.character(lgcode)) %>%
        distinct(district_np, mun_np, .keep_all = TRUE)
}

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  fy <- extract_fy(raw[[4, 1]])

  # Validate row 6 target group names at C4, C6, ..., C20
  tg_cols <- seq(4, 20, by = 2)
  r6_tg <- as.character(unlist(raw[6, tg_cols]))
  expected_tg <- names(TARGET_GROUP_LABEL)
  if (!identical(r6_tg, expected_tg)) {
    stop("Target-group header mismatch in ", basename(file),
         "\n  expected: ", paste(expected_tg, collapse=" | "),
         "\n  got     : ", paste(r6_tg, collapse=" | "))
  }

  # Build value column names: <tg>_bud, <tg>_exp x 9 groups
  val_names <- character()
  for (lbl in unname(TARGET_GROUP_LABEL)) {
    val_names <- c(val_names, paste0(lbl, "_bud"), paste0(lbl, "_exp"))
  }
  vals <- as.data.frame(lapply(raw[, 4:21], parse_np_num))
  names(vals) <- val_names

  df <- bind_cols(
    tibble(lg_code_raw = np_digits_to_ascii(raw[[2]]),
           lg_name_np  = as.character(raw[[3]])),
    vals
  )
  df <- df[8:nrow(df), , drop = FALSE]

  # Keep only LG rows (11-digit code, name has comma)
  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{10,12}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]

  # Extract the 8-digit LG code prefix for the lookup join
  df$lg_code_8 <- substr(df$lg_code_raw, 1, 8)

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])

  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)

  df[, c("fy","lgcode","district_np","mun_np","lg_code_8","lg_code_raw","lg_name_np",
         val_names, "source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  lookup <- load_lookup(LOOKUP_FILE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- map_dfr(files, ~ clean_one(.x, lookup))

  miss <- sum(is.na(combined$lgcode))
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols (%d unmatched lgcode)",
                  out_path, nrow(combined), ncol(combined), miss))
}

main()
