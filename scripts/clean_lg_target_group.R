# =============================================================================
# Clean: लक्षित समूह अनुसार बजेट तथा खर्च / LG Target Group
# Per-LG budget vs expenditure across target groups (women, dalit, etc.).
# Target-group columns detected from row 6 by name.
# =============================================================================

source("scripts/_helpers.R")

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

clean_one <- function(file, lookup) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  if (nrow(raw) < 8 || ncol(raw) < 6) stop("file too small")
  fy <- extract_fy(raw[[4, 1]])

  r5 <- as.character(unlist(raw[5, ]))
  r6 <- as.character(unlist(raw[6, ]))
  r7 <- as.character(unlist(raw[7, ]))

  bud_cols <- integer(); exp_cols <- integer(); names_vec <- character()
  for (c in seq_along(r6)) {
    tg <- r6[c]
    if (is.na(tg) || tg == "" || !(tg %in% names(TARGET_GROUP_LABEL))) next
    if (c + 1 > length(r7)) next
    if (!(r7[c] %in% names(BUDEXP_LABEL)) ||
        !(r7[c+1] %in% names(BUDEXP_LABEL))) next
    pref <- TARGET_GROUP_LABEL[[tg]]
    bud_cols  <- c(bud_cols, c)
    exp_cols  <- c(exp_cols, c + 1)
    names_vec <- c(names_vec,
                   paste0(pref, "_", BUDEXP_LABEL[[r7[c]]]),
                   paste0(pref, "_", BUDEXP_LABEL[[r7[c+1]]]))
  }
  if (!length(bud_cols)) stop("no target-group columns detected")

  data_cols  <- as.integer(rbind(bud_cols, exp_cols))
  data_names <- make.unique(names_vec, sep = "_")

  id <- tibble(
    lg_code_raw = np_digits_to_ascii(raw[[2]]),
    lg_name_np  = as.character(raw[[3]])
  )
  vals <- as.data.frame(lapply(raw[, data_cols], parse_np_num),
                        col.names = data_names)
  df <- bind_cols(id, vals)
  df <- df[8:nrow(df), , drop = FALSE]

  # 11-digit code = 8-digit LG code + 3-digit target-group suffix.
  # Some files may have 10-digit codes; allow 10-12 to be safe but exclude
  # the 8-digit aggregate / total rows.
  keep <- !is.na(df$lg_code_raw) &
          grepl("^[0-9]{10,12}$", df$lg_code_raw) &
          !is.na(df$lg_name_np) & grepl(",", df$lg_name_np)
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) stop("no valid rows")
  df$lg_code_8 <- substr(df$lg_code_raw, 1, 8)

  parts <- str_split_fixed(df$lg_name_np, ",", 2)
  df$mun_np      <- str_squish(parts[, 1])
  df$district_np <- str_squish(parts[, 2])
  if (!is.null(lookup)) df <- df %>% left_join(lookup, by = c("district_np","mun_np"))
  else df$lgcode <- NA_character_

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","lgcode","district_np","mun_np","lg_code_8","lg_code_raw","lg_name_np",
         data_names, "source_file"), drop = FALSE]
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
