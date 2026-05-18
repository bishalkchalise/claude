# =============================================================================
# Clean: लक्षित समूह अनुसार बजेट तथा खर्च / Trimester Target Group
#
# National (not per-LG) rollup of budget vs expenditure split by target
# group across the three Nepali FY trimesters.
#
# Structure:
#   - rows 1-4 : title (R4C1 = FY)
#   - rows 5-6 : 2-level header
#       R5: sn | शीर्षक | प्रथम चौमासिक | दोश्रो चौमासिक | तेस्रो चौमासिक | जम्मा
#       R6: संकेत | नाम | बजेट/खर्च per trimester + खर्च(%) + मौज्दात
#   - rows 7..n-1 : data (first data row often has NA target group code:
#                   that's a "<all groups>" aggregate -- DROP it)
#   - row n    : कुल जम्मा (drop)
# =============================================================================

suppressWarnings(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(readxl); library(writexl); library(dplyr)
  library(stringr); library(purrr)
})

INPUT_DIR   <- "लक्षित समूह अनुसार बजेट तथा खर्च/Trimester Target Group"
OUTPUT_DIR  <- "cleaned_output"
OUTPUT_FILE <- "trimester_target_group.xlsx"

# 3-digit target group code -> short English label
TG_LABEL <- c(
  "100" = "other",
  "101" = "women",
  "102" = "children",
  "103" = "indigenous",
  "104" = "madhesi",
  "105" = "dalit",
  "106" = "marginalized",
  "107" = "disabled",
  "108" = "senior_citizen"
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

clean_one <- function(file) {
  message("Reading: ", basename(file))
  raw <- suppressMessages(read_excel(file, col_names = FALSE,
                                     .name_repair = "minimal"))
  fy <- extract_fy(raw[[4, 1]])

  # Validate R5/R6 headers
  if (raw[[5, 4]] != "प्रथम चौमासिक" ||
      raw[[5, 6]] != "दोश्रो चौमासिक" ||
      raw[[5, 8]] != "तेस्रो चौमासिक")
    stop("R5 trimester headers mismatch in ", basename(file))
  if (raw[[6, 12]] != "खर्च(%)" || raw[[6, 13]] != "मौज्दात")
    stop("R6 trailing headers mismatch in ", basename(file))

  vals <- as.data.frame(lapply(raw[, 4:13], parse_np_num))
  names(vals) <- c("t1_bud","t1_exp","t2_bud","t2_exp","t3_bud","t3_exp",
                   # 10,11 = jamma total (drop later), 12 = exp_pct, 13 = balance
                   "total_bud_drop","total_exp_drop","exp_pct","balance")

  df <- bind_cols(
    tibble(target_group_code = np_digits_to_ascii(raw[[2]]),
           target_group_np   = as.character(raw[[3]])),
    vals
  )
  df <- df[7:nrow(df), , drop = FALSE]

  # Drop the leading "<all groups>" aggregate row (code = NA) and any
  # "कुल जम्मा" footer row.
  df <- df %>%
    filter(!is.na(target_group_code),
           grepl("^[0-9]+$", target_group_code),
           !is.na(target_group_np),
           nzchar(str_squish(target_group_np)))

  df$target_group <- ifelse(df$target_group_code %in% names(TG_LABEL),
                            unname(TG_LABEL[df$target_group_code]),
                            NA_character_)
  bad <- df %>% filter(is.na(target_group))
  if (nrow(bad)) {
    stop("Unmapped target group code(s) in ", basename(file), ":\n  ",
         paste(unique(bad$target_group_code), collapse=", "))
  }

  df$fy <- fy
  df$source_file <- basename(file)
  df[, c("fy","target_group_code","target_group","target_group_np",
         "t1_bud","t1_exp","t2_bud","t2_exp","t3_bud","t3_exp",
         "exp_pct","balance","source_file"), drop = FALSE]
}

main <- function() {
  stopifnot(dir.exists(INPUT_DIR))
  if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
  files <- list.files(INPUT_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No .xlsx files in ", INPUT_DIR)
  combined <- map_dfr(files, clean_one)
  out_path <- file.path(OUTPUT_DIR, OUTPUT_FILE)
  writexl::write_xlsx(combined, out_path)
  message(sprintf("Wrote %s -> %d rows × %d cols",
                  out_path, nrow(combined), ncol(combined)))
}

main()
