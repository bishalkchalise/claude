# =============================================================================
# Scan every cleaned_output xlsx, collect rows where lgcode is NA, and write
# a single review CSV: lookup/unmatched_review.csv
#
# Columns: district_np, mun_np, n_rows, source_files, suggested_lgcode,
#          suggested_district_np, suggested_mun_np, fuzzy_distance
#
# Workflow:
#   1. Run scripts/run_all.R once
#   2. Run scripts/reconcile_lookup.R       <-- this script
#   3. Open lookup/unmatched_review.csv. For each row, decide:
#        - the suggestion is correct -> the suggested_lgcode column tells you
#          the right code. Append a row to lookup/lgcode.csv mapping
#          (district_np, mun_np) to that lgcode.
#        - the suggestion is wrong -> look up the right code manually,
#          add to lookup/lgcode.csv.
#   4. Re-run scripts/run_all.R. Unmatched count should drop to 0.
# =============================================================================

source("scripts/_helpers.R")

CLEANED_DIR <- "cleaned_output"
LOOKUP_FILE <- "lookup/lgcode.csv"
OUT_PATH    <- "lookup/unmatched_review.csv"

main <- function() {
  if (!dir.exists(CLEANED_DIR)) stop("Run scripts/run_all.R first.")
  lookup <- load_lookup(LOOKUP_FILE)
  if (is.null(lookup)) stop("Lookup missing: ", LOOKUP_FILE)
  has_sd <- requireNamespace("stringdist", quietly = TRUE)

  files <- list.files(CLEANED_DIR, pattern = "\\.xlsx$", full.names = TRUE)
  if (!length(files)) stop("No cleaned outputs found.")

  collected <- list()
  for (f in files) {
    d <- tryCatch(suppressMessages(read_excel(f)), error = function(e) NULL)
    if (is.null(d) || !all(c("district_np","mun_np","lgcode") %in% names(d))) next
    um <- d %>%
      filter(is.na(lgcode)) %>%
      group_by(district_np, mun_np) %>%
      summarise(n_rows = n(),
                source_files = paste(unique(basename(f)), collapse = ";"),
                .groups = "drop")
    if (nrow(um)) collected[[basename(f)]] <- um
  }

  if (!length(collected)) {
    message("No unmatched rows in any cleaned output - all good!")
    if (file.exists(OUT_PATH)) file.remove(OUT_PATH)
    return(invisible(NULL))
  }

  # Aggregate across outputs
  all_um <- bind_rows(collected) %>%
    group_by(district_np, mun_np) %>%
    summarise(n_rows = sum(n_rows),
              source_files = paste(unique(unlist(strsplit(source_files, ";"))),
                                   collapse = ";"),
              .groups = "drop") %>%
    arrange(desc(n_rows))

  all_um$district_key <- normalize_np(all_um$district_np)
  all_um$mun_key      <- normalize_np(all_um$mun_np)

  # Suggest closest lookup match per unmatched row
  all_um$suggested_lgcode      <- NA_character_
  all_um$suggested_district_np <- NA_character_
  all_um$suggested_mun_np      <- NA_character_
  all_um$fuzzy_distance        <- NA_integer_

  for (i in seq_len(nrow(all_um))) {
    dk <- all_um$district_key[i]; mk <- all_um$mun_key[i]
    cand <- lookup %>% filter(district_key == dk)
    # If district unknown, search the full lookup
    if (!nrow(cand)) cand <- lookup
    if (!nrow(cand)) next
    if (has_sd) {
      d <- stringdist::stringdist(mk, cand$mun_key, method = "osa")
      j <- which.min(d)
      all_um$suggested_lgcode[i]      <- cand$lgcode[j]
      all_um$suggested_district_np[i] <- cand$district_np[j]
      all_um$suggested_mun_np[i]      <- cand$mun_np[j]
      all_um$fuzzy_distance[i]        <- d[j]
    }
  }

  out <- all_um %>%
    select(district_np, mun_np, n_rows, source_files,
           suggested_lgcode, suggested_district_np, suggested_mun_np,
           fuzzy_distance)

  if (!dir.exists(dirname(OUT_PATH))) dir.create(dirname(OUT_PATH),
                                                 recursive = TRUE)
  write.csv(out, OUT_PATH, row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("%d unmatched (district, mun) pairs across %d outputs",
                  nrow(out), length(collected)))
  message("Wrote: ", OUT_PATH)
  message("Review it -- for confirmed matches, append rows to ",
          LOOKUP_FILE, " mapping the data spelling to the suggested lgcode.")
}

main()
