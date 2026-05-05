#!/usr/bin/env Rscript
# Supreme Court of Nepal - Daily Cause List sampler
#
# Two-stage scrape per BS date:
#   1) submit (year, month, day) to the daily_public form  -> bench summary
#   2) for each bench row, follow its link                 -> case detail
#
# Saves raw HTML + parsed CSV so we can diff structure across years.
#
# Run:
#   Rscript scripts/scrape_scnp_sample.R inspect           # show form fields
#   Rscript scripts/scrape_scnp_sample.R one 2083 1 22     # one date end-to-end
#   Rscript scripts/scrape_scnp_sample.R sample            # the 20-date sample

suppressPackageStartupMessages({
  need <- c("httr2", "rvest", "xml2", "stringr", "dplyr", "purrr", "readr", "tibble")
  miss <- need[!need %in% rownames(installed.packages())]
  if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
  lapply(need, library, character.only = TRUE)
})

BASE      <- "https://supremecourt.gov.np"
FORM_URL  <- paste0(BASE, "/lic/sys.php?d=reports&f=daily_public")
OUT_ROOT  <- "samples"
UA        <- "Mozilla/5.0 (X11; Linux x86_64) scnp-research/0.1 (+contact: you@example.com)"
SLEEP_SEC <- 1.5   # be polite

dir.create(OUT_ROOT, showWarnings = FALSE, recursive = TRUE)

# ---- core HTTP -------------------------------------------------------------

new_session <- function() {
  request(BASE) |>
    req_user_agent(UA) |>
    req_headers(
      "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.7,ne;q=0.6"
    ) |>
    req_retry(max_tries = 4, backoff = ~ 2^.x)
}

get_html <- function(url, query = NULL) {
  r <- new_session() |> req_url(url)
  if (!is.null(query)) r <- r |> req_url_query(!!!query)
  resp <- req_perform(r)
  Sys.sleep(SLEEP_SEC)
  resp_body_html(resp)
}

post_html <- function(url, body) {
  resp <- new_session() |>
    req_url(url) |>
    req_body_form(!!!body) |>
    req_perform()
  Sys.sleep(SLEEP_SEC)
  resp_body_html(resp)
}

# ---- step 0: inspect the form so we know real field names ------------------

inspect_form <- function() {
  page <- get_html(FORM_URL)
  forms <- html_elements(page, "form")
  cat("Found", length(forms), "form(s)\n\n")
  for (i in seq_along(forms)) {
    f <- forms[[i]]
    cat(sprintf("FORM %d: action=%s  method=%s\n",
                i,
                html_attr(f, "action") %||% "(none)",
                html_attr(f, "method") %||% "GET"))
    inputs <- html_elements(f, "input,select,textarea,button")
    for (inp in inputs) {
      cat(sprintf("  <%s name=%s type=%s value=%s>\n",
                  html_name(inp),
                  html_attr(inp, "name")  %||% "",
                  html_attr(inp, "type")  %||% "",
                  html_attr(inp, "value") %||% ""))
    }
    cat("\n")
  }
  # also dump full form HTML for me to see
  writeLines(as.character(forms), file.path(OUT_ROOT, "_form.html"))
  cat("Wrote", file.path(OUT_ROOT, "_form.html"), "\n")
}
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || identical(a, "")) b else a

# ---- step 1: submit a date -> summary page ---------------------------------
# NOTE: field names are guesses (year/month/day). After running `inspect`,
# edit FIELD_YEAR / FIELD_MONTH / FIELD_DAY below to the real ones.

FIELD_YEAR  <- "year"
FIELD_MONTH <- "month"
FIELD_DAY   <- "day"
SUBMIT_METHOD <- "GET"   # change to "POST" if inspect shows method=post

fetch_summary <- function(y, m, d) {
  body <- setNames(
    list(sprintf("%04d", y), sprintf("%02d", m), sprintf("%02d", d)),
    c(FIELD_YEAR, FIELD_MONTH, FIELD_DAY)
  )
  if (toupper(SUBMIT_METHOD) == "POST") {
    post_html(FORM_URL, body)
  } else {
    get_html(FORM_URL, body)
  }
}

# ---- step 2: parse summary into bench rows + drilldown links ---------------

parse_summary <- function(page, base_url = FORM_URL) {
  tbls <- html_elements(page, "table")
  if (!length(tbls)) return(tibble())
  # heuristic: the summary table is the one whose header row mentions न्यायाधीश
  pick <- which(sapply(tbls, function(t) {
    h <- paste(html_text2(html_elements(t, "th,tr:first-child td")), collapse = " ")
    grepl("न्यायाधीश|पेसी|फैसला", h)
  }))
  if (!length(pick)) return(tibble())
  tbl <- tbls[[pick[1]]]

  # extract every row, capturing any <a href> in the row
  rows <- html_elements(tbl, "tr")
  out <- map_dfr(rows, function(tr) {
    cells <- html_elements(tr, "td")
    if (!length(cells)) return(NULL)
    txt   <- html_text2(cells)
    links <- html_attr(html_elements(tr, "a"), "href")
    tibble(
      cells = list(txt),
      n_cells = length(txt),
      first_link = if (length(links)) links[1] else NA_character_,
      all_links  = list(links)
    )
  })

  # also try to widen into named columns when shape is consistent
  ncol_mode <- as.integer(names(sort(table(out$n_cells), decreasing = TRUE))[1])
  wide <- out |>
    filter(n_cells == ncol_mode) |>
    mutate(row_id = row_number()) |>
    tidyr::unnest_wider(cells, names_sep = "_c")

  list(rows = out, wide = wide, ncols = ncol_mode)
}

absolutize <- function(href, base = FORM_URL) {
  if (is.na(href) || !nzchar(href)) return(NA_character_)
  xml2::url_absolute(href, base)
}

# ---- step 3: fetch bench detail page ---------------------------------------

fetch_bench <- function(href) {
  url <- absolutize(href)
  if (is.na(url)) return(NULL)
  get_html(url)
}

parse_bench <- function(page) {
  tbls <- html_elements(page, "table")
  if (!length(tbls)) return(tibble())
  # take the largest table -- bench detail is usually the biggest
  sizes <- sapply(tbls, function(t) length(html_elements(t, "tr")))
  tbl <- tbls[[which.max(sizes)]]
  tryCatch(html_table(tbl, fill = TRUE), error = function(e) tibble())
}

# ---- end-to-end for one date ----------------------------------------------

scrape_day <- function(y, m, d) {
  tag <- sprintf("%04d-%02d-%02d", y, m, d)
  daydir <- file.path(OUT_ROOT, tag)
  dir.create(daydir, showWarnings = FALSE, recursive = TRUE)

  message("[", tag, "] summary…")
  summ <- tryCatch(fetch_summary(y, m, d), error = function(e) { message("  summary FAIL: ", e$message); NULL })
  if (is.null(summ)) return(invisible(NULL))
  writeLines(as.character(summ), file.path(daydir, "summary.html"))

  parsed <- parse_summary(summ)
  saveRDS(parsed, file.path(daydir, "summary_parsed.rds"))
  if (nrow(parsed$wide)) write_csv(parsed$wide |> select(-all_links), file.path(daydir, "summary.csv"))

  links <- na.omit(unique(parsed$rows$first_link))
  message("  benches: ", length(links))

  for (i in seq_along(links)) {
    href <- links[i]
    safe <- str_replace_all(href, "[^A-Za-z0-9]+", "_") |> substr(1, 80)
    fp_html <- file.path(daydir, sprintf("bench_%02d_%s.html", i, safe))
    fp_csv  <- file.path(daydir, sprintf("bench_%02d_%s.csv",  i, safe))
    page <- tryCatch(fetch_bench(href), error = function(e) { message("  bench ", i, " FAIL: ", e$message); NULL })
    if (is.null(page)) next
    writeLines(as.character(page), fp_html)
    df <- parse_bench(page)
    if (is.data.frame(df) && nrow(df)) write_csv(df, fp_csv)
  }
  invisible(tag)
}

# ---- 20-date sample across years ------------------------------------------
# BS dates only; weekdays-ish (Sun..Fri). Adjust freely.
SAMPLE_DATES <- tibble::tribble(
  ~y,   ~m, ~d,
  2060,  4, 15,
  2062,  6, 10,
  2064,  2, 20,
  2066,  9,  5,
  2068,  3, 18,
  2069, 11, 12,
  2070,  7,  9,
  2071,  1, 25,
  2072,  5, 14,
  2073,  1, 22,
  2074,  8,  6,
  2075,  3, 11,
  2076,  6, 19,
  2077, 10,  3,
  2078,  2, 28,
  2079,  4,  7,
  2080,  9, 15,
  2081,  1, 10,
  2082,  6, 21,
  2083,  1, 22
)

run_sample <- function() {
  for (i in seq_len(nrow(SAMPLE_DATES))) {
    with(SAMPLE_DATES[i, ], scrape_day(y, m, d))
  }
}

# ---- CLI dispatch ---------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("usage: Rscript scrape_scnp_sample.R [inspect | one Y M D | sample]\n")
} else if (args[1] == "inspect") {
  inspect_form()
} else if (args[1] == "one" && length(args) == 4) {
  scrape_day(as.integer(args[2]), as.integer(args[3]), as.integer(args[4]))
} else if (args[1] == "sample") {
  run_sample()
} else {
  stop("unknown args: ", paste(args, collapse = " "))
}
