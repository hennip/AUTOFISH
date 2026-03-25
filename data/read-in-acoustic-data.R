# ----------------
# ICES Acoustic (flat CSV with section headers) - Build one joint tibble (Data + SurveyYear)
# Only files starting with "Acoustic"
# ----------------

# ===== CONFIG =====
input_dir <- pathA  # <-- set this folder

# ===== HELPERS =====

# Find candidate files: basenames start with "Acoustic" and contain "Data,Header"
find_acoustic_files <- function(dir) {
  cand <- list.files(
    dir,
    pattern = "(?i)^Acoustic.*\\.(csv|txt)$",
    full.names = TRUE,
    recursive = FALSE
  )
  keep(cand, function(fp) {
    lines <- tryCatch(
      readr::read_lines(fp, n_max = 300, locale = locale(encoding = "UTF-8")),
      error = function(e) character()
    )
    any(str_detect(lines, "^Data,Header"))
  })
}

# Split all present sections by the "Section,Header/Record" pattern
split_sections <- function(lines) {
  lines <- str_replace_all(lines, "\r", "")
  lines <- str_trim(lines)
  
  hdr_idx <- which(str_detect(
    lines,
    "^(Instrument|Calibration|DataAcquisition|DataProcessing|EchoType|Cruise|Log|Data),Header"
  ))
  if (!length(hdr_idx)) {
    stop("No section headers found. Not an ICES Acoustic flat export?")
  }
  
  hdrs <- lapply(hdr_idx, function(i) {
    parts   <- str_split(lines[i], ",", simplify = TRUE)
    section <- parts[1]
    cols    <- parts[3:ncol(parts)]
    list(section = section, cols = cols)
  })
  
  headers <- setNames(lapply(hdrs, `[[`, "cols"), sapply(hdrs, `[[`, "section"))
  
  split_record <- function(line) str_split(line, ",", simplify = TRUE)
  
  recs <- lapply(names(headers), function(sec) {
    idx  <- which(str_detect(lines, paste0("^", sec, ",Record")))
    cols <- headers[[sec]]
    
    if (!length(idx)) {
      out <- as.data.frame(matrix(nrow = 0, ncol = length(cols)))
      colnames(out) <- cols
      return(out)
    }
    
    rows <- lapply(idx, function(i) {
      parts <- split_record(lines[i])
      vals  <- parts[3:length(parts)]  # drop 'Section,Record'
      # pad/truncate to header length
      if (length(vals) < length(cols)) {
        vals <- c(vals, rep(NA_character_, length(cols) - length(vals)))
      } else if (length(vals) > length(cols)) {
        vals <- vals[seq_along(cols)]
      }
      vals
    })
    
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    colnames(df) <- cols
    df
  })
  
  names(recs) <- names(headers)
  recs
}

# Extract SurveyYear (priority: CruiseStartDate -> LogTime -> filename)
extract_survey_year <- function(sec_list, source_file) {
  extract_year <- function(x) {
    y <- str_extract(x, "(19|20)\\d{2}")
    suppressWarnings(as.integer(y))
  }
  
  # a) Prefer CruiseStartDate (or similar) in Cruise section
  if (!is.null(sec_list$Cruise) && nrow(sec_list$Cruise)) {
    cols <- names(sec_list$Cruise)
    cand <- cols[str_detect(cols, "(?i)CruiseStartDate|StartDate|Start")]
    if (!length(cand)) cand <- cols
    vals <- sec_list$Cruise[, cand, drop = FALSE] %>% unlist(use.names = FALSE) %>% as.character()
    yrs  <- vals %>% map_int(~ extract_year(.x))
    yrs  <- yrs[!is.na(yrs)]
    if (length(yrs)) return(yrs[1])
  }
  
  # b) Otherwise use LogTime (or any date/time-like) in Data section
  if (!is.null(sec_list$Data) && nrow(sec_list$Data)) {
    dtcol <- names(sec_list$Data)[str_detect(names(sec_list$Data), "(?i)LogTime|Time|Date")]
    if (length(dtcol)) {
      vals <- sec_list$Data[[dtcol[1]]] %>% as.character()
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals)) {
        yr <- extract_year(vals[1])
        if (!is.na(yr)) return(yr)
      }
    }
  }
  
  # c) Fallback: year in filename (prefer after "Acoustic_")
  bn <- basename(source_file)
  y1 <- str_match(bn, "(?i)Acoustic_-\\d{2})")[,2]
  if (!is.na(y1)) return(as.integer(y1))
  y2 <- str_extract(bn, "(19|20)\\d{2}")
  if (!is.na(y2)) return(as.integer(y2))
  
  NA_integer_
}

# Clean empty placeholders to NA
empty_to_na <- function(df) {
  if (!nrow(df)) return(as_tibble(df))
  df %>% mutate(across(everything(), ~ ifelse(. %in% c("", "NA", "NaN"), NA, .)))
}

# Parse one file -> tibble of Data with SurveyYear (no other returns)
process_acoustic_file_to_data <- function(fp) {
  lines <- readr::read_lines(fp, locale = locale(encoding = "UTF-8"))
  
  sec <- tryCatch(split_sections(lines), error = function(e) {
    warning("Skipping (no parseable sections): ", fp, " | ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(sec) || is.null(sec$Data)) return(NULL)
  
  year <- extract_survey_year(sec, fp)
  
  sec$Data %>%
    as_tibble() %>%
    empty_to_na() %>%
    mutate(SurveyYear = year, .before = 1) %>%
    # If you also want to keep source filename, uncomment next line:
    # mutate(SourceFile = basename(fp), .after = SurveyYear) %>%
    identity()
}

# ===== RUN =====

files <- find_acoustic_files(input_dir)
if (!length(files)) {
  stop("No 'Acoustic*.csv|.txt' ICES Acoustic flat files found in: ", input_dir)
} else {
  message("Found ", length(files), " acoustic file(s).")
}

# One joint tibble
acoustic_data_all <- files %>%
  map(process_acoustic_file_to_data) %>%
  compact() %>%
  bind_rows()

# Quick peek
acoustic_data_all %>% glimpse()
