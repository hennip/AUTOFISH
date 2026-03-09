# ----------------
# ICES Biotic (flat CSV with section headers) - Batch parse and attach SurveyYear
# ----------------

# Packages
suppressPackageStartupMessages({
  library(readr)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
})

# ===== CONFIG =====
input_dir <- path  # <-- set this

# ===== HELPERS =====



# Return all "flat biotic" candidate files (.csv/.txt) that contain at least one section header
find_biotic_files <- function(dir) {
  cand <- list.files(dir, pattern = "(?i)\\.(csv|txt)$", full.names = TRUE, recursive = FALSE)
  keep(cand, function(fp) {
    # Read a small chunk to detect headers
    # (avoid reading whole files to keep this fast)
    lines <- tryCatch(readr::read_lines(fp, n_max = 300, locale = locale(encoding = "UTF-8")), error = function(e) character())
    any(str_detect(lines, "^(Cruise|Haul|Catch|Biology),Header"))
  })
}


# Split sections (Cruise/Haul/Catch/Biology) using line-based pattern, à la your snippet
split_sections <- function(lines) {
  # normalize line endings and trim
  lines <- str_replace_all(lines, "\r", "")
  lines <- str_trim(lines)
  
  hdr_idx <- which(str_detect(lines, "^(Cruise|Haul|Catch|Biology),Header"))
  if (!length(hdr_idx)) {
    stop("No section headers found. File may not be an ICES Biotic flat export (e.g., 'Haul,Header,...').")
  }
  
  # Parse each header line to get section name and column names
  hdrs <- lapply(hdr_idx, function(i) {
    parts <- str_split(lines[i], ",", simplify = TRUE)
    section <- parts[1]
    # columns start from field 3 onward
    cols <- parts[3:ncol(parts)]
    list(section = section, cols = cols)
  })
  
  headers <- setNames(lapply(hdrs, `[[`, "cols"), sapply(hdrs, `[[`, "section"))
  
  # Helper to split a CSV record line into fields with simple comma split
  # (This matches your approach; if quoted commas exist, a more complex parser would be needed)
  split_record <- function(line) str_split(line, ",", simplify = TRUE)
  
  # Build each section's data frame of 'Record' lines
  recs <- lapply(names(headers), function(sec) {
    idx <- which(str_detect(lines, paste0("^", sec, ",Record")))
    cols <- headers[[sec]]
    
    if (!length(idx)) {
      out <- as.data.frame(matrix(nrow = 0, ncol = length(cols)))
      colnames(out) <- cols
      return(out)
    }
    
    # Extract fields after the first 2 tokens (section + 'Record')
    rows <- lapply(idx, function(i) {
      parts <- split_record(lines[i])
      # drop first two tokens
      vals <- parts[3:length(parts)]
      # pad or truncate to match header length
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

# Extract YEAR in priority order: Cruise date cols -> Haul date cols -> filename
extract_survey_year <- function(sec_list, source_file) {
  # Try Cruise section first
  find_year_in_df <- function(df) {
    if (is.null(df) || !nrow(df)) return(NA_integer_)
    # Candidate columns that often carry date/year-like content
    cand_cols <- names(df)[str_detect(names(df), "(?i)(date|time|year)")]
    if (!length(cand_cols)) cand_cols <- names(df)
    
    # Look for a 4-digit year 19xx or 20xx in values
    vals <- df[, cand_cols, drop = FALSE] %>% unlist(use.names = FALSE) %>% as.character()
    yr <- str_extract(vals, "(19|20)\\d{2}")
    yr <- yr[!is.na(yr)]
    if (length(yr)) return(as.integer(yr[1])) else return(NA_integer_)
  }
  
  yr <- NA_integer_
  
  if (!is.null(sec_list$Cruise)) {
    yr <- find_year_in_df(sec_list$Cruise)
    if (!is.na(yr)) return(yr)
  }
  
  if (!is.null(sec_list$Haul)) {
    yr <- find_year_in_df(sec_list$Haul)
    if (!is.na(yr)) return(yr)
  }
  
  # Fallback: infer from filename
  bn <- basename(source_file)
  yr <- str_extract(bn, "(19|20)\\d{2}")
  if (!is.na(yr)) return(as.integer(yr))
  
  NA_integer_
}

# Convert empty strings to NA for cleanliness
empty_to_na <- function(df) {
  df %>% mutate(across(everything(), ~ ifelse(. %in% c("", "NA", "NaN"), NA, .)))
}

# Gracefully parse one file into sections + attach SurveyYear and SourceFile
process_biotic_file <- function(fp) {
  lines <- readr::read_lines(fp, locale = locale(encoding = "UTF-8"))
  sec <- tryCatch(split_sections(lines), error = function(e) {
    warning("Skipping (no parseable sections): ", fp, " | ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(sec)) return(NULL)
  
  required_sections <- c("Haul", "Catch", "Biology")
  missing_sections <- setdiff(required_sections, names(sec))
  if (length(missing_sections)) {
    warning("File missing sections [", paste(missing_sections, collapse = ", "), "]: ", fp)
  }
  
  survey_year <- extract_survey_year(sec, fp)
  
  add_meta <- function(df) {
    if (is.null(df)) return(NULL)
    if (!nrow(df)) return(NULL)
    df <- df %>% empty_to_na()
    df <- df %>% mutate(SurveyYear = survey_year, SourceFile = basename(fp), .before = 1)
    tibble::as_tibble(df)
  }
  
  list(
    Haul    = add_meta(sec$Haul),
    Catch   = add_meta(sec$Catch),
    Biology = add_meta(sec$Biology),
    Cruise  = add_meta(sec$Cruise)  # optional; can be useful for QA
  )
}

# ===== RUN =====

files <- find_biotic_files(input_dir)

if (!length(files)) {
  stop("No ICES Biotic 'flat' files found in: ", input_dir)
} else {
  message("Found ", length(files), " candidate file(s).")
}

parsed <- lapply(files, process_biotic_file) %>% compact()

# Bind across files for each section (only non-empty)
hauls_all <- parsed %>% map("Haul")    %>% compact() %>% list_rbind()
catch_all <- parsed %>% map("Catch")   %>% compact() %>% list_rbind()
bio_all   <- parsed %>% map("Biology") %>% compact() %>% list_rbind()
cruise_all<- parsed %>% map("Cruise")  %>% compact() %>% list_rbind()  # optional

hauls_all
