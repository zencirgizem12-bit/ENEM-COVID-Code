# --------------------------------------
# 01_process_raw.R
# Process raw ENEM data to municipality-level aggregates
# Years: 2017, 2018, 2019, 2021 (2020 excluded)
# --------------------------------------

library(data.table)

# Configuration
YEARS <- c(2017, 2018, 2019, 2021)
RAW_PATH <- "data/raw"
OUT_PATH <- "data/processed"

# Function to process a single year
process_year <- function(year) {
  
  cat("\n========================================\n")
  cat("Processing year:", year, "\n")
  cat("========================================\n")
  
  # Raw data folder
  raw_dir <- file.path(RAW_PATH, paste0("microdados_enem_", year), "DADOS")
  
  if(!dir.exists(raw_dir)) {
    cat("❌ Folder not found:", raw_dir, "\n")
    return(NULL)
  }
  
  # Find CSV files
  csv_files <- list.files(raw_dir, pattern = ".csv$", full.names = TRUE)
  
  if(length(csv_files) == 0) {
    cat("❌ No CSV file found\n")
    return(NULL)
  }
  
  cat("CSV files found:", length(csv_files), "\n")
  
  # Find the main data file (largest file)
  file_sizes <- file.size(csv_files)
  main_file <- csv_files[which.max(file_sizes)]
  
  cat("Main file:", basename(main_file), "\n")
  
  # Encoding (Latin-1 for 2017-2019, UTF-8 for 2020+)
  encoding <- ifelse(year <= 2019, "Latin-1", "UTF-8")
  
  # Required columns
  cols <- c(
    "CO_MUNICIPIO_PROVA", "NU_NOTA_MT", "NU_NOTA_CN", "NU_NOTA_CH", 
    "NU_NOTA_LC", "NU_NOTA_REDACAO", "TP_SEXO", "NU_IDADE", "SG_UF_PROVA",
    "TP_DEPENDENCIA_ADM_ESC", "TP_LOCALIZACAO_ESC",
    "Q001", "Q002", "Q003", "Q004", "Q005", "Q006", "Q024", "Q025",
    "TP_PRESENCA_MT", "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC"
  )
  
  # Read data
  data <- fread(main_file, select = cols, encoding = encoding, showProgress = TRUE)
  
  # Keep only students who took all exams
  data <- data[TP_PRESENCA_MT == 1 & TP_PRESENCA_CN == 1 & 
               TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1]
  
  cat("Students after filtering:", format(nrow(data), big.mark = ","), "\n")
  
  # Internet quality (0 = none, 1 = mobile only, 2 = computer + internet)
  data[, internet_quality := fcase(
    is.na(Q025) | Q025 != "A", 0,
    Q025 == "A" & (is.na(Q024) | Q024 != "A"), 1,
    Q025 == "A" & Q024 == "A", 2,
    default = NA_real_
  )]
  
  # Aggregate to municipality level
  city_data <- data[, .(
    n_students = .N,
    math_mean = mean(NU_NOTA_MT, na.rm = TRUE),
    science_mean = mean(NU_NOTA_CN, na.rm = TRUE),
    human_mean = mean(NU_NOTA_CH, na.rm = TRUE),
    language_mean = mean(NU_NOTA_LC, na.rm = TRUE),
    essay_mean = mean(NU_NOTA_REDACAO, na.rm = TRUE),
    pct_no_internet = mean(internet_quality == 0, na.rm = TRUE),
    pct_mobile_internet = mean(internet_quality == 1, na.rm = TRUE),
    pct_full_remote = mean(internet_quality == 2, na.rm = TRUE),
    internet_quality_mean = mean(internet_quality, na.rm = TRUE),
    income_median = median(as.numeric(as.factor(Q006)), na.rm = TRUE),
    pct_urban = mean(TP_LOCALIZACAO_ESC == 1, na.rm = TRUE),
    school_type_private = mean(TP_DEPENDENCIA_ADM_ESC == 4, na.rm = TRUE),
    state = first(SG_UF_PROVA)
  ), by = .(city_code = CO_MUNICIPIO_PROVA)]
  
  city_data[, year := year]
  
  # Save
  out_file <- file.path(OUT_PATH, paste0("city_panel_", year, ".rds"))
  saveRDS(city_data, out_file)
  cat("✅ Saved:", basename(out_file), "-", nrow(city_data), "municipalities\n")
  
  return(city_data)
}

# Process all years
all_data <- list()
for(y in YEARS) {
  all_data[[as.character(y)]] <- process_year(y)
}

# Combine
city_panel <- rbindlist(all_data[!sapply(all_data, is.null)], fill = TRUE)
saveRDS(city_panel, file.path(OUT_PATH, "city_panel_2017_2021.rds"))

cat("\n✅ Panel created:", nrow(city_panel), "observations\n")
