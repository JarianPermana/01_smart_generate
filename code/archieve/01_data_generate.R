
# BIODIVERSITY DATA PIPELINE - KALIMANTAN BARAT
# Fauna & Flora International

# Struktur script ini:
#   1. Setup & Instalasi Paket
#   2. Konfigurasi Path
#   3. Fungsi Pembantu (Helper Functions)
#   4. Import & Pembersihan Data per Taksa
#   5. Kompilasi Data Deteksi
#   6. Penentuan Status RTE Spesies
#   7. Analisis Keanekaragaman (Diversity)
#   8. Ekspor Output (CSV, RData, SHP, GeoPackage)




# 1. SETUP & INSTALASI PAKET-------


rm(list = ls())
Sys.Date()
Sys.timezone()
# CLEAN LIST - Hapus package yang bermasalah
packages_needed <- c(
  # Data wrangling
  "dplyr", "tidyr", "tidyverse", "readr", "data.table",
  "lubridate", "readxl", "purrr", "stringr", "hms",
  # Spasial
  "sp", "sf", "mapview", # ("maptools",) install ini jika perlu untuk spasial visual
  # Visualisasi
  "ggplot2", "ggrepel", "patchwork", "tidyquant",
  # Utilitas
  "here", "fs", "rstudioapi", "stats", "rsconnect", "renv",
  # Keanekaragaman & Ekologi
  "vegan", "BiodiversityR", "moments", "qqplotr",
  "BIOMASS", "Distance",
  # Konservasi & Taksonomi
  "rredlist", "rcites", "taxize"
)

pk_to_install <- packages_needed[!(packages_needed %in% rownames(installed.packages()))]
if (length(pk_to_install) > 0) {
  install.packages(pk_to_install, repos = "http://cran.r-project.org")
}

lapply(packages_needed, require, character.only = TRUE)

# 2. KONFIGURASI PATH --------


base_path   <- "D:/0_Learning_DataR"
analysis    <- file.path(base_path, "2_Analysis")
nationaldb  <- file.path(base_path, "1_Database")
output      <- file.path(analysis, "01_Data_generate/output")
data_folder <- file.path(analysis, "01_Data_generate/data")

# Subfolder output
output_link   <- file.path(output, "Link_file_info")
output_temp   <- file.path(output, "tempfile")
output_img    <- file.path(output, "images")
output_tabular<- file.path(output, "tabular")
spatial_out   <- file.path(output, "spatial")
spatial_std   <- file.path(spatial_out, "standardized")
spatial_log   <- file.path(spatial_out, "logs")

# Buat semua subfolder sekaligus
for (d in c(output_link, output_temp, output_img, output_tabular, spatial_std, spatial_log)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Source fungsi eksternal
source(file.path(analysis, "01_Data_generate/code/source/2_lib_function_generatingdata.R"))


# 3. Generating Data Categorize-------------

# Kumpulkan semua file biodiversitas dari folder nasional
categories_all <- c("SMART") # c("Biodiversity Survey", "Species Target Orangutans", "SMART", "Camera Trap")
all_files_bio      <- map(categories_all, ~excel_files(file.path(nationaldb, .x))) %>% flatten_chr()
print(all_files_bio)

