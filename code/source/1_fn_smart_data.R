# Helper Function ---------
packages_needed <- c("dplyr", "tidyr","sp","sf","tidyverse","data.table","utils", "lubridate",
                     "readr", "ggplot2", "here", "fs", "readxl", "purrr",
                     "stats", "renv", "janitor", "hms")

pk_to_install <- packages_needed[!(packages_needed %in% rownames(installed.packages()))]
if(length(pk_to_install)>0 ){
  install.packages(pk_to_install, repos="http://cran.r-project.org")
}

for (package in packages_needed) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = FALSE)
  }
}

lapply(packages_needed, require, character.only = TRUE)


# Clean Patrols Summary Function ---------
clean_patrol_summary <- function(file_path, landscape) {
  # Cek apakah file exists
  if(!file.exists(file_path)) {
    stop("File tidak ditemukan: ", file_path)
  }
  
  # Baca shapefile
  tryCatch({
    data_sf <- st_read(file_path, quiet = TRUE)
  }, error = function(e) {
    stop("Gagal membaca shapefile: ", e$message)
  })
  
  # Cek kolom yang diperlukan
  required_cols <- c("Patrol_L_1", "Patrol_L_2", "Armed", "Patrol_Leg")
  available_cols <- required_cols[required_cols %in% names(data_sf)]
  
  if(length(available_cols) == 0) {
    warning("Tidak ada kolom yang di-drop. Mungkin nama kolom berbeda?")
    # Tampilkan nama kolom yang tersedia
    cat("Kolom yang tersedia:\n")
    print(names(data_sf))
  }
  
  # Proses data
  result <- data_sf %>%
    mutate(
      landscape = landscape,
      distance_m = round(st_length(.),2),  # distance dalam meter
      distance_km = round(as.numeric(distance_m / 1000),2)  # konversi ke kilometer
    )
  
  # Drop kolom jika ada
  cols_to_drop <- intersect(c("Patrol_L_1", "Patrol_L_2", "Armed", "Patrol_Leg"), names(result))
  if(length(cols_to_drop) > 0) {
    result <- result %>% select(-all_of(cols_to_drop))
  }
  
  # Konversi ke data.frame
  result <- result %>%
    as.data.frame() %>%
    select(-geometry)
  
  # Ubah nama kolom menjadi lowercase
  names(result) <- tolower(names(result))
  
  return(result)
}


# Distribute Path Folder Function ---------
## a. Patrol summaries function ---------
summary_patrol_data <- function(patrol.summary, nationaldb_smart, tabular_type = "tabular") {
  # Group by region, site and year
  grouped_data <- patrol.summary %>% 
    group_by(region, site, session)
  
  # Making combination region, site and session
  combinations <- grouped_data %>% 
    group_keys()
  
  # Loop for all combinations
  for (i in 1:nrow(combinations)) {
    current_region <- combinations$region[i]
    current_site <- combinations$site[i]
    current_session <- combinations$session[i]
    
    # Make folder path - PERBAIKAN
    region_dir <- file.path(nationaldb_smart, current_region)
    site_dir <- file.path(region_dir, current_site)
    session_dir <- file.path(site_dir, as.character(current_session))
    tabular_dir <- file.path(session_dir, tabular_type)  # HAPUS current_tabular
    
    # Make folder based site and year
    if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)
    if (!dir.exists(site_dir)) dir.create(site_dir, recursive = TRUE)
    if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
    if (!dir.exists(tabular_dir)) dir.create(tabular_dir, recursive = TRUE)
    
    # Filter data
    filtered_data <- grouped_data %>% 
      filter(region == current_region,
             site == current_site, 
             session == current_session) %>%
      ungroup()
    
    # Define filename - PERBAIKAN dengan placeholder
    filename <- sprintf("Ringkasan Patroli.csv", 
                        current_region, current_site, current_session)
    
    # Save data
    write.csv(filtered_data, file.path(tabular_dir, filename), row.names = FALSE)
    
    cat("The files are stored in:", file.path(tabular_dir, filename), "\n")
  }
}

## b. Fungsi untuk Ancaman Patroli -----------
threats_patrol_data <- function(patrol.threats, nationaldb_smart, tabular_type = "tabular") {
  # Group by region, site dan session
  grouped_data <- patrol.threats %>% 
    group_by(region, site, session)
  
  # Dapatkan kombinasi unik region, site dan session
  combinations <- grouped_data %>% 
    group_keys()
  
  # Loop for all combinations
  for (i in 1:nrow(combinations)) {
    current_region <- combinations$region[i]
    current_site <- combinations$site[i]
    current_session <- combinations$session[i]
    
    # Make folder path - PERBAIKAN
    region_dir <- file.path(nationaldb_smart, current_region)
    site_dir <- file.path(region_dir, current_site)
    session_dir <- file.path(site_dir, as.character(current_session))
    tabular_dir <- file.path(session_dir, tabular_type)  # HAPUS current_tabular
    
    # Make folder based site and year
    if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)
    if (!dir.exists(site_dir)) dir.create(site_dir, recursive = TRUE)
    if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
    if (!dir.exists(tabular_dir)) dir.create(tabular_dir, recursive = TRUE)
    
    # Filter data
    filtered_data <- grouped_data %>% 
      filter(region == current_region,
             site == current_site, 
             session == current_session) %>%
      ungroup()
    
    # Define filename - PERBAIKAN
    filename <- sprintf("Temuan Aktivitas Manusia.csv", 
                        current_region, current_site, current_session)
    
    # Save data
    write.csv(filtered_data, file.path(tabular_dir, filename), row.names = FALSE)
    
    cat("The files are stored in:", file.path(tabular_dir, filename), "\n")
  }
}

## c. Fungsi untuk Hidupan Liar ---------
ff_patrol_data <- function(patrol.fauna.flora, nationaldb_smart, tabular_type = "tabular") {
  # Group by region, site and session
  grouped_data <- patrol.fauna.flora %>% 
    group_by(region, site, session)
  combinations <- grouped_data %>% 
    group_keys()
  
  # Loop for all combinations
  for (i in 1:nrow(combinations)) {
    current_region <- combinations$region[i]
    current_site <- combinations$site[i]
    current_session <- combinations$session[i]
    
    # Make folder path - PERBAIKAN
    region_dir <- file.path(nationaldb_smart, current_region)
    site_dir <- file.path(region_dir, current_site)
    session_dir <- file.path(site_dir, as.character(current_session))
    tabular_dir <- file.path(session_dir, tabular_type)  # HAPUS current_tabular
    
    # Make folder based site and year
    if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)
    if (!dir.exists(site_dir)) dir.create(site_dir, recursive = TRUE)
    if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
    if (!dir.exists(tabular_dir)) dir.create(tabular_dir, recursive = TRUE)
    
    # Filter data
    filtered_data <- grouped_data %>% 
      filter(region == current_region,
             site == current_site, 
             session == current_session) %>%
      ungroup()
    
    # Define filename - PERBAIKAN
    filename <- sprintf("Temuan Hidupan Liar.csv", 
                        current_region, current_site, current_session)
    
    # Save data
    write.csv(filtered_data, file.path(tabular_dir, filename), row.names = FALSE)
    
    cat("The files are stored in:", file.path(tabular_dir, filename), "\n")
  }
}

