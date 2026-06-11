
# 0. Setup and Packages Install----------------
rm(list = ls())
packages_needed <- c("dplyr", "tidyr", "janitor","sp","sf","tidyverse",
                     "data.table","utils", "lubridate",
                     "readr", "mapview", "ggplot2","here", 
                     "fs", "readxl", "purrr", "hms",
                     "flextable")

pk_to_install <- packages_needed [!( packages_needed %in% rownames(installed.packages())  )]
if(length(pk_to_install)>0 ){
  install.packages(pk_to_install,repos="http://cran.r-project.org")
}


for (package in packages_needed) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = FALSE)
  }
}

lapply(packages_needed , require, character.only = TRUE)



# 1. Path Configuration----
## a. Main path ------
base_path        <- "D:/0_DataCentre"
analysis         <- file.path(base_path, "2_Analysis/01_smart_generate")
analysis_analysis <- file.path(base_path, "2_Analysis/02_smart_analysis")
analysis_dashboard <- file.path(base_path, "2_Analysis/03_smart_dashboard_report")
nationaldb       <- file.path(base_path, "1_Database")
nationaldb_smart <- file.path(nationaldb, "SMART")
output           <- file.path(analysis, "output")
data_folder      <- file.path(analysis, "data")


# 2. Data and Function Import ----------
source(file.path(analysis, "code/source", "1_fn_smart_data.r"))

all.observation <- read_csv(
  file.path(nationaldb, "SMART/All Observation/All_Observation.csv"),
  show_col_types = FALSE
)

glimpse(all.observation)

patrol_member_raw <- read_csv(
  file.path(nationaldb, "SMART/All Observation/Patrol_Member.csv"),
  show_col_types = FALSE, 
  skip = 1,  # Skip baris header yang kosong
  col_names = c("patrol_id", "leader", "member_name", 
                "patrol_transport_type", "number_of_patrols",
                "number_of_days", "distance_km"),
  na = c("", "NA")
) %>%
  clean_names() %>%
  rename(patrol_frequence = number_of_patrols,
         patrol_days = number_of_days)
glimpse(patrol_member_raw)



# 3. Data Cleaning ------
## 0. All observation Cleaning -----
all.observation <- all.observation %>%
  clean_names() %>%
  rename(kategori_temuan_0 = observation_category_0,
         kategori_temuan_1 = observation_category_1) %>%
  mutate(
    landscape = "Kalimantan Barat",
    patrol_start_date = parse_date_time(patrol_start_date, 
                                        orders = c("mdy", "dmy", "ymd", "my", "mdy HMS")),
    patrol_end_date = parse_date_time(patrol_end_date, 
                                      orders = c("mdy", "dmy", "ymd", "my", "mdy HMS")),
    waypoint_date = parse_date_time(waypoint_date, 
                                    orders = c("mdy", "dmy", "ymd", "my", "mdy HMS")),
    waypoint_time_final = as_hms(parse_date_time(waypoint_time, orders = c("HM", "HMS")))
  ) %>%
  select(-waypoint_time) %>% rename(waypoint_time = waypoint_time_final)



glimpse(all.observation) # check data coloumn format



## a. Patrol Summary --------
### Run with function helper ------
patrol.summary <- clean_patrol_summary(
  file_path = file.path(nationaldb, "SMART/All Observation/All_track.shp"),
  landscape = "Kalimantan Barat") 

patrol.summary <- patrol.summary %>%
  mutate(patrol_start_date = lubridate::mdy(patrol_sta),
         patrol_end_date = lubridate::mdy(patrol_end),
         patrol_days = as.numeric(patrol_end_date - patrol_start_date + 1)) %>%
  group_by(patrol_id) %>%
  reframe(
    distance_m = sum(distance_m, na.rm = TRUE),
    distance_km = sum(distance_km, na.rm = TRUE),
    patrol_days = first(patrol_days),
    patrol_start_date = first(patrol_start_date),
    patrol_end_date = first(patrol_end_date),
    across(everything(), first)
  ) %>%
  rename(patrol_transport_type = patrol_tra, 
         sumber_anggaran = sumber_ang) %>%
  mutate(site = case_when(
    station %in% c("Resort 1 Kawasan A", "Resort 2 Kawasan A") ~ "Kawasan A",
    station == "Resort 1 Kawasan B" ~ "Kawasan B" ,
    # lanjutkan jika masih ada yang belum
    TRUE ~ station)) %>%
  mutate(region = case_when(
    site %in% c("Kawasan A")  ~ "Seksi Wilayah I",
    site %in% c("Kawasan B")  ~ "Seksi Wilayah II",
    # Tambahkan seksi lainnya 
    TRUE ~ site
  )) %>%
  mutate(session = year(patrol_start_date),
         month = month(patrol_start_date),
         semester = ifelse(month %in% 1:6, 1, 2),
         quarter = case_when(
           month %in% 1:3  ~ 1,
           month %in% 4:6  ~ 2,
           month %in% 7:9  ~ 3,
           month %in% 10:12 ~ 4,
           TRUE ~ NA_integer_
         )) %>%
  select(landscape, region, site, station, patrol_id, 
         patrol_start_date, patrol_end_date, patrol_days,
         month, session, semester, quarter,type,
         patrol_transport_type, distance_m, distance_km, team,leader, pilot,
         objective, mandate, sumber_anggaran)
  
glimpse(patrol.summary)  


## b. Patrol Member -----
### Join member data dengan patrol summary ----
patrol_member_summary <- patrol_member_raw %>%
  filter(!is.na(patrol_frequence)) %>%
  dplyr::select(patrol_id, member_name) %>%
  inner_join(
    patrol.summary,
    by = "patrol_id"
  )

glimpse(patrol_member_summary)


## c. Patrol Threats ---------
patrol.threats <- all.observation %>% 
  filter(kategori_temuan_0 == "Aktivitas Manusia") %>%
  mutate(landscape = "Kalimantan Barat") %>%
  mutate(site = case_when(
    station %in% c("Resort 1 Kawasan A", "Resort 2 Kawasan A") ~ "Kawasan A",
    station == "Resort 1 Kawasan B" ~ "Kawasan B",
    # lanjutkan jika masih ada yang belum
    TRUE ~ station)) %>%
  mutate(region = case_when(
    site %in% c("Kawasan A")  ~ "Seksi Wilayah I",
    site %in% c("Kawasan B")  ~ "Seksi Wilayah II",
    # Tambahkan seksi lainnya 
    TRUE ~ site
  )) %>%
  mutate(session = year(patrol_start_date),
         month = month(patrol_start_date),
         semester = ifelse(month %in% 1:6, 1, 2),
         quarter = case_when(
           month %in% 1:3  ~ 1,
           month %in% 4:6  ~ 2,
           month %in% 7:9  ~ 3,
           month %in% 10:12 ~ 4,
           TRUE ~ NA_integer_
         )) %>%
  select(landscape, region, site, station, patrol_id, patrol_start_date, patrol_end_date,
         patrol_transport_type, waypoint_date, waypoint_time, x, y, kategori_temuan_0,
         kategori_temuan_1, tipe_temuan, jumlah, satuan, keaktifan,month, session, semester, quarter,
         keterangan)

glimpse(patrol.threats)



## d. Patrol Fauna & Flora -------
patrol.fauna.flora <- all.observation %>% 
  filter(kategori_temuan_0 == "Satwa Liar" | kategori_temuan_0 == "Tumbuhan") %>%
  mutate(
    site = case_when(
      station %in% c("Resort 1 Kawasan A", "Resort 2 Kawasan A") ~ "Kawasan A",
      station == "Resort 1 Kawasan B" ~ "Kawasan B",
      TRUE ~ station
    ),
    region = case_when(
      site %in% c("Kawasan A") ~ "Seksi Wilayah I",
      site %in% c("Kawasan B") ~ "Seksi Wilayah II",
      TRUE ~ site
    ),
    session = year(patrol_start_date),
    month = month(patrol_start_date),
    semester = ifelse(month %in% 1:6, 1, 2),
    quarter = case_when(
      month %in% 1:3  ~ 1,
      month %in% 4:6  ~ 2,
      month %in% 7:9  ~ 3,
      month %in% 10:12 ~ 4,
      TRUE ~ NA_integer_
    ),
    # --- Extraction data ---
    raw_text = coalesce(jenis_satwa, jenis_tumbuhan),# Gabungkan kedua kolom sumber
    has_dash = grepl("-", raw_text), # Cek apakah ada tanda "-"
    Local_Name_raw = str_trim(str_extract(raw_text, "^[^-]+")), # Ekstrak Local_Name: teks sebelum "-"
    Scientific_Name_raw = str_trim(str_extract(raw_text, "[^-]+$")), # Ekstrak Scientific_Name: teks setelah "-"
    Local_Name = case_when(
      !has_dash & grepl("^[A-Z][a-z]+\\s[a-z]", raw_text) ~ NA_character_, # Jika TIDAK ada dash: cek apakah teks berformat Latin
      !has_dash ~ raw_text, # Jika TIDAK ada dash & bukan Latin: anggap sebagai nama lokal
      TRUE ~ Local_Name_raw # Jika ADA dash: pakai hasil ekstraksi
    ),
    Scientific_Name = case_when(
      !has_dash & grepl("^[A-Z][a-z]+\\s[a-z]", raw_text) ~ raw_text, # Jika TIDAK ada dash tapi berformat Latin: ini nama ilmiah
      has_dash ~ Scientific_Name_raw, # Jika ADA dash: pakai hasil ekstraksi
      TRUE ~ NA_character_
    ),
    Scientific_Name = ifelse(
      is.na(Scientific_Name) | Scientific_Name == "" | Scientific_Name == Local_Name,
      NA_character_,
      Scientific_Name
    ),
    Local_Name = ifelse(
      is.na(Local_Name) | Local_Name == "", # Jika Local_Name kosong → isi dari Scientific_Name (untuk kasus hanya nama Latin)
      NA_character_,
      Local_Name
    ),
    satuan = case_when(
      kategori_temuan_0 == "Tumbuhan" ~ "individu",
      kategori_temuan_1 == "Perjumpaan Satwa" ~ "individu",
      TRUE ~ "temuan"
    ),
    tipe_temuan = case_when(
      kategori_temuan_1 == "Perjumpaan Satwa" ~ "Perjumpaan langsung",
      TRUE ~ tipe_temuan
    ),
    jumlah = case_when(
      jumlah == 0 ~ 1, 
      TRUE ~ jumlah
    )
  ) %>%
  dplyr::select(
    landscape, region, site, station, patrol_id, 
    patrol_start_date, patrol_end_date, patrol_transport_type, 
    waypoint_date, waypoint_time, x, y, 
    kategori_temuan_0, kategori_temuan_1, 
    Local_Name, Scientific_Name,
    tipe_temuan, jumlah, satuan,
    umur_satwa, kondisi_tumbuhan, usia_temuan, 
    month, session, semester, quarter,
    keterangan
  )
glimpse(patrol.fauna.flora)
unique(patrol.fauna.flora$tipe_temuan)




# 4. Distribute data to folder database ---------

## a. Patrol Summaries
summary_patrol_data(patrol.summary, nationaldb_smart)

## b. Patrol Threats ------
threats_patrol_data(patrol.threats, nationaldb_smart)

## c. Patrol Fauna-Flora-------
ff_patrol_data(patrol.fauna.flora, nationaldb_smart)



# Save RData ------
## Save in this work directory ----
save.image("D:/0_DataCentre/2_Analysis/01_smart_generate/data/smart_generate_data.RData")


## Save in folder for next analysis -------
rdata_objects <- list(
  all.observation, patrol_member_raw, patrol_member_summary,patrol.summary, patrol.summary, patrol.threats, patrol.threats
)

rdata_targets <- c(file.path(base_path, "2_Analysis/01_smart_generate/data/smart_generate_data.RData"),
"D:/0_DataCentre/2_Analysis/02_smart_analysis/data/smart_generate_data.RData")

for (path in rdata_targets) {
  obj_to_save <- ls(pattern = "^(all|patrol)")
  save(list = obj_to_save, file = path)
  message(sprintf(
    "✓ Saved %d object(s) to:\n  %s\n  (Directory: %s)\n",
    length(obj_to_save),
    normalizePath(path),
    basename(dirname(path))
  ))
}

## save track for next analysis -----

# Multiple target directories
targets <- c(
  analysis_analysis,
  file.path(analysis_analysis, "data"),
  analysis_dashboard,
  data_folder
)

# Create directories
for (dir in targets) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Read data
management_areas <- st_read(file.path(nationaldb, "SMART/All Observation/site_area.shp"))
names(management_areas)
patrol.tracks <- st_read(file.path(nationaldb, "SMART/All Observation/All_track.shp")) 
names(patrol.tracks) <- tolower(names(patrol.tracks))

# Save to all targets
for (target in targets) {
  saveRDS(management_areas, file.path(target, "management_areas.rds"))
  saveRDS(patrol.tracks, file.path(target, "patrol_tracks.rds"))
  cat(sprintf("✓ Saved to: %s\n", target))
}
