
# 0. Setup and Packages Install----------------
rm(list = ls())
packages_needed <- c("dplyr", "tidyr", "janitor","sp","sf","tidyverse","data.table","utils", "lubridate",
                     "readr", "mapview", "ggplot2","here", "fs", "readxl", "purrr", "hms")

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


# 3. Data Cleaning ------
## 0. All observation Cleaning -----
all.observation <- all.observation %>%
  clean_names() %>%
  rename(kategori_temuan_0 = observation_category_0,
         kategori_temuan_1 = observation_category_1) %>%
  mutate(
    landscape = "Kalimantan Barat",
    patrol_start_date = lubridate::mdy(patrol_start_date),
    patrol_end_date = lubridate::mdy(patrol_end_date), 
    waypoint_date = lubridate::mdy(waypoint_date),
    waypoint_time_final = as_hms(parse_date_time(waypoint_time, orders = c("HM", "HMS")))
  ) %>%
  select(-waypoint_time) %>% rename(waypoint_time = waypoint_time_final)
glimpse(all.observation) # check data coloumn format

## a. Patrol Summary --------
### Run with function helper ------
CPS <- clean_patrol_summary(
  file_path = file.path(nationaldb, "SMART/All Observation/All_track.shp"),
  landscape = "Kalimantan Barat") 

patrol.summary <- CPS %>%
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

## b. Patrol Threats ---------
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



## c. Patrol Fauna & Flora -------
patrol.fauna.flora <- all.observation %>% 
  filter(kategori_temuan_0 == "Satwa Liar" | kategori_temuan_0 == "Tumbuhan") %>%
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
  mutate(
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
    # str_trim for erasing space at start and end word
    Local_Name = str_trim(coalesce(
      str_extract(jenis_satwa, "^[^-]+"),
      str_extract(jenis_tumbuhan, "^[^-]+")
    )),
    Scientific_Name = str_trim(coalesce(
      str_extract(jenis_satwa, "[^-]+$"),
      str_extract(jenis_tumbuhan, "[^-]+$")
    )),
    satuan = case_when(
      kategori_temuan_0 == "Tumbuhan" ~ "individu",
      kategori_temuan_1 == "Perjumpaan Satwa" ~ "individu",
      TRUE ~ "temuan"
    )
  ) %>%
  select(landscape, region, site, station, patrol_id, patrol_start_date, patrol_end_date,
         patrol_transport_type, waypoint_date, waypoint_time, x, y, kategori_temuan_0,
         kategori_temuan_1, Local_Name, Scientific_Name,tipe_temuan, jumlah, satuan,
         umur_satwa,kondisi_tumbuhan, usia_temuan, month, session, semester, quarter,
         keterangan)

glimpse(patrol.fauna.flora)


# 4. Distribute data to folder database ---------

## a. Patrol Summaries
summary_patrol_data(patrol.summary, nationaldb_smart)

## b. Patrol Threats ------
threats_patrol_data(patrol.threats, nationaldb_smart)

## c. Patrol Fauna-Flora-------
ff_patrol_data(patrol.fauna.flora, nationaldb_smart)


save.image("D:/0_DataCentre/2_Analysis/01_smart_generate/data/smart_generate_data.RData")
