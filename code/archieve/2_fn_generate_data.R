# LIBRARY FUNCTIONS FOR BIODIVERSITY DATA MANAGEMENT

# Version: 2.0 (Integrated and Optimized)
# Date: 2026-03-24



# 0. LOAD REQUIRED LIBRARIES


suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(hms)
  library(sf)
  library(writexl)
  library(tools)
})


# 1. EXPORT DATA BY REGION FUNCTION ----------


#' Export dataframe to subfolders based on Region
#' 
#' @param df Dataframe to export
#' @param region_col Name of column containing region (default: "Region")
#' @param output_dir Main output directory (default: "output")
#' @param file_prefix Prefix for filenames (default: "data_")
#' @param export_csv Logical, whether to export to CSV (default: TRUE)
#' @param export_excel Logical, whether to export to Excel (default: TRUE)
#' @param add_date Logical, whether to add date to filename (default: TRUE)
#' @param encoding Encoding for CSV files (default: "UTF-8")
#' @return Displays confirmation messages and returns output path invisibly
#' 
#' @examples
#' data <- data.frame(
#'   name = c("Andi", "Budi", "Cici"),
#'   Region = c("Java", "Sumatra", "Java"),
#'   value = c(85, 90, 88)
#' )
#' export_by_region(data)

export_by_region <- function(df, region_col = "Region", 
                             output_dir = "output", 
                             file_prefix = "data_",
                             export_csv = TRUE,
                             export_excel = TRUE,
                             add_date = TRUE,
                             encoding = "UTF-8") {
  
  # Validate input
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (!region_col %in% names(df)) {
    stop(paste("Column", region_col, "not found in dataframe"))
  }
  
  # Create main directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Creating main directory: ", output_dir)
  }
  
  # Export for each unique region
  regions <- unique(df[[region_col]])
  regions <- regions[!is.na(regions)]  
  total_files <- 0
  
  for (reg in regions) {
    # Clean the region name for folder/filenames
    clean_name <- gsub("[^[:alnum:]_-]", "_", as.character(reg))
    
    # Create subfolder path
    dir_path <- file.path(output_dir, clean_name)
    
    # Create subfolder if it doesn't exist
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message("Creating subfolder: ", dir_path)
    }
    
    # Filter data by specific region
    df_region <- df %>% dplyr::filter(.data[[region_col]] == reg)
    
    # Create base filename
    base_name <- paste0(file_prefix, clean_name)
    
    # Add date if requested
    if (add_date) {
      date_suffix <- format(Sys.Date(), "%Y%m%d")
      base_name <- paste0(base_name, "_", date_suffix)
    }
    
    # Export to Excel
    if (export_excel) {
      excel_file <- paste0(base_name, ".xlsx")
      excel_path <- file.path(dir_path, excel_file)
      tryCatch({
        writexl::write_xlsx(df_region, path = excel_path)
        total_files <- total_files + 1
        message("Successfully exported: ", excel_path)
      }, error = function(e) {
        warning("Failed to export Excel for region ", reg, ": ", e$message)
      })
    }
    
    # Export to CSV
    if (export_csv) {
      csv_file <- paste0(base_name, ".csv")
      csv_path <- file.path(dir_path, csv_file)
      tryCatch({
        write.csv(df_region, file = csv_path, 
                  row.names = FALSE, fileEncoding = encoding)
        total_files <- total_files + 1
        message("Successfully exported: ", csv_path)
      }, error = function(e) {
        warning("Failed to export CSV for region ", reg, ": ", e$message)
      })
    }
  }
  
  # Export summary
  message("\n========================================")
  message("EXPORT SUMMARY:")
  message("Unique regions: ", length(regions))
  message("Total files exported: ", total_files)
  message("Output location: ", normalizePath(output_dir))
  message("========================================")
  
  # Return output path invisibly
  invisible(normalizePath(output_dir))
}


# 2. TIME CONVERSION FUNCTIONS (UNIFIED) ----------


#' Convert various time formats to hms (HH:MM:SS)
#' 
#' Supports formats:
#' - Numeric decimal Excel (0-1)
#' - hms, difftime, POSIXct objects
#' - Character: "HH:MM" or "HH:MM:SS"
#' 
#' @param x Vector with various time formats
#' @return hms object

to_hms <- function(x) {
  if (is.null(x) || all(is.na(x))) return(as_hms(NA))
  
  result <- vector("numeric", length(x))
  result[] <- NA_real_
  
  for (i in seq_along(x)) {
    val <- x[[i]]
    if (is.na(val)) next
    
    seconds <- tryCatch({
      # Case 1: Numeric decimal Excel (0-1)
      if (is.numeric(val) && val >= 0 && val <= 1) {
        round(val * 86400)
      }
      # Case 2: hms or difftime
      else if (inherits(val, c("hms", "difftime"))) {
        as.numeric(val)
      }
      # Case 3: POSIXct timestamp
      else if (inherits(val, "POSIXct")) {
        as.numeric(as_hms(format(val, "%H:%M:%S", tz = "Asia/Jakarta")))
      }
      # Case 4: Character
      else {
        val_chr <- as.character(val)
        
        # Format HH:MM or HH:MM:SS
        if (grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", val_chr)) {
          as.numeric(as_hms(val_chr))
        }
        # Numeric string (decimal)
        else if (grepl("^[0-9.]+$", val_chr)) {
          round(as.numeric(val_chr) * 86400)
        }
        else {
          NA_real_
        }
      }
    }, error = function(e) NA_real_)
    
    result[i] <- seconds
  }
  
  as_hms(result)
}

#' Convert to radian time (radian)
#' 
#' Radian time represents time in a 24-hour circle
#' 0 radian = 00:00, π radian = 12:00, 2π radian = 24:00
#' 
#' @param time_hms Time object (hms) or convertible format
#' @return Numeric vector in radians

to_radian <- function(time_hms) {
  # Convert to seconds
  seconds <- if (inherits(time_hms, "hms")) {
    as.numeric(time_hms)
  } else {
    as.numeric(to_hms(time_hms))
  }
  
  # Convert to radian: (seconds / 86400) * 2π
  (seconds / 86400) * 2 * pi
}

#' Process time column into hms and radian formats
#' 
#' @param col Time column vector
#' @return List containing time (hms) and radian (numeric)

process_time <- function(col) {
  time_hms <- to_hms(col)
  list(
    time = time_hms,
    radian = to_radian(time_hms)
  )
}

#' Convert radian time back to hms
#' 
#' @param radian Radian time vector in radians
#' @return hms object

radian_to_time <- function(radian) {
  seconds <- (radian / (2 * pi)) * 86400
  as_hms(seconds)
}

#' Format hms for display
#' 
#' @param time_hms hms object
#' @return Character in "HH:MM" format

format_time_display <- function(time_hms) {
  format(time_hms, "%H:%M")
}


# 3. DATE CONVERSION FUNCTIONS ------------


#' Parse various date formats
#' 
#' @param x Vector with various date formats
#' @return Date object


# CUSTOM PARSE DATE FUNCTION - IMPROVED VERSION


#' Parse various date formats with better handling
#' 
#' @param x Vector with various date formats
#' @return Date object

parse_date_custom <- function(x) {
  # Handle NULL or empty vector
  if (is.null(x) || length(x) == 0) return(as.Date(NA))
  
  # Convert to character if not already
  if (!is.character(x)) {
    x <- as.character(x)
  }
  
  # Replace empty strings, "NA", "NULL", "NaN" with NA
  x <- trimws(x)  # Remove leading/trailing spaces
  x[x %in% c("", "NA", "NULL", "NaN", "null", "na", "none", "None", "-")] <- NA_character_
  
  # If all NA, return NA
  if (all(is.na(x))) return(as.Date(NA))
  
  # List of supported date formats (ordered from most specific)
  formats <- c(
    "%Y-%m-%d",           # 2024-03-24
    "%Y/%m/%d",           # 2024/03/24
    "%Y.%m.%d",           # 2024.03.24
    "%d/%m/%Y",           # 24/03/2024
    "%d-%m-%Y",           # 24-03-2024
    "%d.%m.%Y",           # 24.03.2024
    "%m/%d/%Y",           # 03/24/2024
    "%m-%d-%Y",           # 03-24-2024
    "%Y%m%d",             # 20240324
    "%d%m%Y",             # 24032024
    "%B %d, %Y",          # March 24, 2024
    "%d %B %Y",           # 24 March 2024
    "%b %d, %Y",          # Mar 24, 2024
    "%d %b %Y"            # 24 Mar 2024
  )
  
  # Prepare result vector
  result <- as.Date(rep(NA, length(x)))
  
  # Process each value individually for maximum flexibility
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    
    val <- x[i]
    parsed <- NA_Date_
    
    # Try all formats
    for (fmt in formats) {
      parsed <- suppressWarnings(as.Date(val, format = fmt))
      if (!is.na(parsed)) break
    }
    
    # If failed, try Excel numeric conversion
    if (is.na(parsed) && grepl("^[0-9.]+$", val)) {
      parsed <- tryCatch({
        as.Date(as.numeric(val), origin = "1899-12-30")
      }, error = function(e) NA_Date_)
    }
    
    # If still failed, try with lubridate (if available)
    if (is.na(parsed) && requireNamespace("lubridate", quietly = TRUE)) {
      parsed <- tryCatch({
        lubridate::parse_date_time(val, orders = c("ymd", "dmy", "mdy", "ymd HMS"))
      }, error = function(e) NA_Date_)
    }
    
    result[i] <- parsed
  }
  
  return(result)
}


# SPECIAL FUNCTIONS FOR SETUP_DATE AND RETRIEVAL_DATE


parse_camera_trap_date <- function(x) {
  # Step 1: Clean input
  if (is.null(x)) return(as.Date(NA))
  
  # Convert to character
  x <- as.character(x)
  
  # Remove spaces
  x <- trimws(x)
  
  # Handle empty values and NA
  x[x %in% c("", "NA", "NULL", "NaN", "null", "na", "-", "..", "...")] <- NA_character_
  
  # Handle "NA" value from Excel (sometimes read as string "NA")
  x[x == "NA"] <- NA_character_
  
  # Step 2: If all NA, return NA
  if (all(is.na(x))) return(as.Date(NA))
  
  # Step 3: Parse with various formats
  result <- as.Date(rep(NA, length(x)))
  
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val)) next
    
    # Try standard format
    parsed <- tryCatch({
      # Format YYYY-MM-DD (most common)
      as.Date(val, format = "%Y-%m-%d")
    }, error = function(e) NA_Date_)
    
    if (is.na(parsed)) {
      parsed <- tryCatch({
        # Format YYYY/MM/DD
        as.Date(val, format = "%Y/%m/%d")
      }, error = function(e) NA_Date_)
    }
    
    if (is.na(parsed)) {
      parsed <- tryCatch({
        # Excel numeric format
        as.Date(as.numeric(val), origin = "1899-12-30")
      }, error = function(e) NA_Date_)
    }
    
    if (is.na(parsed) && nchar(val) == 8 && grepl("^[0-9]+$", val)) {
      # Format YYYYMMDD (8 digit number)
      parsed <- tryCatch({
        as.Date(val, format = "%Y%m%d")
      }, error = function(e) NA_Date_)
    }
    
    result[i] <- parsed
  }
  
  return(result)
}


# 4. COLUMN SCHEMA AND APPLICATION ----------


#' Data type schema for each column
#' 
#' Defines expected data types for each column
#' to ensure consistency when merging multiple files

col_schema <- list(
  "Date" = "date", "Time" = "time", "Longitude" = "numeric", "Latitude" = "numeric",
  "Code.Indv" = "character", "DBH" = "numeric", "TBC" = "numeric", "High.plant" = "numeric",
  "Landscape" = "character", "Region" = "character", "Site" = "character",
  "Transect" = "character", "Plot.Id" = "character", "Observation.Type" = "character",
  "Scientific.Name" = "character", "Taxon.Rank" = "character",
  "Indv" = "numeric", "Distance" = "numeric", "Observer" = "character",
  "Photo.Id" = "character", "Notes" = "character", "Survey.Method" = "character",
  "Habitat.Type" = "character", "Day.Night" = "character", "Speciment.ID" = "character",
  "Family" = "character", "Group1" = "character", "Group2" = "character", "SVL" = "numeric",
  "TaL" = "numeric", "Hor" = "numeric", "Ver" = "numeric",
  "Substrate" = "character", "Activity" = "character", "Note" = "character",
  "Altitude" = "numeric", "PPD" = "numeric", "Age" = "character", "Sex" = "character",
  "HB" = "numeric", "FA" = "numeric", "E" = "numeric", "T" = "numeric",
  "HF" = "numeric", "W" = "numeric", "AT" = "numeric", "TR" = "numeric",
  "Photo.ID" = "character", "Session" = "character", "Nest.Id" = "character",
  "Left.Right" = "character", "Class.Nest" = "character", "Position.Nest" = "character",
  "Tall.Nest" = "numeric", "Tall.Tree" = "numeric", "Canopy" = "numeric",
  "Canopy.Class" = "character", "Local.Name" = "character", "Count" = "numeric",
  "Plot.size" = "numeric", "Methods" = "character", "Area.Transect" = "numeric",
  "Functioning.1" = "character", "Functioning.2" = "character",
  "Pheno.1" = "character", "Pheno.2" = "character", "Length.Transect" = "numeric",
  "Width.Transect" = "numeric", "Area.region" = "numeric", "Area.site" = "numeric",
  "Replicate" = "numeric", "Date.Observation" = "date", "Type" = "character",
  "Type.ID" = "character", "Start.Date" = "date", "End.Date" = "date",
  "Start.Time" = "time", "End.Time" = "time", "GPS.ID" = "character",
  "Patrol.Start.Date" = "date", "Patrol.End.Date" = "date",
  "Waypoint.ID" = "character", "Waypoint.Date" = "date",
  "Waypoint.Time" = "time", "X" = "numeric", "Y" = "numeric", "Jumlah" = "numeric",
  "Patrol.ID" = "character", "Patrol.Leg.ID" = "character",
  "Patrol_Sta" = "date", "Patrol_End" = "date", "Jarak" = "numeric",
  "Date_Observation" = "date", "Setup_date" = "date", "Retrieval_date" = "date",
  "Problem1_from" = "date", "Problem1_to" = "date", "Setup_time" = "time",
  "Retrieval_time" = "time", "Individual" = "numeric"
)

#' Apply column schema to dataframe
#' 
#' @param df Input dataframe
#' @param keep_time_char Logical, whether to keep original time column as character
#' @return Dataframe with converted data types

apply_col_schema <- function(df, keep_time_char = FALSE) {
  for (col in names(col_schema)) {
    if (!col %in% names(df)) next
    
    target_type <- col_schema[[col]]
    
    if (target_type == "numeric") {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
      
    } else if (target_type == "character") {
      df[[col]] <- as.character(df[[col]])
      
    } else if (target_type == "date") {
      df[[col]] <- parse_date_custom(df[[col]])
      # Debug: Show warning if unexpected NA count
      na_count <- sum(is.na(df[[col]]))
      if (na_count > 0 && na_count < nrow(df)) {
        # Check original values that became NA
        original_vals <- unique(df[[col]][is.na(df[[col]])])
        original_vals <- original_vals[!is.na(original_vals) & original_vals != ""]
        if (length(original_vals) > 0) {
          message("Warning in column ", col, ": ", na_count, " values could not be parsed")
          message("  Example values: ", paste(head(original_vals, 3), collapse = ", "))
        }
      }
      
    } else if (target_type == "time") {
      # Process time using unified function
      time_result <- process_time(df[[col]])
      
      # Save hms and radian formats
      df[[paste0(col, "_hms")]] <- time_result$time
      df[[paste0(col, "_radian")]] <- time_result$radian
      
      # Determine original column output
      if (keep_time_char) {
        # Keep as character for bind_rows compatibility
        df[[col]] <- format(time_result$time, "%H:%M")
      } else {
        # Save as hms (actual time format)
        df[[col]] <- time_result$time
      }
    }
  }
  # Add data source information for debugging
  attr(df, "date_parsing_warnings") <- TRUE
  
  return(df)
}


# 5. FILE READING FUNCTIONS-----------


#' Get list of all Excel/CSV files in directory
#' 
#' @param base_path Main directory path
#' @return Vector of full file paths

excel_sup_csv <- function(base_path) {
  list.files(
    path = base_path,
    pattern = "\\.(xlsx|xls|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}
excel_files <- function(base_path) {
  list.files(
    path = base_path,
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
}


#' Read Excel or CSV file
#' 
#' @param file_path File path
#' @param sheet_name Sheet name (for Excel)
#' @return Dataframe


read_file <- function(file_path, sheet_name = NULL) {
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext %in% c("xlsx", "xls")) {
    if (!is.null(sheet_name)) {
      return(readxl::read_excel(file_path, sheet = sheet_name, 
                                col_types = "text", 
                                .name_repair = "universal"))
    } else {
      return(readxl::read_excel(file_path, col_types = "text", 
                                .name_repair = "universal"))
    }
  } else if (ext == "csv") {
    return(read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  } else {
    stop("File format not supported: ", ext)
  }
}



#' Get sheet names from Excel file
#' 
#' @param file_path Excel file path
#' @return Vector of sheet names or NULL for non-Excel

get_sheet_names <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("xlsx", "xls")) {
    return(readxl::excel_sheets(file_path))
  } else {
    return(NULL)  # CSV has no sheets
  }
}

detect_data_category <- function(file_path) {
  # Detect based on filename
  if (grepl("Camera Trap", file_path, ignore.case = TRUE)) {
    return(list(
      label = "Camera Trap",
      sheet_occasion = "Detection"
    ))
  } else if (grepl("SMART", file_path, ignore.case = TRUE)) {
    return(list(
      label = "SMART",
      sheet_occasion = c("Temuan", "Occasion")
    ))
  } else if (grepl("Biodiversity Survey", file_path, ignore.case = TRUE)) {
    return(list(
      label = "Biodiversity",
      sheet_occasion = "Occasion"
    ))
  } else if (grepl("Species Target Orangutans", file_path, ignore.case = TRUE)) {
    return(list(
      label = "Orangutan",
      sheet_occasion = "Occasion"
    ))
  } else {
    return(NULL)  # Category not recognized
  }
}

# 6. BIODIVERSITY DATA READING FUNCTIONS ----------


#' Read occasion data (occurrence records)
#' 
#' @param file_path File path
#' @return Dataframe with additional metadata


read_occasion_data <- function(file_path) {
  tryCatch({
    # Find target sheet
    sheets <- readxl::excel_sheets(file_path)
    target_sheet <- intersect(c("Occasion", "Temuan", "Detection"), sheets)[1]
    
    if (is.na(target_sheet)) {
      message("Occasion sheet not found: ", basename(file_path))
      return(NULL)
    }
    
    # Read data
    df <- readxl::read_excel(file_path, 
                             sheet = target_sheet, 
                             col_types = "text", 
                             .name_repair = "universal")
    
    # Apply column schema
    df <- apply_col_schema(df, keep_time_char = FALSE)
    
    # Metadata
    df$source_file <- file_path
    df$sheet_name <- target_sheet
    df$data_category <- ifelse(grepl("Camera Trap", file_path), "Camera Trap",
                               ifelse(grepl("SMART", file_path), "SMART",
                                      ifelse(grepl("Biodiversity Survey", file_path), "Biodiversity",
                                             ifelse(grepl("Species Target Orangutans", file_path), "Orangutan", "Other"))))
    
    return(df)
    
  }, error = function(e) {
    message("Error: ", basename(file_path), " - ", e$message)
    return(NULL)
  })
}


#' Read event data (sampling event records)
#' 
#' @param file_path File path
#' @return Dataframe with additional metadata

read_event_data <- function(file_path) {
  tryCatch({
    # Find target sheet
    sheets <- readxl::excel_sheets(file_path)
    target_sheet <- intersect(c("Sampling.Event", "Ringkasan_Patroli", "Station"), sheets)[1]
    
    if (is.na(target_sheet)) {
      message("Event sheet not found: ", basename(file_path))
      return(NULL)
    }
    
    # Read data
    df <- readxl::read_excel(file_path, 
                             sheet = target_sheet, 
                             col_types = "text", 
                             .name_repair = "universal")
    
    # Apply column schema
    df <- apply_col_schema(df, keep_time_char = FALSE)
    
    # Metadata
    df$source_file <- file_path
    df$sheet_name <- target_sheet
    df$data_category <- ifelse(grepl("Camera Trap", file_path), "Camera Trap",
                               ifelse(grepl("SMART", file_path), "SMART",
                                      ifelse(grepl("Biodiversity Survey", file_path), "Biodiversity",
                                             ifelse(grepl("Species Target Orangutans", file_path), "Orangutan", "Other"))))
    
    return(df)
    
  }, error = function(e) {
    message("Error: ", basename(file_path), " - ", e$message)
    return(NULL)
  })
}



#' Read Camera Trap data specifically
#' 
#' @param base_path Directory path
#' @return List containing occasion and event dataframes

read_camera_trap_data <- function(base_path) {
  all_files <- excel_sup_csv(base_path)
  camera_files <- all_files[str_detect(all_files, "Camera Trap")]
  
  if (length(camera_files) == 0) {
    message("No Camera Trap files found in: ", base_path)
    return(list(occasion = NULL, event = NULL))
  }
  
  list(
    occasion = map_dfr(camera_files, read_occasion_data),
    event = map_dfr(camera_files, read_event_data)
  )
}
read_rai_data <- function(file_path) {
  tryCatch({
    is_camera_trap <- str_detect(file_path, "Camera Trap")
    
    if (is_camera_trap) {
      df <- read_file(file_path)
      df <- apply_col_schema(df)
      df$source_file    <- file_path
      df$sheet_name     <- ifelse(tolower(tools::file_ext(file_path)) %in% c("xlsx","xls"),
                                  "main", "CSV")
      df$data_category  <- "Camera Trap"
      
    } else {
      sheets       <- get_sheet_names(file_path)
      target_sheet <- intersect(c("Occasion", "Temuan", "Detection"), sheets)[1]
      
      if (!is.na(target_sheet)) {
        df <- read_file(file_path, sheet_name = target_sheet)
        df <- apply_col_schema(df)
        df$source_file   <- file_path
        df$sheet_name    <- target_sheet
        df$data_category <- case_when(
          str_detect(file_path, "Camera Trap") ~ "Camera Trap",
          TRUE ~ "Other"
        )
      } else {
        message("Sheet 'Occasion'/'Temuan'/'Detection' not found: ", file_path)
        return(NULL)
      }
    }
    
    return(df)
    
  }, error = function(e) {
    message("Error reading file: ", file_path, " - ", e$message)
    return(NULL)
  })
}



# 7. DATA CLEANING AND ATTRIBUTE STANDARDIZATION ----------


#' Standardize column names for shapefile compatibility
#' 
#' @param df Input dataframe
#' @return Dataframe with standardized column names

standardize_attributes <- function(df) {
  
  # Mapping old column names to new (max 10 characters for shapefile)
  rename_map <- c(
    "Scientific.Name"  = "Sci_Name",
    "Local.Name"       = "Local_Name",
    "Taxon.Rank"       = "Taxon_Rank",
    "Class.object"     = "Class_Obj",
    "data_category"    = "Data_Cat",
    "Observation.Type" = "Obs_Type",
    "Survey.Method"    = "Surv_Meth",
    "Photo.ID"         = "Photo_ID",
    "Patrol.Start.Date"= "Pat_Start",
    "Patrol.End.Date"  = "Pat_End",
    "Patrol.ID"        = "Patrol_ID",
    "Kategori.temuan"  = "Kat_Temuan",
    "Tipe.temuan"      = "Tip_Temuan",
    "Class.Nest"       = "Class_Nest",
    "Position.Nest"    = "Pos_Nest",
    "Nest.Id"          = "Nest_ID",
    "Canopy.class"     = "Canopy_Cls",
    "Tall.Nest"        = "Tall_Nest",
    "Tall.Tree"        = "Tall_Tree",
    "Left.Right"       = "Left_Right",
    "Orangutan.Feeds"  = "OU_Feeds",
    "TransectOrStation"= "Trans_Stat",
    "Plot.ID"          = "Plot_ID",
    "Mandate"          = "Mandate",
    "Methods"          = "Methods",
    "genusName"        = "Genus",
    "speciesName"      = "Species_Ep",
    "className"        = "Class_Tax",
    "orderName"        = "Order_Tax",
    "familyName"       = "Family_Tax",
    "Appendix"         = "CITES_App",
    "Protected"        = "Protected",
    "Endemic"          = "Endemic",
    "Migratory"        = "Migratory",
    "Status"           = "IUCN_Stat"
  )
  
  for (old_name in names(rename_map)) {
    new_name <- rename_map[[old_name]]
    if (old_name %in% names(df) && !new_name %in% names(df)) {
      df <- df %>% rename(!!new_name := !!sym(old_name))
    }
  }
  
  # Trim long strings for shapefile compatibility
  char_cols <- names(df)[sapply(df, is.character)]
  df[char_cols] <- lapply(df[char_cols], function(x) substr(x, 1, 254))
  
  df
}


## -- 7a. Standardize Site names ----------------------------------------------
# This function is used across all taxa for consistent location naming
clean_site <- function(df) {
  df %>%
    mutate(Site = case_when(
      Site == "LD BATU RANGKAYA"            ~ "Hutan Desa Batu Rangkaya",
      Site == "LD BATU SENGKUMANG"          ~ "Hutan Desa Batu Sengkumang",
      Site == "LD RIAM SEBUNSUK"            ~ "Hutan Desa Riam Sebunsuk",
      Site == "LD SILING API"               ~ "Hutan Desa Siling Api",
      Site == "LD LAYANG TANGGUI"           ~ "Hutan Desa Layang Tanggui",
      Site == "LD TABUH LESTARI"            ~ "Hutan Desa Tabuh Lestari",
      Site == "Hutan Desa Sepauhan Raya"    ~ "Hutan Desa Batu Sengkumang",
      Site == "Hutan Desa Batu Payung Dua"  ~ "Hutan Desa Riam Sebunsuk",
      Site == "Hutan Desa Belaban"          ~ "Hutan Desa Riam Gansing",
      Site == "Hutan Desa Sungai Besar"     ~ "Hutan Desa Rawa Gambut",
      Site == "Hutan Desa Sungai Pelang"    ~ "Hutan Desa Wana Gambut",
      Site == "Hutan Desa Rantau Panjang"   ~ "Hutan Desa Muara Palung",
      Site == "Hutan Desa Penjalaan"        ~ "Hutan Desa Simpang Keramat",
      Site == "Hutan Desa Sepakat Jaya"     ~ "Hutan Desa Lemmanis",
      Site == "Hutan Desa Sungai Nanjung"   ~ "Hutan Desa Sunan Bersatu",
      Site == "Hutan Desa Tanjung Beulang"  ~ "Hutan Desa Batu Bolah",
      Site == "Hutan Desa Laman Satong"     ~ "Hutan Desa Manjau",
      Site %in% c("Hutan Desa Beringin Rayo",
                  "Hutan Desa Beringin rayo") ~ "Hutan Desa Cuhai",
      Site == "Hutan Desa Sebadak Raya"     ~ "Hutan Desa Bukit Layang",
      Site == "Hutan Desa Menguluk Kompas"  ~ "Hutan Desa Manguluk Kompas",
      Site == "Resort Sempurna"             ~ "Resort Sampurna",
      Site == "Sempurna Resort"             ~ "Resort Sampurna",
      Site %in% c("Kendawangan",
                  "Kendawangan Resort",
                  "CAMK",
                  "Muara Kendawangan Nature Reserve") ~ "Resort Kendawangan",
      Site %in% c("Bangkul")               ~ "Resort Kendawangan - Bangkul",
      Site %in% c("Pembedilan",
                  "Pembedilan Resort")     ~ "Resort Pembedilan",
      Site == "Seriam"                      ~ "Resort Kendawangan - Seriam",
      Site == "Beginci Darat"               ~ "Hutan Desa Bujang Dua Serupa",
      Site == "Village Forest Muara Palung" ~ "Hutan Desa Muara Palung",
      Site == "Village Forest Simpang Keramat" ~ "Hutan Desa Simpang Keramat",
      Site == "Gunung Palung National Park" ~ "Resort Sampurna",
      TRUE ~ Site
    ))
}

## -- 7b. Standardize Region names --------------------------------------------
clean_region <- function(df) {
  df %>%
    mutate(Region = case_when(
      Site == "Hutan Desa Manjau"           ~ "HPK Laman Satong",
      Site == "Hutan Desa Simpang Keramat"  ~ "Sungai Purang",
      Site == "Hutan Desa Muara Palung"     ~ "Sungai Purang",
      Site == "Hutan Desa Rawa Gambut"      ~ "Sungai Besar",
      Site == "Hutan Desa Wana Gambut"      ~ "Sungai Pelang",
      Site %in% c("Hutan Desa Tabuh Lestari",
                  "Hutan Desa Siling Api")  ~ "Gunung Burung",
      Site %in% c("Hutan Desa Bujang Dua Serupa",
                  "Hutan Desa Manguluk Kompas") ~ "Batu Nyambu",
      Site %in% c("Hutan Desa Batu Rangkaya",
                  "Hutan Desa Layang Tanggui") ~ "Bukit Perai",
      Site == "Hutan Desa Bukit Layang"     ~ "Bukit Layang",
      Site %in% c("Hutan Desa Riam Sebunsuk",
                  "Hutan Desa Riam Gansing",
                  "Hutan Desa Batu Sengkumang",
                  "Hutan Desa Riam Sicangguran") ~ "Gunung Raya",
      Site == "Hutan Desa Lemmanis"         ~ "Batu Menangis",
      Site == "Hutan Desa Sunan Bersatu"    ~ "S.Tengar - S.Pesaguan",
      Site == "Hutan Desa Nanga Betung"     ~ "Nanga Betung",
      Site %in% c("Hutan Desa Batu Bolah",
                  "Hutan Desa Cuhai")       ~ "Gunung Berubayan",
      Site %in% c("Resort Sampurna",
                  "Resort Pangkal Tapang")  ~ "Taman Nasional Gunung Palung",
      Site %in% c("Resort Pembedilan",
                  "Resort Kendawangan",
                  "Resort Kendawangan - Bangkul",
                  "Resort Kendawangan - Seriam",
                  "Bangkul", "Seriam",
                  "Pos Jaga Seriam",
                  "Cagar Alam Muara Kendawangan") ~ "Cagar Alam Muara Kendawangan",
      Region == "Gunung Tarak"              ~ "HPK Laman Satong",
      Region == "Sebadak Raya"              ~ "Bukit Layang",
      Region == "Gunung Palung"             ~ "Taman Nasional Gunung Palung",
      Region == "Muara Kendawangan"         ~ "Cagar Alam Muara Kendawangan",
      TRUE ~ Region
    ))
}

clean_region_from_site <- function(df) {
  df %>%
    mutate(Region = case_when(
      Site %in% c("Hutan Desa Manjau") ~ "HPK Laman Satong",
      Site %in% c("Hutan Desa Simpang Keramat") ~ "Sungai Purang",
      Site %in% c("Hutan Desa Muara Palung") ~ "Sungai Purang",
      Site %in% c("Hutan Desa Rawa Gambut") ~ "Sungai Besar",
      Site %in% c("Hutan Desa Wana Gambut") ~ "Sungai Pelang",
      Site %in% c("Hutan Desa Tabuh Lestari") ~ "Gunung Burung",
      Site %in% c("Hutan Desa Siling Api") ~ "Gunung Burung",
      Site %in% c("Hutan Desa Bujang Dua Serupa") ~ "Batu Nyambu",
      Site %in% c("Hutan Desa Manguluk Kompas") ~ "Batu Nyambu",
      Site %in% c("Hutan Desa Batu Rangkaya") ~ "Bukit Perai",
      Site %in% c("Hutan Desa Layang Tanggui") ~ "Bukit Perai",
      Site %in% c("Hutan Desa Bukit Layang") ~ "Bukit Layang",
      Site %in% c("Hutan Desa Riam Sebunsuk") ~ "Gunung Raya",
      Site %in% c("Hutan Desa Riam Gansing") ~ "Gunung Raya",
      Site %in% c("Hutan Desa Batu Sengkumang") ~ "Gunung Raya",
      Site %in% c("Hutan Desa Riam Gansing") ~ "Gunung Raya",
      Site %in% c("Hutan Desa Riam Sicangguran") ~ "Gunung Raya",
      Site %in% c("Hutan Desa Lemmanis") ~ "Batu Menangis",
      Site %in% c("Hutan Desa Sunan Bersatu") ~ "S.Tengar - S.Pesaguan",
      Site %in% c("Hutan Desa Nanga Betung") ~ "Nanga Betung",
      Site %in% c("Hutan Desa Batu Bolah") ~ "Gunung Berubayan",
      Site %in% c("Hutan Desa Cuhai") ~ "Gunung Berubayan",
      Site %in% c("Hutan Desa Bukit Layang") ~ "Bukit Layang",
      Site %in% c("Resort Sampurna") ~ "Taman Nasional Gunung Palung",
      Site %in% c("Resort Pangkal Tapang") ~ "Taman Nasional Gunung Palung",
      Site %in% c("Resort Pembedilan") ~ "Cagar Alam Muara Kendawangan",
      Site %in% c("Resort Kendawangan - Seriam") ~ "Cagar Alam Muara Kendawangan",
      Site %in% c("Resort Kendawangan - Bangkul") ~ "Cagar Alam Muara Kendawangan",
      Site %in% c("Resort Kendawangan") ~ "Cagar Alam Muara Kendawangan",
      Site %in% c("Cagar Alam Muara Kendawangan") ~ "Cagar Alam Muara Kendawangan"
    ))
}


## -- 7c. Standardize Landscape names -----------------------------------------
clean_landscape <- function(df) {
  df %>%
    mutate(Landscape = case_when(
      Landscape %in% c("West Kalimantan", "Kalbar") ~ "Kalimantan Barat",
      TRUE ~ Landscape
    ))
}

## -- 7d. Assign Site from Transect code --------------------------------
# Used for event data (sampling) that uses transect codes
assign_site_from_transect <- function(df) {
  df %>%
    mutate(Site = case_when(
      Transect == "BPLK1"   ~ "Hutan Desa Layang Tanggui",
      Transect == "BPLK2"   ~ "Hutan Desa Batu Rangkaya",
      Transect %in% c("BNBD1", "BNBD2") ~ "Hutan Desa Bujang Dua Serupa",
      Transect == "GR-BL"   ~ "Hutan Desa Riam Gansing",
      Transect == "GR-BP"   ~ "Hutan Desa Riam Sebunsuk",
      Transect == "GR-SR"   ~ "Hutan Desa Batu Sengkumang",
      Transect %in% c("GP-PJ1","GP-PJ2","GP-PJL","GP-PJM1","GP-PJM2",
                      "GP-PJ3","GP-PJ4","GP-PJ5") ~ "Hutan Desa Simpang Keramat",
      Transect %in% c("GP-RP1","GP-RP2","GP-RPL","GP-RPM1","GP-RPM2",
                      "GP-RPM3","GP-RPM4","GP-RP3","GP-RP4","GP-RP5",
                      "GP-RP6","GP-RP7") ~ "Hutan Desa Muara Palung",
      TRUE ~ Site
    ))
}

## -- 7e. Full pipeline: clean occasion data (non-orangutan) ------------
clean_occasion <- function(df, date_col = "Date") {
  df %>%
    mutate(Session = as.character(year(.data[[date_col]]))) %>%
    clean_site() %>%
    clean_landscape()
}

## -- 7f. Full pipeline: clean event/sampling data ----------------------
clean_event <- function(df, date_col = "Date") {
  df %>%
    mutate(Session = as.character(year(.data[[date_col]]))) %>%
    assign_site_from_transect() %>%
    clean_site() %>%
    clean_region() %>%
    clean_landscape()
}




# 8. SPATIAL FUNCTIONS ------------

# Add admin columns to attribute table -------

#' Add administrative columns to dataframe
#' Priority: (1) Spatial intersection with admin shapefile, (2) Lookup table
#' @param sf_obj sf object in WGS84
#' @param admin_sf Administrative boundary sf (NULL if not available)
#' @param lookup_df Lookup dataframe Region -> District
#' @return sf_obj with added admin columns


add_admin_columns <- function(sf_obj, admin_sf = NULL, lookup_df) {
  
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)
  
  # Method 1: Spatial intersection if admin shapefile is available
  if (!is.null(admin_sf)) {
    
    # Ensure admin name column exists
    # Look for columns that might contain district names
    possible_kab_cols <- c("KABKOT", "KAB_KOTA", "KABUPATEN", "NAME_2",
                           "ADM2_EN", "ADM2_ID", "WADMKK", "NM_KAB")
    kab_col <- possible_kab_cols[possible_kab_cols %in% names(admin_sf)][1]
    
    possible_prov_cols <- c("PROVINSI", "NAME_1", "ADM1_EN", "ADM1_ID", "WADMPR", "NM_PROV")
    prov_col <- possible_prov_cols[possible_prov_cols %in% names(admin_sf)][1]
    
    if (!is.na(kab_col)) {
      message("  >> Spatial intersection with admin column: ", kab_col)
      
      # Intersection: find district for each point
      tryCatch({
        sf_intersect <- st_join(sf_obj,
                                admin_sf[, c(kab_col, if (!is.na(prov_col)) prov_col else NULL)],
                                join = st_intersects,
                                left = TRUE)
        
        sf_obj <- sf_intersect %>%
          rename(District = !!sym(kab_col))
        
        if (!is.na(prov_col)) {
          sf_obj <- sf_obj %>% rename(Province = !!sym(prov_col))
        } else {
          sf_obj$Province <- "West Kalimantan"
        }
        
        message("  [OK] Spatial intersection successful")
        return(sf_obj)
        
      }, error = function(e) {
        message("  [WARN] Spatial intersection failed, using lookup table: ", e$message)
      })
    }
  }
  
  # Method 2: Lookup table based on Region column
  message("  >> Using lookup table Region -> District...")
  
  if ("Region" %in% names(sf_obj)) {
    sf_obj <- sf_obj %>%
      left_join(lookup_df, by = "Region")
    
    # Fill remaining NA values
    sf_obj <- sf_obj %>%
      mutate(
        District = case_when(
          !is.na(District) ~ District,
          grepl("Kendawangan|Pembedilan|Bangkul|Seriam", Region, ignore.case = TRUE) ~ "Ketapang",
          grepl("Sampurna|Pangkal.Tapang|Gunung.Palung", Region, ignore.case = TRUE) ~ "Kayong Utara",
          TRUE ~ "Needs Verification"
        ),
        Province = ifelse(is.na(Province), "West Kalimantan", Province)
      )
  } else {
    sf_obj$District <- "Needs Verification"
    sf_obj$Province  <- "West Kalimantan"
  }
  
  sf_obj
}



# West Kalimantan coordinate bounds (with buffer)
LON_MIN <- 107.5
LON_MAX <- 118.0
LAT_MIN <- -4.5
LAT_MAX <- 2.5

#' Clean and validate coordinates
#' 
#' @param df Dataframe with coordinate columns
#' @param lon_col Name of longitude column
#' @param lat_col Name of latitude column
#' @param dataset_name Dataset name for logging
#' @return List containing df_clean and df_invalid

clean_coordinates <- function(df, lon_col = "Longitude", lat_col = "Latitude",
                              dataset_name = "Unknown") {
  
  # Validate columns
  if (!lon_col %in% names(df)) {
    message(paste0("  [WARN] Column '", lon_col, "' not found in ", dataset_name))
    return(list(df_clean = df[0, ], df_invalid = df))
  }
  if (!lat_col %in% names(df)) {
    message(paste0("  [WARN] Column '", lat_col, "' not found in ", dataset_name))
    return(list(df_clean = df[0, ], df_invalid = df))
  }
  
  n_original <- nrow(df)
  
  # Standardize column names
  if (lon_col != "Longitude" || lat_col != "Latitude") {
    df <- df %>%
      rename(Longitude = !!sym(lon_col),
             Latitude = !!sym(lat_col))
  }
  
  # Convert to numeric
  df <- df %>%
    mutate(
      Longitude = suppressWarnings(as.numeric(Longitude)),
      Latitude = suppressWarnings(as.numeric(Latitude))
    )
  
  # Detect and correct swapped coordinates
  df <- df %>%
    mutate(
      lon_looks_like_lat = !is.na(Longitude) & !is.na(Latitude) &
        abs(Longitude) < 10 & abs(Latitude) > 100,
      Longitude = ifelse(lon_looks_like_lat, Latitude, Longitude),
      Latitude = ifelse(lon_looks_like_lat, Longitude, Latitude)
    ) %>%
    dplyr::select(-lon_looks_like_lat)
  
  # Validate coordinates
  df <- df %>%
    mutate(
      coord_valid = !is.na(Longitude) & !is.na(Latitude) &
        Longitude != 0 & Latitude != 0 &
        Longitude >= LON_MIN & Longitude <= LON_MAX &
        Latitude >= LAT_MIN & Latitude <= LAT_MAX
    )
  
  df_clean <- df %>% filter(coord_valid) %>% dplyr::select(-coord_valid)
  df_invalid <- df %>% filter(!coord_valid) %>% dplyr::select(-coord_valid)
  
  message(paste0("  [", dataset_name, "] Total: ", n_original,
                 " | Valid: ", nrow(df_clean),
                 " | Invalid: ", nrow(df_invalid)))
  
  list(df_clean = df_clean, df_invalid = df_invalid)
}

#' Convert dataframe to sf object
#' 
#' @param df Dataframe with Longitude and Latitude columns
#' @param dataset_name Dataset name for logging
#' @return sf object or NULL

to_sf_wgs84 <- function(df, dataset_name = "Unknown") {
  if (is.null(df) || nrow(df) == 0) {
    message(paste0("  [", dataset_name, "] Empty data, cannot convert"))
    return(NULL)
  }
  
  tryCatch({
    sf_obj <- st_as_sf(df,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326,
                       remove = FALSE)
    message(paste0("  [", dataset_name, "] Successfully converted to sf: ",
                   nrow(sf_obj), " features"))
    sf_obj
  }, error = function(e) {
    message(paste0("  [ERROR] Failed to convert ", dataset_name, ": ", e$message))
    NULL
  })
}

#' Save sf object to shapefile with column name handling
#' 
#' @param sf_obj sf object
#' @param output_path Output path (without extension)
#' @param layer_name Layer name (optional)

write_shapefile_safe <- function(sf_obj, output_path, layer_name = NULL) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    message(paste0("  [SKIP] Empty data: ", output_path))
    return(invisible(NULL))
  }
  
  # Shapefile limits column names to 10 characters
  names_too_long <- names(sf_obj)[nchar(names(sf_obj)) > 10 & names(sf_obj) != "geometry"]
  
  if (length(names_too_long) > 0) {
    message(paste0("  [WARN] Truncated columns: ", paste(names_too_long, collapse = ", ")))
    new_names <- make.unique(substr(names(sf_obj), 1, 10), sep = "_")
    geo_idx <- which(names(sf_obj) == "geometry")
    if (length(geo_idx) > 0) new_names[geo_idx] <- "geometry"
    names(sf_obj) <- new_names
  }
  
  full_path <- paste0(output_path, ".shp")
  
  tryCatch({
    st_write(sf_obj, full_path,
             layer = if (!is.null(layer_name)) layer_name else basename(output_path),
             delete_layer = TRUE,
             quiet = TRUE)
    message(paste0("  [OK] Shapefile: ", full_path, " (", nrow(sf_obj), " features)"))
    invisible(sf_obj)
  }, error = function(e) {
    message(paste0("  [ERROR] Failed to save: ", full_path, " - ", e$message))
    invisible(NULL)
  })
}




# 9. VISUALIZATION FUNCTIONS ------


#' Visualize radian time in a circle
#' 
#' @param radian_times Radian time vector in radians
#' @param main Plot title

plot_radian_time <- function(radian_times, main = "Radian Time Visualization") {
  plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
       asp = 1, xlab = "", ylab = "", axes = FALSE, main = main)
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
  
  x <- cos(radian_times)
  y <- sin(radian_times)
  points(x, y, pch = 16, col = "blue")
  
  # Add time labels
  for (i in seq_along(radian_times)) {
    time_label <- format(radian_to_time(radian_times[i]), "%H:%M")
    text(x[i] * 1.1, y[i] * 1.1, time_label, cex = 0.8)
  }
  
  abline(h = 0, v = 0, col = "gray", lty = 2)
}


# 10. UTILITY FUNCTIONS ------


#' Data summary for debugging
#' 
#' @param df Dataframe
#' @param name Dataset name

summary_data <- function(df, name = "Dataset") {
  cat("\n========================================\n")
  cat("SUMMARY: ", name, "\n")
  cat("========================================\n")
  cat("Rows: ", nrow(df), "\n")
  cat("Columns: ", ncol(df), "\n")
  cat("\nColumns and data types:\n")
  print(sapply(df, class))
  cat("========================================\n")
}


# 11. DIVERSITY FUNCTIONS ----

## -- 11a. Diversity plot function (reused per taxa) --------------
plot_diversity <- function(data_site, subtitle_text) {
  data_site %>%
    ungroup() %>%
    dplyr::select(Site, shannon, margalef, richness) %>%
    pivot_longer(-Site, names_to = "index", values_to = "values") %>%
    ggplot(aes(fill = index, y = values, x = Site)) +
    geom_col(position = "dodge", width = 0.75, color = "white", linewidth = 0.3, alpha = 0.9) +
    geom_text(aes(label = round(values, 1)), position = position_dodge(0.75),
              vjust = 0.5, hjust = -0.1, size = 3.2, fontface = "bold") +
    coord_flip() +
    theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    ) +
    labs(title = "Diversity Indices Across Study Sites", subtitle = subtitle_text,
         x = "Study Sites", y = "Index Values", fill = "Diversity Indices:") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(
      values = c("shannon" = "#88B0B5", "margalef" = "#D4A5A5", "richness" = "#9FB5A2"),
      labels = c("shannon" = "Shannon-Wiener", "margalef" = "Margalef", "richness" = "Species Richness")
    ) +
    guides(fill = guide_legend(nrow = 1))
}

## -- 11b. Calculate diversity indices per group (avoid code duplication) -------
calc_diversity <- function(df, group_col, taxon_label) {
  df %>%
    count(Session, Scientific.Name, .data[[group_col]]) %>%
    group_by(Session, .data[[group_col]]) %>%
    summarize(
      richness  = specnumber(n),
      abundance = sum(n),
      shannon   = diversity(n, index = "shannon"),
      margalef  = (specnumber(n) - 1) / log(sum(n)),
      evenness  = shannon / log(length(n)),
      simpson   = 1 - diversity(n, index = "simpson"),
      .groups = "drop"
    ) %>%
    mutate(across(c(richness, abundance, shannon, evenness, simpson), ~round(., 2)),
           Taxon = taxon_label)
}



### Clean patrol summary function -------
clean_patrol_summary <- function(file_path, landscape) {
  # Check if file exists
  if(!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Read shapefile
  tryCatch({
    data_sf <- st_read(file_path, quiet = TRUE)
  }, error = function(e) {
    stop("Failed to read shapefile: ", e$message)
  })
  
  # Check required columns
  required_cols <- c("Patrol_L_1", "Patrol_L_2", "Armed", "Patrol_Leg")
  available_cols <- required_cols[required_cols %in% names(data_sf)]
  
  if(length(available_cols) == 0) {
    warning("No columns to drop. Column names may be different?")
    # Display available column names
    cat("Available columns:\n")
    print(names(data_sf))
  }
  
  # Process data
  result <- data_sf %>%
    mutate(
      landscape = landscape,
      distance_m = round(st_length(.),2),  # distance in meters
      distance_km = round(as.numeric(distance_m / 1000),2)  # convert to kilometers
    )
  
  # Drop columns if they exist
  cols_to_drop <- intersect(c("Patrol_L_1", "Patrol_L_2", "Armed", "Patrol_Leg"), names(result))
  if(length(cols_to_drop) > 0) {
    result <- result %>% select(-all_of(cols_to_drop))
  }
  
  # Convert to data.frame
  result <- result %>%
    as.data.frame() %>%
    select(-geometry)
  
  # Convert column names to lowercase
  names(result) <- tolower(names(result))
  
  return(result)
}


# ----------END OF FUNCTIONS--------------