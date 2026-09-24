# -------------------------------------------------------------------------------------->
# Script: pdf_fields.r
# Description: 
#   Reads the fields from a given pdf file
#   Sets the fields from an export from EpiTrax
#
#    Note that the following fields are malformed in the pdf template, specifically the factor levels:
#    'Exposure'
#    'Timeframe for Tdap'
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: August 11, 2026
# Revised:
# -------------------------------------------------------------------------------------->
pkgs <- c("staplr", "purrr", "stringr", "lubridate", "dplyr", "tidyr", "readr")
purrr::walk(.x = pkgs, .f = \(p) library(p, character.only = TRUE, quietly = TRUE))
source("R/field_functions.r")  # load custom functions for this project.
rm(pkgs)

file <- "forms/Pertussis-Fillable-Feb2022.pdf"
fields <- get_fields(input_filepath = file)
map(fields, ~ .x$type) |> 
  unlist() |> 
  table()

# Remove problem field
#problem_fields <- fields[377:378]
#undebug(set_text_value)
#map(problem_fields, set_text_value)
#fields[c(377, 378)] <- NULL

# avoids the problematic fields.  Change this to all fields when pdf form is corrected. 
#fields_value_set <- map(fields[c(1:376, 379:384)], set_text_value)
#fields_value_set <- map(fields[c(1:384)], set_text_value)

fields_value_set <- map(fields, set_text_value)

buttons <- keep(.x = fields_value_set, .p = \(x) x[["type"]] == "Button") 

# Map EpiTrax values to fields ------------------------------------------------------------------------------------
## --- Read data file
fn  <- "data//raw/Pertussis_Aug_17_2026.csv"
raw <- read_csv(fn, col_types = cols(.default = col_character()))

prefixes <- c("(patient|person|address_at_diagnosis|pertussis_col_pertussis)_")
work_status <- c("approved_by_lhd", "investigation_complete")

pertussis <- raw |> 
  rename_with( \(x) str_remove(x, prefixes), everything()) |> 
  filter(
    workflow_state %in% work_status,
    lhd_case_status %in% c("Confirmed", "Probable")
  ) |> 
  mutate( 
    across(contains("date"), \(x) ymd_hms(x, truncated = 3) |> as_date())
  )

glimpse(pertussis |> select(ends_with("date")))
skim(pertussis)

map_to_fields <- function(fields, record){
    # Split the dates into components and enter into fields
  ret_fields <- fields    # copy of fields that will be returned
  
  dates <- list(
    report        = record$first_reported_ph_date, # 1
    invest_start  = record$lhd_investigation_start_date, #2
    invest_finish = record$last_investigation_completed_lhd_date, #3
    birth         = record$birth_date,  #4
    pregnancy_due = as.Date("1900-01-01"),  #5 
    onset         = record$disease_onset_date, #6
    diagnosis     = record$date_diagnosed #7
  ) |> 
    map(split_date)
  
  
  # internal function to set dates in ret_fields
  set_date_fields <- function(item, date_number){
    stopifnot(length(dates) >=  date_number)
    # create character vector to select date field components based on number
    sel <- paste0(c("Month", "Day", "Year"), date_number)
    date <- dates[[date_number]]
    sq <- seq_along(date)
    # set the item value for each date component
    ret_fields[sel] <<- map2(ret_fields[sel], sq,  \(component, number) set_specific(component, date[[number]]))
    return(sel)   # return the date field names for use when selecting fields set at end of function
  }

    # set the date fields to the corrosponging record value
  date_field_names <- map2(dates, seq_along(dates),  set_date_fields) |> 
    unlist()
  
   quick_race <- function(race_name){
     get_race_setting(ret_fields[[race_name]], record$race)
   }
    
   
  # a named list of functions or values to set. This maps the record 
  # The name is the filed name as set in the template fill-able pdf file.
  # The source of the value comes from record.
  set_fields <- list(
    "Case Status"     = record$lhd_case_status,
    "Patients Name"   = format_name(record$last_name, record$first_name, record$middle_name),
    "Address"         = record$street,
    "City"            = record$city,
    "County"          = record$county,
    "Zip"             = record$zip,
    "Phone_1"         = format_phone(record$phone_number),
    "Region"          = "PHR 2/3",
    "ParentGuardian"  = str_to_title(record$parent_guardian),
    # Physcian information
    "Physician"       = format_name(record$clinician_last_name, record$clinician_first_name),
    "Phone_2"         = format_phone(record$clinician_phone),
    "Address 1"       = record$facility_street,
    "Address 2"       = glue::glue("{record$facility_address_city}, {record$facility_state} {record$facility_address_zip}"),
    # Reporter information
    "Reported by"     = format_name(record$reporter_last_name, record$reporter_first_name),
    "Agency"          = record$reporting_agency,
    "Phone_3"         = format_phone(record$reporter_phone_phone_number),
    # Investigator informatiom
    "Investigated by" = record$investigator,
    "Agency_2"          = "Tarrant County Public Health",
    "Phone_4"         = format_phone("8173215350"),
    # Demographics
    "SEX"                                           = record$birth_sex,
    "AGE"                                           = record$age_at_event_in_years,
    "Place of Birth"                                = recode_values(record$country_of_birth,
                                     "United States" ~ "USA",
                                     NA              ~ "Unknown",
                                     default        = "Other"),
    "If female is patient currently pregnant"       = get_ynu(ret_fields$`If female is patient currently pregnant`, record$pregnant),
    "White"                                         = quick_race("White"),
    "Black"                                         = quick_race("Black"),
    "Asian"                                         = quick_race("Asian"),
    "Native Hawaiian or Other Pac Islander"         = quick_race("Native Hawaiian or Other Pac Islander"),
    "Am Indian or Alaska Native"                    =  quick_race("Am Indian or Alaska Native")#,
    #quick_race("Unknown")
    )
  browser()
  # set the fields in ret_fields
  iwalk(set_fields, \(x,name)  ret_fields[[name]] <<- set_specific(ret_fields[[name]], x))
  field_set_names <- c(date_field_names, names(set_fields))
  ret_fields[field_set_names]
}

record <- pertussis[2, ]
glimpse(record)
tmp <- map_to_fields(fields = fields, record = record)

out_file <- file.path("data/processed", glue::glue("{record$record_number}_{record$last_name}.pdf"))
set_fields(input_filepath = file, output_filepath = out_file, fields = tmp)
out_file



get_field_record <- function(name){
  term = regex(name, ignore_case = TRUE)  
  list(
    field  = names(fields) |> keep(.p = ~ str_detect(.x, term)),
    record = names(record) |> keep(.p = ~ str_detect(.x, term))
  )
}

map_chr(buttons, ~.x$name) |> sort()



library(tarr)
et <- tarr::epitrax_import()
et$patient_race |> unique()
str_remove("Other_2", pattern = "_\\d")





et$patient_race |> unique()
