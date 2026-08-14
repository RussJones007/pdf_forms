# -------------------------------------------------------------------------------------->
# Script: pdf_fields.r
# Description: 
#   Reads the fields from a given pdf file
#   Sets the fields from an export form EpiTrax
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
library(purrr)
library(staplr)
source("R/field_functions.r")  # load custom functions for this project.

file <- "forms/Pertussis-Fillable-Feb2022.pdf"
fields <- get_fields(input_filepath = file)
map(fields, ~ .x$type) |> 
  unlist() |> 
  table()


# avoids the problematic fields.  Change this to all fields when pdf form is corrected. 
fields_value_set <- map(fields[c(1:376, 379:384)], set_text_value)
#fields_value_set <- map(fields[c(1:384)], set_text_value)

  

fields_value_set$`Timeframe for Tdap` <- set_specific(item = fields_value_set$`Timeframe for Tdap`,
                                                     value = "at delivery")
fields_value_set$Exposure <- set_specific(fields_value_set$Exposure, "close contact")
fields_value_set$`Timeframe for Tdap` <- NULL
fields_value_set$Exposure <- NULL
out_file <- "Pertussis_test.pdf"
set_fields(input_filepath = file, output_filepath = out_file, fields = fields_value_set, overwrite = TRUE)

problem_fields <- fields_value_set[377:378]
problem_fields



fields_value_set[["White"]] 
fields[["White"]]$value

set_text_value(fields[["SEX"]])


fields_value_set[["SEX"]]$value <- factor(
  levels(fields[["SEX"]]$value)[2],
  levels = levels(fields[["SEX"]]$value)
)


buttons <- keep(.x = fields_value_set, .p = \(x) x[["type"]] == "Button") 
map(buttons, ~.x[["value"]])


