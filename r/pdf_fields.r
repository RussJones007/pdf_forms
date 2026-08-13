# -------------------------------------------------------------------------------------->
# Script: pdf_fields.r
# Description: 
#   Reads the fields from a given pdf file
#
#
# Steps:
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: August 11, 2026
# Revised:
# -------------------------------------------------------------------------------------->
library(purrr)
library(staplr)

file <- "forms/Pertussis-Fillable-Feb2022.pdf"
fields <- get_fields(input_filepath = file)
map(fields, ~ .x$type) |> 
  unlist() |> 
  table()

# set the value of the list item to a value to identify the variable name, or button to check
set_text_value <- function(item){
  type <- item[["type"]]
  
  item[["value"]] <- switch(
    EXPR = type,
     "Button" = factor(
       levels(item[["value"]])[2],
       levels = levels(item[["value"]])
     ),
    "Text"   = item[["name"]]
  )
  return(item)
}

fields_value_set <- map(fields[c(1:376, 379:384)], set_text_value)
fields_value_set <- map(fields[c(1:384)], set_text_value)

out_file <- "Pertussis_test.pdf"
set_fields(input_filepath = file, output_filepath = out_file, fields = fields_value_set, overwrite = TRUE)

problem_fields <- fields[377:378]




fields_value_set[["White"]] 
fields[["White"]]$value

set_text_value(fields[["SEX"]])


fields_value_set[["SEX"]]$value <- factor(
  levels(fields[["SEX"]]$value)[2],
  levels = levels(fields[["SEX"]]$value)
)


buttons <- keep(.x = fields_value_set, .p = \(x) x[["type"]] == "Button") 
map(buttons, ~.x[["value"]])







