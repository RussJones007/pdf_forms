# -------------------------------------------------------------------------------------->
# Script: pdf_fields.r
# Description: 
#   Reads the fileds fom a given pdf file
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
file <- "Pertussis-Fillable-Feb2022.pdf"
fields <- get_fields(input_filepath = file)
map(fields, ~ .x$type) |> 
  unlist() |> 
  table()

# set the value of the list item to something to identify the variable name, or button to check
set_text_value <- function(item){
  type <- item[["type"]]
  if(type != "Text") return(item)
  
  item[["value"]] <- item[["name"]]
  return(item)
}

fields_value_set <- map(fields, set_text_value)

is_button <- function(item){
  item[["type"]] == "Button"
}

is_button(fields[[2]])

buttons <- keep(.x = fields, .p = \(x) x[["type"]] == "Button")
levels(buttons[[1]]$value)[2] |> class()

out_file <- "Pertussis_test.pdf"
set_fields(input_filepath = file, output_filepath = out_file, fields = fields_value_set, overwrite = TRUE)



