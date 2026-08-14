# ------------------------------------------------------------------------------------------------------------------->
# Script:  field_functions.r
# Description:
#   This script defines function for use by otehr scripts in manipluating data elements and settnig 
#   from fileds 
# 
# 
# 
# Steps:
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created:  August 14, 2026 
# 
# ------------------------------------------------------------------------------------------------------------------->

# set the value of the list item to a value to identify the variable name, or button to check
#' Set Field Value to Field Name
#' 
#'  Take a field resulting from get_fields, and sets the value to either the field name for Text
#'  fields, or the second category in levels of Value for Button fields.
#'  Use this for setting fields in a pdf file for viewing names of the fields and where buttons
#'  are located.  C
#'
#' @param item is one field from the field list returned by [staplr::get_fields()]
#'#' @returns  the same filed but with the value set
#' @export
#'
#' @examples
#' fn <- "forms/Pertussis-Fillable_Frb2022.pdf
#' fields <- get_fields(input_path = fn)
#' 
#' fields[[1]] <- set_text_value(fields[[1]])

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




#' Set one field to a specific value
#'
#' @param item is the field from a field list
#' @param value is the value to set for the field.  If the 
#'
#' @returns
#' @export
#'
#' @examples
set_specific <- function(item, value){
  
  button_set <- function() {
    categories <- levels(item$value)
    if(!any(value  %in% categories)) stop("value not found in levels of item")
    item$value <- factor(value, levels = categories)
    return(item)
  }
  
  text_set <- function(){
    item$value <- value
  }
  
  item <- switch(item$type,
                 "Button" = button_set(),
                 "Text"   = text_set()
  )
  
  return(item)
}
