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
#' Use this function in laaply or a map to set the values of the field_list in the template pdf.
#' Handles buttons which have factor values, and text entries as well. 
#'
#' @param item is the field from a field list
#' @param value is the value to set for the field.  If the 
#'
#' @returns the item with set to "value
set_specific <- function(item, value){
  
  button_set <- function() {
    categories <- levels(item$value)
    if(!any(value  %in% categories)) stop("value: ", value,  " not found in factor levels of item: ", item$name)
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


# Functions to translate from EpiTrax to CRF fields ---------------------------------------------------------------

#' Format names
#'
#' @param last,first,middle are character vectors fo names to formatted and concatenated.  The function
#' is vectorised so each vector is sized tot he one with the greatest length
#'
#' @returns a character vector of "last, first middle".
format_name <- function(last, first, middle = ""){
  paste0(last, ", ", first, " ", middle) |> 
    str_to_title()
}


#' Split a Date into Components
#' 
#' When given a Date or POSIX date-time object splits into month, day, and year components
#'
#' @param date a Date or posix date_time_object
#' @returns a named list of character vectors consisting of month, day, and year components
split_date <- function(date){
  type <- class(date)
  if(! any(type  %in% c("Date", "POSIXct", "POSIXlt"))) stop("The 'date' argument must be a Date or POSIXct object")
  lt <- as.POSIXlt(date)
  return( list(month = lt$mon + 1, 
               day   = lt$mday, 
               year  = lt$year + 1900) 
  ) |> 
    map(as.integer)
}

