The pdf_forms project is for TCPH Epidemiology Division Use.  The project is to setup to 
take one or more exports from EPiTrax (or possibly runs a query on the database) for a condition 
that requires a pdf investigation form to be sent to the DSHS surveillance group. 
Pertussis is the first condition to be implemented.


Steps for implementation:
1.  Using the DSHS pdf fillable form, extract the fillable field names and types as a list.
  a. Set each field "value" to the name of the field in the list.
  b. Set the fields in a "test" pef file.  The result are text fields pronted with the 
  field name.  This pdf file can be used to determine where each field is on the form.
2.  Identify EpiTrax fields that will need to be exported - create an EpiTrax export template
3.  Map the EpiTrax fields to each field name.
4.  Using helper functions, transform the EpiTrax field to each pdf file field.  
  a. Example, some pdf date elements expect sepearte month, day and year fields to be set.  So 
     a date extracted from epitrax would have to be split into those text components. 

