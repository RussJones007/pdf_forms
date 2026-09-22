The pdf_forms project is for TCPH Epidemiology Division Use. The project is to take one or more exports from EpiTrax (or possibly runs a query on the database) for a condition where DSHS needs a pdf investigation for their surveillance group. Pertussis is the first condition to be implemented.

Steps for implementation:

1.  Using the DSHS pdf fill-able form, extract the fill-able field names and types as a list.
    a.  Set each field "value" to the name of the field in the list.
    b.  Set the fields in a "test" pdf file. The result are text fields printed with the field name. This pdf file can be used to determine where each field is on the form.
2.  Identify EpiTrax fields that will need to be exported - create an EpiTrax export template
3.  Map the EpiTrax fields to each field name.
4.  Using helper functions, transform the EpiTrax field to each pdf file field.
    a.  Example, some pdf date elements expect separate month, day and year fields to be set. So a date extracted from epitrax would have to be split into those text components.
