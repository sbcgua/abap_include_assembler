report zis_example.

*%%ASSEMBLY-START @ZIS_EXAMPLE_TOP ------------------------------------*
*include zis_example_top.
*%%ASSEMBLY-START @ZIS_EXAMPLE_DOC ------------------------------------*
*include zis_example_doc.
* Some documentation here
*%%ASSEMBLY-END   @ZIS_EXAMPLE_DOC ------------------------------------*


types: begin of t_sometype.
  include structure textpool.
types: end of t_sometype.

constants gstr type string value 'The test string'.
data include type char1.
*%%ASSEMBLY-END   @ZIS_EXAMPLE_TOP ------------------------------------*

*%%ASSEMBLY-START @ZIS_EXAMPLE_F01 ------------------------------------*
*include zis_example_f01.
form perform_write.
  write / gstr.
endform.
*%%ASSEMBLY-END   @ZIS_EXAMPLE_F01 ------------------------------------*


start-of-selection.

include = 'X'.
perform perform_write.
