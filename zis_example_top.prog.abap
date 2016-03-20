include zis_example_doc.

types: begin of t_sometype.
  include structure textpool.
types: end of t_sometype.

constants gstr type string value 'The test string'.
data include type char1.