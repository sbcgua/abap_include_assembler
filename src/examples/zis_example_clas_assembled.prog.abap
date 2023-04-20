*%%INCLUDING ZIF_IASM_EXAMPLE

interface zif_iasm_example
   .

  constants hello type string value 'world'.
endinterface.


*%%INCLUDING ZCL_IASM_EXAMPLE

class lcl_local_class definition.
  public section.
endclass.
class lcl_local_class implementation.
  " some code
endclass.
class ZCL_IASM_EXAMPLE definition
  final
  create public .

  public section.
    class-methods write.
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_IASM_EXAMPLE IMPLEMENTATION.


  method write.
    write: / zif_iasm_example=>hello.
  endmethod.
ENDCLASS.
