class ZCL_IASM_EXAMPLE definition
  public
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
