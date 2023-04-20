class ZCL_IASM_DEVOBJ_SAVERS definition
  public
  final
  create public .

  public section.
    class-methods to_display returning value(ri_instance) type ref to zif_iasm_devobj_saver.
    class-methods to_program returning value(ri_instance) type ref to zif_iasm_devobj_saver.
    class-methods to_file returning value(ri_instance) type ref to zif_iasm_devobj_saver.

  protected section.
  private section.
ENDCLASS.



CLASS ZCL_IASM_DEVOBJ_SAVERS IMPLEMENTATION.


  method to_display.
    create object ri_instance type lcl_saver_to_display.
  endmethod.


  method to_file.
    create object ri_instance type lcl_saver_to_file.
  endmethod.


  method to_program.
    create object ri_instance type lcl_saver_to_program.
  endmethod.
ENDCLASS.
