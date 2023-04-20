class ZCX_IASM_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  data MSG type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !MSG type STRING optional .
  class-methods RAISE
    importing
      !I_MSG type STRING optional
    raising
      ZCX_IASM_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IASM_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
me->MSG = MSG .
endmethod.


method raise.

  data sys_call    type sys_calls.
  data sys_stack   type sys_callst.
  data ex          type ref to zcx_iasm_error.

  call function 'SYSTEM_CALLSTACK'
    exporting max_level    = 2
    importing et_callstack = sys_stack.

  read table sys_stack into sys_call index 2.

  create object ex.
  ex->msg = sys_call-eventname && `(): ` && i_msg.
  raise exception ex.

endmethod.
ENDCLASS.
