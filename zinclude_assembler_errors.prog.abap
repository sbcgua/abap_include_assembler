**********************************************************************
* EXCEPTIONS
**********************************************************************
class lcx_error definition inheriting from cx_static_check final.
  public section.
    data msg type string.
    class-methods raise importing i_msg type string optional raising lcx_error.
endclass.

class lcx_error implementation.
  method raise.
    data sys_call    type sys_calls.
    data sys_stack   type sys_callst.
    data ex          type ref to lcx_error.

    call function 'SYSTEM_CALLSTACK'
      exporting max_level    = 2
      importing et_callstack = sys_stack.

    read table sys_stack into sys_call index 2.

    create object ex.
    ex->msg = sys_call-eventname && `(): ` && i_msg.
    raise exception ex.
  endmethod.
endclass.
