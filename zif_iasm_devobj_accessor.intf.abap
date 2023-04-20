interface zif_iasm_devobj_accessor
  public.

  methods get_code
    importing
      i_progname       type sobj_name
    returning
      value(r_codetab) type string_table
    raising
      zcx_iasm_error.
  methods get_devc
    importing
      i_progname       type sobj_name
    returning
      value(r_devc)    type devclass
    raising
      zcx_iasm_error.

endinterface.
