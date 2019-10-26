**********************************************************************
* INTERFACE
**********************************************************************

interface lif_devobj_accessor.
  methods get_code
    importing
      i_progname       type sobj_name
    returning
      value(r_codetab) type string_table
    raising
      lcx_error.
  methods get_devc
    importing
      i_progname       type sobj_name
    returning
      value(r_devc)    type devclass
    raising
      lcx_error.
endinterface.

interface lif_devobj_saver.
  methods save
    importing
      i_path    type string
      i_codetab type string_table
    raising
      lcx_error.
endinterface.
