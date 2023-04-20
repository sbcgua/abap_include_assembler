interface zif_iasm_devobj_saver
  public.

  methods save
    importing
      i_path    type string
      i_codetab type string_table
    raising
      zcx_iasm_error.

endinterface.
