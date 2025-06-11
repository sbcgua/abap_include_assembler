class lcl_cloner_app definition final.
  public section.

    methods constructor
      importing
        i_classes         type zif_iasm_types=>tt_class_names
        i_progs           type zif_iasm_types=>tt_prog_names
        i_rename_from     type seoclasstx-clsname
        i_rename_to       type seoclasstx-clsname
        i_target_pkg      type devclass
        i_extra_renames   type zif_iasm_types=>tt_renames
      raising
        zcx_iasm_error.

    methods run
      raising
        zcx_iasm_error.

  private section.

    types:
      begin of ty_obj,
        obj_type type tadir-object,
        obj_name type tadir-obj_name,
      end of ty_obj.

    data:
      m_objects         type standard table of ty_obj,
      m_renames         type zif_iasm_types=>ts_renames,
      m_target_package  type devclass.

endclass.

class lcl_cloner_app implementation.

  method constructor.

    if i_rename_from is initial or i_rename_to is initial.
      zcx_iasm_error=>raise( |Rename pattern is not specified| ).
    endif.

    m_target_package = i_target_pkg.

    field-symbols <c> like line of i_classes.
    field-symbols <p> like line of i_progs.
    field-symbols <obj> like line of m_objects.

    loop at i_classes assigning <c>.
      append initial line to m_objects assigning <obj>.
      <obj>-obj_type = zcl_iasm_cloner=>oo_auto.
      <obj>-obj_name = to_upper( <c> ).
    endloop.

    loop at i_progs assigning <p>.
      append initial line to m_objects assigning <obj>.
      <obj>-obj_type = 'PROG'.
      <obj>-obj_name = to_upper( <p> ).
    endloop.

    data r like line of m_renames.

    loop at m_objects assigning <obj>.
      r-from = to_lower( <obj>-obj_name ).
      r-to   = to_lower( replace(
        val  = <obj>-obj_name
        sub  = i_rename_from
        with = i_rename_to ) ).
      if r-from = r-to.
        zcx_iasm_error=>raise( |Object is not renamed { r-from }| ).
      endif.
      insert r into table m_renames.
    endloop.

    insert lines of i_extra_renames into table m_renames.

  endmethod.

  method run.

    data ls_req type trwbo_request_header.
    call function 'TR_REQUEST_CHOICE'
      exporting
        iv_request_types = 'K'
      importing
        es_request = ls_req
      exceptions
        others = 4.
    if ls_req is initial.
      zcx_iasm_error=>raise( 'Please specify the transport' ).
    endif.

    data cloner type ref to zcl_iasm_cloner.
    field-symbols <obj> like line of m_objects.

    loop at m_objects assigning <obj>.

      create object cloner
        exporting
          i_obj_type       = <obj>-obj_type
          i_obj_name       = <obj>-obj_name
          i_target_package = m_target_package
          i_trans          = ls_req-trkorr
          i_verbose        = abap_true
          it_renames       = m_renames.
      cloner->clone( ).

    endloop.

  endmethod.

endclass.
