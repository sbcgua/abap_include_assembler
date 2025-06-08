class lcl_cloner_app definition final.
  public section.

    methods constructor
      importing
        i_classes         type zif_iasm_types=>tt_class_names
        i_rename_from     type seoclasstx-clsname
        i_rename_to       type seoclasstx-clsname
        i_target_pkg      type devclass
      raising
        zcx_iasm_error.

    methods run
      raising
        zcx_iasm_error.

  private section.
    data:
      m_renames         type zif_iasm_types=>ts_renames,
      m_classes         type zif_iasm_types=>tt_class_names,
      m_target_package  type devclass.
endclass.

class lcl_cloner_app implementation.

  method constructor.

    m_classes        = i_classes.
    m_target_package = i_target_pkg.

    field-symbols <i> like line of m_classes.
    data r like line of m_renames.

    if i_rename_from is not initial and i_rename_to is not initial.
      loop at m_classes assigning <i>.
        r-from = to_lower( <i> ).
        r-to   = to_lower( replace( val = <i> sub = i_rename_from with = i_rename_to ) ).
        if r-from = r-to.
          zcx_iasm_error=>raise( |class is not renamed { r-from }| ).
        endif.
        insert r into table m_renames.
      endloop.
    endif.

  endmethod.

  method run.

    write: 'Hello from cloner'.

    field-symbols <c> like line of m_classes.
    loop at m_classes assigning <c>.

      data cloner type ref to zcl_iasm_oo_cloner.
      create object cloner
        exporting
          i_class = <c>
          i_target_package = m_target_package
          it_renames = m_renames.
      cloner->clone( ).
    endloop.

  endmethod.

endclass.
