class ZCL_IASM_OO_CLONER definition
  public
  final
  create public.

  public section.

    methods constructor
      importing
        i_class type seoclsname
        it_renames type zif_iasm_types=>ts_renames
        i_target_package type devclass
        i_trans type e070-trkorr
        i_verbose type abap_bool default abap_false
      raising
        zcx_iasm_error.
    methods clone
      raising
        zcx_iasm_error.

  protected section.
  private section.

    data m_class type seoclsname.
    data m_class_to type seoclsname.
    data mt_renames type zif_iasm_types=>ts_renames.
    data m_target_package type devclass.
    data m_trans type e070-trkorr.
    data m_verbose type abap_bool.

    methods patch_clas
      importing
        i_dst_item type zif_abapgit_definitions=>ty_item
        i_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.

    methods patch_intf
      importing
        i_dst_item type zif_abapgit_definitions=>ty_item
        i_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.

    methods patch_code
      importing
        i_files type ref to zcl_abapgit_objects_files
      raising
        zcx_iasm_error.

ENDCLASS.



CLASS ZCL_IASM_OO_CLONER IMPLEMENTATION.


  method clone.

    if m_verbose = abap_true.
      data lv_pkg_str type string.
      lv_pkg_str = |({ m_target_package })|.
      write: / 'Cloning:', m_class, '->', m_class_to, lv_pkg_str.
    endif.

    data ls_src_item type zif_abapgit_definitions=>ty_item.
    data ls_dst_item type zif_abapgit_definitions=>ty_item.

    ls_src_item-obj_name = m_class.
    ls_src_item-obj_type = lcl_functions=>get_object_type( |{ m_class }| ).
    if ls_src_item-obj_type is initial.
      write: / 'Only CLAS/INTF objects are supported yet', m_class.
      return.
    endif.

    ls_dst_item-obj_type = ls_src_item-obj_type.
    ls_dst_item-obj_name = m_class_to.
    ls_dst_item-devclass = m_target_package.

    data files type ref to zcl_abapgit_objects_files.
    data lo_xml type ref to lcl_pseudo_xml.

    lcl_functions=>serialize(
      exporting
        is_item = ls_src_item
      importing
        e_files = files
        e_xml   = lo_xml ).

    case ls_src_item-obj_type.
      when 'CLAS'.
        patch_clas(
          i_dst_item = ls_dst_item
          i_xml      = lo_xml ).
        patch_code( i_files = files ).
      when 'INTF'.
        patch_intf(
          i_dst_item = ls_dst_item
          i_xml      = lo_xml ).
        patch_code( i_files = files ).
      when others.
        zcx_iasm_error=>raise( |Unexpected type of object { ls_src_item-obj_type }| ).
    endcase.

    lcl_functions=>set_default_transport( m_trans ).

    data log type ref to zif_abapgit_log.
    log = lcl_functions=>deserialize(
      is_item = ls_dst_item
      i_files = files
      i_xml   = lo_xml ).

    lcl_functions=>set_default_transport( '' ). " reset

*      zcl_abapgit_factory=>get_default_transport( )->set( 'SBSK900746' ).
*      zcl_abapgit_factory=>get_default_transport( )->reset( ).
*      zcl_abapgit_factory=>get_cts_api( )->insert_transport_object( ... )
*      zcl_abapgit_objects=>update_package_tree?

    lcl_functions=>put_to_transport(
      is_item = ls_dst_item
      i_trans = m_trans ).

  endmethod.


  method constructor.

    m_class          = to_upper( i_class ).
    mt_renames       = it_renames.
    m_target_package = to_upper( i_target_package ).
    m_trans          = i_trans.
    m_verbose        = i_verbose.

    data r like line of it_renames.
    loop at it_renames into r.
      if to_lower( r-from ) = to_lower( i_class ).
        m_class_to = to_upper( r-to ).
        exit.
      endif.
    endloop.

    if m_class_to is initial.
      zcx_iasm_error=>raise( |Cannot rename class { i_class }| ).
    endif.

    assert m_class is not initial.
    assert m_class_to is not initial.
    assert m_target_package is not initial.

  endmethod.


  method patch_clas.

    data lr_props_c type ref to vseoclass.

    lr_props_c ?= i_xml->get_ref( 'VSEOCLASS' ).
    lr_props_c->clsname = i_dst_item-obj_name.
    lr_props_c->with_unit_tests = ''.

  endmethod.


  method patch_code.

    data lx_ag type ref to zcx_abapgit_exception.
    data files_data type zif_abapgit_git_definitions=>ty_files_tt.
    field-symbols <file> like line of files_data.

    try.
      files_data = i_files->get_files( ).

      loop at files_data assigning <file>.
        if <file>-filename cp '*.clas.testclasses.abap'.
          delete files_data index sy-tabix.
          continue.
        endif.
        if not <file>-filename cp '*.abap'.
          continue. " ???
        endif.

        data lt_codetab type string_table.

        split zcl_abapgit_convert=>xstring_to_string_utf8( <file>-data ) at cl_abap_char_utilities=>newline into table lt_codetab.

        zcl_iasm_utils=>apply_renames(
          exporting
            it_renames = mt_renames
          changing
            ct_codetab = lt_codetab ).

        <file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( concat_lines_of(
          table = lt_codetab
          sep   = cl_abap_char_utilities=>newline ) ).

      endloop.

      i_files->set_files( files_data ).

    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.


  method patch_intf.

    data lr_props_i type ref to vseointerf.

    lr_props_i ?= i_xml->get_ref( 'VSEOINTERF' ).
    lr_props_i->clsname = i_dst_item-obj_name.

  endmethod.
ENDCLASS.
