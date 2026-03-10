class ZCL_IASM_CLONER definition
  public
  final
  create public.

  public section.

    constants oo_auto type tadir-object value 'OO'. "#EC NOTEXT

    methods constructor
      importing
        !i_obj_type type tadir-object
        !i_obj_name type tadir-obj_name
        !it_renames type zif_iasm_types=>ts_renames
        !i_target_package type devclass
        !i_trans type e070-trkorr
        !i_verbose type abap_bool default abap_false
      raising
        zcx_iasm_error.
    methods clone
      raising
        zcx_iasm_error.

  protected section.
  private section.

    data mt_renames type zif_iasm_types=>ts_renames.
    data m_trans    type e070-trkorr.
    data m_verbose  type abap_bool.

    data ms_src_item type zif_abapgit_definitions=>ty_item.
    data ms_dst_item type zif_abapgit_definitions=>ty_item.

    methods patch_clas
      importing
        i_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.

    methods patch_intf
      importing
        i_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.

    methods patch_prog
      importing
        i_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.

    methods patch_code
      importing
        i_files type ref to zcl_abapgit_objects_files
      raising
        zcx_iasm_error.

ENDCLASS.



CLASS ZCL_IASM_CLONER IMPLEMENTATION.


  method clone.

    if m_verbose = abap_true.
      data lv_pkg_str type string.
      lv_pkg_str = |({ ms_dst_item-devclass })|.
      write: / 'Cloning:', ms_src_item-obj_type, ms_src_item-obj_name, '->', ms_dst_item-obj_name, lv_pkg_str.
    endif.

    data files type ref to zcl_abapgit_objects_files.
    data lo_xml type ref to lcl_pseudo_xml.

    lcl_functions=>serialize(
      exporting
        is_item = ms_src_item
      importing
        e_files = files
        e_xml   = lo_xml ).

    case ms_src_item-obj_type.
      when 'CLAS'.
        patch_clas( lo_xml ).
        patch_code( files ).
      when 'INTF'.
        patch_intf( lo_xml ).
        patch_code( files ).
      when 'PROG'.
        patch_prog( lo_xml ).
        patch_code( files ).
      when others.
        zcx_iasm_error=>raise( |Unexpected type of object { ms_src_item-obj_type }| ).
    endcase.

    lcl_functions=>set_default_transport( m_trans ).

    data log type ref to zif_abapgit_log.
    log = lcl_functions=>deserialize(
      is_item = ms_dst_item
      i_files = files
      i_xml   = lo_xml ).

    lcl_functions=>set_default_transport( '' ). " reset

*      zcl_abapgit_factory=>get_default_transport( )->set( 'SBSK900746' ).
*      zcl_abapgit_factory=>get_default_transport( )->reset( ).
*      zcl_abapgit_factory=>get_cts_api( )->insert_transport_object( ... )
*      zcl_abapgit_objects=>update_package_tree?

    lcl_functions=>put_to_transport(
      is_item = ms_dst_item
      i_trans = m_trans ).

  endmethod.


  method constructor.

    if i_obj_type is initial.
      zcx_iasm_error=>raise( |i_obj_type cannot be empty| ).
    endif.
    if i_obj_name is initial.
      zcx_iasm_error=>raise( |i_obj_name cannot be empty| ).
    endif.
    if it_renames is initial.
      zcx_iasm_error=>raise( |it_renames cannot be empty| ).
    endif.
    if i_target_package is initial.
      zcx_iasm_error=>raise( |i_target_package cannot be empty| ).
    endif.

    ms_src_item-obj_name = to_upper( i_obj_name ).
    if i_obj_type = oo_auto. " Autodetect class / interface
      ms_src_item-obj_type = lcl_functions=>get_object_type( ms_src_item-obj_name ).
      if ms_src_item-obj_type is initial.
        zcx_iasm_error=>raise( |i_obj_type was not detected| ).
      endif.
    else.
      ms_src_item-obj_type = i_obj_type. " PROG
    endif.

    mt_renames = it_renames.
    m_trans    = i_trans.
    m_verbose  = i_verbose.

    ms_dst_item-obj_type = ms_src_item-obj_type.
    ms_dst_item-devclass = to_upper( i_target_package ).

    data r like line of it_renames.
    loop at it_renames into r.
      if to_lower( r-from ) = to_lower( ms_src_item-obj_name ).
        ms_dst_item-obj_name = to_upper( r-to ).
        exit.
      endif.
    endloop.

    if ms_dst_item-obj_name is initial.
      zcx_iasm_error=>raise( |Cannot rename { ms_src_item-obj_name }| ).
    endif.

  endmethod.


  method patch_clas.

    data lr_props_c type ref to vseoclass.

    lr_props_c ?= i_xml->get_ref( 'VSEOCLASS' ).
    lr_props_c->clsname = ms_dst_item-obj_name.
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
    lr_props_i->clsname = ms_dst_item-obj_name.

  endmethod.


  method patch_prog.

    data lr_props_p type ref to data.
    field-symbols <struc> type any.
    field-symbols <progname> type progdir-name.

    lr_props_p = i_xml->get_ref( 'PROGDIR' ).
    assign lr_props_p->* to <struc>.
    assign component 'NAME' of structure <struc> to <progname>.
    <progname> = ms_dst_item-obj_name.

  endmethod.
ENDCLASS.
