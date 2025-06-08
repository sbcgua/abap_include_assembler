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

ENDCLASS.



CLASS ZCL_IASM_OO_CLONER IMPLEMENTATION.


  method clone.

    data ls_src_item type zif_abapgit_definitions=>ty_item.
    data ls_dst_item type zif_abapgit_definitions=>ty_item.

    " class or interface -> SEOCLASS table or TADIR
    if zcl_abapgit_factory=>get_tadir( )->read_single(
      iv_object   = 'CLAS'
      iv_obj_name = |{ m_class }| ) is not initial.
      ls_src_item-obj_type = 'CLAS'.
    elseif zcl_abapgit_factory=>get_tadir( )->read_single(
      iv_object = 'INTF'
      iv_obj_name = |{ m_class }| ) is not initial.
      ls_src_item-obj_type = 'INTF'.
    else.
      write: / 'Only CLAS/INTF objects are supported yet', m_class.
      return.
    endif.

    ls_src_item-obj_name = m_class.

    ls_dst_item-obj_type = ls_src_item-obj_type.
    ls_dst_item-obj_name = m_class_to.
    ls_dst_item-devclass = m_target_package.



    data lx_ag type ref to zcx_abapgit_exception.
    try.

      data lo_i18n_params type ref to zcl_abapgit_i18n_params.
      lo_i18n_params = zcl_abapgit_i18n_params=>new(
        iv_main_language = 'E'
        iv_main_language_only = abap_true ).

      data ser type ref to zif_abapgit_object.
      data files type ref to zcl_abapgit_objects_files.
      files = zcl_abapgit_objects_files=>new( ls_src_item ).

      create object ser type zcl_abapgit_object_clas
        exporting
          is_item        = ls_src_item
          iv_language    = 'E'
          io_files       = files
          io_i18n_params = lo_i18n_params.

      if ls_src_item-obj_type = 'INTF'.
        create object ser type zcl_abapgit_object_intf
          exporting
            is_item        = ls_src_item
            iv_language    = 'E'
            io_files       = files
            io_i18n_params = lo_i18n_params.
      else.
        create object ser type zcl_abapgit_object_clas
          exporting
            is_item        = ls_src_item
            iv_language    = 'E'
            io_files       = files
            io_i18n_params = lo_i18n_params.
      endif.

      data lo_xml type ref to lcl_pseudo_xml.
      create object lo_xml.

      ser->serialize( lo_xml ).

*      data str type string.
*      str = li_xml->render( ).

      " Patch
      data lr_props_c type ref to vseoclass.
      data lr_props_i type ref to vseointerf.

      lr_props_c ?= lo_xml->get_ref( 'VSEOCLASS' ).
      if lr_props_c is not initial.
        lr_props_c->clsname = ls_dst_item-obj_name.
        lr_props_c->with_unit_tests = ''.
      else.
        lr_props_i ?= lo_xml->get_ref( 'VSEOINTERF' ).
        lr_props_i->clsname = ls_dst_item-obj_name.
      endif.

      data files_data type zif_abapgit_git_definitions=>ty_files_tt.
      field-symbols <file> like line of files_data.
      files_data = files->get_files( ).

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

        <file>-data = zcl_abapgit_convert=>string_to_xstring_utf8( concat_lines_of( table = lt_codetab sep = cl_abap_char_utilities=>newline ) ).

      endloop.

      files->set_files( files_data ).

      data deser type ref to zif_abapgit_object.

      if ls_src_item-obj_type = 'INTF'.
        create object deser type zcl_abapgit_object_intf
          exporting
            is_item        = ls_dst_item
            iv_language    = 'E'
            io_files       = files
            io_i18n_params = lo_i18n_params.
      else.
        create object deser type zcl_abapgit_object_clas
          exporting
            is_item        = ls_dst_item
            iv_language    = 'E'
            io_files       = files
            io_i18n_params = lo_i18n_params.
      endif.

      data lt_steps type zif_abapgit_objects=>ty_deserialization_step_tt.
      data step like line of lt_steps.
      lt_steps = deser->get_deserialize_steps( ).

      data log type ref to zif_abapgit_log.
      create object log type zcl_abapgit_log.

      zcl_abapgit_factory=>get_default_transport( )->set( 'SBSK900746' ).

      loop at lt_steps into step.
        deser->deserialize(
          iv_package   = ls_dst_item-devclass
          io_xml       = lo_xml
          iv_step      = step
          ii_log       = log
          iv_transport = 'SBSK900746'
        ).
      endloop.

      zcl_abapgit_factory=>get_default_transport( )->reset( ).

      " ZCL_ABAPGIT_OBJECTS=>UPDATE_PACKAGE_TREE?

      assert 1 = 1.

    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.


  method constructor.

    m_class          = to_upper( i_class ).
    mt_renames       = it_renames.
    m_target_package = to_upper( i_target_package ).

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
ENDCLASS.
