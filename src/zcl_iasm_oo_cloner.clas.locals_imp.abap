class lcl_pseudo_xml definition final.
  public section.
    interfaces zif_abapgit_xml_input.
    interfaces zif_abapgit_xml_output.

    types:
      begin of ty_stash_item,
        node type string,
        data type ref to data,
      end of ty_stash_item.

    data mt_stash type standard table of ty_stash_item.

    class-methods new returning value(ro) type ref to lcl_pseudo_xml.
    methods get_ref
      importing
        iv_node type string
      returning
        value(r_ref) type ref to data.

endclass.

class lcl_pseudo_xml implementation.

  method new.
    create object ro.
  endmethod.

  method get_ref.

    field-symbols <stash> like line of mt_stash.
    read table mt_stash assigning <stash> with key node = iv_node.
    if sy-subrc = 0.
      r_ref = <stash>-data.
    endif.

  endmethod.

  method zif_abapgit_xml_output~add.

    data stash like line of mt_stash.
    field-symbols <data> type any.

    stash-node = iv_name.
    create data stash-data like ig_data.
    assign stash-data->* to <data>.
    <data> = ig_data.

    append stash to mt_stash.

  endmethod.

  method zif_abapgit_xml_output~set_raw.
  endmethod.
  method zif_abapgit_xml_output~add_xml.
  endmethod.
  method zif_abapgit_xml_output~render.
  endmethod.

  method zif_abapgit_xml_input~read.

    field-symbols <stash> like line of mt_stash.
    field-symbols <data> type any.

    read table mt_stash assigning <stash> with key node = iv_name.
    if sy-subrc = 0.
      assign <stash>-data->* to <data>.
      cg_data = <data>.
    endif.

  endmethod.

  method zif_abapgit_xml_input~get_raw.
  endmethod.
  method zif_abapgit_xml_input~get_metadata.
  endmethod.

endclass.

**********************************************************************

class lcl_functions definition final.
  public section.

    class-methods get_object_type
      importing
        iv_obj_name type tadir-obj_name
      returning
        value(rv_type) type tadir-object.
    class-methods serialize
      importing
        is_item type zif_abapgit_definitions=>ty_item
      exporting
        e_files type ref to zcl_abapgit_objects_files
        e_xml type ref to lcl_pseudo_xml
      raising
        zcx_iasm_error.
    class-methods deserialize
      importing
        is_item type zif_abapgit_definitions=>ty_item
        i_files type ref to zcl_abapgit_objects_files
        i_xml type ref to lcl_pseudo_xml
      returning
        value(ri_log) type ref to zif_abapgit_log
      raising
        zcx_iasm_error.
    class-methods put_to_transport
      importing
        is_item type zif_abapgit_definitions=>ty_item
        i_trans type e070-trkorr
      raising
        zcx_iasm_error.
    class-methods set_default_transport
      importing
        i_trans type e070-trkorr
      raising
        zcx_iasm_error.

endclass.

class lcl_functions implementation.

  method get_object_type.

    " class or interface -> SEOCLASS table or TADIR
    if zcl_abapgit_factory=>get_tadir( )->read_single(
      iv_object   = 'CLAS'
      iv_obj_name = iv_obj_name ) is not initial.
      rv_type = 'CLAS'.
    elseif zcl_abapgit_factory=>get_tadir( )->read_single(
      iv_object = 'INTF'
      iv_obj_name = iv_obj_name ) is not initial.
      rv_type = 'INTF'.
*    else.
    endif.

  endmethod.

  method serialize.

    data lx_ag type ref to zcx_abapgit_exception.
    data ser type ref to zif_abapgit_object.
    data lo_i18n_params type ref to zcl_abapgit_i18n_params.

    try.

      e_files = zcl_abapgit_objects_files=>new( is_item ).
      e_xml   = lcl_pseudo_xml=>new( ).
      lo_i18n_params = zcl_abapgit_i18n_params=>new(
        iv_main_language      = 'E'
        iv_main_language_only = abap_true ).

      if is_item-obj_type = 'INTF'.
        create object ser type zcl_abapgit_object_intf
          exporting
            is_item        = is_item
            iv_language    = 'E'
            io_files       = e_files
            io_i18n_params = lo_i18n_params.
      elseif is_item-obj_type = 'CLAS'.
        create object ser type zcl_abapgit_object_clas
          exporting
            is_item        = is_item
            iv_language    = 'E'
            io_files       = e_files
            io_i18n_params = lo_i18n_params.
      else.
        zcx_iasm_error=>raise( |Unexpected type of object { is_item-obj_type }| ).
      endif.

      ser->serialize( e_xml ).

    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.

  method deserialize.

    data lx_ag type ref to zcx_abapgit_exception.
    data deser type ref to zif_abapgit_object.
    data lo_i18n_params type ref to zcl_abapgit_i18n_params.

    try.

      create object ri_log type zcl_abapgit_log.
      lo_i18n_params = zcl_abapgit_i18n_params=>new(
        iv_main_language      = 'E'
        iv_main_language_only = abap_true ).

      if is_item-obj_type = 'INTF'.
        create object deser type zcl_abapgit_object_intf
          exporting
            is_item        = is_item
            iv_language    = 'E'
            io_files       = i_files
            io_i18n_params = lo_i18n_params.
      elseif is_item-obj_type = 'CLAS'.
        create object deser type zcl_abapgit_object_clas
          exporting
            is_item        = is_item
            iv_language    = 'E'
            io_files       = i_files
            io_i18n_params = lo_i18n_params.
      else.
        zcx_iasm_error=>raise( |Unexpected type of object { is_item-obj_type }| ).
      endif.

      data lt_steps type zif_abapgit_objects=>ty_deserialization_step_tt.
      data step like line of lt_steps.
      lt_steps = deser->get_deserialize_steps( ).

      loop at lt_steps into step.
        deser->deserialize(
          iv_package   = is_item-devclass
          io_xml       = i_xml
          iv_step      = step
          ii_log       = ri_log
          iv_transport = '' ).
      endloop.

    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.

  method put_to_transport.

    data lx_ag type ref to zcx_abapgit_exception.
    data l_ord type e070-trkorr.
    data l_task type e070-trkorr.

    try.
      call function 'RS_CORR_INSERT'
        exporting
          object              = is_item-obj_name
          object_class        = is_item-obj_type
          devclass            = is_item-devclass
          master_language     = 'E'
          mode                = 'I'
          global_lock         = abap_true
          suppress_dialog     = abap_true
          korrnum             = i_trans
        importing
          korrnum  = l_ord
          ordernum = l_task
        exceptions
          cancelled           = 1
          permission_failure  = 2
          unknown_objectclass = 3
          others              = 4.
      if sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      endif.
    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.

  method set_default_transport.

    data lx_ag type ref to zcx_abapgit_exception.

    try.
      if i_trans is initial.
        zcl_abapgit_factory=>get_default_transport( )->reset( ).
      else.
        zcl_abapgit_factory=>get_default_transport( )->set( i_trans ).
      endif.
    catch zcx_abapgit_exception into lx_ag.
      zcx_iasm_error=>raise( lx_ag->get_text( ) ).
    endtry.

  endmethod.

endclass.
