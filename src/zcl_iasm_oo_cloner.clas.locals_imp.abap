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

    methods get_ref
      importing
        iv_node type string
      returning
        value(r_ref) type ref to data.

endclass.

class lcl_pseudo_xml implementation.

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
