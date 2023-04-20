* This is a reduced copy of the class from https://github.com/abapGit/abapGit
* MIT licenced to Lars Hvam Petersen and abapGit Contributors

CLASS zcl_abapgit_oo_serializer DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS serialize_abap_clif_source
      IMPORTING
        !is_class_key    TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table
      RAISING
        cx_sy_dyn_call_error .
    METHODS serialize_locals_imp
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table.
    METHODS serialize_locals_def
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table.
    METHODS serialize_macros
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS serialize_abap_new
      IMPORTING
        !is_clskey       TYPE seoclskey
      RETURNING
        VALUE(rt_source) TYPE string_table
      RAISING
        cx_sy_dyn_call_error .
    METHODS read_include
      IMPORTING
        !is_clskey       TYPE seoclskey
        !iv_type         TYPE seop_include_ext_app
      RETURNING
        VALUE(rt_source) TYPE seop_source_string .
    METHODS reduce
      CHANGING
        !ct_source TYPE string_table .
ENDCLASS.

CLASS ZCL_ABAPGIT_OO_SERIALIZER IMPLEMENTATION.

  METHOD read_include.

    DATA: ls_include TYPE progstruc.


    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    READ REPORT ls_include INTO rt_source STATE 'A'.

  ENDMETHOD.

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE abap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source.
    ENDIF.

  ENDMETHOD.

  METHOD serialize_abap_clif_source.
    rt_source = serialize_abap_new( is_class_key ).
  ENDMETHOD.

  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.

  METHOD serialize_locals_def.

    rt_source = read_include(
      is_clskey = is_clskey
      iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.

  METHOD serialize_locals_imp.

    rt_source = read_include(
      is_clskey = is_clskey
      iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.

  METHOD serialize_macros.

    rt_source = read_include(
      is_clskey = is_clskey
      iv_type = seop_ext_class_macros ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.

ENDCLASS.
