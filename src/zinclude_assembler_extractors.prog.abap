**********************************************************************
* INTERFACE IMPLEMENTATION FOR CODE EXTRACTION
**********************************************************************

class lcl_extractor_prog definition final.
  public section.
    interfaces zif_iasm_devobj_accessor.
endclass.

class lcl_extractor_prog implementation.

  method zif_iasm_devobj_accessor~get_code.
    data l_status type c.

    call function 'CHECK_EXIST'
      exporting
        iv_pgmid    = 'R3TR'
        iv_object   = 'PROG'
        iv_obj_name = i_progname
      importing
        e_exist     = l_status
      exceptions
        others     = 1.

    if sy-subrc <> 0 or l_status <> 'X'. " Exist and active
      zcx_iasm_error=>raise( |check_exist { i_progname }| ).  "#EC NOTEXT
    endif.

    data ls_tadir type tadir.
    select single * into corresponding fields of ls_tadir
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |select tadir { i_progname }| ).  "#EC NOTEXT
    endif.

    data lt_codetab type abaptxt255_tab.

    call function 'RPY_PROGRAM_READ'
      exporting
        program_name     = i_progname
        with_lowercase   = 'X'
      tables
        source_extended  = lt_codetab
      exceptions
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        others           = 4.
    if sy-subrc is not initial.
      zcx_iasm_error=>raise( |Cannot read program| ).  "#EC NOTEXT
    endif.

    field-symbols <c> like line of lt_codetab.
    loop at lt_codetab assigning <c>.
      append <c>-line to r_codetab.
    endloop.

  endmethod.

  method zif_iasm_devobj_accessor~get_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |Cannot find devclass { i_progname }| ).  "#EC NOTEXT
    endif.

  endmethod.

endclass.

class lcl_extractor_clas definition final.
  public section.
    interfaces zif_iasm_devobj_accessor.

    methods strip_public " public to be testable, improve
      changing
        ct_source type string_table
      raising
        zcx_iasm_error.

  private section.
    data mv_obj_name type string.
    methods get_obj_type
      importing
        iv_obj type tadir-obj_name
      returning
        value(rv_type) type tadir-object.
endclass.

class lcl_extractor_clas implementation.

  method zif_iasm_devobj_accessor~get_code.

    data lt_source_part type string_table.
    data ls_class_key type seoclskey.
    data lo_serializer type ref to zcl_abapgit_oo_serializer.

    create object lo_serializer.

    ls_class_key-clsname = i_progname.

    if get_obj_type( i_progname ) = 'CLAS'.
      lt_source_part = lo_serializer->serialize_macros( ls_class_key ).
      if lt_source_part is not initial.
        append lines of lt_source_part to r_codetab.
      endif.

      lt_source_part = lo_serializer->serialize_locals_def( ls_class_key ).
      if lt_source_part is not initial.
        append lines of lt_source_part to r_codetab.
      endif.

      lt_source_part = lo_serializer->serialize_locals_imp( ls_class_key ).
      if lt_source_part is not initial.
        append lines of lt_source_part to r_codetab.
      endif.
    endif.

    lt_source_part = lo_serializer->serialize_abap_clif_source( ls_class_key ).
    strip_public( changing ct_source = lt_source_part ).

    append lines of lt_source_part to r_codetab.

  endmethod.

  method strip_public.

    data lv_in_definition type abap_bool.
    data lv_idx type i.
    field-symbols <s> like line of ct_source.

    " Simplified stripper
    " expects public in a separate line, create public should not be splitted as well

    loop at ct_source assigning <s>.
      lv_idx = sy-tabix.

      if lv_in_definition = abap_false.
        if find( val = <s> regex = '^\s*class\s+\w+\s+definition' case = abap_false ) = -1 and
          find( val = <s> regex = '^\s*interface\s+\w+' case = abap_false ) = -1.
          continue.
        endif.
        lv_in_definition = abap_true.
      else.
        if find( val = <s> regex = '^\s*public(\.|\s|$)' case = abap_false ) <> -1.
          <s> = replace( val = <s> sub = 'public' case = abap_false with = '' ).
          if <s> co ` ` or <s> is initial.
            delete ct_source index lv_idx.
          endif.
          lv_in_definition = abap_false.
          exit. " no more publics
        elseif <s> ca '.'.
          lv_in_definition = abap_false.
          exit. " no more publics
        endif.
      endif.

    endloop.

    if lv_in_definition = abap_true.
      zcx_iasm_error=>raise( |Public stripper: could not detect definition bounds in { mv_obj_name }| ) .
    endif.

  endmethod.

  method get_obj_type.

    select single object into rv_type
      from tadir
      where pgmid    = 'R3TR'
      and   ( object = 'CLAS' or object = 'INTF' )
      and   obj_name = iv_obj.

  endmethod.

  method zif_iasm_devobj_accessor~get_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   ( object = 'CLAS' or object = 'INTF' )
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |Cannot find devclass { i_progname }| ).  "#EC NOTEXT
    endif.

  endmethod.

endclass.



**********************************************************************
* TEST  - CODE EXTRACTOR
**********************************************************************

class ltcl_extractor_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods get_prog_code for testing.
endclass.

class ltcl_extractor_test implementation.
  method get_prog_code.
    data lo_if   type ref to zif_iasm_devobj_accessor.
    data lo_obj  type ref to lcl_extractor_prog.
    data lt_code type string_table.

    create object lo_obj.
    lo_if ?= lo_obj.

    try.
      clear sy-subrc.
      lt_code = lo_if->get_code( i_progname = '~~DOES~NOT~EXIST~~' ).
    catch zcx_iasm_error.
      sy-subrc = 1.
    endtry.

    cl_abap_unit_assert=>assert_subrc( act = sy-subrc  exp = 1 ).

    try.
      clear sy-subrc.
      lt_code = lo_if->get_code( i_progname = sy-cprog ).
    catch zcx_iasm_error.
      sy-subrc = 1.
    endtry.

    cl_abap_unit_assert=>assert_subrc( act = sy-subrc  exp = 0 ).
    cl_abap_unit_assert=>assert_not_initial( act = lines( lt_code ) ).

  endmethod.
endclass.

**********************************************************************
* TEST stripper
**********************************************************************

class ltcl_strip_public definition for testing risk level harmless duration short.
  public section.
    methods test_clas_strip for testing raising zcx_iasm_error.
    methods test_intf_strip for testing raising zcx_iasm_error.
endclass.
class ltcl_strip_public implementation.
  method test_clas_strip.
    data lo type ref to lcl_extractor_clas.
    data lt_act type string_table.
    data lt_exp type string_table.

    create object lo.

    append ' class ZCL_XXX DEFINITION inheriting from YYY' to lt_act.
    append '  public' to lt_act.
    append '  create public ' to lt_act.
    append '  .' to lt_act.
    lt_exp = lt_act.
    delete lt_exp index 2.

    lo->strip_public( changing ct_source = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.

  method test_intf_strip.
    data lo type ref to lcl_extractor_clas.
    data lt_act type string_table.
    data lt_exp type string_table.

    create object lo.

    append ' interface ZIF_XXX' to lt_act.
    append '  public .' to lt_act.
    lt_exp = lt_act.
    delete lt_exp index 2.
    append '   .' to lt_exp.

    lo->strip_public( changing ct_source = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  endmethod.
endclass.


**********************************************************************
* INTERFACE IMPLEMENTATION FOR DUMMY EXTRACTION - tests and example
**********************************************************************

class ltcl_dummy_extractor definition final.
  public section.
    interfaces zif_iasm_devobj_accessor.
endclass.

class ltcl_dummy_extractor implementation.

  method zif_iasm_devobj_accessor~get_code.
    data lt_code like r_codetab.
    data l_line  like line of lt_code.

    define append_codeline.
      l_line = &1.
      append l_line to lt_code.
    end-of-definition.

    case i_progname.
      when 'XTESTPROG'.
        append_codeline 'report xtestprog.'.              "#EC NOTEXT
        append_codeline 'include xtestprog_top.'.         "#EC NOTEXT
        append_codeline 'include xtestprog_f01.'.         "#EC NOTEXT
        append_codeline 'include xtestprog_ext.'.         "#EC NOTEXT
        append_codeline 'start-of-selection.'.            "#EC NOTEXT
        append_codeline '  perform perform_write.'.       "#EC NOTEXT
      when 'XTESTPROG_TOP'.
        append_codeline 'include xtestprog_doc.'.                     "#EC NOTEXT
        append_codeline 'constants gstr type char4 value ''Test''.'.  "#EC NOTEXT
        append_codeline 'types: begin of t_sometype.'.                "#EC NOTEXT
        append_codeline '        include structure textpool.'.        "#EC NOTEXT
        append_codeline 'types: end of t_sometype.'.                  "#EC NOTEXT
      when 'XTESTPROG_F01'.
        append_codeline 'form perform_write.'.            "#EC NOTEXT
        append_codeline '  write / gstr.'.                "#EC NOTEXT
        append_codeline 'endform.'.                       "#EC NOTEXT
      when 'XTESTPROG_DOC'.
        append_codeline '*Just some documentation here'.  "#EC NOTEXT
      when 'XTESTPROG_EXT'.
        append_codeline '*Include from another package'.  "#EC NOTEXT
      when 'ASSEMBLED_RESULT'. " For testing
        append_codeline 'report xtestprog.'.              "#EC NOTEXT
        append_codeline '*include xtestprog_top.'.        "#EC NOTEXT
        append_codeline '*include xtestprog_doc.'.        "#EC NOTEXT
        append_codeline '*Just some documentation here'.  "#EC NOTEXT
        append_codeline ''.
        append_codeline 'constants gstr type char4 value ''Test''.'.  "#EC NOTEXT
        append_codeline 'types: begin of t_sometype.'.                "#EC NOTEXT
        append_codeline '        include structure textpool.'.        "#EC NOTEXT
        append_codeline 'types: end of t_sometype.'.                  "#EC NOTEXT
        append_codeline ''.
        append_codeline '*include xtestprog_f01.'.        "#EC NOTEXT
        append_codeline 'form perform_write.'.            "#EC NOTEXT
        append_codeline '  write / gstr.'.                "#EC NOTEXT
        append_codeline 'endform.'.                       "#EC NOTEXT
        append_codeline ''.
        append_codeline 'include xtestprog_ext.'.         "#EC NOTEXT
        append_codeline 'start-of-selection.'.            "#EC NOTEXT
        append_codeline '  perform perform_write.'.       "#EC NOTEXT
      when others.
        zcx_iasm_error=>raise( |test get { i_progname }| ).
    endcase.

    r_codetab = lt_code.

  endmethod.

  method zif_iasm_devobj_accessor~get_devc.

    case i_progname.
      when 'XTESTPROG'.
        r_devc = 'XTEST'.
      when 'XTESTPROG_TOP'.
        r_devc = 'XTEST'.
      when 'XTESTPROG_F01'.
        r_devc = 'XTEST'.
      when 'XTESTPROG_DOC'.
        r_devc = 'XTEST'.
      when 'XTESTPROG_EXT'.
        r_devc = 'XTEST_EXT'.
    endcase.

  endmethod.

endclass.
