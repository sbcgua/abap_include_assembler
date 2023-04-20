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

  endmethod. "zif_iasm_devobj_accessor~get_prog_code

  method zif_iasm_devobj_accessor~get_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |Cannot find devclass { i_progname }| ).  "#EC NOTEXT
    endif.

  endmethod. "zif_iasm_devobj_accessor~get_prog_devc

endclass.

class lcl_extractor_clas definition final.
  public section.
    interfaces zif_iasm_devobj_accessor.
endclass.

class lcl_extractor_clas implementation.

  method zif_iasm_devobj_accessor~get_code.

    data lo_factory type ref to cl_oo_factory.
    lo_factory = cl_oo_factory=>create_instance( ).

    data lo_source type ref to if_oo_clif_source.
    lo_source = lo_factory->create_clif_source(
      clif_name = i_progname
      version = 'A' ).

    data lt_source type string_table.
    lo_source->get_source( importing source = lt_source ).

    r_codetab = lt_source.

  endmethod. "zif_iasm_devobj_accessor~get_prog_code

  method zif_iasm_devobj_accessor~get_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'CLAS'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |Cannot find devclass { i_progname }| ).  "#EC NOTEXT
    endif.

  endmethod. "zif_iasm_devobj_accessor~get_prog_devc

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

  endmethod. "lcl_dummy_extractor~get_prog_code

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

  endmethod. "lcl_dummy_extractor~get_prog_devc

endclass.
