*/--------------------------------------------------------------------------------\
*| Include assembler - create a program text with statically included includes.   |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2016 Alexander Tsybulsky, SBCG Team (www.sbcg.com.ua)            |
*|                                                                                |
*| Permission is hereby granted, free of charge, to any person obtaining a copy   |
*| of this software and associated documentation files (the "Software"), to deal  |
*| in the Software without restriction, including without limitation the rights   |
*| to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      |
*| copies of the Software, and to permit persons to whom the Software is          |
*| furnished to do so, subject to the following conditions:                       |
*|                                                                                |
*| The above copyright notice and this permission notice shall be included in all |
*| copies or substantial portions of the Software.                                |
*|                                                                                |
*| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     |
*| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       |
*| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    |
*| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         |
*| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  |
*| OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  |
*| SOFTWARE.                                                                      |
*\--------------------------------------------------------------------------------/
*/--------------------------------------------------------------------------------\
*| Developers : Alexander Tsybulsky (atsybulsky@sbcg.com.ua)                      |
*| project homepage: https://github.com/sbcgua/mockup_loader                      |
*\--------------------------------------------------------------------------------/

report zinclude_assembler.

**********************************************************************
* EXCEPTIONS
**********************************************************************
class lcx_error definition inheriting from cx_static_check final.
  public section.
    data msg type string.
    class-methods raise importing i_msg type string optional raising lcx_error.
endclass.

class lcx_error implementation.
  method raise.
    data sys_call    type sys_calls.
    data sys_stack   type sys_callst.
    data ex          type ref to lcx_error.

    call function 'SYSTEM_CALLSTACK'
      exporting max_level    = 2
      importing et_callstack = sys_stack.

    read table sys_stack into sys_call index 2.

    create object ex.
    ex->msg = sys_call-eventname && `(): ` && i_msg.
    raise exception ex.
  endmethod.
endclass.

**********************************************************************
* INTERFACE
**********************************************************************

interface lif_devobj_accessor.
  methods get_prog_code importing i_progname       type sobj_name
                        returning value(r_codetab) type abaptxt255_tab
                        raising   lcx_error.
  methods get_prog_devc importing i_progname       type sobj_name
                        returning value(r_devc)    type devclass
                        raising   lcx_error.
endinterface.

interface lif_devobj_saver.
  methods save_prog     importing i_path     type string
                                  i_codetab  type abaptxt255_tab
                        raising   lcx_error.
endinterface.


**********************************************************************
* INTERFACE IMPLEMENTATION FOR CODE EXTRACTION
**********************************************************************

class lcl_extractor definition final.
  public section.
    interfaces lif_devobj_accessor.
endclass.

class lcl_extractor implementation.

  method lif_devobj_accessor~get_prog_code.
    data l_status type c.

    call function 'CHECK_EXIST'
      exporting iv_pgmid    = 'R3TR'
                iv_object   = 'PROG'
                iv_obj_name = i_progname
      importing e_exist     = l_status
      exceptions others     = 1.

    if sy-subrc <> 0 or l_status <> 'X'. " Exist and active
      lcx_error=>raise( |check_exist { i_progname }| ).  "#EC NOTEXT
    endif.

    data ls_tadir type tadir.
    select single * into corresponding fields of ls_tadir
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      lcx_error=>raise( |select tadir { i_progname }| ).  "#EC NOTEXT
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
      lcx_error=>raise( |Cannot read program| ).  "#EC NOTEXT
    endif.

    r_codetab = lt_codetab.

  endmethod. "lif_devobj_accessor~get_prog_code

  method lif_devobj_accessor~get_prog_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      lcx_error=>raise( |select devclass { i_progname }| ).  "#EC NOTEXT
    endif.

  endmethod. "lif_devobj_accessor~get_prog_devc

endclass.

***
* TEST  - CODE EXTRACTOR
***

class lcl_extractor_test definition inheriting from cl_aunit_assert final
  for testing duration short risk level harmless.

  private section.
    methods get_prog_code for testing.
endclass.

class lcl_extractor_test implementation.
  method get_prog_code.
    data lo_if   type ref to lif_devobj_accessor.
    data lo_obj  type ref to lcl_extractor.
    data lt_code type abaptxt255_tab.

    create object lo_obj.
    lo_if ?= lo_obj.

    try.
      clear sy-subrc.
      lt_code = lo_if->get_prog_code( i_progname   = '~~DOES~NOT~EXIST~~' ).
    catch lcx_error.
      sy-subrc = 1.
    endtry.

    assert_subrc( act = sy-subrc  exp = 1 ).

    try.
      clear sy-subrc.
      lt_code = lo_if->get_prog_code( i_progname   = sy-cprog ).
    catch lcx_error.
      sy-subrc = 1.
    endtry.

    assert_subrc( act = sy-subrc  exp = 0 ).
    assert_not_initial( act = lines( lt_code ) ).

  endmethod.
endclass.

**********************************************************************
* INTERFACE IMPLEMENTATION FOR DUMMY EXTRACTION - tests and example
**********************************************************************

class lcl_dummy_extractor definition final.
  public section.
    interfaces lif_devobj_accessor.
endclass.

class lcl_dummy_extractor implementation.

  method lif_devobj_accessor~get_prog_code.
    data lt_code type abaptxt255_tab.
    data l_line  type abaptxt255.

    define append_codeline.
      l_line-line = &1.
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
        lcx_error=>raise( |test get { i_progname }| ).
    endcase.

    r_codetab = lt_code.

  endmethod. "lcl_dummy_extractor~get_prog_code

  method lif_devobj_accessor~get_prog_devc.

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

**********************************************************************
* PROGRAMM CODE CLASS ABSTRACTION
**********************************************************************

class lcl_code_object definition create private final.
  public section.
    types:  begin of ty_include,
              lnum  type i,
              obj   type ref to lcl_code_object,
            end of ty_include.

    data a_name      type sobj_name.
    data at_includes type standard table of ty_include read-only.
    data at_codetab  type abaptxt255_tab read-only.
    data a_devclass  type devclass read-only.

    class-methods load importing io_accessor type ref to lif_devobj_accessor
                                 i_progname  type sobj_name
                       returning value(ro_obj) type ref to lcl_code_object
                       raising lcx_error.
endclass.

class lcl_include_matcher definition final.
  " I hope I'm not going to far with this parsing thing ... to reconsider RS_GET_ALL_INCLUDES, see #1
  public section.
    methods match_include importing i_abapline       type abaptxt255-line
                          returning value(r_include) type sobj_name.
  private section.
    data a_codeline        type string.
    data a_lines_collected type i.

    methods collect_line importing i_abapline   type abaptxt255-line.
    methods do_match returning value(r_include) type sobj_name.
endclass.

class lcl_code_object implementation.
  method load.
    data lo         type ref to lcl_code_object.
    data lo_ex      type ref to lcx_error.
    data l_line     type abaptxt255.
    data l_incname  type sobj_name.
    data ls_include type ty_include.

    create object lo.
    lo->at_codetab = io_accessor->get_prog_code( i_progname = i_progname ).
    lo->a_devclass = io_accessor->get_prog_devc( i_progname = i_progname ).
    lo->a_name     = i_progname.

    data lo_matcher type ref to lcl_include_matcher.
    create object lo_matcher.

    loop at lo->at_codetab into l_line.
      clear l_incname.
      ls_include-lnum = sy-tabix.
      l_incname       = lo_matcher->match_include( l_line-line ).

      if l_incname is not initial.
        try.
          ls_include-obj  = load( io_accessor = io_accessor i_progname = l_incname ).
        catch lcx_error into lo_ex.
          lo_ex->msg = lo_ex->msg && `; ` && i_progname && '@' && |{ ls_include-lnum }|.
          raise exception lo_ex.
        endtry.
        append ls_include to lo->at_includes.
      endif.
    endloop.

    ro_obj = lo.

  endmethod.

endclass.

class lcl_include_matcher implementation.
  method match_include.
    data l_cnt type i.

    collect_line( i_abapline ).
    find first occurrence of '.' in me->a_codeline match count l_cnt.
    " multi-operations per line not supported

    if l_cnt = 1. " check if line complete
      r_include = do_match( ).
      clear me->a_codeline.
      clear me->a_lines_collected.
    endif.
  endmethod.

  method collect_line.
    data l_str type text255.
    data l_cnt type i.
    data l_off type i.

    l_str = i_abapline.
    add 1 to me->a_lines_collected.

    if l_str+0(1) = '*'. " Ignore comments
      return.
    endif.

    shift l_str left deleting leading space.

    find first occurrence of '"' in l_str match offset l_off match count l_cnt.
    if l_cnt = 1.
      if l_off = 0.
        return.
      else.
        l_str = l_str+0(l_off).
      endif.
    endif.

    if l_str is initial.
      return.
    endif.

    if me->a_codeline is not initial.
      me->a_codeline = me->a_codeline && ` `.
    endif.

    me->a_codeline = me->a_codeline && l_str.

  endmethod.

  method do_match.
    translate me->a_codeline to upper case.

    data l_cnt      type i.
    data result_tab type match_result_tab.
    data l_result   type match_result.

    find first occurrence of regex '^INCLUDE\s+(\w+)\s*\.' in me->a_codeline
      match count l_cnt
      results     result_tab.

    if l_cnt <> 1.
      return.
    endif.

    read table result_tab into l_result index 1.
    l_cnt = lines( l_result-submatches ).

    if l_cnt <> 1.
      return.
    endif.

    data l_submatch type submatch_result.
    read table l_result-submatches into l_submatch index 1.

    r_include = me->a_codeline+l_submatch-offset(l_submatch-length).

  endmethod.
endclass.


***
* TEST - CODE OBJECT
***

class lcl_code_object_test definition inheriting from cl_aunit_assert final
  for testing duration short risk level harmless.

  private section.
    methods load for testing.
endclass.

class lcl_code_object_test implementation.
  method load.
    data lo_obj  type ref to lcl_code_object.
    data lo_acc  type ref to lcl_dummy_extractor.

    create object lo_acc.

    try.
      lo_obj = lcl_code_object=>load( io_accessor = lo_acc i_progname = 'XTESTPROG' ).

      assert_not_initial( act = lines( lo_obj->at_codetab ) ).
      assert_not_initial( act = lines( lo_obj->at_includes ) ).
      assert_equals(      act = lo_obj->a_devclass  exp = 'XTEST' ).
      assert_equals(      exp = lo_acc->lif_devobj_accessor~get_prog_code( 'XTESTPROG' )
                          act = lo_obj->at_codetab ).

      data ls_include type lcl_code_object=>ty_include.
      read table lo_obj->at_includes into ls_include index 1.
      assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
      assert_not_initial( act = lines( ls_include-obj->at_includes ) ).
      assert_equals(      act = ls_include-obj->a_devclass  exp = 'XTEST' ).
      assert_equals(      act = ls_include-lnum exp = 2 ).
      assert_equals(      exp = lo_acc->lif_devobj_accessor~get_prog_code( 'XTESTPROG_TOP' )
                          act = ls_include-obj->at_codetab ).

      read table lo_obj->at_includes into ls_include index 2.
      assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
      assert_initial(     act = lines( ls_include-obj->at_includes ) ).
      assert_equals(      act = ls_include-obj->a_devclass  exp = 'XTEST' ).
      assert_equals(      act = ls_include-lnum exp = 3 ).
      assert_equals(      exp = lo_acc->lif_devobj_accessor~get_prog_code( 'XTESTPROG_F01' )
                          act = ls_include-obj->at_codetab ).

      read table lo_obj->at_includes into ls_include index 3.
      assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
      assert_initial(     act = lines( ls_include-obj->at_includes ) ).
      assert_equals(      act = ls_include-obj->a_devclass  exp = 'XTEST_EXT' ).

    catch lcx_error.
      fail( ).
    endtry.

  endmethod.

endclass.

class lcl_include_matcher_test definition inheriting from cl_aunit_assert final
  for testing duration short risk level harmless.
  private section.
    methods match_include for testing.
endclass.

class lcl_include_matcher_test implementation.
  method match_include.
    data lo_matcher type ref to lcl_include_matcher.
    create object lo_matcher.

    assert_equals( act = lo_matcher->match_include( 'include z_test1.' )      exp = 'Z_TEST1' ).
    assert_equals( act = lo_matcher->match_include( 'include z_test1' )       exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'inc ude z_test1.' )      exp = '' ).
    assert_equals( act = lo_matcher->match_include( '*include z_test1.' )     exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'include " z_test1.' )    exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'include structure Z.' )  exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'include type Z.' )       exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'include method Z.' )     exp = '' ).

    " multiline
    assert_equals( act = lo_matcher->match_include( 'include " comment' )     exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'z_test1' )               exp = '' ).
    assert_equals( act = lo_matcher->match_include( '.' )                     exp = 'Z_TEST1' ).
    assert_equals( act = lo_matcher->match_include( 'exporting' )             exp = '' ).
    assert_equals( act = lo_matcher->match_include( ' include = ' )           exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'ztest1.' )               exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'include' )               exp = '' ).
    assert_equals( act = lo_matcher->match_include( '"comment' )              exp = '' ).
    assert_equals( act = lo_matcher->match_include( '*comment' )              exp = '' ).
    assert_equals( act = lo_matcher->match_include( '' )                      exp = '' ).
    assert_equals( act = lo_matcher->match_include( 'z_test1.' )              exp = 'Z_TEST1' ).

  endmethod.
endclass.


**********************************************************************
* ASSEMBLER CLASS
**********************************************************************

class lcl_assembler definition final.
  public section.
    types: begin of ty_params,
             disable_marking type abap_bool,
           end of ty_params.

    methods constructor importing io_prog type ref to lcl_code_object.
    methods assemble importing i_params type ty_params optional
                     returning value(r_codetab) type abaptxt255_tab.
  private section.
    data as_params type ty_params.
    data o_prog    type ref to lcl_code_object.

    methods pad_line importing i_str type text255
                     returning value(r_str) type text255.
    methods _assemble importing io_prog type ref to lcl_code_object
                      returning value(r_codetab) type abaptxt255_tab.
endclass.

class lcl_assembler implementation.
  method constructor.
    o_prog = io_prog.
  endmethod.

  method assemble.
    me->as_params = i_params.
    r_codetab = _assemble( io_prog = o_prog ).
  endmethod.

  method _assemble.
    data lt_codetab type abaptxt255_tab.
    data l_line     type abaptxt255.
    data inc_idx    type i value 1.
    data ls_include type lcl_code_object=>ty_include.

    read table io_prog->at_includes into ls_include index inc_idx. "If fail -> it is OK, lnum = 0

    loop at io_prog->at_codetab into l_line.
      if sy-tabix = ls_include-lnum.
        if ls_include-obj->a_devclass = o_prog->a_devclass.
          data lt_codeinc type abaptxt255_tab.
          data l_tmp   type abaptxt255.
          lt_codeinc  = _assemble( ls_include-obj ).
          l_line-line = '*' && l_line-line.

          define add_marker_line.
            if me->as_params-disable_marking is initial.
              l_tmp-line = &1.
              replace first occurrence of '{INC}' in l_tmp-line with ls_include-obj->a_name.
              l_tmp-line = pad_line( l_tmp-line ).
              append l_tmp to lt_codetab.
            endif.
          end-of-definition.

          add_marker_line '*%%ASSEMBLY-START @{INC}'.
          append l_line to lt_codetab.
          append lines of lt_codeinc to lt_codetab.
          add_marker_line '*%%ASSEMBLY-END   @{INC}'.

          l_line-line = ''.
          append l_line to lt_codetab.
        else.
          append l_line to lt_codetab.
        endif.

        " read next include, If fail -> it is OK, lnum = 0
        add 1 to inc_idx.
        clear ls_include.
        read table io_prog->at_includes into ls_include index inc_idx.

      else.
        append l_line to lt_codetab.
      endif.
    endloop.

    r_codetab = lt_codetab.

  endmethod.

  method pad_line.
    constants c_code_width type i value 70.
    data cnt type i.
    data tmp type text80.

    cnt = c_code_width - strlen( i_str ).
    if cnt < 0.
      cnt = 0.
    endif.

    do cnt times.
      tmp = tmp && '-'.
    enddo.

    r_str = i_str && ` ` && tmp && '*'.
  endmethod.

endclass.

***
* TEST - ASSEMBLER
***

class lcl_assembler_test definition inheriting from cl_aunit_assert final
  for testing duration short risk level harmless.

  private section.
    methods assemble for testing.
endclass.

class lcl_assembler_test implementation.
  method assemble.
    data lo_prog type ref to lcl_code_object.
    data lo_acc  type ref to lcl_dummy_extractor.
    data lt_exp  type abaptxt255_tab.

    create object lo_acc.

    try.
      lo_prog = lcl_code_object=>load( io_accessor = lo_acc i_progname = 'XTESTPROG' ).
      lt_exp  = lo_acc->lif_devobj_accessor~get_prog_code( 'ASSEMBLED_RESULT' ).
    catch lcx_error.
      fail( ).
    endtry.

    data lo_assembler type ref to lcl_assembler.
    create object lo_assembler exporting io_prog = lo_prog.

    data lt_code   type abaptxt255_tab.
    data ls_params type lcl_assembler=>ty_params.
    ls_params-disable_marking = abap_true.
    lt_code = lo_assembler->assemble( ls_params ).

    assert_equals( exp = lt_exp
                   act = lt_code ).

  endmethod.
endclass.

**********************************************************************
* SAVERS
**********************************************************************

* SAVE TO DISPLAY -> SHOW
class lcl_saver_to_display definition final.
  public section.
    interfaces lif_devobj_saver.
endclass.

class lcl_saver_to_display implementation.
  method lif_devobj_saver~save_prog.

    cl_demo_output=>display( i_codetab ).

  endmethod.
endclass.

* SAVE TO PROGRAM
class lcl_saver_to_program definition final.
  public section.
    interfaces lif_devobj_saver.
endclass.

class lcl_saver_to_program implementation.
  method lif_devobj_saver~save_prog.
    if strlen( i_path ) > 40.
      lcx_error=>raise( 'Program name must be <= 40 symbols' ). "#EC NOTEXT
    endif.

    if i_path is initial.
      lcx_error=>raise( 'Target program name is empty' ).       "#EC NOTEXT
    endif.

    data l_progname type reposrc-progname.
    select single progname from reposrc into l_progname
      where progname = i_path.

    if sy-subrc is not initial.
      lcx_error=>raise( |Target program { i_path } must be created manually first| ). "#EC NOTEXT
    endif.

    call function 'RPY_PROGRAM_UPDATE'
      exporting
        program_name     = l_progname
        save_inactive    = 'I'
      tables
        source_extended  = i_codetab
      exceptions
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        others           = 4.

    IF sy-subrc is not initial.
      if sy-msgid = 'EU' and sy-msgno = '510'.
        lcx_error=>raise( |Target program { i_path } is being edited by someone else| ). "#EC NOTEXT
      else.
        lcx_error=>raise( |Cannot update program { i_path }| ). "#EC NOTEXT
      endif.
    endif.

  endmethod.
endclass.

* SAVE TO FILE
class lcl_saver_to_file definition final.
  public section.
    interfaces lif_devobj_saver.
endclass.

class lcl_saver_to_file implementation.
  method lif_devobj_saver~save_prog.
    data l_length type i.
    data lt_data  type xml_rawdata.

    if i_path is initial.
      lcx_error=>raise( 'Path is empty' ).             "#EC NOTEXT
    endif.

    call function 'SCMS_TEXT_TO_BINARY'
      exporting  encoding      = '4110'
      importing  output_length = l_length
      tables     text_tab      = i_codetab
                 binary_tab    = lt_data
      exceptions failed        = 1.

    if sy-subrc is not initial.
      lcx_error=>raise( 'Cannot convert to binary' ).  "#EC NOTEXT
    endif.

    call method cl_gui_frontend_services=>gui_download
      exporting
        bin_filesize            = l_length
        filename                = i_path
        filetype                = 'BIN'
      changing
        data_tab                = lt_data
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others                  = 24.

    if sy-subrc is not initial.
      lcx_error=>raise( |Cannot save file ({ sy-subrc })| ). "#EC NOTEXT
    endif.

  endmethod.
endclass.


**********************************************************************
* MAIN
**********************************************************************
class lcl_main definition final.
  public section.
    methods run
      importing
        i_progname        type sobj_name
        i_disable_marking type abap_bool
        i_path            type char255
        i_saver           type char1.

    methods list_includes importing io_prog type ref to lcl_code_object.
endclass.

class lcl_main implementation.
  method run.
    data lo_ex  type ref to lcx_error.
    data l_path type string.

    write: / 'Assembling program:', i_progname. "#EC NOTEXT

    data lo_accessor type ref to lcl_extractor.
    data lo_progcode type ref to lcl_code_object.
    create object lo_accessor.

    try.
      lo_progcode = lcl_code_object=>load( io_accessor = lo_accessor i_progname = i_progname ).
    catch lcx_error into lo_ex.
      write: / 'ERROR: could not read the program'. "#EC NOTEXT
      write: / lo_ex->msg.
      return.
    endtry.

    list_includes( lo_progcode ).

    data lt_codetab   type abaptxt255_tab.
    data lo_assembler type ref to lcl_assembler.
    data ls_params    type lcl_assembler=>ty_params.

    create object lo_assembler exporting io_prog = lo_progcode.
    ls_params-disable_marking = i_disable_marking.
    lt_codetab = lo_assembler->assemble( ls_params ).

    skip. uline.

    data lo_saver type ref to lif_devobj_saver.

    l_path = i_path.
    case i_saver.
      when 'D'.
        create object lo_saver type lcl_saver_to_display.
        clear l_path.

      when 'F'.
        create object lo_saver type lcl_saver_to_file.

        data len type i.
        len = strlen( l_path ) - 1.
        if len >= 0 and i_path+len(1) = '\'.
          concatenate l_path i_progname '.abap' into l_path. "#EC NOTEXT
        endif.

      when 'C'.
        create object lo_saver type lcl_saver_to_program.

      when others.
        write: / 'ERROR: unknown saver'. "#EC NOTEXT
    endcase.

    try.
      lo_saver->save_prog( i_path = l_path i_codetab = lt_codetab ).
    catch lcx_error into lo_ex.
      write: / 'ERROR: cannot save code'. "#EC NOTEXT
      write: / lo_ex->msg.
      return.
    endtry.

    case i_saver.
      when 'F'.
        write: / 'Result saved to file:', l_path.     "#EC NOTEXT
      when 'C'.
        write: / 'Result saved to program:', l_path.  "#EC NOTEXT
        write: / 'The program remained inactive'.      "#EC NOTEXT
    endcase.

  endmethod.

  method list_includes.
    data ls_include type lcl_code_object=>ty_include.
    data l_tmp      type string.

    loop at io_prog->at_includes into ls_include.
      l_tmp = |@{ io_prog->a_name }:{ ls_include-lnum } |.
      write: / '  include found:', ls_include-obj->a_name, l_tmp, "#EC NOTEXT
               'DEVC =', ls_include-obj->a_devclass.
      list_includes( ls_include-obj ).
    endloop.
  endmethod.
endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
selection-screen comment (24) txt_prog  for field p_prog.
parameter p_prog type programm default 'ZIS_EXAMPLE' obligatory.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_wom  for field p_womark.
parameter p_womark type xfeld.
selection-screen end of line.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.

selection-screen begin of line.
selection-screen comment (24) txt_disp  for field p_disp.
parameter p_disp   type char1 radiobutton group r1 default 'X'.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_file  for field p_file.
parameter p_file type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_code  for field p_code.
parameter p_code type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_path  for field p_path  modif id pth.
parameter p_path type char255                             modif id pth.
selection-screen end of line.

selection-screen end of block b2.


initialization.
  txt_b1   = 'Source program'.          "#EC NOTEXT
  txt_prog = 'Program name'.            "#EC NOTEXT
  txt_wom  = 'Hide %ASSEMBLY markers'.  "#EC NOTEXT

  txt_b2   = 'Save parameters'.         "#EC NOTEXT
  txt_disp = 'Show on display'.         "#EC NOTEXT
  txt_file = 'Save to file'.            "#EC NOTEXT
  txt_code = 'Save to target program'.  "#EC NOTEXT
  txt_path = 'Path to file / Prog name'."#EC NOTEXT

  " TODO normal parameters show/hide and file/prog-search

**********************************************************************
* ENTRY POINT
**********************************************************************
start-of-selection.

  data go_main type ref to lcl_main.
  data l_saver type char1.

  case 'X'.
    when p_disp.
      l_saver = 'D'.
    when p_file.
      l_saver = 'F'.
    when p_code.
      l_saver = 'C'.
  endcase.

  create object go_main.
  go_main->run(
    i_progname        = p_prog
    i_disable_marking = p_womark
    i_path            = p_path
    i_saver           = l_saver ).
