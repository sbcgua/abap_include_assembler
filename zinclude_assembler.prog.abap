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
class lcx_error definition inheriting from cx_static_check.
endclass.

**********************************************************************
* INTERFACE
**********************************************************************

interface lif_devobj_accessor.
  methods get_prog_code importing i_progname       type sobj_name
                        returning value(r_codetab) type abaptxt255_tab
                        raising lcx_error.
  methods get_prog_devc importing i_progname       type sobj_name
                        returning value(r_devc)    type devclass
                        raising lcx_error.
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
      raise exception type lcx_error.
    endif.

    data ls_tadir type tadir.
    select single * into corresponding fields of ls_tadir
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      raise exception type lcx_error.
    endif.

    data lt_e071 type standard table of e071.
    data ls_e071 type e071.
    move-corresponding ls_tadir to ls_e071.

    call function 'SUMO_RESOLVE_E071_OBJ'
      exporting e071_obj = ls_e071
      tables obj_tab     = lt_e071[]
      exceptions others  = 1.

    if sy-subrc <> 0.
      raise exception type lcx_error.
    endif.

    read table lt_e071 into ls_e071 with key object = 'REPS'.

    if sy-subrc <> 0.
      raise exception type lcx_error.
    endif.

    data l_obj type svrs2_versionable_object.

    l_obj-objtype = ls_e071-object.
    l_obj-objname = ls_e071-obj_name.

    call function 'SVRS_GET_VERSION_REPOSITORY'
      changing obj      = l_obj
      exceptions others = 1.

    if sy-subrc <> 0.
      raise exception type lcx_error.
    endif.

    r_codetab = l_obj-reps-abaptext.

  endmethod. "lif_devobj_accessor~get_prog_code

  method lif_devobj_accessor~get_prog_devc.

    select single devclass into r_devc
      from tadir
      where pgmid    = 'R3TR'
      and   object   = 'PROG'
      and   obj_name = i_progname.

    if sy-subrc <> 0.
      raise exception type lcx_error.
    endif.

  endmethod. "lif_devobj_accessor~get_prog_devc

endclass.

***
* TEST  - CODE EXTRACTOR
***

class lcl_extractor_test definition inheriting from cl_aunit_assert
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
      lt_code = lo_if->get_prog_code( i_progname   = '~~DOES~NOT~EXISTS~~' ).
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
        append_codeline 'report xtestprog.'.
        append_codeline 'include xtestprog_top.'.
        append_codeline 'include xtestprog_f01.'.
        append_codeline 'include xtestprog_ext.'.
        append_codeline 'start-of-selection.'.
        append_codeline '  perform perform_write.'.
      when 'XTESTPROG_TOP'.
        append_codeline 'include xtestprog_doc.'.
        append_codeline 'constants gstr type string value ''The test string''.'.
      when 'XTESTPROG_F01'.
        append_codeline 'form perform_write.'.
        append_codeline '  write / gstr.'.
        append_codeline 'endform.'.
      when 'XTESTPROG_DOC'.
        append_codeline '*Just some documentation here'.
      when 'XTESTPROG_EXT'.
        append_codeline '*Include from another package'.
      when 'ASSEMBLED_RESULT'. " For testing
        append_codeline 'report xtestprog.'.
        append_codeline '*include xtestprog_top.'.
        append_codeline '*include xtestprog_doc.'.
        append_codeline '*Just some documentation here'.
        append_codeline ''.
        append_codeline 'constants gstr type string value ''The test string''.'.
        append_codeline ''.
        append_codeline '*include xtestprog_f01.'.
        append_codeline 'form perform_write.'.
        append_codeline '  write / gstr.'.
        append_codeline 'endform.'.
        append_codeline ''.
        append_codeline 'include xtestprog_ext.'.
        append_codeline 'start-of-selection.'.
        append_codeline '  perform perform_write.'.
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

    class-methods match_include importing i_abapline       type abaptxt255-line
                                returning value(r_include) type sobj_name.

endclass.

class lcl_code_object implementation.
  method load.
    data lo         type ref to lcl_code_object.
    data l_line     type abaptxt255.
    data l_incname  type sobj_name.
    data ls_include type ty_include.

    create object lo.
    lo->at_codetab = io_accessor->get_prog_code( i_progname = i_progname ).
    lo->a_devclass = io_accessor->get_prog_devc( i_progname = i_progname ).
    lo->a_name     = i_progname.

    loop at lo->at_codetab into l_line.
      clear l_incname.
      ls_include-lnum = sy-tabix.
      l_incname       = match_include( l_line-line ).

      if l_incname is not initial.
        ls_include-obj  = load( io_accessor = io_accessor i_progname = l_incname ).
        append ls_include to lo->at_includes.
      endif.
    endloop.

    ro_obj = lo.

  endmethod.

  method match_include.
    data l_str type text255.

    l_str = i_abapline.

    if l_str+0(1) = '*'. " Ignore comments
      return.
    endif.

    shift     l_str left deleting leading space.
    translate l_str to upper case.

    if l_str+0(7) <> 'INCLUDE'.
      return.
    endif.

    shift l_str left by 7 places.
    shift l_str left deleting leading space.

    data l_offs type i. " Find end of statement
    data l_cnt  type i.
    find first occurrence of '.' in l_str match offset l_offs match count l_cnt.

    if l_cnt <> 1. " BTW 2 statements in the line not supported
      return.
    endif.

    l_str = l_str+0(l_offs).

    if l_str ca '"'. " Ignore is comment is in the middle
      return.
    endif.

    r_include = l_str.

  endmethod.

endclass.

***
* TEST - CODE OBJECT
***

class lcl_code_object_test definition inheriting from cl_aunit_assert
  for testing duration short risk level harmless.

  private section.
    methods load for testing.
    methods match_include for testing.
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

  method match_include.

    assert_equals( act = lcl_code_object=>match_include( 'include z_test1.' )       exp = 'Z_TEST1' ).
    assert_equals( act = lcl_code_object=>match_include( ' include z_test1. "cmt' ) exp = 'Z_TEST1' ).
    assert_equals( act = lcl_code_object=>match_include( 'include z_test1' )        exp = '' ).
    assert_equals( act = lcl_code_object=>match_include( 'inc ude z_test1.' )       exp = '' ).
    assert_equals( act = lcl_code_object=>match_include( '*include z_test1.' )      exp = '' ).
    assert_equals( act = lcl_code_object=>match_include( 'include " z_test1.' )     exp = '' ).

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
          add_marker_line '*%%ASSEMBLY-INCLUDE @{INC}'.
          append lines of lt_codeinc to lt_codetab.
          add_marker_line '*%%ASSEMBLY-END @{INC}'.

          l_line-line = '"'. " TODO - change this when normal output is ready
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
    data cnt type i.
    data tmp type text80.

    cnt = 72 - strlen( i_str ) - 2.
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

class lcl_assembler_test definition inheriting from cl_aunit_assert
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
* MAIN
**********************************************************************
class lcl_main definition final.
  public section.
    class-methods run
      importing
        i_progname        type sobj_name
        i_disable_marking type abap_bool.
endclass.

class lcl_main implementation.
  method run.

    write: / 'Assembling program:', i_progname.

    data lo_accessor type ref to lcl_extractor.
    data lo_progcode type ref to lcl_code_object.
    create object lo_accessor.

    try.
      lo_progcode = lcl_code_object=>load( io_accessor = lo_accessor i_progname = i_progname ).
    catch lcx_error.
      write: / 'ERROR: could not read the program'.
      return.
    endtry.

    data ls_include type lcl_code_object=>ty_include.

    loop at lo_progcode->at_includes into ls_include.
      write: / '  include found:', ls_include-obj->a_name, '@line', ls_include-lnum,
               'DEVC =', ls_include-obj->a_devclass.
    endloop.

    data lt_codetab   type abaptxt255_tab.
    data lo_assembler type ref to lcl_assembler.
    data ls_params    type lcl_assembler=>ty_params.

    create object lo_assembler exporting io_prog = lo_progcode.
    ls_params-disable_marking = i_disable_marking.
    lt_codetab = lo_assembler->assemble( ls_params ).

    skip.
    uline.
*    data l_line     type abaptxt255.
*    loop at lt_codetab into l_line.
*      if l_line-line is initial.
*        skip.
*      else.
*        write / l_line-line.
*      endif.
*    endloop.

    cl_demo_output=>display( lt_codetab ).

  endmethod.
endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

selection-screen begin of block b1 with frame title txt_b1.

selection-screen begin of line.
selection-screen comment (20) txt_prog  for field p_prog.
parameter p_prog type programm default 'ZIS_EXAMPLE' obligatory.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (20) txt_wom  for field p_womark.
parameter p_womark type xfeld.
selection-screen end of line.

selection-screen end of block b1.

initialization.
  txt_b1   = 'Control parameneters'. "#EC NOTEXT
  txt_prog = 'Program name'.         "#EC NOTEXT
  txt_wom  = 'Without markup'.       "#EC NOTEXT

**********************************************************************
* ENTRY POINT
**********************************************************************
start-of-selection.

  data gt_objects type standard table of ko100.
  call function 'TR_OBJECT_TABLE'
    tables wt_object_text = gt_objects
    exceptions others     = 1.

  lcl_main=>run(
      i_progname        = p_prog
      i_disable_marking = p_womark ).