*/--------------------------------------------------------------------------------\
*| Include assembler - create a program text with statically included includes.   |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2019 Alexander Tsybulsky                                         |
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
*| project homepage: https://github.com/sbcgua/abap_include_assembler             |
*\--------------------------------------------------------------------------------/

report zinclude_assembler.

include zinclude_assembler_errors.
include zinclude_assembler_interfaces.
include zinclude_assembler_extractors.
include zinclude_assembler_matchers.
include zinclude_assembler_code_obj.
include zinclude_assembler_assembler.
include zinclude_assembler_saver.


**********************************************************************
* MAIN
**********************************************************************
class lcl_main definition final.
  public section.
    types:
      tt_class_names type standard table of seoclasstx-clsname with default key.

    data:
      m_progname        type sobj_name,
      m_classes         type tt_class_names,
      m_disable_marking type abap_bool,
      m_path            type string,
      m_saver           type char1.

    methods constructor
      importing
        i_progname        type sobj_name
        i_classes         type tt_class_names
        i_disable_marking type abap_bool
        i_path            type string
        i_saver           type char1.

    methods run.

    methods list_includes
      importing
        io_prog type ref to lcl_code_object.

    methods save
      importing
        it_codetab        type string_table
      raising lcx_error.

    methods process_prog
      returning
        value(rt_codetab) type string_table
      raising lcx_error.

    methods process_clas
      returning
        value(rt_codetab) type string_table
      raising lcx_error.

endclass.

class lcl_main implementation.

  method constructor.
    m_progname        = i_progname.
    m_classes         = i_classes.
    m_disable_marking = i_disable_marking.
    m_path            = i_path.
    m_saver           = i_saver.
  endmethod.

  method run.
    data lo_ex  type ref to lcx_error.
    data lt_codetab type string_table.

    try.
      write: / 'Assembling program:', m_progname. "#EC NOTEXT
      if m_progname is not initial.
        lt_codetab = process_prog( ).
      elseif m_classes is not initial.
        lt_codetab = process_clas( ).
      else.
        lcx_error=>raise( 'No source specified' ).
      endif.
      skip. uline.
      save( lt_codetab ).
    catch lcx_error into lo_ex.
      write: / lo_ex->msg.
      return.
    endtry.

  endmethod.

  method process_prog.

    data lo_accessor type ref to lcl_extractor_prog.
    data lo_progcode type ref to lcl_code_object.
    create object lo_accessor.

    lo_progcode = lcl_code_object=>load(
      io_accessor = lo_accessor
      i_progname  = m_progname ).

    list_includes( lo_progcode ).

    data lt_codetab   type string_table.
    data lo_assembler type ref to lcl_assembler.
    data ls_params    type lcl_assembler=>ty_params.

    create object lo_assembler
      exporting
        io_prog = lo_progcode.
    ls_params-disable_marking = m_disable_marking.
    rt_codetab = lo_assembler->assemble( ls_params ).

  endmethod.

  method process_clas.

    data lt_code like rt_codetab.
    data lo_accessor type ref to lcl_extractor_clas.
    create object lo_accessor.


    field-symbols <c> like line of m_classes.
    loop at m_classes assigning <c>.

      if rt_codetab is not initial and m_disable_marking = abap_false.
        append '' to rt_codetab.
        append '*INCLUDE ASSEMBLER MARKER' to rt_codetab.
        append '' to rt_codetab.
      endif.

      lt_code = lo_accessor->lif_devobj_accessor~get_code( |{ <c> }| ).
      append lines of lt_code to rt_codetab.

    endloop.

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

  method save.

    data lo_saver type ref to lif_devobj_saver.
    data l_path type string.

    l_path = m_path.
    case m_saver.
      when 'D'.
        create object lo_saver type lcl_saver_to_display.

      when 'F'.
        create object lo_saver type lcl_saver_to_file.

        data len type i.
        len = strlen( l_path ) - 1.
        if len >= 0 and m_path+len(1) = '\'.
          concatenate l_path m_progname '.abap' into l_path. "#EC NOTEXT
        endif.

      when 'C'.
        create object lo_saver type lcl_saver_to_program.

      when others.
        lcx_error=>raise( 'ERROR: unknown saver' ). "#EC NOTEXT
    endcase.

    lo_saver->save(
      i_path = l_path
      i_codetab = it_codetab ).

    case m_saver.
      when 'F'.
        write: / 'Result saved to file:', l_path.     "#EC NOTEXT
      when 'C'.
        write: / 'Result saved to program:', l_path.  "#EC NOTEXT
        write: / 'The program remained inactive'.      "#EC NOTEXT
    endcase.

  endmethod.
endclass.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

tables: seoclasstx.

selection-screen begin of block b1 with frame title txt_b1.

parameter p_prog type programm default 'ZIS_EXAMPLE'.
select-options s_class for seoclasstx-clsname.
parameter p_womark type xfeld.

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
  data g_saver type char1.
  data gt_class_list type lcl_main=>tt_class_names.

  case 'X'.
    when p_disp.
      g_saver = 'D'.
    when p_file.
      g_saver = 'F'.
    when p_code.
      g_saver = 'C'.
  endcase.

  select clsname from seoclasstx
    into table gt_class_list
    where clsname in s_class.

  create object go_main
    exporting
      i_progname        = p_prog
      i_classes         = gt_class_list
      i_disable_marking = p_womark
      i_path            = |{ p_path }|
      i_saver           = g_saver.
  go_main->run( ).
