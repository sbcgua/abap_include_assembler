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

include zinclude_assembler_ag_contrib.
include zinclude_assembler_extractors.
include zinclude_assembler_matchers.
include zinclude_assembler_code_obj.
include zinclude_assembler_assembler.


**********************************************************************
* MAIN
**********************************************************************
class lcl_main definition final.
  public section.
    types:
      begin of ty_deps,
        depends type seoclasstx-clsname,
        from type seoclasstx-clsname,
      end of ty_deps,
      tt_deps type standard table of ty_deps with default key,
      tt_class_names type standard table of seoclasstx-clsname with default key,
      ts_class_names type sorted table of seoclasstx-clsname with unique key table_line.

    data:
      m_progname        type sobj_name,
      m_classes         type ts_class_names,
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
      raising zcx_iasm_error.

    methods process_prog
      returning
        value(rt_codetab) type string_table
      raising zcx_iasm_error.

    methods process_clas
      returning
        value(rt_codetab) type string_table
      raising zcx_iasm_error.

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
    data lo_ex  type ref to zcx_iasm_error.
    data lt_codetab type string_table.

    try.
      write: / 'Assembling program:', m_progname. "#EC NOTEXT
      if m_progname is not initial.
        lt_codetab = process_prog( ).
      elseif m_classes is not initial.
        lt_codetab = process_clas( ).
      else.
        zcx_iasm_error=>raise( 'No source specified' ).
      endif.
      skip. uline.
      save( lt_codetab ).
    catch zcx_iasm_error into lo_ex.
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

    data lo_assembler type ref to lcl_assembler.
    data ls_params    type lcl_assembler=>ty_params.

    create object lo_assembler
      exporting
        io_prog = lo_progcode.
    ls_params-disable_marking = m_disable_marking.
    rt_codetab = lo_assembler->assemble( ls_params ).

  endmethod.

  method process_clas.

    data lt_deps type tt_deps.

    field-symbols <c> like line of m_classes.
    field-symbols <dep> like line of lt_deps.

    loop at m_classes assigning <c>.

      data lt_env type senvi_tab.
      data ls_env_types type envi_types.
      data lv_name type tadir-obj_name.
      data lv_type type euobj-id.
      lv_name = <c>.
      ls_env_types-clas = abap_true.

      data lo_type type ref to cl_abap_typedescr.
      lo_type = cl_abap_typedescr=>describe_by_name( lv_name ).
      if lo_type is not bound.
        zcx_iasm_error=>raise( |Class/intf { lv_name } not found| ).
      endif.
      if lo_type->type_kind = lo_type->typekind_class.
        lv_type = 'CLAS'.
      elseif lo_type->type_kind = lo_type->typekind_intf.
        lv_type = 'INTF'.
      else.
        zcx_iasm_error=>raise( |{ lv_name } has unexpected type kind ({ lo_type->type_kind })| ).
      endif.

      call function 'REPOSITORY_ENVIRONMENT_SET'
        exporting
          obj_type       = lv_type
          object_name    = lv_name
          environment_types = ls_env_types
        tables
          environment    = lt_env
        exceptions
          others         = 4.

      field-symbols <env> like line of lt_env.
      data lv_search_key type seoclasstx-clsname.
      data lv_dep_size type i.
      lv_dep_size = lines( lt_deps ).

      loop at lt_env assigning <env>.
        check <env>-type = 'CLAS' or <env>-type = 'INTF' or <env>-type = 'OM'.
        if <env>-type = 'CLAS' or <env>-type = 'INTF'.
          lv_search_key = <env>-object.
        elseif <env>-type = 'OM'.
          lv_search_key = <env>-encl_obj.
        else.
          continue.
        endif.
        read table m_classes with key table_line = lv_search_key transporting no fields.
        if sy-subrc = 0.
          append initial line to lt_deps assigning <dep>.
          <dep>-depends = <c>.
          <dep>-from    = lv_search_key.
        endif.
      endloop.

      if lines( lt_deps ) = lv_dep_size. " No dependencies
        append initial line to lt_deps assigning <dep>.
        <dep>-depends = <c>.
      endif.

*      data lt_tadir type if_ris_environment_types=>ty_t_senvi_tadir.
*      cl_wb_ris_environment=>convert_senvi_to_tadir(
*        exporting
*          senvi       = lt_env
*        importing
*          senvi_tadir = lt_tadir ).

    endloop.

    " Protection from self cycle
    sort lt_deps by depends from.
    delete adjacent duplicates from lt_deps.
    loop at lt_deps assigning <dep>.
      read table lt_deps
        transporting no fields
        binary search
        with key
          depends = <dep>-from
          from    = <dep>-depends.
      if sy-subrc = 0.
        zcx_iasm_error=>raise( 'Class self cycle detected' ).
      endif.
    endloop.

    data lt_ordered_classes type tt_class_names.
    data lt_unordered_classes type tt_class_names.
    data l_index type i.
    lt_unordered_classes = m_classes.

    while lines( lt_unordered_classes ) > 0.

      loop at lt_unordered_classes assigning <c>.
        l_index = sy-tabix.
        read table lt_deps
          assigning <dep>
          binary search
          with key
            depends = <c>.
        assert sy-subrc = 0.
        if <dep>-from is initial.
          delete lt_deps index sy-tabix.
          loop at lt_deps assigning <dep> where from = <c>.
            clear <dep>-from.
          endloop.
          append <c> to lt_ordered_classes.
          delete lt_unordered_classes index l_index.
        endif.
      endloop.

    endwhile.

    " Serialize

    data lt_code like rt_codetab.
    data lo_accessor type ref to lcl_extractor_clas.
    data lv_marker type string.

    create object lo_accessor.

    loop at lt_ordered_classes assigning <c>.

      if rt_codetab is not initial.
        append '' to rt_codetab.
        append '' to rt_codetab.
      endif.

      if m_disable_marking = abap_false.
        lv_marker = |*%%INCLUDING { <c> }|.
        append lv_marker to rt_codetab.
        append '' to rt_codetab.
      endif.

      lt_code = lo_accessor->zif_iasm_devobj_accessor~get_code( |{ <c> }| ).
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

    data li_saver type ref to zif_iasm_devobj_saver.
    data l_path type string.

    l_path = m_path.
    case m_saver.
      when 'D'.
        li_saver = zcl_iasm_devobj_savers=>to_display( ).

      when 'F'.
        li_saver = zcl_iasm_devobj_savers=>to_file( ).

        data len type i.
        len = strlen( l_path ) - 1.
        if len >= 0 and m_path+len(1) = '\'.
          concatenate l_path m_progname '.abap' into l_path. "#EC NOTEXT
        endif.

      when 'C'.
        li_saver = zcl_iasm_devobj_savers=>to_program( ).

      when others.
        zcx_iasm_error=>raise( 'ERROR: unknown saver' ). "#EC NOTEXT
    endcase.

    li_saver->save(
      i_path    = l_path
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

parameters p_prog type programm default 'ZIS_EXAMPLE'.
select-options s_class for seoclasstx-clsname.
parameters p_womark type xfeld.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title txt_b2.

selection-screen begin of line.
selection-screen comment (24) txt_disp  for field p_disp.
parameters p_disp   type char1 radiobutton group r1 default 'X'.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_file  for field p_file.
parameters p_file type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_code  for field p_code.
parameters p_code type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_path  for field p_path  modif id pth.
parameters p_path type char255                            modif id pth.
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
form main.

  data lo_app type ref to lcl_main.
  data lv_saver_type type c length 1.
  data lt_class_list type lcl_main=>tt_class_names.

  case 'X'.
    when p_disp.
      lv_saver_type = 'D'.
    when p_file.
      lv_saver_type = 'F'.
    when p_code.
      lv_saver_type = 'C'.
  endcase.

  select clsname from seoclasstx
    into table lt_class_list
    where clsname in s_class.

  create object lo_app
    exporting
      i_progname        = p_prog
      i_classes         = lt_class_list
      i_disable_marking = p_womark
      i_path            = |{ p_path }|
      i_saver           = lv_saver_type.
  lo_app->run( ).

endform.

start-of-selection.
  perform main.
