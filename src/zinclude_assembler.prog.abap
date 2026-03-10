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
      ts_deps type sorted table of ty_deps with unique key depends from.

    data:
      m_renames         type zif_iasm_types=>ts_renames,
      m_progname        type sobj_name,
      m_classes         type zif_iasm_types=>ts_class_names,
      m_disable_marking type abap_bool,
      m_path            type string,
      m_saver           type char1.

    methods constructor
      importing
        i_progname        type sobj_name
        i_classes         type zif_iasm_types=>tt_class_names
        i_disable_marking type abap_bool
        i_path            type string
        i_saver           type char1
        i_rename_from     type seoclasstx-clsname
        i_rename_to       type seoclasstx-clsname.

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

    class-methods order_classes_by_dep
      importing
        i_classes type zif_iasm_types=>ts_class_names
      returning
        value(r_ordered_classes) type zif_iasm_types=>tt_class_names
      raising
        zcx_iasm_error.

    class-methods get_class_dependencies
      importing
        i_class type seoclasstx-clsname
      returning
        value(rt_env) type senvi_tab
      raising
        zcx_iasm_error.

    class-methods check_dep_cycles
      importing
        i_deps    type ts_deps
      raising
        zcx_iasm_error.

    class-methods sort_classes_by_deps
      importing
        i_classes type zif_iasm_types=>ts_class_names
        i_deps    type ts_deps
      returning
        value(r_ordered_classes) type zif_iasm_types=>tt_class_names
      raising
        zcx_iasm_error.

endclass.

class lcl_main implementation.

  method constructor.
    m_progname        = i_progname.
    m_classes         = i_classes.
    m_disable_marking = i_disable_marking.
    m_path            = i_path.
    m_saver           = i_saver.

    if i_rename_from is not initial and i_rename_to is not initial.
      field-symbols <i> like line of m_classes.
      data r like line of m_renames.
      loop at m_classes assigning <i>.
        r-from = to_lower( <i> ).
        r-to   = to_lower( replace( val = <i> sub = i_rename_from with = i_rename_to ) ).
        insert r into table m_renames.
      endloop.
    endif.

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

  method get_class_dependencies.

    data ls_env_types type envi_types.
    data lv_name type tadir-obj_name.
    data lv_type type euobj-id.
    data lo_type type ref to cl_abap_typedescr.

    lv_name = i_class.
    ls_env_types-clas = abap_true.
*    ls_env_types-intf = abap_true. " ???
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
        obj_type          = lv_type
        object_name       = lv_name
        environment_types = ls_env_types
      tables
        environment       = rt_env
      exceptions
        others            = 4.

    " TODO check RC ?

  endmethod.

  method order_classes_by_dep.

    " TODO extract to an independent class

    data lt_deps type ts_deps.

    field-symbols <c> like line of i_classes.

    loop at i_classes assigning <c>.

      data lv_search_key type seoclasstx-clsname.
      data lt_env type senvi_tab.
      data ls_dep like line of lt_deps.
      field-symbols <env> like line of lt_env.

      lt_env = get_class_dependencies( <c> ).

      loop at lt_env assigning <env>.
        check <env>-type = 'CLAS' or <env>-type = 'INTF' or <env>-type = 'OM'.
        if <env>-type = 'CLAS' or <env>-type = 'INTF'.
          lv_search_key = <env>-object.
        elseif <env>-type = 'OM'.
          lv_search_key = <env>-encl_obj.
        else.
          continue.
        endif.
        read table i_classes with key table_line = lv_search_key transporting no fields.
        if sy-subrc = 0.
          ls_dep-depends = <c>.
          ls_dep-from    = lv_search_key.
          insert ls_dep into table lt_deps. " Sorted table prevents duplicates !
        endif.
      endloop.

*      data lt_tadir type if_ris_environment_types=>ty_t_senvi_tadir.
*      cl_wb_ris_environment=>convert_senvi_to_tadir(
*        exporting
*          senvi       = lt_env
*        importing
*          senvi_tadir = lt_tadir ).

    endloop.

    check_dep_cycles( lt_deps ).

    r_ordered_classes = sort_classes_by_deps(
      i_classes = i_classes
      i_deps    = lt_deps ).

  endmethod.

  method check_dep_cycles.

    field-symbols <dep> like line of i_deps.

    " Protection from self cycle
    " For now just 1 level deps

    loop at i_deps assigning <dep>.
      read table i_deps
        transporting no fields
        with key
          depends = <dep>-from
          from    = <dep>-depends.
      if sy-subrc = 0.
        zcx_iasm_error=>raise( |Class self cycle detected: { <dep>-from } on { <dep>-depends }| ).
      endif.
    endloop.

  endmethod.

  method sort_classes_by_deps.

    data lt_unordered_classes type zif_iasm_types=>tt_class_names.
    data lt_deps like i_deps.
    data l_index type i.
    data l_control_count type i.

    field-symbols <c> like line of lt_unordered_classes.

    lt_deps = i_deps.
    lt_unordered_classes = i_classes.

    while lines( lt_unordered_classes ) > 0.

      l_control_count = lines( lt_unordered_classes ).

      loop at lt_unordered_classes assigning <c>.
        l_index = sy-tabix.
        read table lt_deps
          transporting no fields
          with key depends = <c>.
        " If no deps -> add it to the queue
        if sy-subrc <> 0.
          append <c> to r_ordered_classes.
          delete lt_deps where from = <c>.
          delete lt_unordered_classes index l_index.
        endif.
      endloop.

      " watchdog: if nothing changed after the iteration - something is wrong - cyclic deps?
      if l_control_count = lines( lt_unordered_classes ).
        field-symbols <dep> like line of lt_deps.
        loop at lt_deps assigning <dep>.
          write: <dep>-depends, 'from', <dep>-from.
        endloop.
        zcx_iasm_error=>raise( |Cannot reduce dependencies further| ).
      endif.

    endwhile.

  endmethod.

  method process_clas.

    data lt_ordered_classes type zif_iasm_types=>tt_class_names.
    lt_ordered_classes = order_classes_by_dep( m_classes ).

    " Serialize

    data lt_code like rt_codetab.
    data lo_accessor type ref to lcl_extractor_clas.
    data lv_marker type string.
    field-symbols <c> like line of lt_ordered_classes.

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
      zcl_iasm_utils=>apply_renames(
        exporting it_renames = m_renames
        changing  ct_codetab = lt_code ).
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
* CLONER APP
**********************************************************************

include zinclude_assembler_cloner_app.

**********************************************************************
* SELECTION SCREEN
**********************************************************************

tables: seoclasstx, trdir.

selection-screen begin of block b1 with frame title txt_b1.

select-options s_prog for trdir-name.
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
selection-screen comment (24) txt_copy  for field p_copy.
parameters p_copy type char1 radiobutton group r1.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (24) txt_path  for field p_path  modif id pth.
parameters p_path type char255                            modif id pth.
selection-screen end of line.

selection-screen end of block b2.

selection-screen begin of block b3 with frame title txt_b3.

selection-screen begin of line.
selection-screen comment (24) txt_rena  for field p_ren_f modif id cop.
parameters p_ren_f type char20 modif id cop.
parameters p_ren_t type char20 modif id cop.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (21) txt_rx1 for field s_renx1 modif id cop.
select-options s_renx1 for trdir-name no intervals modif id cop.
selection-screen comment 50(18) txt_rx2 for field s_renx2 modif id cop.
select-options s_renx2 for trdir-name no intervals lower case modif id cop.
selection-screen end of line.

selection-screen end of block b3.

*at selection-screen output.
*  loop at screen.
*    if screen-group1 = 'COP'.
*      if p_copy = 'X'.
*        screen-active = 1.
*      else.
*        screen-active = 0.
*      endif.
*    endif.
*    modify screen.
*  endloop.

*at selection-screen on radiobutton group r1.

initialization.
  txt_b1   = 'Source program'.          "#EC NOTEXT

  txt_b2   = 'Save parameters'.         "#EC NOTEXT
  txt_disp = 'Show on display'.         "#EC NOTEXT
  txt_file = 'Save to file'.            "#EC NOTEXT
  txt_code = 'Save to target program'.  "#EC NOTEXT
  txt_copy = 'Copy to package'.         "#EC NOTEXT
  txt_path = 'Target (File/Prog/Pkg)'.  "#EC NOTEXT

  txt_b3   = 'Copy options'.
  txt_rena = 'Rename'.                  "#EC NOTEXT'
  txt_rx1  = 'Extra renames'.            "#EC NOTEXT
  txt_rx2  = 'match (case sens.)'.               "#EC NOTEXT

  " TODO normal parameters show/hide and file/prog-search

**********************************************************************
* ENTRY POINT
**********************************************************************
form main.

  data lv_saver_type type c length 1.
  data lt_class_list type zif_iasm_types=>tt_class_names.
  data lt_prog_list type zif_iasm_types=>tt_prog_names.
  data lv_1st_prog like line of lt_prog_list.
  data lx type ref to zcx_iasm_error.
  data lt_extra_renames type zif_iasm_types=>tt_renames.

  field-symbols <r> like line of lt_extra_renames.

  case 'X'.
    when p_disp.
      lv_saver_type = 'D'.
    when p_file.
      lv_saver_type = 'F'.
    when p_code.
      lv_saver_type = 'C'.
  endcase.

  if s_class[] is not initial.
    select clsname from seoclasstx
      into table lt_class_list
      where clsname in s_class.
  endif.

  if s_prog[] is not initial.
    select name from trdir
      into table lt_prog_list
      where name in s_prog.
    read table lt_prog_list into lv_1st_prog index 1.
  endif.

  if s_renx1[] is not initial.
    if lines( s_renx1[] ) <> lines( s_renx2[] ).
      message 'extra renames must have same number of records' type 'E' display like 'S'.
      return.
    endif.
    loop at s_renx1.
      read table s_renx2 index sy-tabix.
      append initial line to lt_extra_renames assigning <r>.
      <r>-from = to_lower( s_renx1-low ).
      <r>-to   = s_renx2-low.
    endloop.
  endif.

  try.
    if p_copy = 'X'.
      data lo_app_cloner type ref to lcl_cloner_app.
      create object lo_app_cloner
        exporting
          i_progs           = lt_prog_list
          i_classes         = lt_class_list
          i_target_pkg      = |{ p_path }|
          i_rename_from     = |{ p_ren_f }|
          i_rename_to       = |{ p_ren_t }|
          i_extra_renames   = lt_extra_renames.
      lo_app_cloner->run( ).
    else.
      data lo_app type ref to lcl_main.
      create object lo_app
        exporting
          i_progname        = lv_1st_prog
          i_classes         = lt_class_list
          i_disable_marking = p_womark
          i_path            = |{ p_path }|
          i_saver           = lv_saver_type
          i_rename_from     = |{ p_ren_f }|
          i_rename_to       = |{ p_ren_t }|.
      lo_app->run( ).
    endif.

  catch zcx_iasm_error into lx.
    message lx->msg type 'S' display like 'E'.
  endtry.

endform.

start-of-selection.
  perform main.
