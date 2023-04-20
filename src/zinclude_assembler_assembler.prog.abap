**********************************************************************
* ASSEMBLER CLASS
**********************************************************************

class lcl_assembler definition final.
  public section.
    types:
      begin of ty_params,
        disable_marking type abap_bool,
      end of ty_params.

    methods constructor
      importing
        io_prog type ref to lcl_code_object.
    methods assemble
      importing
        i_params type ty_params optional
      returning
        value(r_codetab) type string_table.
  private section.
    data as_params type ty_params.
    data o_prog    type ref to lcl_code_object.

    methods pad_line
      importing
        i_str type string
      returning
        value(r_str) type string.
    methods _assemble
      importing
        io_prog type ref to lcl_code_object
      returning
        value(r_codetab) type string_table.
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
    data lt_codetab type string_table.
    data l_line     like line of lt_codetab.
    data inc_idx    type i value 1.
    data ls_include type lcl_code_object=>ty_include.

    read table io_prog->at_includes into ls_include index inc_idx. "If fail -> it is OK, lnum = 0

    loop at io_prog->at_codetab into l_line.
      if sy-tabix = ls_include-lnum.
        if ls_include-obj->a_devclass = o_prog->a_devclass.
          data lt_codeinc type string_table.
          data l_tmp   type string.
          lt_codeinc  = _assemble( ls_include-obj ).
          l_line = '*' && l_line.

          define add_marker_line.
            if me->as_params-disable_marking is initial.
              l_tmp = &1.
              replace first occurrence of '{INC}' in l_tmp with ls_include-obj->a_name.
              l_tmp = pad_line( l_tmp ).
              append l_tmp to lt_codetab.
            endif.
          end-of-definition.

          add_marker_line '*%%ASSEMBLY-START @{INC}'.
          append l_line to lt_codetab.
          append lines of lt_codeinc to lt_codetab.
          add_marker_line '*%%ASSEMBLY-END   @{INC}'.

          l_line = ''.
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
    data tmp type string.

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

class ltcl_assembler_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods assemble for testing raising zcx_iasm_error.
endclass.

class ltcl_assembler_test implementation.
  method assemble.
    data lo_prog type ref to lcl_code_object.
    data lo_acc  type ref to ltcl_dummy_extractor.
    data lt_exp  type string_table.

    create object lo_acc.
    lo_prog = lcl_code_object=>load( io_accessor = lo_acc i_progname = 'XTESTPROG' ).
    lt_exp  = lo_acc->zif_iasm_devobj_accessor~get_code( 'ASSEMBLED_RESULT' ).

    data lo_assembler type ref to lcl_assembler.
    create object lo_assembler exporting io_prog = lo_prog.

    data lt_code   type string_table.
    data ls_params type lcl_assembler=>ty_params.
    ls_params-disable_marking = abap_true.
    lt_code = lo_assembler->assemble( ls_params ).

    cl_abap_unit_assert=>assert_equals(
      exp = lt_exp
      act = lt_code ).

  endmethod.
endclass.
