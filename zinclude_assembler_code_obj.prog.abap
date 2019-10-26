**********************************************************************
* PROGRAMM CODE CLASS ABSTRACTION
**********************************************************************

class lcl_code_object definition create private final.
  public section.
    types:
      begin of ty_include,
        lnum  type i,
        obj   type ref to lcl_code_object,
      end of ty_include.

    data a_name      type sobj_name.
    data at_includes type standard table of ty_include read-only.
    data at_codetab  type string_table read-only.
    data a_devclass  type devclass read-only.

    class-methods load
      importing
        io_accessor type ref to lif_devobj_accessor
        i_progname  type sobj_name
      returning
        value(ro_obj) type ref to lcl_code_object
      raising
        lcx_error.
endclass.


class lcl_code_object implementation.
  method load.
    data lo         type ref to lcl_code_object.
    data lo_ex      type ref to lcx_error.
    data l_line     like line of lo->at_codetab.
    data l_incname  type sobj_name.
    data ls_include type ty_include.

    create object lo.
    lo->at_codetab = io_accessor->get_code( i_progname = i_progname ).
    lo->a_devclass = io_accessor->get_devc( i_progname = i_progname ).
    lo->a_name     = i_progname.

    data lo_matcher type ref to lcl_include_matcher.
    create object lo_matcher.

    loop at lo->at_codetab into l_line.
      clear l_incname.
      ls_include-lnum = sy-tabix.
      l_incname       = lo_matcher->match_include( l_line ).

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

**********************************************************************
* TEST - CODE OBJECT
**********************************************************************

class ltcl_code_object_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods load for testing raising lcx_error.
endclass.

class ltcl_code_object_test implementation.
  method load.
    data lo_obj  type ref to lcl_code_object.
    data lo_acc  type ref to ltcl_dummy_extractor.

    create object lo_acc.

    lo_obj = lcl_code_object=>load( io_accessor = lo_acc i_progname = 'XTESTPROG' ).

    cl_abap_unit_assert=>assert_not_initial( act = lines( lo_obj->at_codetab ) ).
    cl_abap_unit_assert=>assert_not_initial( act = lines( lo_obj->at_includes ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_obj->a_devclass
      exp = 'XTEST' ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_acc->lif_devobj_accessor~get_code( 'XTESTPROG' )
      act = lo_obj->at_codetab ).

    data ls_include type lcl_code_object=>ty_include.
    read table lo_obj->at_includes into ls_include index 1.
    cl_abap_unit_assert=>assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
    cl_abap_unit_assert=>assert_not_initial( act = lines( ls_include-obj->at_includes ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_include-obj->a_devclass
      exp = 'XTEST' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_include-lnum
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_acc->lif_devobj_accessor~get_code( 'XTESTPROG_TOP' )
      act = ls_include-obj->at_codetab ).

    read table lo_obj->at_includes into ls_include index 2.
    cl_abap_unit_assert=>assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
    cl_abap_unit_assert=>assert_initial( act = lines( ls_include-obj->at_includes ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_include-obj->a_devclass
      exp = 'XTEST' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_include-lnum
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_acc->lif_devobj_accessor~get_code( 'XTESTPROG_F01' )
      act = ls_include-obj->at_codetab ).

    read table lo_obj->at_includes into ls_include index 3.
    cl_abap_unit_assert=>assert_not_initial( act = lines( ls_include-obj->at_codetab ) ).
    cl_abap_unit_assert=>assert_initial( act = lines( ls_include-obj->at_includes ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_include-obj->a_devclass
      exp = 'XTEST_EXT' ).

  endmethod.

endclass.
