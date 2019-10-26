class lcl_include_matcher definition final.
  " I hope I'm not going to far with this parsing thing ... to reconsider RS_GET_ALL_INCLUDES, see #1
  public section.
    methods match_include
      importing
        i_abapline       type string
      returning
        value(r_include) type sobj_name.
  private section.
    data a_codeline        type string.
    data a_lines_collected type i.

    methods collect_line
      importing
        i_abapline   type string.
    methods do_match
      returning
        value(r_include) type sobj_name.
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
    data l_str type string.
    data l_cnt type i.
    data l_off type i.

    l_str = i_abapline.
    add 1 to me->a_lines_collected.

    if l_str is not initial and l_str+0(1) = '*'. " Ignore comments
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

class ltcl_include_matcher_test definition final
  for testing
  duration short
  risk level harmless.
  private section.
    methods match_include for testing.
endclass.

class ltcl_include_matcher_test implementation.
  method match_include.
    data lo_matcher type ref to lcl_include_matcher.
    create object lo_matcher.

    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include z_test1.' )      exp = 'Z_TEST1' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include z_test1' )       exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'inc ude z_test1.' )      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( '*include z_test1.' )     exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include " z_test1.' )    exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include structure Z.' )  exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include type Z.' )       exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include method Z.' )     exp = '' ).

    " multiline
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include " comment' )     exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'z_test1' )               exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( '.' )                     exp = 'Z_TEST1' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'exporting' )             exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( ' include = ' )           exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'ztest1.' )               exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'include' )               exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( '"comment' )              exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( '*comment' )              exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( '' )                      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lo_matcher->match_include( 'z_test1.' )              exp = 'Z_TEST1' ).

  endmethod.
endclass.
