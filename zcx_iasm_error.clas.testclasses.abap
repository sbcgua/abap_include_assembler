class ltcl_x_test definition for testing
  risk level harmless
  duration short.

  public section.

    methods smoketest for testing.

endclass.

class ltcl_x_test implementation.
  method smoketest.

    data lx type ref to zcx_iasm_error.

    try.
      zcx_iasm_error=>raise( 'Hello' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_iasm_error into lx.
      cl_abap_unit_assert=>assert_char_cp(
        act = lx->msg
        exp = '*Hello*' ).
    endtry.

  endmethod.
endclass.
