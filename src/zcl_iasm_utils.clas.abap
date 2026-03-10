class ZCL_IASM_UTILS definition
  public
  final
  create public.

  public section.

    class-methods apply_renames
      importing
        it_renames type zif_iasm_types=>ts_renames
      changing
        value(ct_codetab) type string_table.

  protected section.
  private section.
ENDCLASS.



CLASS ZCL_IASM_UTILS IMPLEMENTATION.


  method apply_renames.

    field-symbols <i> like line of ct_codetab.
    field-symbols <r> like line of it_renames.
    data regex type string.

    " basic apprach - just replace full names
    loop at it_renames assigning <r>.
      regex = `\b` && <r>-from && `\b`.
      loop at ct_codetab assigning <i>.
        <i> = replace(
          val   = <i>
          regex = regex
          with  = <r>-to
          case  = abap_false
          occ   = 0 ).
      endloop.
    endloop.

  endmethod.
ENDCLASS.
