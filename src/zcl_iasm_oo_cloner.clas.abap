class ZCL_IASM_OO_CLONER definition
  public
  final
  create public.

  public section.

    methods constructor
      importing
        i_class type seoclsname
        it_renames type zif_iasm_types=>ts_renames
        i_target_package type devclass
      raising
        zcx_iasm_error.

  protected section.
  private section.

    data m_class type seoclsname.
    data m_class_to type seoclsname.
    data mt_renames type zif_iasm_types=>ts_renames.
    data m_target_package type devclass.

ENDCLASS.



CLASS ZCL_IASM_OO_CLONER IMPLEMENTATION.


  method constructor.

    m_class = i_class.
    mt_renames = it_renames.
    m_target_package = i_target_package.

    data r like line of it_renames.
    read table it_renames into r with key from = i_class.
    if sy-subrc <> 0.
      zcx_iasm_error=>raise( |Cannot rename class { i_class }| ).
    endif.
    m_class_to = r-to.

    assert m_class is not initial.
    assert m_class_to is not initial.
    assert m_target_package is not initial.

  endmethod.
ENDCLASS.
