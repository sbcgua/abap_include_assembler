**********************************************************************
* SAVERS
**********************************************************************

* SAVE TO DISPLAY -> SHOW
class lcl_saver_to_display definition final.
  public section.
    interfaces zif_iasm_devobj_saver.
endclass.

class lcl_saver_to_display implementation.
  method zif_iasm_devobj_saver~save.
    cl_demo_output=>display( i_codetab ).
  endmethod.
endclass.

* SAVE TO PROGRAM
class lcl_saver_to_program definition final.
  public section.
    interfaces zif_iasm_devobj_saver.
endclass.

class lcl_saver_to_program implementation.
  method zif_iasm_devobj_saver~save.
    if strlen( i_path ) > 40.
      zcx_iasm_error=>raise( 'Program name must be <= 40 symbols' ). "#EC NOTEXT
    endif.

    if i_path is initial.
      zcx_iasm_error=>raise( 'Target program name is empty' ).       "#EC NOTEXT
    endif.

    data l_progname type reposrc-progname.
    select single progname from reposrc into l_progname
      where progname = i_path.

    if sy-subrc is not initial.
      zcx_iasm_error=>raise( |Target program { i_path } must be created manually first| ). "#EC NOTEXT
    endif.

    data lt_codetab type abaptxt255_tab.
    field-symbols <c> like line of lt_codetab.
    field-symbols <s> like line of i_codetab.
    loop at i_codetab assigning <s>.
      append initial line to lt_codetab assigning <c>.
      <c>-line = <s>.
    endloop.

    call function 'RPY_PROGRAM_UPDATE'
      exporting
        program_name     = l_progname
        save_inactive    = 'I'
      tables
        source_extended  = lt_codetab
      exceptions
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        others           = 4.

    IF sy-subrc is not initial.
      if sy-msgid = 'EU' and sy-msgno = '510'.
        zcx_iasm_error=>raise( |Target program { i_path } is being edited by someone else| ). "#EC NOTEXT
      else.
        zcx_iasm_error=>raise( |Cannot update program { i_path }| ). "#EC NOTEXT
      endif.
    endif.

  endmethod.
endclass.

* SAVE TO FILE
class lcl_saver_to_file definition final.
  public section.
    interfaces zif_iasm_devobj_saver.
endclass.

class lcl_saver_to_file implementation.
  method zif_iasm_devobj_saver~save.
    data l_length type i.
    data lt_data  type xml_rawdata.

    if i_path is initial.
      zcx_iasm_error=>raise( 'Path is empty' ).             "#EC NOTEXT
    endif.

    data lt_codetab type abaptxt255_tab.
    field-symbols <c> like line of lt_codetab.
    field-symbols <s> like line of i_codetab.
    loop at i_codetab assigning <s>.
      append initial line to lt_codetab assigning <c>.
      <c>-line = <s>.
    endloop.

    call function 'SCMS_TEXT_TO_BINARY'
      exporting
        encoding      = '4110'
      importing
        output_length = l_length
      tables
        text_tab      = lt_codetab
        binary_tab    = lt_data
      exceptions failed        = 1.

    if sy-subrc is not initial.
      zcx_iasm_error=>raise( 'Cannot convert to binary' ).  "#EC NOTEXT
    endif.

    call method cl_gui_frontend_services=>gui_download
      exporting
        bin_filesize            = l_length
        filename                = i_path
        filetype                = 'BIN'
      changing
        data_tab                = lt_data
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others                  = 24.

    if sy-subrc is not initial.
      zcx_iasm_error=>raise( |Cannot save file ({ sy-subrc })| ). "#EC NOTEXT
    endif.

  endmethod.
endclass.
